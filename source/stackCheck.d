module callisto.stackCheck;

import std.conv;
import std.range;
import std.stdio;
import std.format;
import std.algorithm;
import callisto.error;
import callisto.parser;

struct Effect {
	size_t push;
	size_t pop;
}

struct Word {
	bool   manual;
	Effect effect;
	bool   unsafe;
}

struct StackCell {
	Node node;
}

class StackCheckerError : Exception {
	this() {
		super("", "", 0);
	}
}

class StackChecker {
	Word[string]     words;
	StackCell[]      stack;
	string[]         identifiers;
	string[][string] structs;
	size_t[]         whileStacks;
	string[]         types;
	string           thisFunc;

	this() {
		structs["Array"]     = ["length", "memberSize", "elements"];
		structs["Exception"] = ["error", "msg"];

		types = [
			"cell", "bool", "addr", "size", "usize",
			"u8", "u16", "u32", "u64",
			"i8", "i16", "i32", "i64",
			"Array", "Exception"
		];

		foreach (ref type ; types) {
			identifiers ~= format("%s.sizeOf", type);
		}

		foreach (key, structure ; structs) {
			foreach (ref member ; structure) {
				identifiers ~= format("%s.%s", key, member);
			}
		}

		identifiers ~= ["true", "false"];
	}

	void ErrorNoThrow(Char, A...)(ErrorInfo error, in Char[] fmt, A args) {
		ErrorBegin(error);
		stderr.writeln(format(fmt, args));
		PrintErrorLine(error);
	}

	void Error(Char, A...)(ErrorInfo error, in Char[] fmt, A args) {
		ErrorBegin(error);
		stderr.writeln(format(fmt, args));
		PrintErrorLine(error);
		throw new StackCheckerError();
	}

	void Note(Char, A...)(ErrorInfo error, in Char[] fmt, A args) {
		NoteBegin(error);
		stderr.writeln(format(fmt, args));
		PrintErrorLine(error);
	}

	void StackOverflow(Node node, size_t start) {
		ErrorNoThrow(node.error, "Stack overflow");

		for (size_t i = start; i < stack.length; ++ i) {
			auto   cell = stack[i];
			string cellStr;

			switch (cell.node.type) {
				case NodeType.Integer:
				case NodeType.Word:
				case NodeType.Array:
				case NodeType.String:
				case NodeType.Addr:
				case NodeType.Set: cellStr = cell.node.toString(); break;
				default:           cellStr = text(cell.node.type); break;
			}

			stderr.writefln(
				" '%s' -> %s:%d:%d", cellStr, cell.node.error.file,
				cell.node.error.line + 1, cell.node.error.col + 1
			);
		}

		throw new StackCheckerError();
	}

	void Push(Node node, size_t n) {
		foreach (i ; 0 .. n) {
			stack ~= StackCell(node);
		}
	}

	void Pop(Node node, size_t n) {
		foreach (i ; 0 .. n) {
			if (stack.length == 0) {
				Error(node.error, "Stack underflow");
				return;
			}

			stack = stack[0 .. $ - 1];
		}
	}

	void EvaluateWord(WordNode node) {
		if (node.name in words) {
			auto word = words[node.name];

			Pop(node, word.effect.pop);
			Push(node, word.effect.push);
		}
		else if (
			identifiers.canFind(node.name) ||
			identifiers.canFind(node.name.split('.')[0])
		) {
			Push(node, 1);
		}
		else switch (node.name) {
			case "throw": {
				Pop(node, 1);
				break;
			}
			default: Error(node.error, "Unknown word '%s'", node.name);
		}
	}

	void EvaluateFuncDef(FuncDefNode node) {
		words[node.name] = Word(
			node.manual, Effect(node.returnTypes.length, node.params.length)
		);

		if (node.unsafe) return;

		auto oldStack = stack;
		stack = [];
		auto oldIdentifiers = identifiers.dup;

		if (node.manual) {
			Push(node, node.params.length);
		}
		else {
			foreach (ref param ; node.params) {
				identifiers ~= param;
			}
		}

		thisFunc = node.name;
		Evaluate(node.nodes);

		if (stack.length > node.returnTypes.length) {
			StackOverflow(node, node.returnTypes.length);
		}

		stack       = oldStack;
		identifiers = oldIdentifiers;
	}

	private void EvaluateSingleIf(IfNode node) {
		auto oldStack = stack.dup;
		Evaluate(node.condition[0]);

		Pop(node, 1);
		oldStack = stack.dup;

		Evaluate(node.doIf[0]);

		if (oldStack.length != stack.length) {
			Error(node.error, "Single if condition must have a body with no stack effect");
		}
	}

	void EvaluateIf(IfNode node) {
		bool   setExpect = false;
		size_t expectSize;
		string note;
		bool   allowCondPop = false;

		if (!node.hasElse && (node.condition.length == 1)) {
			EvaluateSingleIf(node);
			return;
			// allowCondPop = true;
			// setExpect    = true;
			// note         = "Single if condition must have a body with no stack effect";
		}
		else if (node.hasElse) {
			auto oldStack = stack.dup;

			Evaluate(node.doElse);
			expectSize = stack.length;
			stack      = oldStack;
			setExpect  = true;

			if (node.condition.length == 1) {
				allowCondPop = true;
				setExpect    = false;
				note         = "Single if condition must have same stack effect as else block";
			}
			else {
				note = "If blocks must have same stack effect as else block";
			}
		}
		else {
			note = "Inconsistent stack effect in if statement";
		}

		foreach (i, ref cond ; node.condition) {
			//if (cond.empty()) {
			//	Error(node.error, "Empty condition in if statement");
			//}

			StackCell[] oldStack;

			if (allowCondPop) {
				Evaluate(cond);
				Pop(node, 1);
				oldStack = stack.dup;

				if (!setExpect) {
					expectSize = stack.length;
				}
			}
			else {
				oldStack = stack.dup;
				Evaluate(cond);
				Pop(node, 1);
			}

			Evaluate(node.doIf[i]);

			Node errorNode = node.doIf[i].empty()? node : node.doIf[i][$ - 1];

			if (!setExpect) {
				expectSize = stack.length;
				setExpect  = true;
			}

			if (stack.length > expectSize) {
				Note(errorNode.error, note);
				Note(errorNode.error, "Expected stack size %d, got %d", expectSize, stack.length);
				StackOverflow(errorNode, expectSize);
			}
			else if (stack.length < expectSize) {
				Note(errorNode.error, note);
				Note(errorNode.error, "Expected stack size %d, got %d", expectSize, stack.length);
				Error(errorNode.error, "Stack underflow");
			}

			stack = oldStack;
		}

		if (expectSize > stack.length) {
			Push(node, expectSize - stack.length);
		}
		else if (expectSize < stack.length) {
			Pop(node, stack.length - expectSize);
		}
	}

	void EvaluateWhile(WhileNode node) {
		auto oldStack = stack.dup;

		if (node.condition.empty()) {
			Error(node.error, "Empty while condition");
		}

		Evaluate(node.condition);
		Pop(node.condition[$ - 1], 1);

		size_t oldSize  = stack.length;
		whileStacks    ~= stack.length;
		Evaluate(node.doWhile);

		if (oldSize != stack.length) {
			if (stack.length > oldSize) {
				StackOverflow(node.doWhile[$ - 1], oldSize);
			}
			else {
				Error(node.doWhile[$ - 1].error, "Stack underflow");
			}
		}

		whileStacks = whileStacks[0 .. $ - 1];

		stack = oldStack;
	}

	void EvaluateBreakContinue(WordNode node) {
		if (whileStacks.length == 0) {
			Error(node.error, "Not in while loop");
		}

		if (stack.length > whileStacks[$ - 1]) {
			StackOverflow(node, whileStacks[$ - 1]);
		}
		else if (stack.length < whileStacks[$ - 1]) {
			Error(node.error, "Stack underflow");
		}
	}

	void EvaluateDef(LetNode node) {
		identifiers ~= node.name;
	}
	
	void EvaluateDef(ConstNode node) {
		identifiers ~= node.name;
	}

	void EvaluateExtern(ExternNode node) {
		string name = node.asName == ""? node.func : node.asName;

		if (name in words) {
			Error(node.error, "Word '%s' already exists", name);
		}

		if (node.externType == ExternType.C) {
			words[name] = Word(
				false, Effect(
					((node.retType.name == "void") && !node.retType.ptr)? 0 : 1,
					node.types.length
				)
			);
		}
		else {
			words[name] = Word(false, Effect.init, false);
		}
	}

	void EvaluateImplement(ImplementNode node) {
		auto oldStack = stack;
		stack         = [StackCell(node)];

		Evaluate(node.nodes);

		if (!stack.empty()) {
			Note(node.error, "Implement block must leave the stack empty");
			StackOverflow(node, 0);
		}

		stack = oldStack;
	}

	void EvaluateTryCatch(TryCatchNode node) {
		if (node.func !in words) {
			Error(node.error, "Unknown function '%s'", node.func);
		}

		auto word = words[node.func];

		assert(!word.unsafe);
		Pop(node, word.effect.pop);

		auto oldStack = stack;
		stack         = [];

		ptrdiff_t successRes = stack.length + word.effect.push;

		Evaluate(node.catchBlock);

		if (stack.length != successRes) {
			Note(
				node.error,
				"Catch block must have same stack effect as the called function"
			);
			if (cast(ptrdiff_t) stack.length > successRes) {
				StackOverflow(node.catchBlock[$ - 1], cast(size_t) successRes);
			}
			else {
				auto error = node.catchBlock.empty()?
					node.error : node.catchBlock[$ - 1].error;

				Error(node.catchBlock[$ - 1].error, "Stack underflow");
			}
		}

		size_t newLength = stack.length;
		stack = oldStack;

		foreach (i ; 0 .. newLength) {
			stack ~= StackCell(node);
		}
	}

	void EvaluateStruct(StructNode node) {
		string[] structure;
		foreach (ref member ; node.members) {
			structure ~= member.name;
		}

		if (node.name in structs) {
			Error(node.error, "Structure '%s' already exists", node.name);
		}

		structs[node.name] = structure;

		foreach (ref member ; structure) {
			identifiers ~= format("%s.%s", node.name, member);
		}

		identifiers ~= format("%s.sizeOf", node.name);
	}

	void EvaluateEnum(EnumNode node) {
		identifiers ~= format("%s.sizeOf", node.name);
		identifiers ~= format("%s.min", node.name);
		identifiers ~= format("%s.max", node.name);

		foreach (ref value ; node.names) {
			identifiers ~= format("%s.%s", node.name, value);
		}
	}

	void EvaluateUnion(UnionNode node) {
		identifiers ~= format("%s.sizeOf", node.name);
	}

	void EvaluateAlias(AliasNode node) {
		identifiers ~= format("%s.sizeOf", node.to);
	}

	void EvaluateUnsafe(UnsafeNode node) {
		Pop(node, node.paramTypes.length);
		Push(node, node.retTypes.length);
	}

	void EvaluateNode(Node node) {
		switch (node.type) {
			case NodeType.Word: {
				auto wnode = cast(WordNode) node;

				switch (wnode.name) {
					case "return":   Pop(node, words[thisFunc].effect.push); break;
					case "continue": EvaluateBreakContinue(wnode); break;
					case "break":    EvaluateBreakContinue(wnode); break;
					case "call":     Error(node.error, "Call is unsafe"); break;
					case "throw":    Pop(node, 1); break;
					case "error":    Error(node.error, "Error thrown by code"); break;
					default:         EvaluateWord(wnode);
				}
				break;
			}
			case NodeType.Integer: Push(node, 1); break;
			case NodeType.FuncDef: EvaluateFuncDef(cast(FuncDefNode) node); break;
			case NodeType.Asm: {
				Error(node.error, "Inline assembly can only be used in unsafe code");
				break;
			}
			case NodeType.If:        EvaluateIf(cast(IfNode) node); break;
			case NodeType.While:     EvaluateWhile(cast(WhileNode) node); break;
			case NodeType.Let:       EvaluateDef(cast(LetNode) node); break;
			case NodeType.Array:     Push(node, 1); break;
			case NodeType.String:    Push(node, 1); break;
			case NodeType.Const:     EvaluateDef(cast(ConstNode) node); break;
			case NodeType.Extern:    EvaluateExtern(cast(ExternNode) node); break;
			case NodeType.Addr:      Push(node, 1); break;
			case NodeType.Implement: EvaluateImplement(cast(ImplementNode) node); break;
			case NodeType.Set:       Pop(node, 1); break;
			case NodeType.TryCatch:  EvaluateTryCatch(cast(TryCatchNode) node); break;
			case NodeType.Struct:    EvaluateStruct(cast(StructNode) node); break;
			case NodeType.Enum:      EvaluateEnum(cast(EnumNode) node); break;
			case NodeType.Union:     EvaluateUnion(cast(UnionNode) node); break;
			case NodeType.Alias:     EvaluateAlias(cast(AliasNode) node); break;
			case NodeType.Unsafe:    EvaluateUnsafe(cast(UnsafeNode) node); break;
			case NodeType.Anon:      Push(node, 1); break;
			default: break;
		}
	}

	void Evaluate(Node[] nodes) {
		foreach (ref node ; nodes) {
			EvaluateNode(node);
		}
	}

	void DumpFunctions() {
		size_t spaces = words.keys().fold!((a, e) => e.length > a.length? e : a).length;

		auto keys = words.keys().sort();

		foreach (key ; keys) {
			auto value = words[key];

			writefln(
				"%s %s= %d %s-> %d", key, replicate([' '], spaces - key.length),
				value.effect.pop, value.manual? "m" : "", value.effect.push
			);
		}
	}
}
