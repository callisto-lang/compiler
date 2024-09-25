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

	this() {
		structs["Array"]     = ["length", "memberSize", "elements"];
		structs["Exception"] = ["error", "msg"];

		foreach (key, structure ; structs) {
			foreach (ref member ; structure) {
				identifiers ~= format("%s.%s", key, member);
			}
		}
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
		else if (identifiers.canFind(node.name)) {
			Push(node, 1);
		}
		else {
			Error(node.error, "Unknown word '%s'", node.name);
		}
	}

	void EvaluateFuncDef(FuncDefNode node) {
		words[node.name] = Word(Effect(node.returnTypes.length, node.params.length));

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

		Evaluate(node.nodes);

		if (stack.length > node.returnTypes.length) {
			StackOverflow(node, node.returnTypes.length);
		}

		stack       = oldStack;
		identifiers = oldIdentifiers;
	}

	void EvaluateIf(IfNode node) {
		size_t blockStack;

		if (node.hasElse) {
			auto oldStack = stack.dup;
			Evaluate(node.doElse);
			blockStack = stack.length;
			stack      = oldStack;
		}

		foreach (i, ref cond ; node.condition) {
			auto oldStack = stack.dup;

			Node conditionNode = node.condition.empty()? node : cond[$ - 1];

			Evaluate(cond);
			Pop(conditionNode, 1);
			Evaluate(node.doIf[i]);

			if ((i == 0) && !node.hasElse) {
				blockStack = stack.length;
			}
			else if (stack.length != blockStack) {
				Node errorNode = node.doIf[i].empty()?
					node : node.doIf[i][$ - 1];

				Note(errorNode.error, "Expected stack length '%d', got '%d'", blockStack, stack.length);
				Error(errorNode.error, "If statement has inconsistent stack effects");

				if (stack.length > blockStack) {
					StackOverflow(errorNode, blockStack);
				}
			}

			stack = oldStack;
		}
	}

	void EvaluateWhile(WhileNode node) {
		auto oldStack = stack.dup;

		if (node.condition.empty()) {
			Error(node.error, "Empty while condition");
		}

		Evaluate(node.condition);
		Pop(node.condition[$ - 1], 1);

		size_t oldSize = stack.length;
		Evaluate(node.doWhile);

		if (oldSize != stack.length) {
			if (stack.length > oldSize) {
				StackOverflow(node.doWhile[$ - 1], oldSize);
			}
			else {
				Error(node.doWhile[$ - 1].error, "Stack underflow");
			}
		}

		stack = oldStack;
	}

	void EvaluateDef(LetNode node) {
		identifiers ~= node.name;
	}
	
	void EvaluateDef(ConstNode node) {
		identifiers ~= node.name;
	}

	void EvaluateExtern(ExternNode node) {
		string name = node.asName == ""? node.func : node.asName;

		if (node.externType == ExternType.C) {
			words[name] = Word(
				Effect(node.retType == "void"? 0 : 1, node.types.length)
			);
		}
		else {
			words[name] = Word(Effect.init, false);
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

		stack = oldStack;
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
	}

	void EvaluateNode(Node node) {
		switch (node.type) {
			case NodeType.Word:    EvaluateWord(cast(WordNode) node); break;
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
			default: break;
		}
	}

	void Evaluate(Node[] nodes) {
		foreach (ref node ; nodes) {
			EvaluateNode(node);
		}
	}
}
