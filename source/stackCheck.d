module callisto.stackCheck;

import std.conv;
import std.range;
import std.stdio;
import std.format;
import std.algorithm;
import callisto.error;
import callisto.parser;
import callisto.profiler;
import callisto.preprocessor;
import callisto.mod.sections;

struct Effect {
	size_t push;
	size_t pop;
}

struct Word {
	string mod;
	string name;
	bool   manual;
	Effect effect;
	bool   unsafe;
}

struct Identifier {
	string mod;
	string name;
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
	string           mod;
	Word[]           words;
	StackCell[]      stack;
	Identifier[]     identifiers;
	string[]         locals;
	string[][string] structs; // never used? TODO
	size_t[]         whileStacks;
	string[]         types;
	string           thisFunc;
	Preprocessor     preproc;
	bool             inScope;
	Profiler         profiler;

	this() {
		structs["Array"]     = ["length", "memberSize", "elements"];
		structs["Exception"] = ["error", "msg"];

		types = [
			"cell", "icell", "bool", "addr", "isize", "usize",
			"u8", "u16", "u32", "u64",
			"i8", "i16", "i32", "i64",
			"Array", "Exception"
		];

		foreach (ref type ; types) {
			identifiers ~= Identifier("core", format("%s.sizeOf", type));
		}

		foreach (key, structure ; structs) {
			foreach (ref member ; structure) {
				identifiers ~= Identifier("core", format("%s.%s", key, member));
			}
		}

		// TODO: do this depending on backend
		// doesn't matter much because if you misuse it then it will error on
		// the compiler stage
		identifiers ~= Identifier("core", "__LUA_EXCEPTION");
		identifiers ~= [Identifier("core", "true"), Identifier("core", "false")];
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

	bool MatchMod(string thisMod, string thisName, string toMatch) {
		return (thisName == toMatch) || (format("%s.%s", thisMod, thisName) == toMatch);
	}

	size_t CountWords(string name) {
		size_t ret = 0;

		foreach (ref word ; words) {
			if (MatchMod(word.mod, word.name, name)) ++ ret;
		}

		return ret;
	}

	size_t CountAll(string name) {
		size_t ret = CountWords(name);

		foreach (ref ident ; identifiers) {
			if (MatchMod(ident.mod, ident.name, name)) ++ ret;
		}

		return ret;
	}

	Word GetWord(string name) {
		foreach (ref word ; words) {
			if (MatchMod(word.mod, word.name, name)) return word;
		}

		assert(0);
	}

	bool WordExists(string name) {
		return words.any!(v => MatchMod(v.mod, v.name, name));
	}

	bool WordExistsHere(string name) {
		return words.any!(v => (v.mod == mod) && (v.name == name));
	}

	bool FullWordExists(string name) {
		return words.any!(v => format("%s.%s", v.mod, v.name) == name);
	}

	string[] WordMatches(string name) {
		string[] ret;

		foreach (ref word ; words) {
			if (MatchMod(word.mod, word.name, name)) ret ~= word.name;
		}

		return ret;
	}

	bool IdentifierExists(string name) {
		return identifiers.any!(v => MatchMod(v.mod, v.name, name));
	}

	bool IdentifierPrefExists(string name) {
		foreach (ref ident ; identifiers) {
			if (
				name.startsWith(ident.name ~ '.') ||
				name.startsWith(format("%s.%s.", ident.mod, ident.name))
			) {
				return true;
			}
		}

		return false;
	}

	void EvaluateWord(WordNode node) {
		/*size_t count = CountAll(node.name);
		if (count > 1) {
			Error(node.error, "Multiple matches for identifier '%s'", node.name);
		}
		else {
			foreach (ref word ; words) {
				writefln("%s.%s", word.mod, word.name);
			}

			Error(node.error, "Unknown word '%s'", node.name);
		}*/

		if (WordExists(node.name)) {
			auto word = GetWord(node.name);

			Pop(node, word.effect.pop);
			Push(node, word.effect.push);
		}
		else if (
			IdentifierExists(node.name) || IdentifierExists(node.name.split('.')[0]) ||
			IdentifierPrefExists(node.name) || locals.canFind(node.name) ||
			locals.canFind(node.name.split('.')[0])
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
		words ~= Word(
			mod, node.name, node.manual,
			Effect(node.returnTypes.length, node.params.length)
		);

		if (node.unsafe) return;

		auto oldStack = stack;
		stack   = [];
		locals  = [];
		inScope = true;

		if (node.manual) {
			Push(node, node.params.length);
		}
		else {
			foreach (ref param ; node.params) {
				locals ~= param;
			}
		}

		thisFunc = mod ~ '.' ~ node.name;
		Evaluate(node.nodes);

		if (stack.length > node.returnTypes.length) {
			StackOverflow(node, node.returnTypes.length);
		}

		stack   = oldStack;
		locals  = [];
		inScope = false;
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
		if (inScope) {
			locals ~= node.name;
		}
		else {
			identifiers ~= Identifier(mod, node.name);
		}
	}
	
	void EvaluateDef(ConstNode node) {
		identifiers ~= Identifier(mod, node.name);
	}

	void EvaluateExtern(ExternNode node) {
		string name = node.asName == ""? node.func : node.asName;

		if (WordExists(name)) {
			Error(node.error, "Word '%s' already exists", name);
		}

		if (node.externType == ExternType.C) {
			words ~= Word(
				mod, name,
				false, Effect(
					((node.retType.name == "void") && !node.retType.ptr)? 0 : 1,
					node.types.length
				)
			);
		}
		else {
			words ~= Word(mod, name, false, Effect.init, false);
		}
	}

	void EvaluateImplement(ImplementNode node) {
		auto oldStack = stack;
		stack         = [StackCell(node)];
		locals        = [];
		inScope       = true;

		Evaluate(node.nodes);

		if (!stack.empty()) {
			Note(node.error, "Implement block must leave the stack empty");
			StackOverflow(node, 0);
		}

		stack   = oldStack;
		locals  = [];
		inScope = false;
	}

	void EvaluateTryCatch(TryCatchNode node) {
		if (!WordExists(node.func)) {
			Error(node.error, "Unknown function '%s'", node.func);
		}

		auto word = GetWord(node.func);

		assert(!word.unsafe); // TODO: what?
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

	// TODO: doesn't this need to handle inheritance??
	// also check this with the import struct version
	void EvaluateStruct(StructNode node) {
		string[] structure;
		foreach (ref member ; node.members) {
			structure ~= member.name;
		}

		structs[node.name] = structure;

		foreach (ref member ; structure) {
			identifiers ~= Identifier(mod, format("%s.%s", node.name, member));
		}

		identifiers ~= Identifier(mod, format("%s.sizeOf", node.name));
	}

	void EvaluateEnum(EnumNode node) {
		identifiers ~= Identifier(mod, format("%s.sizeOf", node.name));
		identifiers ~= Identifier(mod, format("%s.min", node.name));
		identifiers ~= Identifier(mod, format("%s.max", node.name));

		foreach (ref value ; node.names) {
			identifiers ~= Identifier(mod, format("%s.%s", node.name, value));
		}
	}

	void EvaluateUnion(UnionNode node) {
		identifiers ~= Identifier(mod, format("%s.sizeOf", node.name));
	}

	void EvaluateAlias(AliasNode node) {
		identifiers ~= Identifier(mod, format("%s.sizeOf", node.to));
	}

	void EvaluateUnsafe(UnsafeNode node) {
		Pop(node, node.paramTypes.length);
		Push(node, node.retTypes.length);
	}

	void EvaluateReturn(WordNode node) {
		Pop(node, GetWord(thisFunc).effect.push);

		if (stack.length > 0) {
			StackOverflow(node, 0);
		}
	}

	void EvaluateNode(Node node) {
		switch (node.type) {
			case NodeType.Word: {
				auto wnode = cast(WordNode) node;

				switch (wnode.name) {
					case "return":   EvaluateReturn(wnode); break;
					case "continue": EvaluateBreakContinue(wnode); break;
					case "break":    EvaluateBreakContinue(wnode); break;
					case "call":     Error(node.error, "Call is unsafe"); break;
					case "throw":    Pop(node, 1); break;
					case "error":    Error(node.error, "Error thrown by code"); break;
					default:         EvaluateWord(wnode);
				}
				break;
			}
			case NodeType.SignedInt: Push(node, 1); break;
			case NodeType.Integer:   Push(node, 1); break;
			case NodeType.FuncDef:   EvaluateFuncDef(cast(FuncDefNode) node); break;
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

	void Run(Node[] nodes) {
		// import summary
		if (mod != "") {
			profiler.Begin();

			foreach (ref isect ; preproc.summary.sections) {
				switch (isect.GetType()) {
					case SectionType.FuncDef: {
						auto sect = cast(FuncDefSection) isect;

						words ~= Word(
							sect.inMod, sect.name, false, // manual, not really needed?
							Effect(sect.ret, sect.params),
							false // again, not really needed, TODO
						);
						break;
					}
					case SectionType.Const: {
						auto sect = cast(ConstSection) isect;
						identifiers ~= Identifier(sect.inMod, sect.name);
						break;
					}
					case SectionType.Enum: {
						auto sect = cast(EnumSection) isect;

						identifiers ~= Identifier(
							sect.inMod, format("%s.sizeOf", sect.name)
						);
						identifiers ~= Identifier(
							sect.inMod, format("%s.min", sect.name)
						);
						identifiers ~= Identifier(
							sect.inMod, format("%s.max", sect.name)
						);

						foreach (ref value ; sect.entries) {
							identifiers ~= Identifier(
								sect.inMod, format("%s.%s", sect.name, value.name)
							);
						}
						break;
					}
					case SectionType.Union: {
						auto sect = cast(UnionSection) isect;

						identifiers ~= Identifier(mod, format("%s.sizeOf", sect.name));
						break;
					}
					case SectionType.Alias: {
						auto sect = cast(AliasSection) isect;

						identifiers ~= Identifier(
							mod, format("%s.sizeOf", sect.newName)
						);
						break;
					}
					case SectionType.Let: {
						auto sect = cast(LetSection) isect;

						identifiers ~= Identifier(mod, sect.name);
						break;
					}
					case SectionType.Struct: {
						auto sect = cast(StructSection) isect;

						string[] structure;
						foreach (ref member ; sect.entries) {
							structure ~= member.name;
						}

						structs[sect.name] = structure;

						foreach (ref member ; structure) {
							identifiers ~= Identifier(
								sect.inMod, format("%s.%s", sect.name, member)
							);
						}

						identifiers ~= Identifier(
							sect.inMod, format("%s.sizeOf", sect.name)
						);
						break;
					}
					case SectionType.Extern: {
						auto sect = cast(ExternSection) isect;

						words ~= Word(
							sect.inMod, sect.funcName, false,
							Effect(sect.returns.length, sect.params.length), false
						);
						break;
					}
					default: break;
				}
			}

			profiler.End("stack checker (import)");
		}

		profiler.Begin();
		Evaluate(nodes);
		profiler.End("stack checker");
	}

	void DumpFunctions() {
		size_t spaces = words.fold!(
			(a, e) => e.name.length > a.name.length? e : a
		).name.length;

		foreach (word ; words) {
			writefln(
				"%s %s= %d %s-> %d", word.name,
				replicate([' '], spaces - word.name.length), word.effect.pop,
				word.manual? "m" : "", word.effect.push
			);
		}
	}
}
