module callisto.backends.y32;

import std.conv;
import std.stdio;
import std.range;
import std.format;
import std.algorithm;
import callisto.util;
import callisto.error;
import callisto.parser;
import callisto.compiler;
import callisto.language;

private struct Word {
	bool   raw;
	bool   inline;
	Node[] inlineNodes;
}

private struct StructEntry {
	Type*  type;
	string name;
	bool   array;
	size_t size;
}

private struct Type {
	ulong         size;
	bool          isStruct;
	StructEntry[] structure;
}

private struct Variable {
	string name;
	Type   type;
	uint   offset; // SP + offset to access
	bool   array;
	ulong  arraySize;

	size_t Size() => array? arraySize * type.size : type.size;
}

private struct Global {
	Type  type;
	bool  array;
	ulong arraySize;

	size_t Size() => array? arraySize * type.size : type.size;
}

private struct Constant {
	Node value;
}

private struct Array {
	string[] values;
	Type     type;
	bool     global;

	size_t Size() => type.size * values.length;
}

class BackendY32 : CompilerBackend {
	Word[string]     words;
	Type[string]     types;
	string           thisFunc;
	Constant[string] consts;
	bool             inScope;
	uint             blockCounter;
	bool             inWhile;
	uint             currentLoop;
	Variable[]       variables;
	Global[string]   globals;
	Array[]          arrays;

	this() {
		// built in integer types
		types["u8"]    = Type(1);
		types["i8"]    = Type(1);
		types["u16"]   = Type(2);
		types["i16"]   = Type(2);
		types["u32"]   = Type(4);
		types["i32"]   = Type(4);
		types["addr"]  = Type(4);
		types["size"]  = Type(4);
		types["usize"] = Type(4);
		types["cell"]  = Type(4);

		// built in structs
		types["Array"] = Type(4 * 3, true, [
			StructEntry("usize" in types, "length"),
			StructEntry("usize" in types, "memberSize"),
			StructEntry("addr" in types,  "elements")
		]);
		NewConst("Array.length",     0);
		NewConst("Array.memberSize", 4);
		NewConst("Array.elements",   8);
		NewConst("Array.sizeof",     4 * 3);

		foreach (name, ref type ; types) {
			NewConst(format("%s.sizeof", name), cast(long) type.size);
		}
	}

	override string[] GetVersions() => [
		// platform
		"Y32", "Yeti32", "LittleEndian", "16Bit", "32Bit",
		// features
		"IO", "Exit"
	];

	override string[] FinalCommands() => [
		format("mv %s %s.asm", compiler.outFile, compiler.outFile),
		format("yeti32 asm %s.asm -o %s", compiler.outFile, compiler.outFile),
		format("rm %s.asm", compiler.outFile)
	];

	override long MaxInt() => 0xFFFFFFFF;

	override string DefaultHeader() => "";

	override void NewConst(string name, long value, ErrorInfo error = ErrorInfo.init) {
		consts[name] = Constant(new IntegerNode(error, value));
	}

	bool VariableExists(string name) => variables.any!(v => v.name == name);

	Variable GetVariable(string name) {
		foreach (ref var ; variables) {
			if (var.name == name) {
				return var;
			}
		}

		assert(0);
	}

	size_t GetStackSize() {
		// old
		//return variables.empty()? 0 : variables[0].offset + variables[0].type.size;

		size_t size;
		foreach (ref var ; variables) {
			size += var.Size();
		}

		return size;
	}

	override void BeginMain() {
		output ~= "__calmain:\n";
	}

	override void Init() {
		output ~= "jmpb __calmain\n";
	}

	override void End() {
		output ~= "hlt";
	}

	override void CompileWord(WordNode node) {
		if (node.name in words) {
			auto word = words[node.name];

			if (word.inline) {
				foreach (inode ; word.inlineNodes) {
					compiler.CompileNode(inode);
				}
			}
			else {
				if (word.raw) {
					output ~= format("callb %s\n", node.name);
				}
				else {
					output ~= format("callb __func__%s\n", node.name.Sanitise());
				}
			}
		}
		else if (node.name in consts) {
			compiler.CompileNode(consts[node.name].value);
		}
		else {
			Error(node.error, "Undefined identifier '%s'", node.name);
		}
	}

	override void CompileInteger(IntegerNode node) {
		output ~= format("pushd %d\n", node.value);
	}

	override void CompileFuncDef(FuncDefNode node) {
		if (node.name in words) {
			Error(node.error, "Function name '%s' already used", node.name);
		}
		if (Language.bannedNames.canFind(node.name)) {
			Error(node.error, "Name '%s' can't be used", node.name);
		}

		thisFunc = node.name;

		if (node.inline) {
			words[node.name] = Word(false, true, node.nodes);
		}
		else {
			assert(!inScope);
			inScope = true;

			words[node.name]  = Word(node.raw, false, []);

			string symbol =
				node.raw? node.name : format("__func__%s", node.name.Sanitise());

			output ~= format("%s:\n", symbol);

			foreach (ref inode ; node.nodes) {
				compiler.CompileNode(inode);
			}

			output    ~= "ret\n";
			inScope    = false;
			variables  = [];
		}
	}

	override void CompileIf(IfNode node) {
		assert(0);
	}

	override void CompileWhile(WhileNode node) {
		assert(0);
	}

	override void CompileLet(LetNode node) {
		assert(0);
	}

	override void CompileArray(ArrayNode node) {
		assert(0);
	}

	override void CompileString(StringNode node) {
		assert(0);
	}

	override void CompileStruct(StructNode node) {
		assert(0);
	}

	override void CompileReturn(WordNode node) {
		assert(0);
	}

	override void CompileConst(ConstNode node) {
		assert(0);
	}

	override void CompileEnum(EnumNode node) {
		assert(0);
	}

	override void CompileBreak(WordNode node) {
		assert(0);
	}

	override void CompileContinue(WordNode node) {
		assert(0);
	}

	override void CompileUnion(UnionNode node) {
		assert(0);
	}

	override void CompileAlias(AliasNode node) {
		assert(0);
	}

	override void CompileExtern(ExternNode node) {
		assert(0);
	}

	override void CompileCall(WordNode node) {
		assert(0);
	}

	override void CompileFuncAddr(FuncAddrNode node) {
		assert(0);
	}
}
