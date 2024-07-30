module callisto.backends.fox32;

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

private enum WordType {
	Callisto,
	Raw,
	C
}

private struct Word {
	WordType type;
	bool     inline;
	Node[]   inlineNodes;
}

private struct StructEntry {
	Type   type;
	string name;
	bool   array;
	size_t size;
}

private struct Type {
	string        name;
	ulong         size;
	bool          isStruct;
	StructEntry[] structure;
	bool          hasInit;
	bool          hasDeinit;
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

class BackendFox32 : CompilerBackend {
	Word[string]     words;
	uint             blockCounter; // used for block statements
	Type[]           types;
	Variable[]       variables;
	Global[string]   globals;
	Constant[string] consts;
	bool             inScope;
	Array[]          arrays;
	string           thisFunc;
	bool             inWhile;
	uint             currentLoop;
	bool             makeImage;

	this() {
		types ~= Type("u8",    1);
		types ~= Type("i8",    1);
		types ~= Type("u16",   2);
		types ~= Type("i16",   2);
		types ~= Type("u32",   4);
		types ~= Type("u64",   4);
		types ~= Type("addr",  2);
		types ~= Type("size",  2);
		types ~= Type("usize", 2);
		types ~= Type("cell",  2);

		// built in structs
		types ~= Type("Array", 12, true, [
			StructEntry(GetType("usize"), "length"),
			StructEntry(GetType("usize"), "memberSize"),
			StructEntry(GetType("addr"),  "elements")
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
		"Fox32", "LittleEndian", "16Bit", "32Bit"
	];

	override string[] FinalCommands() {
		string[] ret = [
			format("mv %s %s.asm", compiler.outFile, compiler.outFile),
			format("fox32asm %s.asm %s", compiler.outFile, compiler.outFile)
		];

		if (!keepAssembly) {
			ret ~= format("rm %s.asm", compiler.outFile);
		}

		if (makeImage) {
			ret ~= format("ryfs.py -s 16777216 -l cal create %s.img", compiler.outFile);
			ret ~= format("ryfs.py add %s.img %s", compiler.outFile, compiler.outFile);
		}

		return ret;
	}

	override long MaxInt() => 0xFFFFFFFF;

	override string DefaultHeader() => "";

	override bool HandleOption(string opt, ref string[] versions) {
		switch (opt) {
			case "make-img": {
				makeImage = true;
				return true;
			}
			default: return false;
		}
	}

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

	bool TypeExists(string name) => types.any!(v => v.name == name);

	Type GetType(string name) {
		foreach (ref type ; types) {
			if (type.name == name) {
				return type;
			}
		}

		assert(0);
	}

	void SetType(string name, Type ptype) {
		foreach (i, ref type ; types) {
			if (type.name == name) {
				types[i] = ptype;
				return;
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
		// put IO stream in a global variable
		output ~= "pop r1\n";
		output ~= "mov [__global___fox32_stream], r1\n";

		// allocate stack
		output ~= "sub rsp, 2048\n";
		output ~= "mov r31, rsp\n";

		globals["__fox32_stream"] = Global(GetType("u32"));

		output ~= "jmp __calmain\n";
	}

	override void End() {
		output ~= "call end_current_task\n";

		foreach (name, var ; globals) {
			output ~= format(
				"__global_%s: data.fill %d, 0\n", name.Sanitise(), var.Size()
			);
		}

		output ~= "#include \"fox32rom.def\"\n";
		output ~= "#include \"fox32os.def\"\n";
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
				output ~= format("call __func__%s\n", node.name.Sanitise());
			}
		}
		else {
			Error(node.error, "Undefined identifier '%s'", node.name);
		}
	}

	override void CompileInteger(IntegerNode node) {
		output ~= format("mov.32 [r31], %d\n", node.value);
		output ~= "add r31, 4\n";
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
			words[node.name] = Word(WordType.Callisto, true, node.nodes);
		}
		else {
			assert(!inScope);
			inScope = true;

			words[node.name] = Word(node.raw? WordType.Raw : WordType.Callisto , false, []);

			string symbol =
				node.raw? node.name : format("__func__%s", node.name.Sanitise());

			output ~= format("%s:\n", symbol);

			foreach (ref inode ; node.nodes) {
				compiler.CompileNode(inode);
			}

			output    ~= "ret\n";
			//output    ~= format("__func_end__%s:\n", node.name.Sanitise());
			inScope    = false;
			variables  = [];
		}
	}

	override void CompileIf(IfNode node) {
		++ blockCounter;
		auto blockNum = blockCounter;
		uint condCounter;

		foreach (i, ref condition ; node.condition) {
			foreach (ref inode ; condition) {
				compiler.CompileNode(inode);
			}
			output ~= "sub r31, 4\n";
			output ~= "cmp [r31], 0\n";
			output ~= format("ifz jmp __if_%d_%d\n", blockNum, condCounter + 1);

			foreach (ref inode ; node.doIf[i]) {
				compiler.CompileNode(inode);
			}

			output ~= format("jmp __if_%d_end\n", blockNum);

			++ condCounter;
			output ~= format("__if_%d_%d:\n", blockNum, condCounter);
		}

		if (node.hasElse) {
			foreach (ref inode ; node.doElse) {
				compiler.CompileNode(inode);
			}
		}

		output ~= format("__if_%d_end:\n", blockNum);
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

	override void CompileImplement(ImplementNode node) {
		assert(0);
	}
}
