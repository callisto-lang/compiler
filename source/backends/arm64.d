module callisto.backends.arm64;

import std.conv;
import std.file;
import std.path;
import std.stdio;
import std.range;
import std.format;
import std.process;
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

	// for C words
	Type[] params;
	Type   ret;
	bool   isVoid;
	string symbolName;
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

class BackendARM64 : CompilerBackend {
	Word[string]     words;
	Type[]           types;
	string           thisFunc;
	Constant[string] consts;
	bool             inScope;
	uint             blockCounter;
	bool             inWhile;
	uint             currentLoop;
	Variable[]       variables;
	Global[string]   globals;
	Array[]          arrays;
	bool             useLibc;

	this() {
		version (linux) {
			defaultOS = "linux";
		}
		else {
			defaultOS = "bare-metal";
			WarnNoInfo("Default operating system, defaulting to bare-metal OS");
		}

		// built in integer types
		types ~= Type("u8",    1);
		types ~= Type("i8",    1);
		types ~= Type("u16",   2);
		types ~= Type("i16",   2);
		types ~= Type("u32",   4);
		types ~= Type("i32",   4);
		types ~= Type("u64",   8);
		types ~= Type("i64",   8);
		types ~= Type("addr",  8);
		types ~= Type("size",  8);
		types ~= Type("usize", 8);
		types ~= Type("cell",  8);
		types ~= Type("bool",  8);

		// built in structs
		types ~= Type("Array", 24, true, [
			StructEntry(GetType("usize"), "length"),
			StructEntry(GetType("usize"), "memberSize"),
			StructEntry(GetType("addr"),  "elements")
		]);
		NewConst("Array.length",     0);
		NewConst("Array.memberSize", 8);
		NewConst("Array.elements",   16);
		NewConst("Array.sizeof",     8 * 3);

		types ~= Type("Exception", 24 + 8, true, [
			StructEntry(GetType("bool"),  "error"),
			StructEntry(GetType("Array"), "msg")
		]);
		NewConst("Exception.bool",   0);
		NewConst("Exception.msg",    8);
		NewConst("Exception.sizeof", 24 + 8);

		foreach (ref type ; types) {
			NewConst(format("%s.sizeof", type.name), cast(long) type.size);
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
		size_t size;
		foreach (ref var ; variables) {
			size += var.Size();
		}

		return size;
	}

	override string[] GetVersions() {
		// CPU features
		string[] ret = ["arm64", "LittleEndian", "16Bit", "32Bit", "64Bit"];

		// OS features
		switch (os) {
			case "linux": {
				ret ~= ["Linux", "IO", "File", "Args", "Time", "Heap", "Exit"];
				break;
			}
			default: break;
		}

		return ret;
	}

	override string[] FinalCommands() {
		// TODO: allow user to specify commands manually?
		version (AArch64) {
			string assembler = "as";
			string linker    = "ld";
		}
		else {
			string assembler = "aarch64-linux-gnu-as";
			string linker    = "aarch64-linux-gnu-ld";
		}

		string[] ret = [
			format("mv %s %s.asm", compiler.outFile, compiler.outFile),
		];

		string assembleCommand =  format(
			"%s %s.asm -o %s.o", assembler, compiler.outFile, compiler.outFile,
		);

		if (useDebug) {
			assembleCommand ~= " -g";
		}

		ret ~= assembleCommand;

		string linkCommand = format(
			"%s %s.o -o %s", linker, compiler.outFile, compiler.outFile
		);

		foreach (ref lib ; link) {
			linkCommand ~= format(" -l%s", lib);
		}

		if (useLibc) {
			string[] possiblePaths = [
				"/usr/aarch64-linux-gnu/lib/crt1.o",
				"/usr/lib/crt1.o",
				"/usr/lib64/crt1.o",
			];
			bool crt1;

			foreach (ref path ; possiblePaths) {
				if (path.exists) {
					crt1 = true;
					linkCommand ~= format(" %s", path);
					linkCommand ~= format(" %s/crti.o", path.dirName);
					linkCommand ~= format(" %s/crtn.o", path.dirName);
					break;
				}
			}

			if (!crt1) {
				stderr.writeln("WARNING: Failed to find crt1.o, program may behave incorrectly");
			}
		}

		ret ~= linkCommand;

		if (!keepAssembly) {
			 ret ~= format("rm %s.asm %s.o", compiler.outFile, compiler.outFile);
		}
		
		return ret;
	}

	override long MaxInt() => -1;

	override string DefaultHeader() => "";

	override bool HandleOption(string opt, ref string[] versions) {
		switch (opt) {
			case "use-libc": {
				link     ~= "c";
				useLibc   = true;
				versions ~= "LibC";
				return true;
			}
			default: return false;
		}
	}

	override void BeginMain() {
		output ~= "__calmain:\n";

		// call constructors
		foreach (name, global ; globals) {
			if (global.type.hasInit) {
				output ~= format("ldr x9, =__global_%s\n", name.Sanitise());
				output ~= "str x9, [x19], #8\n";
				output ~= format("bl __type_init_%s\n", global.type.name.Sanitise());
			}
		}
	}

	void CallFunction(string name) {
		auto word = words[name];

		if (word.inline) {
			foreach (inode ; word.inlineNodes) {
				compiler.CompileNode(inode);
			}
		}
		else if (word.type == WordType.Raw) {
			output ~= format("bl %s\n", name);
		}
		else if (word.type == WordType.C) {
			assert(0); // TODO: error
		}
		else {
			output ~= format("bl __func__%s\n", name.Sanitise());
		}
	}

	override void Init() {
		string[] oses = ["linux", "bare-metal"];
		if (!oses.canFind(os)) {
			ErrorNoInfo("Backend doesn't support operating system '%s'", os);
		}

		output ~= ".text\n";
		if (useLibc) {
			output ~= ".global main\n";
			output ~= "main:\n";
		} else {
			output ~= ".global _start\n";
			output ~= "_start:\n";
		}

		output ~= "bl __init\n";

		// allocate data stack
		output ~= "sub x20, sp, #4096\n"; // 512 cells
		output ~= "mov x19, x20\n";

		// create functions for interop
		if (exportSymbols) {
			output ~= "
				.global cal_push
				cal_push:
					str x0, [x19], #8
					ret
				.global cal_pop
				cal_pop:
					ldr x0, [x19, #-8]!
					ret
			";
		}

		// jump to main
		output ~= "b __calmain\n";
	}
	
	override void End() {
		// call destructors
		foreach (name, global ; globals) {
			if (global.type.hasDeinit) {
				output ~= format("ldr x9, =__global_%s\n", name.Sanitise());
				output ~= "str x9, [x19], #8\n";
				output ~= format("bl __type_deinit_%s\n", global.type.name.Sanitise());
			}
		}

		// exit program
		if ("__arm64_program_exit" in words) {
			CallFunction("__arm64_program_exit");
		}
		else {
			WarnNoInfo("No exit function available, expect bugs");
		}

		output ~= "ret\n";

		// run init function
		output ~= "__init:\n";
		if ("__arm64_program_init" in words) {
			CallFunction("__arm64_program_init");
		}
		else {
			WarnNoInfo("No program init function available");
		}
		output ~= "ret\n";

		// create global variables
		output ~= ".bss\n";

		foreach (name, var ; globals) {
			output ~= format(".lcomm __global_%s, %d\n", name.Sanitise(), var.Size());

			if (exportSymbols) {
				output ~= format(".global __global_%s\n", name.Sanitise());
			}
		}

		// create arrays
		output ~= ".data\n";
		foreach (i, ref array ; arrays) {
			if (exportSymbols) {
				output ~= format(".global __array_%d\n", i);
			}

			output ~= format("__array_%d: ", i);

			switch (array.type.size) {
				case 1:  output ~= ".byte "; break;
				case 2:  output ~= ".2byte "; break;
				case 4:  output ~= ".4byte "; break;
				case 8:  output ~= ".8byte "; break;
				default: assert(0);
			}

			foreach (j, ref element ; array.values) {
				output ~= element ~ (j == array.values.length - 1? "" : ", ");
			}

			output ~= '\n';

			if (array.global) {
				output ~= format(
					"__array_%d_meta: .8byte %d, %d, __array_%d\n", i,
					array.values.length,
					array.type.size,
					i
				);
			}
		}
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
				if (word.type == WordType.Raw) {
					output ~= format("bl %s\n", node.name);
				}
				else if (word.type == WordType.C) {
					// TODO: support more of the calling convention (especially structs)

					if (word.params.length > 8) {
						Error(node.error, "C call has too many parameters");
					}

					for (auto i = 0; i < word.params.length; i++) {
						auto reg = word.params.length - i - 1;
						output ~= format("ldr x%d, [x19, #-8]!\n", reg);
					}
				
					output ~= format("bl %s\n", word.symbolName);

					if (!word.isVoid) {
						output ~= "str x0, [x19], #8\n";
					}
				}
				else {
					output ~= format("bl __func__%s\n", node.name.Sanitise());
				}
			}
		}
		else if (VariableExists(node.name)) {
			auto var = GetVariable(node.name);

			if (var.type.isStruct) {
				Error(node.error, "Can't push value of struct");
			}

			switch (var.type.size) {
				case 1: output ~= format("ldrb w9, [x20, #%d]\n", var.offset); break;
				case 2: output ~= format("ldrh w9, [x20, #%d]\n", var.offset); break;
				case 4: output ~= format("ldr w9, [x20, #%d]\n", var.offset); break;
				case 8: output ~= format("ldr x9, [x20, #%d]\n", var.offset); break;
				default: Error(node.error, "Bad variable type size");
			}

			output ~= "str x9, [x19], #8\n";
		}
		else if (node.name in globals) {
			auto var = globals[node.name];

			if (var.type.isStruct) {
				Error(node.error, "Can't push value of struct");
			}

			string symbol = format("__global_%s", node.name.Sanitise());
			output ~= format("ldr x9, =%s\n", symbol);

			switch (var.type.size) {
				case 1: output ~= "ldrb w9, [x9]\n"; break;
				case 2: output ~= "ldrh w9, [x9]\n"; break;
				case 4: output ~= "ldr w9, [x9]\n"; break;
				case 8: output ~= "ldr x9, [x9]\n"; break;
				default: Error(node.error, "Bad variable type size");
			}

			output ~= "str x9, [x19], #8\n";
		}
		else if (node.name in consts) {
			auto value  = consts[node.name].value;
			value.error = node.error;

			compiler.CompileNode(consts[node.name].value);
		}
		else {
			Error(node.error, "Undefined identifier '%s'", node.name);
		}
	}
	
	override void CompileInteger(IntegerNode node) {
		output ~= format("ldr x9, =%d\n", node.value);
		output ~= "str x9, [x19], #8\n";
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

			if (exportSymbols) {
				output ~= format(".global %s\n", symbol);
			}

			output ~= format("%s:\n", symbol);
			output ~= "str lr, [x20, #-8]!\n";

			// allocate parameters
			size_t paramSize = node.params.length * 8;
			foreach (ref type ; node.paramTypes) {
				if (!TypeExists(type)) {
					Error(node.error, "Type '%s' doesn't exist", type);
				}
				if (GetType(type).isStruct) {
					Error(node.error, "Structures cannot be used in function parameters");
				}
			}
			if (paramSize > 0) {
				output ~= format("sub x20, x20, #%d\n", paramSize);
				foreach (ref var ; variables) {
					var.offset += paramSize;
				}

				size_t offset;
				foreach (i, ref type ; node.paramTypes) {
					auto     param = node.params[i];
					Variable var;

					var.name      = param;
					var.type      = GetType(type);
					var.offset    = cast(uint) offset;
					offset       += var.Size();
					variables    ~= var;
				}

				// copy data to parameters
				output ~= format("sub x19, x19, #%d\n", paramSize);
				//output ~= format("sub x9, x19, #%d\n", paramSize);
				output ~= "mov x9, x19\n";
				output ~= "mov x10, x20\n";
				output ~= format("mov x11, #%d\n", paramSize);
				output ~= "1:\n";
				output ~= "ldrb w12, [x9], #1\n";
				output ~= "strb w12, [x10], #1\n";
				output ~= "subs x11, x11, #1\n";
				output ~= "bne 1b\n";
			}

			foreach (ref inode ; node.nodes) {
				compiler.CompileNode(inode);
			}

			size_t scopeSize;
			foreach (ref var ; variables) {
				scopeSize += var.Size();

				if (var.type.hasDeinit) {
					output ~= format("add x9, x20, #%d\n", var.offset);
					output ~= "str x9, [x19], #8\n";
					output ~= format("bl __type_deinit_%s\n", var.type.name.Sanitise());
				}
			}
			if (scopeSize > 0) {
				OffsetLocalsStack(scopeSize, false);
			}

			output    ~= "ldr lr, [x20], #8\n";
			output    ~= "ret\n";
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
			output ~= "ldr x9, [x19, #-8]!\n";
			output ~= "cmp x9, #0\n";
			output ~= format("beq __if_%d_%d\n", blockNum, condCounter + 1);

			// create scope
			auto oldVars = variables.dup;
			auto oldSize = GetStackSize();

			foreach (ref inode ; node.doIf[i]) {
				compiler.CompileNode(inode);
			}

			// remove scope
			foreach (ref var ; variables) {
				if (oldVars.canFind(var)) continue;
				if (!var.type.hasDeinit)  continue;

				output ~= format("add x9, x20, #%d\n", var.offset);
				output ~= "str x9, [x19], #8\n";
				output ~= format("bl __type_deinit_%s\n", var.type.name.Sanitise());
			}
			if (GetStackSize() - oldSize > 0) {
				OffsetLocalsStack(GetStackSize() - oldSize, false);
			}
			variables = oldVars;

			output ~= format("b __if_%d_end\n", blockNum);

			++ condCounter;
			output ~= format("__if_%d_%d:\n", blockNum, condCounter);
		}

		if (node.hasElse) {
			// create scope
			auto oldVars = variables.dup;
			auto oldSize = GetStackSize();

			foreach (ref inode ; node.doElse) {
				compiler.CompileNode(inode);
			}

			// remove scope
			foreach (ref var ; variables) {
				if (oldVars.canFind(var)) continue;
				if (!var.type.hasDeinit)  continue;

				output ~= format("add x9, x20, #%d\n", var.offset);
				output ~= "str x9, [x19], #8\n";
				output ~= format("bl __type_deinit_%s\n", var.type.name.Sanitise());
			}
			if (GetStackSize() - oldSize > 0) {
				OffsetLocalsStack(GetStackSize() - oldSize, false);
			}
			variables = oldVars;
		}

		output ~= format("__if_%d_end:\n", blockNum);
	}
	
	override void CompileWhile(WhileNode node) {
		++ blockCounter;
		uint blockNum = blockCounter;
		currentLoop   = blockNum;

		output ~= format("b __while_%d_condition\n", blockNum);
		output ~= format("__while_%d:\n", blockNum);

		// make scope
		auto oldVars = variables.dup;
		auto oldSize = GetStackSize();

		foreach (ref inode ; node.doWhile) {
			inWhile = true;
			compiler.CompileNode(inode);

			currentLoop = blockNum;
		}

		// restore scope
		output ~= format("__while_%d_next:\n", blockNum);
		foreach (ref var ; variables) {
			if (oldVars.canFind(var)) continue;
			if (!var.type.hasDeinit)  continue;

			output ~= format("add x9, x20, #%d\n", var.offset);
			output ~= "str x9, [x19], #8\n";
			output ~= format("bl __type_deinit_%s\n", var.type.name.Sanitise());
		}
		if (GetStackSize() - oldSize > 0) {
			OffsetLocalsStack(GetStackSize() - oldSize, false);
		}
		variables = oldVars;

		inWhile = false;

		output ~= format("__while_%d_condition:\n", blockNum);
		
		foreach (ref inode ; node.condition) {
			compiler.CompileNode(inode);
		}

		output ~= "ldr x9, [x19, #-8]!\n";
		output ~= "cmp x9, #0\n";
		output ~= format("bne __while_%d\n", blockNum);
		output ~= format("__while_%d_end:\n", blockNum);
	}
	
	override void CompileLet(LetNode node) {
		if (!TypeExists(node.varType)) {
			Error(node.error, "Undefined type '%s'", node.varType);
		}
		if (VariableExists(node.name) || (node.name in words)) {
			Error(node.error, "Variable name '%s' already used", node.name);
		}
		if (Language.bannedNames.canFind(node.name)) {
			Error(node.error, "Name '%s' can't be used", node.name);
		}

		if (inScope) {
			Variable var;
			var.name      = node.name;
			var.type      = GetType(node.varType);
			var.offset    = 0;
			var.array     = node.array;
			var.arraySize = node.arraySize;

			foreach (ref ivar ; variables) {
				ivar.offset += var.Size();
			}

			variables ~= var;

			switch (var.Size()) {
				case 1: output ~= "strb wzr, [x20, #-1]!\n"; break;
				case 2: output ~= "strh wzr, [x20, #-2]!\n"; break;
				case 4: output ~= "str wzr, [x20, #-4]!\n"; break;
				case 8: output ~= "str xzr, [x20, #-8]!\n"; break;
				default: OffsetLocalsStack(var.Size(), true);
			}

			if (var.type.hasInit) { // call constructor
				output ~= "str x20, [x19], #8\n";
				output ~= format("bl __type_init_%s\n", var.type.name.Sanitise());
			}
		}
		else {
			Global global;
			global.type        = GetType(node.varType);
			global.array       = node.array;
			global.arraySize   = node.arraySize;
			globals[node.name] = global;
		}
	}
	
	override void CompileArray(ArrayNode node) {
		Array array;

		foreach (ref elem ; node.elements) {
			switch (elem.type) {
				case NodeType.Integer: {
					auto node2    = cast(IntegerNode) elem;
					array.values ~= node2.value.text();
					break;
				}
				default: {
					Error(elem.error, "Type '%s' can't be used in array literal");
				}
			}
		}

		if (!TypeExists(node.arrayType)) {
			Error(node.error, "Type '%s' doesn't exist", node.arrayType);
		}

		array.type    = GetType(node.arrayType);
		array.global  = !inScope || node.constant;
		arrays       ~= array;

		if (!inScope || node.constant) {
			output ~= format("ldr x9, =__array_%d_meta\n", arrays.length - 1);
			output ~= "str x9, [x19], #8\n";
		}
		else {
			// allocate a copy of this array
			OffsetLocalsStack(array.Size(), true);
			output ~= "mov x9, x20\n";
			output ~= format("ldr x10, =__array_%d\n", arrays.length - 1);
			output ~= format("ldr x11, =%d\n", array.Size());
			output ~= "1:\n";
			output ~= "ldrb w12, [x10], #1\n";
			output ~= "strb w12, [x9], #1\n";
			output ~= "subs x11, x11, #1\n";
			output ~= "bne 1b\n";

			Variable var;
			var.type      = array.type;
			var.offset    = 0;
			var.array     = true;
			var.arraySize = array.values.length;

			foreach (ref var2 ; variables) {
				var2.offset += var.Size();
			}

			variables ~= var;

			// create metadata variable
			var.type   = GetType("Array");
			var.offset = 0;
			var.array  = false;

			foreach (ref var2 ; variables) {
				var2.offset += var.Size();
			}

			variables ~= var;

			output ~= "mov x9, x20\n";
			output ~= format("sub x20, x20, #%d\n", var.type.size); // size of Array structure
			output ~= format("ldr x10, =%d\n", array.values.length);
			output ~= "str x10, [x20]\n"; // length
			output ~= format("ldr x10, =%d\n", array.type.size);
			output ~= "str x10, [x20, #8]\n"; // member size
			output ~= "str x9, [x20, #16]\n"; // elements

			// push metadata address
			output ~= "mov x9, x20\n";
			output ~= "str x9, [x19], #8\n";
		}
	}
	
	override void CompileString(StringNode node) {
		auto arrayNode = new ArrayNode(node.error);

		arrayNode.arrayType = "u8";
		arrayNode.constant  = node.constant;

		foreach (ref ch ; node.value) {
			arrayNode.elements ~= new IntegerNode(node.error, cast(long) ch);
		}

		CompileArray(arrayNode);
	}
	
	override void CompileStruct(StructNode node) {
		size_t offset;

		if (TypeExists(node.name)) {
			Error(node.error, "Type '%s' defined multiple times", node.name);
		}

		StructEntry[] entries;
		string[]      members;

		if (node.inherits) {
			if (!TypeExists(node.inheritsFrom)) {
				Error(node.error, "Type '%s' doesn't exist", node.inheritsFrom);
			}

			if (!GetType(node.inheritsFrom).isStruct) {
				Error(node.error, "Type '%s' is not a structure", node.inheritsFrom);
			}

			entries = GetType(node.inheritsFrom).structure;

			foreach (ref member ; GetType(node.inheritsFrom).structure) {
				members ~= member.name;
			}
		}

		foreach (ref member ; node.members) {
			if (!TypeExists(member.type)) {
				Error(node.error, "Type '%s' doesn't exist", member.type);
			}
			if (members.canFind(member.name)) {
				Error(node.error, "Duplicate member '%s'", member.name);
			}

			entries ~= StructEntry(
				GetType(member.type), member.name, member.array, member.size
			);
			members ~= member.name;
		}

		foreach (ref member ; entries) {
			NewConst(format("%s.%s", node.name, member.name), offset);
			offset += member.array? member.type.size * member.size : member.type.size;
		}

		NewConst(format("%s.sizeof", node.name), offset);
		types ~= Type(node.name, offset, true, entries);
	}
	
	override void CompileReturn(WordNode node) {
		if (!inScope) {
			Error(node.error, "Return used outside of function");
		}

		size_t scopeSize;
		foreach (ref var ; variables) {
			scopeSize += var.Size();

			if (var.type.hasDeinit) {
				output ~= format("add x9, x20, #%d\n", var.offset);
				output ~= "str x9, [x19], #8\n";
				output ~= format("bl __type_deinit_%s\n", var.type.name.Sanitise());
			}
		}
		if (scopeSize > 0) {
			OffsetLocalsStack(scopeSize, false);
		}

		output ~= "ldr lr, [x20], #8\n";
		output ~= "ret\n";
	}

	override void CompileConst(ConstNode node) {
		if (node.name in consts) {
			Error(node.error, "Constant '%s' already defined", node.name);
		}
		
		NewConst(node.name, node.value);
	}

	override void CompileEnum(EnumNode node) {
		if (!TypeExists(node.enumType)) {
			Error(node.error, "Enum base type '%s' doesn't exist", node.enumType);
		}
		if (TypeExists(node.name)) {
			Error(node.error, "Enum name is already used by type '%s'", node.enumType);
		}

		auto baseType  = GetType(node.enumType);
		baseType.name  = node.name;
		types         ~= baseType;

		foreach (i, ref name ; node.names) {
			NewConst(format("%s.%s", node.name, name), node.values[i]);
		}

		NewConst(format("%s.min", node.name), node.values.minElement());
		NewConst(format("%s.max", node.name), node.values.maxElement());
		NewConst(format("%s.sizeof", node.name), GetType(node.name).size);
	}

	override void CompileBreak(WordNode node) {
		if (!inWhile) {
			Error(node.error, "Not in while loop");
		}

		output ~= format("b __while_%d_end\n", currentLoop);
	}

	override void CompileContinue(WordNode node) {
		if (!inWhile) {
			Error(node.error, "Not in while loop");
		}

		output ~= format("b __while_%d_next\n", currentLoop);
	}

	override void CompileUnion(UnionNode node) {
		size_t maxSize = 0;

		if (TypeExists(node.name)) {
			Error(node.error, "Type '%s' already exists", node.name);
		}

		string[] unionTypes;

		foreach (ref type ; node.types) {
			if (unionTypes.canFind(type)) {
				Error(node.error, "Union type '%s' defined twice", type);
			}
			unionTypes ~= type;

			if (!TypeExists(type)) {
				Error(node.error, "Type '%s' doesn't exist", type);
			}

			if (GetType(type).size > maxSize) {
				maxSize = GetType(type).size;
			}
		}

		types ~= Type(node.name, maxSize);
		NewConst(format("%s.sizeof", node.name), cast(long) maxSize);
	}

	override void CompileAlias(AliasNode node) {
		if (!TypeExists(node.from)) {
			Error(node.error, "Type '%s' doesn't exist", node.from);
		}
		if ((TypeExists(node.to)) && !node.overwrite) {
			Error(node.error, "Type '%s' already defined", node.to);
		}

		auto baseType  = GetType(node.from);
		baseType.name  = node.to;
		types         ~= baseType;

		NewConst(format("%s.sizeof", node.to), cast(long) GetType(node.to).size);
	}

	override void CompileExtern(ExternNode node) {
		Word word;

		string funcName = node.func;

		final switch (node.externType) {
			case ExternType.Callisto: word.type = WordType.Callisto; break;
			case ExternType.Raw:      word.type = WordType.Raw;      break;
			case ExternType.C: {
				word.type = WordType.C;

				foreach (ref param ; node.types) {
					if (!TypeExists(param)) {
						Error(node.error, "Unknown type '%s'", param);
					}

					word.params ~= GetType(param);
				}

				if (node.retType == "void") {
					word.isVoid = true;
				}
				else {
					if (!TypeExists(node.retType)) {
						Error(node.error, "Unknown type '%s'", node.retType);
					}

					word.ret = GetType(node.retType);
				}

				word.symbolName = node.func;
				funcName        = node.asName;
				break;
			}
		}

		if (word.type != WordType.Callisto) {
			output ~= format(".extern %s\n", node.func);
		}

		words[funcName] = word;
	}

	override void CompileCall(WordNode node) {
		output ~= "ldr x9, [x19, #-8]!\n";
		output ~= "blr x9\n";
	}

	override void CompileAddr(AddrNode node) {
		if (node.func in words) {
			auto   word   = words[node.func];
			string symbol = word.type == WordType.Callisto?
				format("__func__%s", node.func.Sanitise()) : node.func;

			output ~= format("ldr x9, =%s\n", symbol);
			output ~= "str x9, [x19], #8\n";
		}
		else if (node.func in globals) {
			auto var = globals[node.func];

			output ~= format("ldr x9, =__global_%s\n", node.func.Sanitise());
			output ~= "str x9, [x19], #8\n";
		}
		else if (VariableExists(node.func)) {
			auto var = GetVariable(node.func);

			output ~= format("add x9, x20, #%d\n", var.offset);
			output ~= "str x9, [x19], #8\n";
		}
		else {
			Error(node.error, "Undefined identifier '%s'", node.func);
		}
	}

	override void CompileImplement(ImplementNode node) {
		if (!TypeExists(node.structure)) {
			Error(node.error, "Type '%s' doesn't exist", node.structure);
		}
		auto type = GetType(node.structure);

		string labelName;

		switch (node.method) {
			case "init": {
				if (GetType(node.structure).hasInit) {
					Error(node.error, "Already implemented in type");
				}

				type.hasInit = true;
				labelName = format("__type_init_%s", Sanitise(node.structure));
				break;
			}
			case "deinit": {
				if (GetType(node.structure).hasDeinit) {
					Error(node.error, "Already implemented in type");
				}

				type.hasDeinit = true;
				labelName = format("__type_deinit_%s", Sanitise(node.structure));
				break;
			}
			default: Error(node.error, "Unknown method '%s'", node.method);
		}

		SetType(type.name, type);

		assert(!inScope);
		inScope = true;

		output ~= format("%s:\n", labelName);
		output ~= "str lr, [x20, #-8]!\n";

		foreach (ref inode ; node.nodes) {
			compiler.CompileNode(inode);
		}

		size_t scopeSize;
		foreach (ref var ; variables) {
			scopeSize += var.Size();

			if (var.type.hasDeinit) {
				output ~= format("add x9, x20, #%d\n", var.offset);
				output ~= "str x9, [x19], #8\n";
				output ~= format("bl __type_deinit_%s\n", var.type.name.Sanitise());
			}
		}
		if (scopeSize > 0) {
			OffsetLocalsStack(scopeSize, false);
		}

		output    ~= "ldr lr, [x20], #8\n";
		output    ~= "ret\n";
		inScope    = false;
		variables  = [];
	}

	override void CompileSet(SetNode node) {
		output ~= "ldr x9, [x19, #-8]!\n";

		if (VariableExists(node.var)) {
			auto var = GetVariable(node.var);

			if (var.type.isStruct) {
				Error(node.error, "Can't set struct value");
			}

			switch (var.type.size) {
				case 1: output ~= format("strb w9, [x20, #%d]\n", var.offset); break;
				case 2: output ~= format("strh w9, [x20, #%d]\n", var.offset); break;
				case 4: output ~= format("str w9, [x20, #%d]\n", var.offset); break;
				case 8: output ~= format("str x9, [x20, #%d]\n", var.offset); break;
				default: Error(node.error, "Bad variable type size");
			}
		}
		else if (node.var in globals) {
			auto global = globals[node.var];

			if (global.type.isStruct) {
				Error(node.error, "Can't set struct value");
			}

			string symbol = format("__global_%s", node.var.Sanitise());
			output ~= format("ldr x10, =%s\n", symbol);

			switch (global.type.size) {
				case 1: output ~= "strb w9, [x10]\n"; break;
				case 2: output ~= "strh w9, [x10]\n"; break;
				case 4: output ~= "str w9, [x10]\n"; break;
				case 8: output ~= "str x9, [x10]\n"; break;
				default: Error(node.error, "Bad variable type size");
			}
		}
		else {
			Error(node.error, "Variable '%s' doesn't exist", node.var);
		}
	}

	void OffsetLocalsStack(size_t offset, bool sub) {
		if (offset >= 4096) {
			output ~= format("mov x9, #%d\n", offset);
			output ~= format("%s x20, x20, x9\n", sub ? "sub" : "add");
		} else {
			output ~= format("%s x20, x20, #%d\n", sub ? "sub" : "add", offset);
		}
	}


	override void CompileTryCatch(TryCatchNode node) {}
	override void CompileThrow(WordNode node) {}
}
