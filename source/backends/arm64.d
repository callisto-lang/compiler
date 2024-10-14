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
	bool     error;
	Type[]   params;

	// for C words
	Type   ret;
	bool   isVoid;
	string symbolName;
}

class BackendARM64 : CompilerBackend {
	Word[string]     words;
	string           thisFunc;
	bool             inScope;
	uint             blockCounter;
	bool             inWhile;
	uint             currentLoop;
	bool             useLibc;

	this() {
		version (linux) {
			defaultOS = "linux";
		}
		else version (OSX) {
			defaultOS = "osx";
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

		globals ~= Global("_cal_exception", GetType("Exception"), false, 0);

		foreach (ref type ; types) {
			NewConst(format("%s.sizeof", type.name), cast(long) type.size);
		}
	}

	override void NewConst(string name, long value, ErrorInfo error = ErrorInfo.init) {
		consts[name] = Constant(new IntegerNode(error, value));
	}

	override string[] GetVersions() {
		// CPU features
		string[] ret = ["arm64", "LittleEndian", "16Bit", "32Bit", "64Bit"];

		// OS features
		switch (os) {
			case "linux": {
				ret ~= ["Linux"];
				break;
			}
			case "osx": {
				ret ~= ["OSX"];
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
			if (os == "linux") {
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
			} else if (os == "osx") {
				linkCommand ~= " -lSystem -syslibroot `xcrun --sdk macosx --show-sdk-path`";
			} else {
				WarnNoInfo("Cannot use libc on operating system '%s'", os);
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
		foreach (global ; globals) {
			if (global.type.hasInit) {
				LoadAddress("x9", format("__global_%s", global.name.Sanitise()));
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
			output ~= format("bl %s\n", ExternSymbol(name));
		}
		else if (word.type == WordType.C) {
			assert(0); // TODO: error
		}
		else {
			output ~= format("bl __func__%s\n", name.Sanitise());
		}
	}

	override void Init() {
		string[] oses = ["linux", "osx", "bare-metal"];
		if (!oses.canFind(os)) {
			ErrorNoInfo("Backend doesn't support operating system '%s'", os);
		}

		output ~= ".text\n";
		if (os == "osx") {
			output ~= ".global _main\n";
			output ~= "_main:\n";
		} else if (useLibc) {
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

		// jump to main
		output ~= "b __calmain\n";

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
	}
	
	override void End() {
		// call destructors
		foreach (global ; globals) {
			if (global.type.hasDeinit) {
				LoadAddress("x9", format("__global_%s", global.name.Sanitise()));
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

		foreach (var ; globals) {
			output ~= format(".lcomm __global_%s, %d\n", var.name.Sanitise(), var.Size());

			if (exportSymbols) {
				output ~= format(".global __global_%s\n", var.name.Sanitise());
			}
		}

		// create arrays
		output ~= ".data\n";
		foreach (i, ref array ; arrays) {
			output ~= ".align 8\n";
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
				output ~= ".align 8\n";
				output ~= format(
					"__array_%d_meta: .8byte %d, %d, __array_%d\n", i,
					array.values.length,
					array.type.size,
					i
				);
			}
		}
	}

	void PushGlobalValue(Node node, Global var, size_t size = 0, size_t offset = 0, bool member = false) {
		if (size == 0) {
			size = var.type.size;
		}

		if (var.type.isStruct && !member) {
			Error(node.error, "Can't push value of struct");
		}

		LoadAddress("x9", format("__global_%s", var.name.Sanitise()));

		switch (size) {
			case 1: output ~= format("ldrb w9, [x9, #%d]\n", offset); break;
			case 2: output ~= format("ldrh w9, [x9, #%d]\n", offset); break;
			case 4: output ~= format("ldr w9, [x9, #%d]\n", offset); break;
			case 8: output ~= format("ldr x9, [x9, #%d]\n", offset); break;
			default: Error(node.error, "Bad variable type size");
		}

		output ~= "str x9, [x19], #8\n";
	}

	void PushVariableValue(Node node, Variable var, size_t size = 0, size_t offset = 0, bool member = false) {
		if (size == 0) {
			size = var.type.size;
		}

		if (var.type.isStruct && !member) {
			Error(node.error, "Can't push value of struct");
		}

		offset += var.offset;

		switch (size) {
			case 1: output ~= format("ldrb w9, [x20, #%d]\n", offset); break;
			case 2: output ~= format("ldrh w9, [x20, #%d]\n", offset); break;
			case 4: output ~= format("ldr w9, [x20, #%d]\n", offset); break;
			case 8: output ~= format("ldr x9, [x20, #%d]\n", offset); break;
			default: Error(node.error, "Bad variable type size");
		}

		output ~= "str x9, [x19], #8\n";
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
					output ~= format("bl %s\n", ExternSymbol(node.name));
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
				
					output ~= "and sp, x20, ~0xf\n";
					output ~= format("bl %s\n", ExternSymbol(word.symbolName));

					if (!word.isVoid) {
						output ~= "str x0, [x19], #8\n";
					}
				}
				else {
					output ~= format("bl __func__%s\n", node.name.Sanitise());
				}
			}

			if (word.error) {
				if ("__arm64_exception" in words) {
					bool crash;

					if (inScope) {
						crash = !words[thisFunc].error;
					}
					else {
						crash = true;
					}

					if (crash) {
						LoadAddress("x9", "__global_" ~ Sanitise("_cal_exception"));
						output ~= "ldr x9, [x9]\n";
						output ~= "cmp x9, #0\n";
						output ~= format("bne __func__%s\n", Sanitise("__arm64_exception"));
					}
					else {
						CompileReturn(node);
					}
				}
				else {
					Warn(node.error, "No exception handler");
				}
			}
		}
		else if (VariableExists(node.name)) {
			PushVariableValue(node, GetVariable(node.name));
		}
		else if (GlobalExists(node.name)) {
			PushGlobalValue(node, GetGlobal(node.name));
		}
		else if (IsStructMember(node.name)) {
			string name    = node.name[0 .. node.name.countUntil(".")];
			auto structVar = GetStructVariable(node, node.name);

			if (GlobalExists(name)) {
				PushGlobalValue(node, GetGlobal(name), structVar.size, structVar.offset, true);
			}
			else if (VariableExists(name)) {
				PushVariableValue(node, GetVariable(name), structVar.size, structVar.offset, true);
			}
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

		Type[] params;

		foreach (ref type ; node.paramTypes) {
			if (!TypeExists(type)) {
				Error(node.error, "Type '%s' doesn't exist", type);
			}

			params ~= GetType(type);
		}

		if (node.inline) {
			if (node.errors) {
				LoadAddress("x9", "__global_" ~ Sanitise("_cal_exception"));
				output ~= "ldr x10, #0\n";
				output ~= "str x10, [x9]\n";
			}

			words[node.name] = Word(
				WordType.Callisto, true, node.nodes, node.errors, params
			);
		}
		else {
			assert(!inScope);
			inScope = true;

			words[node.name] = Word(
				node.raw? WordType.Raw : WordType.Callisto , false, [], node.errors,
				params
			);

			string symbol =
				node.raw? node.name : format("__func__%s", node.name.Sanitise());

			if (exportSymbols) {
				output ~= format(".global %s\n", symbol);
			}

			output ~= format("%s:\n", symbol);
			output ~= "str lr, [x20, #-8]!\n";

			if (node.errors) {
				LoadAddress("x9", "__global_" ~ Sanitise("_cal_exception"));
				output ~= "ldr x10, #0\n";
				output ~= "str x10, [x9]\n";
			}

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
			if ((paramSize > 0) && !node.manual) {
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
			if (GlobalExists(node.name)) {
				Error(node.error, "Global '%s' already exists", node.name);
			}

			Global global;
			global.type        = GetType(node.varType);
			global.array       = node.array;
			global.arraySize   = node.arraySize;
			global.name        = node.name;
			globals           ~= global;
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
			LoadAddress("x9", format("__array_%d_meta", arrays.length - 1));
			output ~= "str x9, [x19], #8\n";
		}
		else {
			// allocate a copy of this array
			OffsetLocalsStack(array.Size(), true);
			output ~= "mov x9, x20\n";
			LoadAddress("x10", format("__array_%d", arrays.length - 1));
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

				if (node.asName != "") {
					funcName = node.asName;
				}
				break;
			}
		}

		if (word.type != WordType.Callisto) {
			output ~= format(".extern %s\n", ExternSymbol(node.func));
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
				format("__func__%s", node.func.Sanitise()) : ExternSymbol(node.func);

			LoadAddress("x9", symbol);
			output ~= "str x9, [x19], #8\n";
		}
		else if (GlobalExists(node.func)) {
			auto var = GetGlobal(node.func);

			LoadAddress("x9", format("__global_%s", node.func.Sanitise()));
			output ~= "str x9, [x19], #8\n";
		}
		else if (VariableExists(node.func)) {
			auto var = GetVariable(node.func);

			output ~= format("add x9, x20, #%d\n", var.offset);
			output ~= "str x9, [x19], #8\n";
		}
		else if (IsStructMember(node.func)) {
			string name    = node.func[0 .. node.func.countUntil(".")];
			auto structVar = GetStructVariable(node, node.func);
			size_t offset  = structVar.offset;

			if (GlobalExists(name)) {
				auto var = GetGlobal(name);

				LoadAddress("x9", format("__global_%s", node.func.Sanitise()));
				output ~= format("add x9, x9, #%d\n", offset);
				output ~= "str x9, [x19], #8\n";
			}
			else if (VariableExists(node.func)) {
				auto var = GetVariable(name);

				output ~= format("add x9, x20, #%d\n", var.offset + offset);
				output ~= "str x9, [x19], #8\n";
			}
			else assert(0);
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

	void SetVariable(Node node, Variable var, size_t size = 0, size_t offset = 0, bool member = false) {
		if (size == 0) {
			size = var.type.size;
		}

		output ~= "ldr x9, [x19, #-8]!\n";

		if (var.type.isStruct && !member) {
			Error(node.error, "Can't set struct value");
		}

		offset += var.offset;

		switch (size) {
			case 1: output ~= format("strb w9, [x20, #%d]\n", offset); break;
			case 2: output ~= format("strh w9, [x20, #%d]\n", offset); break;
			case 4: output ~= format("str w9, [x20, #%d]\n", offset); break;
			case 8: output ~= format("str x9, [x20, #%d]\n", offset); break;
			default: Error(node.error, "Bad variable type size");
		}
	}

	void SetGlobal(Node node, Global global, size_t size = 0, size_t offset = 0, bool member = false) {
		if (size == 0) {
			size = global.type.size;
		}

		output ~= "ldr x9, [x19, #-8]!\n";

		if (global.type.isStruct && !member) {
			Error(node.error, "Can't set struct value");
		}

		LoadAddress("x10", format("__global_%s", global.name.Sanitise()));

		switch (size) {
			case 1: output ~= format("strb w9, [x10, #%d]\n", offset); break;
			case 2: output ~= format("strh w9, [x10, #%d]\n", offset); break;
			case 4: output ~= format("str w9, [x10, #%d]\n", offset); break;
			case 8: output ~= format("str x9, [x10, #%d]\n", offset); break;
			default: Error(node.error, "Bad variable type size");
		}
	}

	override void CompileSet(SetNode node) {
		if (VariableExists(node.var)) {
			SetVariable(node, GetVariable(node.var));
		}
		else if (GlobalExists(node.var)) {
			SetGlobal(node, GetGlobal(node.var));
		}
		else if (IsStructMember(node.var)) {
			string name    = node.var[0 .. node.var.countUntil(".")];
			auto structVar = GetStructVariable(node, node.var);

			if (VariableExists(name)) {
				SetVariable(node, GetVariable(name), structVar.size, structVar.offset, true);
			}
			else if (GlobalExists(name)) {
				SetGlobal(node, GetGlobal(name), structVar.size, structVar. offset, true);
			}
		}
		else {
			Error(node.error, "Variable '%s' doesn't exist", node.var);
		}
	}

	private void OffsetLocalsStack(size_t offset, bool sub) {
		if (offset >= 4096) {
			output ~= format("mov x9, #%d\n", offset);
			output ~= format("%s x20, x20, x9\n", sub ? "sub" : "add");
		} else {
			output ~= format("%s x20, x20, #%d\n", sub ? "sub" : "add", offset);
		}
	}

	override void CompileTryCatch(TryCatchNode node) {
		if (node.func !in words) {
			Error(node.error, "Function '%s' doesn't exist", node.func);
		}

		auto word = words[node.func];

		if (!word.error) {
			Error(node.error, "Function '%s' doesn't throw", node.func);
		}
		if (word.type != WordType.Callisto) {
			Error(node.error, "Non-callisto functions can't throw");
		}

		if (word.params.length > 0) {
			output ~= format("sub x21, x19, #%d\n", word.params.length * 8);
			output ~= "str x21, [x20, #-8]!\n";
		}
		else {
			output ~= "str x19, [x20, #-8]!\n";
		}

		if (word.inline) {
			foreach (inode ; word.inlineNodes) {
				compiler.CompileNode(inode);
			}
		}
		else {
			output ~= format("bl __func__%s\n", node.func.Sanitise());
		}

		output ~= "ldr x21, [x20], #8\n";

		++ blockCounter;

		LoadAddress("x9", "__global_" ~ Sanitise("_cal_exception"));
		output ~= "ldr x9, [x9]\n";
		output ~= "cmp x9, #0\n";
		output ~= format("beq __catch_%d_end\n", blockCounter);
		output ~= "mov x19, x21\n";

		// create scope
		auto oldVars = variables.dup;
		auto oldSize = GetStackSize();

		foreach (inode ; node.catchBlock) {
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

		output ~= format("__catch_%d_end:\n", blockCounter);
	}

	override void CompileThrow(WordNode node) {
		if (!inScope || (!words[thisFunc].error)) {
			Error(node.error, "Not in a function that can throw");
		}
		if (words[thisFunc].inline) {
			Error(node.error, "Can't use throw in an inline function");
		}

		// set exception error
		LoadAddress("x9", "__global_" ~ Sanitise("_cal_exception"));
		output ~= "mov x10, #-1\n";
		output ~= "str x10, [x9]\n";

		// copy exception message
		output ~= "ldr x10, [x19, #-8]!\n";
		output ~= "add x11, x9, #8\n";
		output ~= "mov x12, #3\n";
		// copy x10 to x11, x12 times
		output ~= "1:\n";
		output ~= "ldr x13, [x10], #8\n";
		output ~= "str x13, [x11], #8\n";
		output ~= "subs x12, x12, #1\n";
		output ~= "bne 1b\n";

		CompileReturn(node);
	}

	private void LoadAddress(string reg, string symbol) {
		if (os == "osx") {
			output ~= format("adrp %s, %s@PAGE\n", reg, symbol);
			output ~= format("add %s, %s, %s@PAGEOFF\n", reg, reg, symbol);
		}
		else {
			output ~= format("ldr %s, =%s\n", reg, symbol);
		}
	}

	private string ExternSymbol(string name) {
		if (os == "osx") {
			return "_" ~ name;
		}
		else {
			return name;
		}
	}
}
