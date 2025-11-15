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
import callisto.output;
import callisto.parser;
import callisto.compiler;
import callisto.language;
import callisto.preprocessor;
import callisto.mod.sections;

class BackendARM64 : CompilerBackend {
	bool useLibc;

	this() {
		addrSize = 8;

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
		types ~= Type("core", "u8",    1, false);
		types ~= Type("core", "i8",    1, true);
		types ~= Type("core", "u16",   2, false);
		types ~= Type("core", "i16",   2, true);
		types ~= Type("core", "u32",   4, false);
		types ~= Type("core", "i32",   4, true);
		types ~= Type("core", "u64",   8, false);
		types ~= Type("core", "i64",   8, true);
		types ~= Type("core", "addr",  8, false);
		types ~= Type("core", "isize", 8, true);
		types ~= Type("core", "usize", 8, false);
		types ~= Type("core", "cell",  8, false);
		types ~= Type("core", "icell", 8, true);
		types ~= Type("core", "bool",  8, false);

		// built in structs
		types ~= Type("core", "Array", 24, false, true, [
			StructEntry(UsedType(GetType("core.usize"), false), "length", false, 8, 0),
			StructEntry(UsedType(GetType("core.usize"), false), "memberSize", false, 8, 8),
			StructEntry(UsedType(GetType("core.addr"), false),  "elements", false, 8, 16)
		]);
		NewConst("core", "Array.length",     0);
		NewConst("core", "Array.memberSize", 8);
		NewConst("core", "Array.elements",   16);
		NewConst("core", "Array.sizeOf",     8 * 3);

		types ~= Type("core", "Exception", 24 + 8, false, true, [
			StructEntry(UsedType(GetType("core.bool"), false),  "error", false, 8, 0),
			StructEntry(UsedType(GetType("core.Array"), false), "msg", false, 8 * 3, 8)
		]);
		NewConst("core", "Exception.bool",   0);
		NewConst("core", "Exception.msg",    8);
		NewConst("core", "Exception.sizeOf", 24 + 8);

		foreach (ref type ; types) {
			NewConst("core", format("%s.sizeOf", type.name), cast(long) type.size);
		}
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
		if (output.mode == OutputMode.Module) return [];

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

		if (os == "osx") {
			linkCommand ~= " -lSystem -syslibroot `xcrun --sdk macosx --show-sdk-path`";
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
			}
			else if (os == "osx") {
				// Always supported and enabled.
			}
			else {
				WarnNoInfo("Cannot use libc on operating system '%s'", os);
			}
		}

		ret ~= linkCommand;

		if (!keepAssembly) {
			 ret ~= format("rm %s.asm %s.o", compiler.outFile, compiler.outFile);
		}
		
		return ret;
	}

	override long   MaxInt()        => -1;
	override string ExecExt()       => "";
	override string DefaultHeader() => "";

	override bool HandleOption(string opt, ref string[] versions, Preprocessor preproc) {
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
		output.StartSection(SectionType.TopLevel);
		if (output.mode != OutputMode.Module) {
			output ~= "__calmain:\n";
		}

		// call constructors
		foreach (global ; globals) {
			if (global.type.hasInit && !global.type.ptr) {
				if (global.mod != output.GetModName()) continue;

				LoadAddress("x9", Label("__global_", global.name.Sanitise()));
				output ~= "str x9, [x19], #8\n";
				output ~= format("bl %s\n", Label("__type_init_", global.type.name.Sanitise()));
			}
		}
	}

	void CallFunction(string name) {
		assert(CountWords(name) == 1);
		auto word = GetWord(name);

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

		if (output.mode == OutputMode.Module) return;

		output ~= ".text\n";
		if (os == "osx") {
			output ~= ".global _main\n";
			output ~= "_main:\n";
		}
		else if (useLibc) {
			output ~= ".global main\n";
			output ~= "main:\n";
		}
		else {
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
			if (global.type.hasDeinit && !global.type.ptr) {
				LoadAddress("x9", Label("__global_", global.name.Sanitise()));
				output ~= "str x9, [x19], #8\n";
				output ~= format("bl %s\n", Label("__type_deinit_", global.type.name.Sanitise()));
			}
		}

		if (output.mode != OutputMode.Module) {
			// exit program
			string exit = "__arm64_program_exit";

			if (WordExists(exit)) {
				if (CountWords(exit) > 1) {
					ErrorNoInfo("Multiple matches for program exit word");
				}

				CallFunction(exit);
			}
			else {
				WarnNoInfo("No exit function available, expect bugs");
			}

			// init program
			output ~= "__init:\n";
			string init = "__arm64_program_init";
			if (WordExists(init)) {
				if (CountWords(exit) > 1) {
					ErrorNoInfo("Multiple matches for program init word");
				}

				CallFunction(init);
			}
			else {
				WarnNoInfo("No program init function available");
			}
			output ~= "ret\n";
		}

		// end of top level code
		output.FinishSection();

		// create global variables
		output.StartSection(SectionType.BSS);

		if (output.mode != OutputMode.Module) {
			output ~= ".bss\n";
			output ~= ".lcomm __exception, 32\n";
		}

		foreach (var ; globals) {
			if (var.mod != output.GetModName()) continue;

			output ~= format(".lcomm %s, %d\n", Label("__global_", var.name.Sanitise()), var.Size());

			if (exportSymbols) {
				output ~= format(".global %s\n", Label("__global_", var.name.Sanitise()));
			}
		}
		output.FinishSection();

		// create arrays
		output.StartSection(SectionType.Data);

		if (output.mode != OutputMode.Module) {
			output ~= ".data\n";
		}

		foreach (i, ref array ; arrays) {
			string arrayLabel = Label("__array_", "%d", i);

			output ~= ".align 8\n";
			if (exportSymbols) {
				output ~= format(".global %s\n", arrayLabel);
			}

			output ~= format("%s: ", arrayLabel);

			switch (array.type.Size()) {
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
					"%s_meta: .8byte %d, %d, %s\n",
					arrayLabel,
					array.values.length,
					array.type.Size(),
					arrayLabel,
				);
			}
		}
		output.FinishSection();
	}

	void PushGlobalValue(
		Node node, Global var, size_t size = 0, size_t offset = 0,
		bool member = false, bool deref = false
	) {
		if (size == 0) {
			size = var.type.Size();
		}

		LoadAddress("x9", ExtLabel(var.mod, "__global_", var.name.Sanitise()));

		if (deref) {
			output ~= "ldr x9, [x9]\n";
		}

		if (var.type.isSigned) {
			switch (size) {
				case 1: output ~= format("ldrsb x9, [x9, #%d]\n", offset); break;
				case 2: output ~= format("ldrsh x9, [x9, #%d]\n", offset); break;
				case 4: output ~= format("ldrsw x9, [x9, #%d]\n", offset); break;
				case 8: output ~= format("ldr x9, [x9, #%d]\n", offset); break;
				default: Error(node.error, "Bad variable type size");
			}
		}
		else {
			switch (size) {
				case 1: output ~= format("ldrb w9, [x9, #%d]\n", offset); break;
				case 2: output ~= format("ldrh w9, [x9, #%d]\n", offset); break;
				case 4: output ~= format("ldr w9, [x9, #%d]\n", offset); break;
				case 8: output ~= format("ldr x9, [x9, #%d]\n", offset); break;
				default: Error(node.error, "Bad variable type size");
			}
		}
	
		output ~= "str x9, [x19], #8\n";
	}

	void PushVariableValue(
		Node node, Variable var, size_t size = 0, size_t offset = 0,
		bool member = false, bool deref = false
	) {
		if (size == 0) {
			size = var.type.Size();
		}

		string base;
		if (deref) {
			output ~= format("ldr x9, [x20, #%d]\n", var.offset);
			base = "x9";
		} else {
			offset += var.offset;
			base = "x20";
		}

		if (var.type.isSigned) {
			switch (size) {
				case 1: output ~= format("ldrsb x9, [%s, #%d]\n", base, offset); break;
				case 2: output ~= format("ldrsh x9, [%s, #%d]\n", base, offset); break;
				case 4: output ~= format("ldrsw x9, [%s, #%d]\n", base, offset); break;
				case 8: output ~= format("ldr x9, [%s, #%d]\n", base, offset); break;
				default: Error(node.error, "Bad variable type size");
			}
		}
		else {
			switch (size) {
				case 1: output ~= format("ldrb w9, [%s, #%d]\n", base, offset); break;
				case 2: output ~= format("ldrh w9, [%s, #%d]\n", base, offset); break;
				case 4: output ~= format("ldr w9, [%s, #%d]\n", base, offset); break;
				case 8: output ~= format("ldr x9, [%s, #%d]\n", base, offset); break;
				default: Error(node.error, "Bad variable type size");
			}
		}

		output ~= "str x9, [x19], #8\n";
	}

	override void CompileWord(WordNode node) {
		if (CountAll(node.name) > 1) {
			Error(node.error, "Multiple matches for identifier '%s'", node.name);
		}

		if (WordExists(node.name)) {
			auto word = GetWord(node.name);

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
					// Calling convention (simplified):
					// First 8 parameters in x0-x7
					// All remaining parameters on the stack
					//   On Linux, these are extended to 8 bytes
					// Return value in x0
					// sp must be 16-byte aligned when used

					output ~= "mov x9, sp\n";
					output ~= "str x9, [x20, #-8]!\n";
					output ~= "and sp, x20, ~0xf\n";

					ulong registerParams = min(word.params.length, 8);
					ulong stackParams = word.params.length - registerParams;

					if (word.params.length > 8) {
						ulong stackSpace = 0;
						if (os == "osx") {
							for (auto i = 8; i < word.params.length; i++) {
								stackSpace += word.params[i].Size();
							}
						}
						else {
							stackSpace = stackParams * 8;
						}
						ulong stackOffset = stackSpace;

						// Ensure 16-byte alignment of the stack
						stackSpace = (stackSpace + 15) & ~15;
						output ~= format("sub sp, sp, #%d\n", stackSpace);

						for (auto i = 0; i < stackParams; i++) {
							output ~= "ldr x9, [x19, #-8]!\n";
							if (os == "osx") {
								auto size = word.params[i + 8].Size();
								stackOffset -= size;
								switch (size) {
									case 1: output ~= format("strb w9, [sp, #%d]\n", stackOffset); break;
									case 2: output ~= format("strh w9, [sp, #%d]\n", stackOffset); break;
									case 4: output ~= format("str w9, [sp, #%d]\n", stackOffset); break;
									case 8: output ~= format("str x9, [sp, #%d]\n", stackOffset); break;
									default: Error(node.error, "Invalid C function argument size");
								}
							}
							else {
								stackOffset -= 8;
								output ~= format("str x9, [sp, #%d]\n", stackOffset);
							}
						}
					}

					for (int i = cast(int) registerParams - 1; i >= 0; i--) {
						output ~= format("ldr x%d, [x19, #-8]!\n", i);
					}
				
					output ~= format("bl %s\n", ExternSymbol(word.symbolName));

					if (!word.isVoid) {
						output ~= "str x0, [x19], #8\n";
					}

					output ~= "ldr x9, [x20], #8\n";
					output ~= "mov sp, x9\n";
				}
				else {
					if (word.error && GetWord(thisFunc).error) {
						size_t paramSize = word.params.length * 8;

						if (paramSize != 0) {
							output ~= format("sub x15, x19, #%d\n", paramSize);
							output ~= "str x15, [x20, #-8]!\n";
						}
						else {
							output ~= "str x19, [x20, #-8]!\n";
						}
					}
					
					output ~= format("bl %s\n", Label(word));
					output.AddCall(node.name);

					if (word.error && GetWord(thisFunc).error) {
						output ~= "ldr x15, [x20], #8\n";
					}
				}
			}

			if (word.error) {
				size_t num = CountWords("__arm64_exception");
				if (num == 1) {
					bool crash;

					if (inScope) {
						crash = !GetWord(thisFunc).error;
					}
					else {
						crash = true;
					}

					LoadAddress("x9", "__exception");
					output ~= "ldr x9, [x9]\n";
					output ~= "cmp x9, #0\n";
					if (crash) {
						auto handler = GetWord("__arm64_exception");
						output ~= format("bne %s\n", Label(handler));
					}
					else {
						output ~= "beq 1f\n";
						output ~= "mov x19, x15\n";
						CompileReturn(node);
						output ~= "1:\n";
					}
				}
				else if (num > 1) {
					Error(node.error, "Multiple exception handlers");
				}
				else {
					Warn(node.error, "No exception handler");
				}
			}
		}
		else if (VariableExists(node.name)) {
			auto var = GetVariable(node.name);
			if (var.type.isStruct && !var.type.ptr) {
				Error(node.error, "Can't push struct value");
			}
			PushVariableValue(node, var);
		}
		else if (GlobalExists(node.name)) {
			auto var = GetGlobal(node.name);
			if (var.type.isStruct && !var.type.ptr) {
				Error(node.error, "Can't push struct value");
			}
			PushGlobalValue(node, var);
		}
		else if (IsStructMember(node.name)) {
			string name    = node.name[0 .. node.name.countUntil(".")];
			auto structVar = GetStructVariable(node, node.name);

			if (CountAll(name)) {
				Error(node.error, "Multiple matches for identifier '%s'", name);
			}

			if (structVar.structure) {
				Error(node.error, "Can't push the value of an array or structure");
			}

			if (GlobalExists(name)) {
				auto var = GetGlobal(name);
				PushGlobalValue(node, var, structVar.size, structVar.offset, true, var.type.ptr);
			}
			else if (VariableExists(name)) {
				auto var = GetVariable(name);
				PushVariableValue(node, var, structVar.size, structVar.offset, true, var.type.ptr);
			}
			else {
				Error(node.error, "Unknown identifier '%s'", name);
			}
		}
		else if (ConstExists(node.name)) {
			auto value  = GetConst(node.name).value;
			value.error = node.error;

			compiler.CompileNode(value);
		}
		else {
			Error(node.error, "Undefined identifier '%s'", node.name);
		}
	}

	override void CompileSignedInt(SignedIntNode node) {
		output ~= format("ldr x9, =%d\n", node.value);
		output ~= "str x9, [x19], #8\n";
	}
	
	override void CompileInteger(IntegerNode node) {
		output ~= format("ldr x9, =%d\n", node.value);
		output ~= "str x9, [x19], #8\n";
	}
	
	override void CompileFuncDef(FuncDefNode node) {
		if (WordExistsHere(node.name)) {
			Error(node.error, "Function name '%s' already used", node.name);
		}
		if (Language.bannedNames.canFind(node.name)) {
			Error(node.error, "Name '%s' can't be used", node.name);
		}

		thisFunc = format("%s.%s", output.GetModName(), node.name);

		UsedType[] params;

		foreach (ref type ; node.paramTypes) {
			if (!TypeExists(type.name)) {
				Error(node.error, "Type '%s' doesn't exist", type.name);
			}

			params ~= UsedType(GetType(type.name), type.ptr);
		}

		output.StartSection(SectionType.FuncDef);
		if (output.mode == OutputMode.Module) {
			auto sect   = output.ThisSection!FuncDefSection();
			// TODO: the stuff that x86_64 says
			sect.pub    = true;
			sect.inline = node.inline;
			sect.calls  = GetCalls(node.nodes);
			sect.name   = node.name;
			sect.params = params.length;
			sect.ret    = node.returnTypes.length;
		}

		if (node.inline) {
			if (node.errors) {
				LoadAddress("x9", Label("__global_", Sanitise("_cal_exception")));
				output ~= "mov x10, #0\n";
				output ~= "str x10, [x9]\n";
			}

			words ~= Word(
				output.GetModName(), node.name,
				WordType.Callisto, false, true, node.nodes, node.errors, params
			);

			if (output.mode == OutputMode.Module) {
				auto sect = output.ThisSection!FuncDefSection();

				foreach (ref inode ; node.nodes) {
					sect.assembly ~= inode.toString() ~ '\n';
				}
			}
		}
		else {
			assert(!inScope);
			inScope = true;

			words ~= Word(
				output.GetModName(), node.name, node.raw? WordType.Raw : WordType.Callisto,
				false, false, [], node.errors, params,
			);

			string symbol =
				node.raw? node.name : Label("__func__", node.name.Sanitise());

			if (exportSymbols) {
				output ~= format(".global %s\n", symbol);
			}

			output ~= format("%s:\n", symbol);
			output ~= "str lr, [x20, #-8]!\n";

			if (node.errors) {
				LoadAddress("x9", "__exception");
				output ~= "mov x10, #0\n";
				output ~= "str x10, [x9]\n";
			}

			// allocate parameters
			size_t paramSize = node.params.length * 8;
			foreach (ref type ; node.paramTypes) {
				if (!TypeExists(type.name)) {
					Error(node.error, "Type '%s' doesn't exist", type.name);
				}
				if (GetType(type.name).isStruct && !type.ptr) {
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
					var.type.type = GetType(type.name);
					var.type.ptr  = type.ptr;
					var.offset    = cast(uint) offset;
					var.stackSize = 8;
					offset       += var.StackSize();
					variables    ~= var;
				}

				// copy data to parameters
				if (node.params.length > 10) {
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
				else {
					foreach_reverse (ref param ; node.params) {
						auto setNode = new SetNode(node.error);
						setNode.var  = param;
						CompileSet(setNode);
					}
				}
			}

			foreach (ref inode ; node.nodes) {
				compiler.CompileNode(inode);
			}

			size_t scopeSize;
			foreach (ref var ; variables) {
				scopeSize += var.StackSize();

				if (var.type.hasDeinit && !var.type.ptr) {
					output ~= format("add x9, x20, #%d\n", var.offset);
					output ~= "str x9, [x19], #8\n";
					output ~= format("bl %s\n", ExtLabel(var.type.mod, "__type_deinit_", "%s", var.type.name.Sanitise()));
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

		output.FinishSection();
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
			output ~= format("beq %s\n", Label("__if_", "%d_%d", blockNum, condCounter + 1));

			// create scope
			auto oldVars = variables.dup;
			auto oldSize = GetStackSize();

			foreach (ref inode ; node.doIf[i]) {
				compiler.CompileNode(inode);
			}

			// remove scope
			foreach (ref var ; variables) {
				if (oldVars.canFind(var)) continue;
				if (!var.type.hasDeinit || var.type.ptr) continue;

				output ~= format("add x9, x20, #%d\n", var.offset);
				output ~= "str x9, [x19], #8\n";
				output ~= format("bl %s\n", ExtLabel(var.type.mod, "__type_deinit_", "%s", var.type.name.Sanitise()));
			}
			if (GetStackSize() - oldSize > 0) {
				OffsetLocalsStack(GetStackSize() - oldSize, false);
			}
			variables = oldVars;

			output ~= format("b %s\n", Label("__if_", "%d_end", blockNum));

			++ condCounter;
			output ~= format("%s:\n", Label("__if_", "%d_%d", blockNum, condCounter));
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
				if (!var.type.hasDeinit || var.type.ptr) continue;

				output ~= format("add x9, x20, #%d\n", var.offset);
				output ~= "str x9, [x19], #8\n";
				output ~= format("bl %s\n", ExtLabel(var.type.mod, "__type_deinit_", "%s", var.type.name.Sanitise()));
			}
			if (GetStackSize() - oldSize > 0) {
				OffsetLocalsStack(GetStackSize() - oldSize, false);
			}
			variables = oldVars;
		}

		output ~= format("%s:\n", Label("__if_", "%d_end", blockNum));
	}
	
	override void CompileWhile(WhileNode node) {
		++ blockCounter;
		uint blockNum = blockCounter;
		currentLoop   = blockNum;

		output ~= format("b %s\n", Label("__while_", "%d_condition", blockNum));
		output ~= format("%s:\n", Label("__while_", "%d", blockNum));

		// make scope
		auto oldVars = variables.dup;
		auto oldSize = GetStackSize();

		foreach (ref inode ; node.doWhile) {
			inWhile = true;
			compiler.CompileNode(inode);

			currentLoop = blockNum;
		}

		// restore scope
		output ~= format("%s:\n", Label("__while", "%d_next", blockNum));
		foreach (ref var ; variables) {
			if (oldVars.canFind(var)) continue;
			if (!var.type.hasDeinit || var.type.ptr) continue;

			output ~= format("add x9, x20, #%d\n", var.offset);
			output ~= "str x9, [x19], #8\n";
			output ~= format("bl %s\n", ExtLabel(var.type.mod, "__type_deinit_", "%s", var.type.name.Sanitise()));
		}
		if (GetStackSize() - oldSize > 0) {
			OffsetLocalsStack(GetStackSize() - oldSize, false);
		}
		variables = oldVars;

		inWhile = false;

		output ~= format("%s:\n", Label("__while_", "%d_condition", blockNum));
		
		foreach (ref inode ; node.condition) {
			compiler.CompileNode(inode);
		}

		output ~= "ldr x9, [x19, #-8]!\n";
		output ~= "cmp x9, #0\n";
		output ~= format("bne %s\n", Label("__while_", "%d", blockNum));
		output ~= format("%s:\n", Label("__while_", "%d_end", blockNum));
	}

	override void CompileLet(LetNode node) {
		if (!TypeExists(node.varType.name)) {
			Error(node.error, "Undefined type '%s'", node.varType.name);
		}
		if (CountTypes(node.varType.name) > 1) {
			Error(node.error, "Multiple matches for type '%s'", node.varType.name);
		}
		if (node.name != "") {
			if (VariableExists(node.name) || (WordExistsHere(node.name))) {
				Error(node.error, "Variable name '%s' already used", node.name);
			}
			if (Language.bannedNames.canFind(node.name)) {
				Error(node.error, "Name '%s' can't be used", node.name);
			}
		}

		if (inScope) {
			Variable var;
			var.name      = node.name;
			var.type.type = GetType(node.varType.name);
			var.type.ptr  = node.varType.ptr;
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

			if (var.name == "") {
				output ~= "str x20, [x19], #8\n";
			}

			if (var.type.hasInit && !var.type.ptr) { // call constructor
				output ~= "str x20, [x19], #8\n";
				output ~= format(
					"bl %s\n",
					ExtLabel(var.type.mod, "__type_init_", "%s", var.type.name.Sanitise())
				);
			}
		}
		else {
			if (GlobalExistsHere(node.name)) {
				Error(node.error, "Global '%s' already exists", node.name);
			}

			Global global;
			global.type.type   = GetType(node.varType.name);
			global.type.ptr    = node.varType.ptr;
			global.array       = node.array;
			global.arraySize   = node.arraySize;
			global.name        = node.name;
			global.mod         = output.GetModName();
			globals           ~= global;

			if (global.name == "") {
				Error(
					node.error,
					"Anonymous variables can only be created inside a function"
				);
			}

			output.AddGlobal(global);
		}
	}
	
	override void CompileArray(ArrayNode node) {
		Array array;

		foreach (ref elem ; node.elements) {
			switch (elem.type) {
				case NodeType.SignedInt: {
					auto node2    = cast(SignedIntNode) elem;
					array.values ~= node2.value.text();
					break;
				}
				case NodeType.Integer: {
					auto node2    = cast(IntegerNode) elem;
					array.values ~= node2.value.text();
					break;
				}
				default: {
					Error(elem.error, "Type '%s' can't be used in array literal", elem.type);
				}
			}
		}

		if (!TypeExists(node.arrayType.name)) {
			Error(node.error, "Type '%s' doesn't exist", node.arrayType.name);
		}
		if (CountTypes(node.arrayType.name) > 1) {
			Error(node.error, "Multiple matches for type '%s'", node.arrayType.name);
		}

		array.type.type  = GetType(node.arrayType.name);
		array.type.ptr   = node.arrayType.ptr;
		array.global     = !inScope || node.constant;
		arrays          ~= array;

		if (!inScope || node.constant) {
			LoadAddress("x9", Label("__array_", "%d_meta", arrays.length - 1));
			output ~= "str x9, [x19], #8\n";
		}
		else {
			// allocate a copy of this array
			OffsetLocalsStack(array.Size(), true);
			output ~= "mov x9, x20\n";
			LoadAddress("x10", Label("__array_", "%d", arrays.length - 1));
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
			var.type.type = UsedType(GetType("core.Array"), false);
			var.type.ptr  = false;
			var.offset    = 0;
			var.array     = false;

			foreach (ref var2 ; variables) {
				var2.offset += var.Size();
			}

			variables ~= var;

			output ~= "mov x9, x20\n";
			output ~= format("sub x20, x20, #%d\n", var.type.Size()); // size of Array structure
			output ~= format("ldr x10, =%d\n", array.values.length);
			output ~= "str x10, [x20]\n"; // length
			output ~= format("ldr x10, =%d\n", array.type.Size());
			output ~= "str x10, [x20, #8]\n"; // member size
			output ~= "str x9, [x20, #16]\n"; // elements

			// push metadata address
			output ~= "mov x9, x20\n";
			output ~= "str x9, [x19], #8\n";
		}
	}
	
	override void CompileString(StringNode node) {
		auto arrayNode = new ArrayNode(node.error);

		arrayNode.arrayType = new TypeNode(node.error, "core.u8", false);
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
			scopeSize += var.StackSize();

			if (var.type.hasDeinit && !var.type.ptr) {
				output ~= format("add x9, x20, #%d\n", var.offset);
				output ~= "str x9, [x19], #8\n";
				output ~= format("bl %s\n", ExtLabel(var.type.mod, "__type_deinit_", "%s", var.type.name.Sanitise()));
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

		output ~= format("b %s\n", Label("__while_", "%d_end", currentLoop));
	}

	override void CompileContinue(WordNode node) {
		if (!inWhile) {
			Error(node.error, "Not in while loop");
		}

		output ~= format("b %s\n", Label("__while_", "%d_next", currentLoop));
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
					if (!TypeExists(param.name)) {
						Error(node.error, "Unknown type '%s'", param.name);
					}
					if (CountTypes(param.name) > 1) {
						Error(node.error, "Multiple matches for type '%s'", param.name);
					}

					word.params ~= UsedType(GetType(param.name), param.ptr);
				}

				if ((node.retType.name == "void") && !node.retType.ptr) {
					word.isVoid = true;
				}
				else {
					if (!TypeExists(node.retType.name)) {
						Error(node.error, "Unknown type '%s'", node.retType.name);
					}
					if (CountTypes(node.retType.name) > 1) {
						Error(
							node.error, "Multiple matches for type '%s'",
							node.retType.name
						);
					}

					word.ret = UsedType(GetType(node.retType.name), node.retType.ptr);
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

		word.name  = funcName;
		words     ~= word;
		
		if (node.externType == ExternType.C) {
			auto sect    = new ExternSection();
			sect.type    = ExternSectType.C;
			sect.returns = word.isVoid?
				[] : [ExternValue(word.ret.ptr, word.ret.FullName())];

			foreach (ref param ; word.params) {
				sect.params ~= ExternValue(param.ptr, param.FullName());
			}

			sect.symbolName = word.symbolName;
			sect.funcName   = word.name;
	
			output.AddSection(sect);
		}
	}

	override void CompileCall(WordNode node) {
		output ~= "ldr x9, [x19, #-8]!\n";
		output ~= "blr x9\n";
	}

	override void CompileAddr(AddrNode node) {
		if (CountAll(node.func) > 1) {
			Error(node.error, "Multiple matches for identifier '%s'", node.func);
		}

		if (WordExists(node.func)) {
			auto   word   = GetWord(node.func);
			string symbol = word.type == WordType.Callisto?
				ExtLabel(word.mod, "__func__", "%s", node.func.Sanitise()) :
				ExternSymbol(node.func);

			LoadAddress("x9", symbol);
			output ~= "str x9, [x19], #8\n";
		}
		else if (GlobalExists(node.func)) {
			auto var = GetGlobal(node.func);

			LoadAddress("x9", ExtLabel(var.mod, "__global_", "%s", node.func.Sanitise()));
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
				if (CountGlobals(name) > 1) {
					Error(node.error, "Multiple matches for identifier '%s'", name);
				}
				auto var = GetGlobal(name);

				LoadAddress("x9", ExtLabel(var.mod, "__global_", "%s", name.Sanitise()));
				if (var.type.ptr) {
					output ~= "ldr x9, [x9]\n";
				}
				output ~= format("add x9, x9, #%d\n", offset);
				output ~= "str x9, [x19], #8\n";
			}
			else if (VariableExists(name)) {
				auto var = GetVariable(name);

				if (var.type.ptr) {
					output ~= format("add x9, x20, #%d\n", var.offset);
					output ~= "ldr x9, [x9]\n";
					output ~= format("add x9, x9, #%d\n", offset);
				} else {
					output ~= format("add x9, x20, #%d\n", var.offset + offset);
				}
				output ~= "str x9, [x19], #8\n";
			}
			else {
				Error(node.error, "Variable '%s' does not exist", name);
			}
		}
		else {
			Error(node.error, "Undefined identifier '%s'", node.func);
		}
	}

	override void CompileImplement(ImplementNode node) {
		if (!TypeExists(node.structure)) {
			Error(node.error, "Type '%s' doesn't exist", node.structure);
		}
		if (CountTypes(node.structure) > 1) {
			Error(node.error, "Multiple matches for type '%s'", node.structure);
		}
		auto type = GetType(node.structure);

		if (type.mod != output.GetModName()) {
			Error(node.error, "Cannot implement method for type defined outside of this module");
		}

		string labelName;

		switch (node.method) {
			case "init": {
				if (GetType(node.structure).hasInit) {
					Error(node.error, "Already implemented in type");
				}

				type.hasInit = true;
				labelName = Label("__type_init_", Sanitise(node.structure));
				break;
			}
			case "deinit": {
				if (GetType(node.structure).hasDeinit) {
					Error(node.error, "Already implemented in type");
				}

				type.hasDeinit = true;
				labelName = Label("__type_deinit_", Sanitise(node.structure));
				break;
			}
			default: Error(node.error, "Unknown method '%s'", node.method);
		}

		SetType(type.FullName(), type);

		assert(!inScope);
		inScope = true;

		output.StartSection(SectionType.Implement);
		if (output.mode == OutputMode.Module) {
			auto sect   = output.ThisSection!ImplementSection();
			sect.type   = type.name;
			sect.method = node.method;
		}

		output ~= format("%s:\n", labelName);
		output ~= "str lr, [x20, #-8]!\n";

		foreach (ref inode ; node.nodes) {
			compiler.CompileNode(inode);
		}

		size_t scopeSize;
		foreach (ref var ; variables) {
			scopeSize += var.StackSize();

			if (var.type.hasDeinit && !var.type.ptr) {
				output ~= format("add x9, x20, #%d\n", var.offset);
				output ~= "str x9, [x19], #8\n";
				output ~= format("bl %s\n", ExtLabel(var.type.mod, "__type_deinit_", "%s", var.type.name.Sanitise()));
			}
		}
		if (scopeSize > 0) {
			OffsetLocalsStack(scopeSize, false);
		}

		output    ~= "ldr lr, [x20], #8\n";
		output    ~= "ret\n";
		inScope    = false;
		variables  = [];

		output.FinishSection();
	}

	void SetVariable(
		Node node, Variable var, size_t size = 0, size_t offset = 0,
		bool member = false, bool deref = false
	) {
		if (size == 0) {
			size = var.type.Size();
		}

		output ~= "ldr x9, [x19, #-8]!\n";

		string base;
		if (deref) {
			output ~= format("ldr x10, [x20, #%d]\n", var.offset);
			base = "x10";
		} else {
			offset += var.offset;
			base = "x20";
		}

		switch (size) {
			case 1: output ~= format("strb w9, [%s, #%d]\n", base, offset); break;
			case 2: output ~= format("strh w9, [%s, #%d]\n", base, offset); break;
			case 4: output ~= format("str w9, [%s, #%d]\n", base, offset); break;
			case 8: output ~= format("str x9, [%s, #%d]\n", base, offset); break;
			default: Error(node.error, "Bad variable type size");
		}
	}

	void SetGlobal(
		Node node, Global global, size_t size = 0, size_t offset = 0,
		bool member = false, bool deref = false
	) {
		if (size == 0) {
			size = global.type.Size();
		}

		output ~= "ldr x9, [x19, #-8]!\n";

		LoadAddress("x10", ExtLabel(global.mod, "__global_", "%s", global.name.Sanitise()));

		if (deref) {
			output ~= "ldr x10, [x10]\n";
		}

		switch (size) {
			case 1: output ~= format("strb w9, [x10, #%d]\n", offset); break;
			case 2: output ~= format("strh w9, [x10, #%d]\n", offset); break;
			case 4: output ~= format("str w9, [x10, #%d]\n", offset); break;
			case 8: output ~= format("str x9, [x10, #%d]\n", offset); break;
			default: Error(node.error, "Bad variable type size");
		}
	}

	override void CompileSet(SetNode node) {
		if (CountAll(node.var) > 1) {
			Error(node.error, "Multiple matches for identifier '%s'", node.var);
		}

		if (VariableExists(node.var)) {
			auto var = GetVariable(node.var);
			if (var.type.isStruct && !var.type.ptr) {
				Error(node.error, "Can't set struct value");
			}
			SetVariable(node, var);
		}
		else if (GlobalExists(node.var)) {
			auto var = GetGlobal(node.var);
			if (var.type.isStruct && !var.type.ptr) {
				Error(node.error, "Can't set struct value");
			}
			SetGlobal(node, var);
		}
		else if (IsStructMember(node.var)) {
			string name = node.var[0 .. node.var.countUntil(".")];

			if (CountAll(name) > 1) {
				Error(node.error, "Multiple matches for identifier '%s'", node.var);
			}

			auto structVar = GetStructVariable(node, node.var);

			if (structVar.structure) {
				Error(node.error, "Can't push the value of an array or structure");
			}

			if (VariableExists(name)) {
				auto var = GetVariable(name);
				SetVariable(node, var, structVar.size, structVar.offset, true, var.type.ptr);
			}
			else if (GlobalExists(name)) {
				auto var = GetGlobal(name);
				SetGlobal(node, var, structVar.size, structVar. offset, true, var.type.ptr);
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
		}
		else {
			output ~= format("%s x20, x20, #%d\n", sub ? "sub" : "add", offset);
		}
	}

	override void CompileTryCatch(TryCatchNode node) {
		if (!WordExists(node.func)) {
			Error(node.error, "Function '%s' doesn't exist", node.func);
		}

		if (CountWords(node.func) > 1) {
			Error(node.error, "Multiple matches for function '%s'", node.func);
		}

		auto word = GetWord(node.func);

		if (!word.error) {
			Error(node.error, "Function '%s' doesn't throw", node.func);
		}
		if (word.type != WordType.Callisto) {
			Error(node.error, "Non-callisto functions can't throw");
		}

		size_t paramSize = word.params.length * 8;

		if (paramSize != 0) {
			output ~= format("sub x15, x19, #%d\n", paramSize);
			output ~= "str x15, [x20, #-8]!\n";
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
			output ~= format("bl %s\n", Label(word));
		}

		output ~= "ldr x15, [x20], #8\n";

		++ blockCounter;

		LoadAddress("x9", "__exception");
		output ~= "ldr x9, [x9]\n";
		output ~= "cmp x9, #0\n";
		output ~= format("beq %s\n", Label("__catch_", "%d_end", blockCounter));

		// function errored, assume that all it did was consume parameters
		output ~= "mov x19, x15\n";

		// create scope
		auto oldVars = variables.dup;
		auto oldSize = GetStackSize();

		foreach (inode ; node.catchBlock) {
			compiler.CompileNode(inode);
		}

		// remove scope
		foreach (ref var ; variables) {
			if (oldVars.canFind(var)) continue;
			if (!var.type.hasDeinit || var.type.ptr) continue;

			output ~= format("add x9, x20, #%d\n", var.offset);
			output ~= "str x9, [x19], #8\n";
			output ~= format("bl %s\n", ExtLabel(var.type.mod, "__type_deinit_", "%s", var.type.name.Sanitise()));
		}
		if (GetStackSize() - oldSize > 0) {
			OffsetLocalsStack(GetStackSize() - oldSize, false);
		}
		variables = oldVars;

		output ~= format("%s:\n", Label("__catch_", "%d_end", blockCounter));
	}

	override void CompileThrow(WordNode node) {
		if (!inScope || (!GetWord(thisFunc).error)) {
			Error(node.error, "Not in a function that can throw");
		}
		if (GetWord(thisFunc).inline) {
			Error(node.error, "Can't use throw in an inline function");
		}

		// set exception error
		LoadAddress("x9", "__exception");
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
