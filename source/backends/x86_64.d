module callisto.backends.x86_64;

import std.conv;
import std.file;
import std.path;
import std.stdio;
import std.range;
import std.format;
import std.algorithm;
import callisto.util;
import callisto.error;
import callisto.parser;
import callisto.output;
import callisto.compiler;
import callisto.language;
import callisto.preprocessor;
import callisto.mod.sections;

private enum ABI {
	SysV,
	Windows
};

private enum WordType {
	Callisto,
	Raw,
	C
}

private struct Word {
	WordType   type;
	bool       inline;
	Node[]     inlineNodes;
	bool       error;
	UsedType[] params;

	// for C words
	UsedType ret;
	bool     isVoid;
	string   symbolName;
}

class BackendX86_64 : CompilerBackend {
	Word[string]     words;
	string           thisFunc;
	bool             inScope;
	uint             blockCounter;
	bool             inWhile;
	uint             currentLoop;
	bool             useLibc;
	uint             tempLabelNum;
	bool             useGas = false;
	bool             useFramePtr = false;
	int[string]      fileID;

	this() {
		addrSize = 8;

		version (linux) {
			defaultOS = "linux";
		}
		else version (OSX) {
			defaultOS = "osx";
		}
		else version (FreeBSD) {
			defaultOS = "freebsd";
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
			StructEntry(UsedType(GetType("usize"), false), "length", false, 8, 0),
			StructEntry(UsedType(GetType("usize"), false), "memberSize", false, 8, 8),
			StructEntry(UsedType(GetType("addr"), false), "elements", false, 8, 16)
		]);
		NewConst("Array.length",     0);
		NewConst("Array.memberSize", 8);
		NewConst("Array.elements",   16);
		NewConst("Array.sizeOf",     8 * 3);

		types ~= Type("Exception", 24 + 8, true, [
			StructEntry(UsedType(GetType("bool"), false),  "error", false, 8, 0),
			StructEntry(UsedType(GetType("Array"), false), "msg", false, 24, 8)
		]);
		NewConst("Exception.error",  0);
		NewConst("Exception.msg",    8);
		NewConst("Exception.sizeOf", 24 + 8);

		foreach (ref type ; types) {
			NewConst(format("%s.sizeOf", type.name), cast(long) type.size);
		}

		globals ~= Global(
			"_cal_exception", UsedType(GetType("Exception"), false), false, 0
		);
	}

	int GetMaxFileID() {
		int ret = 0;
		foreach (key, value ; fileID) {
			if (value > ret) ret = value;
		}
		return ret;
	}

	int GetOrCreateFileID(string file) {
		if (file !in fileID) {
			fileID[file] = GetMaxFileID() + 1;
		}
		return fileID[file];
	}

	string TempLabel() {
		++ tempLabelNum;
		return format("__temp_%d", tempLabelNum);
	}

	override void NewConst(string name, long value, ErrorInfo error = ErrorInfo.init) {
		consts[name] = Constant(new IntegerNode(error, value));
	}

	//override string[] GetVersions() => [
	//	"x86_64", "LittleEndian", "16Bit", "32Bit", "64Bit"
	//] ~ (os == "linux"? ["Linux", "IO", "Exit", "Time", "File", "Args", "Heap"] :
	//	 os == "osx"? ["OSX"]);
	override string[] GetVersions() {
		// CPU features
		string[] ret = ["x86_64", "LittleEndian", "16Bit", "32Bit", "64Bit"];

		// OS features
		switch (os) {
			case "linux": {
				ret ~= ["Linux", "IO", "Exit", "Time", "File", "Args", "Heap"];
				break;
			}
			case "osx": {
				ret ~= ["OSX", "IO", "Exit", "Time", "File", "Args"];
				if (useLibc) ret ~= "Heap";
				break;
			}
			case "freebsd": {
				ret ~= ["FreeBSD", "IO", "Exit", "Time", "File", "Args"];
				if (useLibc) ret ~= "Heap";
				break;
			}
			default: break;
		}

		return ret;
	}

	override string[] FinalCommands() {
		if (output.useMod) return [];

		string objFormat;

		switch (os) {
			case "osx":     objFormat = "macho64"; break;
			case "windows": objFormat = "win32"; break;
			default:        objFormat = "elf64";
		}

		string[] ret = [
			format("mv %s %s.asm", compiler.outFile, compiler.outFile),
			useGas?
				format(
					"as %s.asm -o %s.o %s", compiler.outFile, compiler.outFile,
					useDebug? "-g" : ""
				) :
				format(
					"nasm -f %s %s.asm -o %s.o %s", objFormat,
					compiler.outFile, compiler.outFile, useDebug? "-F dwarf -g" : ""
				)
		];

		string linker;

		switch (os) {
			case "windows": linker = "x86_64-w64-mingw32-ld"; break;
			default:        linker = "ld";
		}

		string linkCommand = format(
			"%s %s.o -o %s", linker, compiler.outFile, compiler.outFile
		);

		if (os == "osx") {
			linkCommand ~= " -platform_version macos 10.6 `xcrun --sdk macosx --show-sdk-version`";
			linkCommand ~= " -ld_classic -no_pie -e _main";
			linkCommand ~= " -lSystem -syslibroot `xcrun --sdk macosx --show-sdk-path`";
		}

		foreach (ref lib ; link) {
			linkCommand ~= format(" -l%s", lib);
		}

		if (!link.empty() && os == "linux") {
			// idk if this is correct on all linux systems but whatever
			linkCommand ~= " -dynamic-linker /lib64/ld-linux-x86-64.so.2";
		}

		if (useLibc) {
			if (os == "linux") {
				string[] possiblePaths = [
					"/usr/lib/crt1.o",
					"/usr/lib/x86_64-linux-gnu/crt1.o"
				];
				bool crt1;

				foreach (ref path ; possiblePaths) {
					if (exists(path)) {
						crt1 = true;
						linkCommand ~= format(" %s", path);
						linkCommand ~= format(" %s/crti.o", path.dirName());
						linkCommand ~= format(" %s/crtn.o", path.dirName());
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

	override long MaxInt() => -1;

	override string DefaultHeader() => "";

	override bool HandleOption(string opt, ref string[] versions, Preprocessor preproc) {
		switch (opt) {
			case "use-libc": {
				link     ~= "c";
				useLibc   = true;
				versions ~= "LibC";
				return true;
			}
			case "frame-ptr": {
				preproc.versions ~= "x86_64_FramePtr";
				useFramePtr       = true;
				return true;
			}
			case "use-gas": {
				useGas = true;
				return true;
			}
			default: return false;
		}
	}

	override void BeforeCompile(Node node) {
		static string thisFile;

		if (useDebug && useGas) {
			if (thisFile != node.error.file) {
				thisFile  = node.error.file;

				if (node.error.file !in fileID) {
					output ~= format(
						".file %d \"%s\"\n", GetOrCreateFileID(node.error.file),
						node.error.file
					);
				}
			}

			output ~= format(
				".loc %d %d %d\n", GetOrCreateFileID(node.error.file),
				node.error.line + 1, node.error.col + 1
			);
		}
	}

	override void BeginMain() {
		output.StartSection(SectionType.TopLevel);
		if (useGas && useDebug) {
			output ~= ".type __calmain, @function\n";
		}
		output ~= "__calmain:\n";

		// call constructors
		foreach (global ; globals) {
			if (global.type.hasInit && !global.type.ptr) {
				output ~= format(
					"lea rax, __global_%s\n", global.name.Sanitise()
				);
				output ~= "mov [r15], rax\n";
				output ~= "add r15, 8\n";
				output ~= format("call __type_init_%s\n", global.type.name.Sanitise());
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
			output ~= format("call %s\n", ExternSymbol(name));
		}
		else if (word.type == WordType.C) {
			assert(0); // TODO: error
		}
		else {
			output ~= format("call __func__%s\n", name.Sanitise());
		}
	}

	override void Init() {
		output.macros["QWORD"] = useGas? "qword ptr" : "qword";
		output.macros["DWORD"] = useGas? "dword ptr" : "dword";
		output.macros["WORD"]  = useGas? "word ptr"  : "word";
		output.macros["BYTE"]  = useGas? "byte ptr"  : "byte";

		string[] oses = ["linux", "bare-metal", "osx", "freebsd"];
		if (!oses.canFind(os)) {
			ErrorNoInfo("Backend doesn't support operating system '%s'", os);
		}

		if (useGas) {
			output ~= ".intel_syntax noprefix\n";
			output ~= ".section .text\n";
		}
		else {
			output ~= "section .text\n";
		}

		if (os == "osx") {
			if (useGas) {
				ErrorNoInfo("Cannot use GNU Assembler on x86_64 macOS");
			}

			output ~= "default rel\n";
			output ~= "global _main\n";
			output ~= "_main:\n";
		}
		else if (useLibc) {
			if (useGas) {
				output ~= ".global main\n";
			}
			else {
				output ~= "global main\n";
			}

			output ~= "main:\n";
		}
		else {
			if (useGas) {
				output ~= ".global _start\n";
			}
			else {
				output ~= "global _start\n";
			}

			output ~= "_start:\n";
		}

		output ~= "call __init\n";

		// allocate data stack
		output ~= "sub rsp, 4096\n"; // 512 cells
		output ~= "mov r15, rsp\n";

		// copy static array constants
		output ~= "call __copy_arrays\n";

		// jump to main
		output ~= "jmp __calmain\n";

		// create functions for interop
		if (exportSymbols) {
			output ~= format("
				%sglobal cal_push
				cal_push:
					mov [r15], rdi
					add r15, 8
					ret
				cal_pop:
					sub r15, 8
					mov rax, [r15]
					ret
			", useGas? "." : "");
		}
	}
	
	override void End() {
		// call destructors
		foreach (global ; globals) {
			if (global.type.hasDeinit && !global.type.ptr) {
				output ~= format("lea rax, __global_%s\n", Sanitise(global.name));
				output ~= "mov [r15], rax\n";
				output ~= "add r15, 8\n";
				output ~= format("call __type_deinit_%s\n", Sanitise(global.type.name));
			}
		}

		// exit program
		if ("__x86_64_program_exit" in words) {
			CallFunction("__x86_64_program_exit");
		}
		else {
			WarnNoInfo("No exit function available, expect bugs");
		}

		// create copy arrays function
		output ~= "__copy_arrays:\n";

		foreach (i, ref array ; arrays) {
			output ~= format("lea rsi, __array_src_%d\n", i);
			output ~= format("lea rdi, __array_%d\n", i);
			output ~= format("mov rcx, %d\n", array.Size());
			output ~= "rep movsb\n";
		}

		output ~= "ret\n";

		// run init function
		output ~= "__init:\n";
		if ("__x86_64_program_init" in words) {
			CallFunction("__x86_64_program_init");
		}
		else {
			WarnNoInfo("No program init function available");
		}
		output ~= "ret\n";

		// create global variables
		if (useGas) {
			output ~= ".section .bss\n";
		}
		else {
			output ~= "section .bss\n";
		}

		foreach (var ; globals) {
			if (useGas) {
				output ~= format(
					"__global_%s: .skip %d\n", var.name.Sanitise(), var.Size()
				);
			}
			else {
				output ~= format(
					"__global_%s: resb %d\n", var.name.Sanitise(), var.Size()
				);
			}

			if (exportSymbols) {
				output ~= format(
					"%sglobal __global_%s\n", useGas? "." : "", var.name.Sanitise()
				);
			}
		}

		foreach (i, ref array ; arrays) {
			if (useGas) {
				output ~= format("__array_%d: .skip %d\n", i, array.Size());
			}
			else {
				output ~= format("__array_%d: resb %d\n", i, array.Size());
			}

			if (exportSymbols) {
				output ~= format("%sglobal __array_%d\n", useGas? "." : "", i);
			}
		}

		// create array source
		output ~= format("%ssection .text\n", useGas? "." : "");
		foreach (i, ref array ; arrays) {
			output ~= format("__array_src_%d: ", i);

			switch (array.type.Size()) {
				case 1:  output ~= useGas? ".byte " : "db "; break;
				case 2:  output ~= useGas? ".word " : "dw "; break;
				case 4:  output ~= useGas? ".long " : "dd "; break;
				case 8:  output ~= useGas? ".quad " : "dq "; break;
				default: assert(0);
			}

			foreach (j, ref element ; array.values) {
				output ~= element ~ (j == array.values.length - 1? "" : ", ");
			}

			output ~= '\n';

			if (array.global) {
				output ~= format(
					"__array_%d_meta: %s %d, %d, __array_%d\n", i,
					useGas? ".quad" : "dq",
					array.values.length,
					array.type.Size(),
					i
				);
			}
		}
	}

	void PushGlobalValue(
		Node node, Global var, size_t size = 0, size_t offset = 0, bool member = false,
		bool deref = false
	) {
		if (size == 0) {
			size = var.type.Size();
		}

		if (size != 8) {
			output ~= "xor rax, rax\n";
		}

		string symbol = format("__global_%s", var.name.Sanitise());

		if (deref) {
			output ~= format("mov rbx, [%s]\n", symbol);

			switch (size) {
				case 1: output ~= format("mov al, [rbx + %d]\n", offset); break;
				case 2: output ~= format("mov ax, [rbx + %d]\n", offset); break;
				case 4: output ~= format("mov eax, [rbx + %d]\n", offset); break;
				case 8: output ~= format("mov rax, [rbx + %d]\n", offset); break;
				default: Error(node.error, "Bad variable type size");
			}
		}
		else {
			switch (size) {
				case 1: output ~= format("mov al, [%s + %d]\n", symbol, offset); break;
				case 2: output ~= format("mov ax, [%s + %d]\n", symbol, offset); break;
				case 4: output ~= format("mov eax, [%s + %d]\n", symbol, offset); break;
				case 8: output ~= format("mov rax, [%s + %d]\n", symbol, offset); break;
				default: Error(node.error, "Bad variable type size");
			}
		}

		output ~= "mov [r15], rax\n";
		output ~= "add r15, 8\n";
	}

	void PushVariableValue(
		Node node, Variable var, size_t size = 0, size_t offset = 0, bool member = false,
		bool deref = false
	) {
		if (size == 0) {
			size = var.type.Size();
		}

		if (size != 8) {
			output ~= "xor rax, rax\n";
		}

		if (deref) {
			output ~= format("mov rbx, [rsp + %d]\n", var.offset);

			switch (size) {
				case 1: output ~= format("mov al, [rbx + %d]\n", offset); break;
				case 2: output ~= format("mov ax, [rbx + %d]\n", offset); break;
				case 4: output ~= format("mov eax, [rbx + %d]\n", offset); break;
				case 8: output ~= format("mov rax, [rbx + %d]\n", offset); break;
				default: Error(node.error, "Bad variable type size");
			}
		}
		else {
			switch (size) {
				case 1: output ~= format("mov al, [rsp + %d]\n", offset + var.offset); break;
				case 2: output ~= format("mov ax, [rsp + %d]\n", offset + var.offset); break;
				case 4: output ~= format("mov eax, [rsp + %d]\n", offset + var.offset); break;
				case 8: output ~= format("mov rax, [rsp + %d]\n", offset + var.offset); break;
				default: Error(node.error, "Bad variable type size");
			}
		}

		output ~= "mov [r15], rax\n";
		output ~= "add r15, 8\n";
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
					output ~= format("call %s\n", ExternSymbol(node.name));
				}
				else if (word.type == WordType.C) {
					if (word.params.length >= 1) {
						output ~= format(
							"mov rdi, [r15 - %d]\n", word.params.length * 8
						);
					}
					if (word.params.length >= 2) {
						output ~= format(
							"mov rsi, [r15 - %d]\n", (word.params.length - 1) * 8
						);
					}
					if (word.params.length >= 3) {
						output ~= format(
							"mov rdx, [r15 - %d]\n", (word.params.length - 2) * 8
						);
					}
					if (word.params.length >= 4) {
						output ~= format(
							"mov rcx, [r15 - %d]\n", (word.params.length - 3) * 8
						);
					}
					if (word.params.length >= 5) {
						output ~= format(
							"mov r8, [r15 - %d]\n", (word.params.length - 4) * 8
						);
					}
					if (word.params.length >= 6) {
						output ~= format(
							"mov r9, [r15 - %d]\n", (word.params.length - 5) * 8
						);
					}

					// align stack pointer
					output ~= "mov rbx, rsp\n";
					output ~= "and rsp, 0xFFFFFFFFFFFFFFF0\n";

					if ((word.params.length > 6) && (word.params.length % 2 != 0)) {
						output ~= format("sub rsp, 8\n");
					}

					if (word.params.length > 6) {
						// push parameters
						foreach_reverse (i ; 6 .. word.params.length) {
							output ~= format(
								"push $(QWORD) [r15 - %d]\n", (word.params.length - i) * 8
							);
						}
					}
					// remove parameters
					output ~= format("sub r15, %d\n", word.params.length * 8);
				
					output ~= format("call %s\n", ExternSymbol(word.symbolName));
					output ~= "mov rsp, rbx\n";

					if (!word.isVoid) {
						output ~= "mov [r15], rax\n";
						output ~= "add r15, 8\n";
					}
				}
				else {
					if (word.error && words[thisFunc].error) {
						size_t paramSize = word.params.length * 8;

						if (paramSize != 0) {
							output ~= format("lea r14, [r15 - %d]\n", paramSize);
							output ~= "push r14\n";
						}
						else {
							output ~= "push r15\n";	
						}
					}

					output ~= format("call __func__%s\n", node.name.Sanitise());

					if (word.error && words[thisFunc].error) {
						output ~= "pop r14\n";
					}
				}
			}

			if (word.error) {
				if ("__x86_64_exception" in words) {
					bool crash;

					if (inScope) {
						crash = !words[thisFunc].error;
					}
					else {
						crash = true;
					}

					if (crash) {
						output ~= format("lea rax, __global_%s\n", Sanitise("_cal_exception"));
						output ~= "cmp $(QWORD) [rax], 0\n";
						output ~= format("jne __func__%s\n", Sanitise("__x86_64_exception"));
					}
					else {
						string temp = TempLabel();
						
						output ~= format(
							"cmp $(QWORD) [__global_%s], 0\n", Sanitise("_cal_exception")
						);
						output ~= format("je %s\n", temp);
						output ~= "mov r15, r14\n";
						CompileReturn(node);
						output ~= format("%s:\n", temp);
					}
				}
				else {
					Warn(node.error, "No exception handler");
				}
			}
			return;
		}
		else if (VariableExists(node.name)) {
			auto var = GetVariable(node.name);

			if (var.type.isStruct && !var.type.ptr) {
				Error(node.error, "Can't push value of structures");
			}

			PushVariableValue(node, GetVariable(node.name));
		}
		else if (GlobalExists(node.name)) {
			auto var = GetGlobal(node.name);

			if (var.type.isStruct && !var.type.ptr) {
				Error(node.error, "Can't push value of structures");
			}

			PushGlobalValue(node, GetGlobal(node.name));
		}
		else if (IsStructMember(node.name)) {
			string name    = node.name[0 .. node.name.countUntil(".")];
			auto structVar = GetStructVariable(node, node.name);

			if (structVar.structure) {
				Error(node.error, "Can't push the value of an array or structure");
			}

			if (GlobalExists(name)) {
				auto var = GetGlobal(name);

				PushGlobalValue(
					node, var, structVar.size, structVar.offset, true, var.type.ptr
				);
			}
			else if (VariableExists(name)) {
				auto var = GetVariable(name);

				PushVariableValue(
					node, var, structVar.size, structVar.offset, true, var.type.ptr
				);
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

	override void CompileSignedInt(SignedIntNode node) {
		if ((node.value < 0) || (node.value > 0xFFFFFFFF)) {
			output ~= format("mov r14, %d\n", node.value);
			output ~= "mov [r15], r14\n";
		}
		else {
			output ~= format("mov $(QWORD) [r15], %d\n", node.value);
		}
		output ~= "add r15, 8\n";
	}
	
	override void CompileInteger(IntegerNode node) {
		if ((node.value < 0) || (node.value > 0xFFFFFFFF)) {
			output ~= format("mov r14, %d\n", node.value);
			output ~= "mov [r15], r14\n";
		}
		else {
			output ~= format("mov $(QWORD) [r15], %d\n", node.value);
		}
		output ~= "add r15, 8\n";
	}
	
	override void CompileFuncDef(FuncDefNode node) {
		if (node.name in words) {
			Error(node.error, "Function name '%s' already used", node.name);
		}
		if (Language.bannedNames.canFind(node.name)) {
			Error(node.error, "Name '%s' can't be used", node.name);
		}

		thisFunc = node.name;

		UsedType[] params;

		foreach (ref type ; node.paramTypes) {
			if (!TypeExists(type.name)) {
				Error(node.error, "Type '%s' doesn't exist", type.name);
			}

			params ~= UsedType(GetType(type.name), type.ptr);
		}

		if (node.inline) {
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
				output ~= format("global %s\n", symbol);
			}

			if (useGas && useDebug) {
				output ~= format(".type %s, @function\n", symbol);
			}
			output ~= format("%s:\n", symbol);

			if (useFramePtr) {
				output ~= "push rbp\n";
				output ~= "mov rbp, rsp\n";
			}

			if (node.errors) {
				output ~= format("lea rax, __global_%s\n", Sanitise("_cal_exception"));
				output ~= "mov $(QWORD) [rax], 0\n";
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
				output ~= format("sub rsp, %d\n", paramSize);
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
					output ~= format("sub r15, %d\n", paramSize);
					output ~= "mov rsi, r15\n";
					output ~= "mov rdi, rsp\n";
					output ~= format("mov rcx, %d\n", paramSize / 8);
					output ~= "rep movsq\n";
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

			//output ~= format("__func_return__%s:\n", node.name.Sanitise());

			size_t scopeSize;
			foreach (ref var ; variables) {
				scopeSize += var.StackSize();

				if (var.type.hasDeinit && !var.type.ptr) {
					output ~= format("lea rax, [rsp + %d]\n", var.offset);
					output ~= "mov [r15], rax\n";
					output ~= "add r15, 8\n";
					output ~= format("call __type_deinit_%s\n", Sanitise(var.type.name));
				}
			}

			if (useFramePtr) {
				output ~= "mov rsp, rbp\n";
				output ~= "pop rbp\n";
			}
			else {
				if (scopeSize == 1) {
					output ~= "inc rsp\n";
				}
				else if (scopeSize > 0) {
					output ~= format("add rsp, %d\n", scopeSize);
				}
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
			output ~= "sub r15, 8\n";
			output ~= "mov rax, [r15]\n";
			output ~= "cmp rax, 0\n";
			output ~= format("je __if_%d_%d\n", blockNum, condCounter + 1);

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

				output ~= format("lea rax, [rsp + %d]\n", var.offset);
				output ~= "mov [r15], rax\n";
				output ~= "add r15, 8\n";
				output ~= format("call __type_deinit_%s\n", Sanitise(var.type.name));
			}
			if (GetStackSize() - oldSize > 0) {
				output ~= format("add rsp, %d\n", GetStackSize() - oldSize);
			}
			variables = oldVars;

			output ~= format("jmp __if_%d_end\n", blockNum);

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
				if (!var.type.hasDeinit || var.type.ptr) continue;

				output ~= format("lea rax, [rsp + %d]\n", var.offset);
				output ~= "mov [r15], rax\n";
				output ~= "add r15, 8\n";
				output ~= format("call __type_deinit_%s\n", Sanitise(var.type.name));
			}
			if (GetStackSize() - oldSize > 0) {
				output ~= format("add rsp, %d\n", GetStackSize() - oldSize);
			}
			variables = oldVars;
		}

		output ~= format("__if_%d_end:\n", blockNum);
	}
	
	override void CompileWhile(WhileNode node) {
		++ blockCounter;
		uint blockNum = blockCounter;
		currentLoop   = blockNum;

		output ~= format("jmp __while_%d_condition\n", blockNum);
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
			if (!var.type.hasDeinit || var.type.ptr) continue;

			output ~= format("lea rax, [rsp + %d]\n", var.offset);
			output ~= "mov [r15], rax\n";
			output ~= "add r15, 8\n";
			output ~= format("call __type_deinit_%s\n", Sanitise(var.type.name));
		}
		if (GetStackSize() - oldSize > 0) {
			output ~= format("add rsp, %d\n", GetStackSize() - oldSize);
		}
		variables = oldVars;

		inWhile = false;

		output ~= format("__while_%d_condition:\n", blockNum);
		
		foreach (ref inode ; node.condition) {
			compiler.CompileNode(inode);
		}

		output ~= "sub r15, 8\n";
		output ~= "mov rax, [r15]\n";
		output ~= "cmp rax, 0\n";
		output ~= format("jne __while_%d\n", blockNum);
		output ~= format("__while_%d_end:\n", blockNum);
	}
	
	override void CompileLet(LetNode node) {
		if (!TypeExists(node.varType.name)) {
			Error(node.error, "Undefined type '%s'", node.varType.name);
		}
		if (node.name != "") {
			if (VariableExists(node.name) || (node.name in words)) {
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

			if (var.Size() == 8) {
				output ~= "push 0\n";
			}
			else {
				output ~= format("sub rsp, %d\n", var.Size());
			}

			if (var.name == "") {
				output ~= "mov [r15], rsp\n";
				output ~= "add r15, 8\n";
			}

			if (var.type.hasInit && !var.type.ptr) { // call constructor
				output ~= "mov [r15], rsp\n";
				output ~= "add r15, 8\n";
				output ~= format("call __type_init_%s\n", Sanitise(var.type.name));
			}
		}
		else {
			if (GlobalExists(node.name)) {
				Error(node.error, "Global '%s' already exists", node.name);
			}

			Global global;
			global.type.type   = GetType(node.varType.name);
			global.type.ptr    = node.varType.ptr;
			global.array       = node.array;
			global.arraySize   = node.arraySize;
			global.name        = node.name;
			globals           ~= global;

			if (global.name == "") {
				Error(
					node.error,
					"Anonymous variables can only be created inside a function"
				);
			}
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

		if (!TypeExists(node.arrayType.name)) {
			Error(node.error, "Type '%s' doesn't exist", node.arrayType.name);
		}

		array.type.type  = GetType(node.arrayType.name);
		array.type.ptr   = node.arrayType.ptr;
		array.global     = !inScope || node.constant;
		arrays          ~= array;

		if (!inScope || node.constant) {
			output ~= format("lea rax, __array_%d_meta\n", arrays.length - 1);
			output ~= "mov $(QWORD) [r15], rax\n";
			output ~= "add r15, 8\n";
		}
		else {
			// allocate a copy of this array
			output ~= format("sub rsp, %d\n", array.Size());
			output ~= "mov rax, rsp\n";
			output ~= format("lea rsi, __array_%d\n", arrays.length - 1);
			output ~= "mov rdi, rax\n";
			output ~= format("mov rcx, %d\n", array.Size());
			output ~= format("rep movsb\n");

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
			var.type   = UsedType(GetType("Array"), false);
			var.offset = 0;
			var.array  = false;

			foreach (ref var2 ; variables) {
				var2.offset += var.Size();
			}

			variables ~= var;

			output ~= "mov rax, rsp\n";
			output ~= format("sub rsp, %d\n", 8 * 3); // size of Array structure
			output ~= format("mov $(QWORD) [rsp], %d\n", array.values.length); // length
			output ~= format("mov $(QWORD) [rsp + 8], %d\n", array.type.Size()); // member size
			output ~= "mov [rsp + 16], rax\n"; // elements

			// push metadata address
			output ~= "mov [r15], rsp\n";
			output ~= "add r15, 8\n";
		}
	}
	
	override void CompileString(StringNode node) {
		auto arrayNode = new ArrayNode(node.error);

		arrayNode.arrayType = new TypeNode(node.error, "u8", false);
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

		if (useFramePtr) {
			output ~= "mov rsp, rbp\n";
			output ~= "pop rbp\n";
		}
		else {
			size_t scopeSize;
			foreach (ref var ; variables) {
				scopeSize += var.StackSize();

				if (var.type.hasDeinit && !var.type.ptr) {
					output ~= format("lea rax, [rsp + %d\n]", var.offset);
					output ~= "mov [r15], rax\n";
					output ~= "add r15, 8\n";
					output ~= format("call __type_deinit_%s\n", Sanitise(var.type.name));
				}
			}
			if (scopeSize == 1) {
				output ~= "inc rsp\n";
			}
			else if (scopeSize > 0) {
				output ~= format("add rsp, %d\n", scopeSize);
			}
		}

		output ~= "ret\n";
	}

	override void CompileBreak(WordNode node) {
		if (!inWhile) {
			Error(node.error, "Not in while loop");
		}

		output ~= format("jmp __while_%d_end\n", currentLoop);
	}

	override void CompileContinue(WordNode node) {
		if (!inWhile) {
			Error(node.error, "Not in while loop");
		}

		output ~= format("jmp __while_%d_next\n", currentLoop);
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

					word.params ~= UsedType(GetType(param.name), param.ptr);
				}

				if ((node.retType.name == "void") && !node.retType.ptr) {
					word.isVoid = true;
				}
				else {
					if (!TypeExists(node.retType.name)) {
						Error(node.error, "Unknown type '%s'", node.retType.name);
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
			output ~= format("%sextern %s\n", useGas? "." : "", ExternSymbol(node.func));
		}

		words[funcName] = word;
	}

	override void CompileCall(WordNode node) {
		output ~= "sub r15, 8\n";
		output ~= "mov rax, [r15]\n";
		output ~= "call rax\n";
	}

	override void CompileAddr(AddrNode node) {
		if (node.func in words) {
			auto   word   = words[node.func];
			string symbol = word.type == WordType.Callisto?
				format("__func__%s", node.func.Sanitise()) : ExternSymbol(node.func);

			output ~= format("lea rax, %s\n", symbol);
			output ~= "mov [r15], rax\n";
			output ~= "add r15, 8\n";
		}
		else if (GlobalExists(node.func)) {
			auto var = GetGlobal(node.func);

			output ~= format(
				"lea rax, __global_%s\n", node.func.Sanitise()
			);
			output ~= "mov [r15], rax\n";
			output ~= "add r15, 8\n";
		}
		else if (VariableExists(node.func)) {
			auto var = GetVariable(node.func);

			output ~= "mov rdi, rsp\n";
			if (var.offset > 0) {
				output ~= format("add rdi, %d\n", var.offset);
			}
			output ~= "mov [r15], rdi\n";
			output ~= "add r15, 8\n";
		}
		else if (IsStructMember(node.func)) {
			string name      = node.func[0 .. node.func.countUntil(".")];
			auto   structVar = GetStructVariable(node, node.func);
			size_t offset    = structVar.offset;

			if (GlobalExists(name)) {
				auto var = GetGlobal(name);

				if (var.type.ptr) {
					output ~= format("mov rax, [__global_%s]\n", name.Sanitise());

					if (offset > 0) {
						output ~= format("lea rax, [rax + %d]\n", offset);
					}
				}
				else {
					output ~= format(
						"lea rax, [__global_%s + %d]\n", name.Sanitise(), offset
					);
				}
				
				output ~= "mov [r15], rax\n";
				output ~= "add r15, 8\n";
			}
			else if (VariableExists(name)) {
				auto var = GetVariable(name);

				if (var.type.ptr) {
					output ~= format("mov rdi, [rsp + %d]\n", var.offset);

					if (offset > 0) {
						output ~= format("lea rdi, [rdi + %d]\n", offset);
					}
				}
				else {
					output ~= "mov rdi, rsp\n";
					if (var.offset > 0) {
						output ~= format("add rdi, %d\n", var.offset + offset);
					}
				}
				
				output ~= "mov [r15], rdi\n";
				output ~= "add r15, 8\n";
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

		if (useFramePtr) {
			output ~= "push rbp\n";
			output ~= "mov rbp, rsp\n";
		}

		foreach (ref inode ; node.nodes) {
			compiler.CompileNode(inode);
		}

		if (useFramePtr) {
			output ~= "mov rsp, rbp\n";
			output ~= "pop rbp\n";
		}
		else {
			size_t scopeSize;
			foreach (ref var ; variables) {
				scopeSize += var.StackSize();

				if (var.type.hasDeinit && !var.type.ptr) {
					output ~= format("lea rax, [rsp + %d]\n", var.offset);
					output ~= "mov [r15], rax\n";
					output ~= "add r15, 8\n";
					output ~= format("call __type_deinit_%s\n", Sanitise(var.type.name));
				}
			}
			if (scopeSize == 1) {
				output ~= "inc rsp\n";
			}
			else if (scopeSize > 0) {
				output ~= format("add rsp, %d\n", scopeSize);
			}
		}

		output    ~= "ret\n";
		inScope    = false;
		variables  = [];
	}

	void SetVariable(
		Node node, Variable var, size_t size = 0, size_t offset = 0, bool member = false,
		bool deref = false
	) {
		if (size == 0) {
			size = var.type.Size();
		}

		output ~= "sub r15, 8\n";
		output ~= "mov rax, [r15]\n";

		if (deref) {
			output ~= format("mov rbx, [rsp + %d]\n", var.offset);

			switch (size) {
				case 1: output ~= format("mov [rbx + %d], al\n", offset); break;
				case 2: output ~= format("mov [rbx + %d], ax\n", offset); break;
				case 4: output ~= format("mov [rbx + %d], eax\n", offset); break;
				case 8: output ~= format("mov [rbx + %d], rax\n", offset); break;
				default: Error(node.error, "Bad variable type size");
			}
		}
		else {
			switch (size) {
				case 1: output ~= format("mov [rsp + %d], al\n", var.offset + offset); break;
				case 2: output ~= format("mov [rsp + %d], ax\n", var.offset + offset); break;
				case 4: output ~= format("mov [rsp + %d], eax\n", var.offset + offset); break;
				case 8: output ~= format("mov [rsp + %d], rax\n", var.offset + offset); break;
				default: Error(node.error, "Bad variable type size");
			}
		}
	}

	void SetGlobal(
		Node node, Global global, size_t size = 0, size_t offset = 0,
		bool member = false, bool deref = false
	) {
		if (size == 0) {
			size = global.type.Size();
		}

		output ~= "sub r15, 8\n";
		output ~= "mov rax, [r15]\n";

		string symbol = format("__global_%s", global.name.Sanitise());

		if (deref) {
			output ~= format("mov rbx, [%s]\n", symbol);

			switch (size) {
				case 1: output ~= format("mov [rbx + %d], al\n", offset); break;
				case 2: output ~= format("mov [rbx + %d], ax\n", offset); break;
				case 4: output ~= format("mov [rbx + %d], eax\n", offset); break;
				case 8: output ~= format("mov [rbx + %d], rax\n", offset); break;
				default: Error(node.error, "Bad variable type size");
			}
		}
		else {
			switch (size) {
				case 1: output ~= format("mov [%s + %d], al\n", symbol, offset); break;
				case 2: output ~= format("mov [%s + %d], ax\n", symbol, offset); break;
				case 4: output ~= format("mov [%s + %d], eax\n", symbol, offset); break;
				case 8: output ~= format("mov [%s + %d], rax\n", symbol, offset); break;
				default: Error(node.error, "Bad variable type size");
			}
		}
	}

	override void CompileSet(SetNode node) {
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

			SetGlobal(node, GetGlobal(node.var));
		}
		else if (IsStructMember(node.var)) {
			string name    = node.var[0 .. node.var.countUntil(".")];
			auto structVar = GetStructVariable(node, node.var);

			if (structVar.structure) {
				Error(node.error, "Can't push the value of an array or structure");
			}

			if (VariableExists(name)) {
				auto var = GetVariable(name);

				SetVariable(
					node, var, structVar.size, structVar.offset, true, var.type.ptr
				);
			}
			else if (GlobalExists(name)) {
				auto var = GetGlobal(name);

				SetGlobal(
					node, var, structVar.size, structVar.offset, true, var.type.ptr
				);
			}
		}
		else {
			Error(node.error, "Variable '%s' doesn't exist", node.var);
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

		size_t paramSize = word.params.length * 8;

		if (paramSize != 0) {
			output ~= format("lea r14, [r15 - %d]\n", paramSize);
			output ~= "push r14\n";
		}
		else {
			output ~= "push r15\n";	
		}

		if (word.inline) {
			foreach (inode ; word.inlineNodes) {
				compiler.CompileNode(inode);
			}
		}
		else {
			output ~= format("call __func__%s\n", node.func.Sanitise());
		}

		output ~= "pop r14\n";

		++ blockCounter;

		output ~= format("lea rax, __global_%s\n", Sanitise("_cal_exception"));
		output ~= "cmp $(QWORD) [rax], 0\n";
		output ~= format("je __catch_%d_end\n", blockCounter);

		// function errored, assume that all it did was consume parameters
		output ~= "mov r15, r14\n";

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

			output ~= format("lea rax, [rsp + %d]\n", var.offset);
			output ~= "mov [r15], rax\n";
			output ~= "add r15, 8\n";
			output ~= format("call __type_deinit_%s\n", Sanitise(var.type.name));
		}
		if (GetStackSize() - oldSize > 0) {
			output ~= format("add rsp, %d\n", GetStackSize() - oldSize);
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
		output ~= format("lea rbx, __global_%s\n", Sanitise("_cal_exception"));
		output ~= "mov rax, 0xFFFFFFFFFFFFFFFF\n";
		output ~= "mov [rbx], rax\n";

		// copy exception message
		output ~= "sub r15, 8\n";
		output ~= "mov rsi, [r15]\n";
		output ~= "lea rdi, [rbx + 8]\n";
		output ~= "mov rcx, 3\n";
		output ~= "rep movsq\n";

		CompileReturn(node);
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
