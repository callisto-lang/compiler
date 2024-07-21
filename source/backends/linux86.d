module callisto.backends.linux86;

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

class BackendLinux86 : CompilerBackend {
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

		foreach (name, ref type ; types) {
			NewConst(format("%s.sizeof", name), cast(long) type.size);
		}

		globals["__linux86_argv"] = Global(GetType("addr"), false, 0);
		globals["__linux86_argc"] = Global(GetType("cell"), false, 0);
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

	override string[] GetVersions() => [
		// platform
		"Linux86", "Linux", "LittleEndian", "16Bit", "32Bit", "64Bit",
		// features
		"IO", "Exit", "Time", "File", "Args", "Heap"
	];

	override string[] FinalCommands() {
		string[] ret = [
			format("mv %s %s.asm", compiler.outFile, compiler.outFile),
			useDebug?
				format(
					"nasm -f elf64 %s.asm -o %s.o -F dwarf -g", compiler.outFile,
					compiler.outFile
				) :
				format("nasm -f elf64 %s.asm -o %s.o", compiler.outFile, compiler.outFile)
		];

		string linkCommand = format("ld %s.o -o %s", compiler.outFile, compiler.outFile);

		foreach (ref lib ; link) {
			linkCommand ~= format(" -l%s", lib);
		}

		if (!link.empty()) {
			// idk if this is correct on all linux systems but whatever
			linkCommand ~= " -dynamic-linker /lib64/ld-linux-x86-64.so.2";
		}

		if (useLibc) {
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
	}

	override void Init() {
		if (useLibc) output ~= "global main\n";
		else         output ~= "global _start\n";
		
		output ~= "section .text\n";
		
		if (useLibc) output ~= "main:\n";
		else         output ~= "_start:\n";

		// get argv and argc
		if (useLibc) {
			output ~= format("mov [__global_%s], edi\n", Sanitise("__linux86_argc"));
			output ~= format("mov [__global_%s], rsi\n", Sanitise("__linux86_argv"));
		}
		else {
			output ~= "mov rsi, [rsp + 8]\n";
			output ~= format("mov [__global_%s], rsi\n", Sanitise("__linux86_argv"));
			output ~= "mov rsi, [rsp]\n";
			output ~= format("mov [__global_%s], rsi\n", Sanitise("__linux86_argc"));
		}

		// allocate data stack
		output ~= "sub rsp, 4096\n"; // 512 cells
		output ~= "mov r15, rsp\n";

		// copy static array constants
		output ~= "call __copy_arrays\n";

		// create functions for interop
		if (exportSymbols) {
			output ~= "
				global cal_push
				cal_push:
					mov [r15], rdi
					add r15, 8
					ret
				cal_pop:
					sub r15, 8
					mov rax, [r15]
					ret
			";
		}

		// jump to main
		output ~= "jmp __calmain\n";
	}
	
	override void End() {
		// call destructors
		foreach (name, global ; globals) {
			if (global.type.hasDeinit) {
				output ~= format("mov qword [r15], qword __global_%s\n", Sanitise(name));
				output ~= "add r15, 8\n";
				output ~= format("call __type_deinit_%s\n", Sanitise(global.type.name));
			}
		}

		// exit program
		output ~= "mov rax, 60\n";
		output ~= "mov rdi, 0\n";
		output ~= "syscall\n";

		// create copy arrays function
		output ~= "__copy_arrays:\n";

		foreach (i, ref array ; arrays) {
			output ~= format("mov rsi, __array_src_%d\n", i);
			output ~= format("mov rdi, __array_%d\n", i);
			output ~= format("mov rcx, %d\n", array.Size());
			output ~= "rep movsb\n";
		}

		output ~= "ret\n";

		// create global variables
		output ~= "section .bss\n";

		foreach (name, var ; globals) {
			output ~= format("__global_%s: resb %d\n", name.Sanitise(), var.Size());

			if (exportSymbols) {
				output ~= format("global __global_%s\n", name.Sanitise());
			}
		}

		foreach (i, ref array ; arrays) {
			output ~= format("__array_%d: resb %d\n", i, array.Size());

			if (exportSymbols) {
				output ~= format("global __array_%d\n", i);
			}
		}

		// create array source
		output ~= "section .text\n";
		foreach (i, ref array ; arrays) {
			output ~= format("__array_src_%d: ", i);

			switch (array.type.size) {
				case 1:  output ~= "db "; break;
				case 2:  output ~= "dw "; break;
				case 4:  output ~= "dd "; break;
				case 8:  output ~= "dq "; break;
				default: assert(0);
			}

			foreach (j, ref element ; array.values) {
				output ~= element ~ (j == array.values.length - 1? "" : ", ");
			}

			output ~= '\n';

			if (array.global) {
				output ~= format(
					"__array_%d_meta: dq %d, %d, __array_%d\n", i,
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
					output ~= format("call %s\n", node.name);
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
					output ~= format("sub r15, %d\n", word.params.length * 8);

					// TODO: support more than 6 parameters
				
					output ~= format("call %s\n", node.name);

					if (!word.isVoid) {
						output ~= "mov [r15], rax\n";
						output ~= "add r15, 8\n";
					}
				}
				else {
					output ~= format("call __func__%s\n", node.name.Sanitise());
				}
			}
		}
		else if (VariableExists(node.name)) {
			auto var = GetVariable(node.name);

			output ~= "mov rdi, rsp\n";
			if (var.offset > 0) {
				output ~= format("add rdi, %d\n", var.offset);
			}
			output ~= "mov [r15], rdi\n";
			output ~= "add r15, 8\n";
		}
		else if (node.name in globals) {
			auto var = globals[node.name];

			output ~= format(
				"mov qword [r15], qword __global_%s\n", node.name.Sanitise()
			);
			output ~= "add r15, 8\n";
		}
		else if (node.name in consts) {
			compiler.CompileNode(consts[node.name].value);
		}
		else {
			Error(node.error, "Undefined identifier '%s'", node.name);
		}
	}
	
	override void CompileInteger(IntegerNode node) {
		if (node.value > 0xFFFF) {
			output ~= format("mov r14, %d\n", node.value);
			output ~= "mov [r15], r14\n";
		}
		else {
			output ~= format("mov qword [r15], %d\n", node.value);
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
				output ~= format("global %s\n", symbol);
			}

			output ~= format("%s:\n", symbol);

			foreach (ref inode ; node.nodes) {
				compiler.CompileNode(inode);
			}

			//output ~= format("__func_return__%s:\n", node.name.Sanitise());

			size_t scopeSize;
			foreach (ref var ; variables) {
				scopeSize += var.Size();

				if (var.type.hasDeinit) {
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
				if (!var.type.hasDeinit)  continue;

				output ~= format("lea rax, [rsp + %d\n]", var.offset);
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
				if (!var.type.hasDeinit)  continue;

				output ~= format("lea rax, [rsp + %d\n]", var.offset);
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
			if (!var.type.hasDeinit)  continue;

			output ~= format("lea rax, [rsp + %d\n]", var.offset);
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

			if (var.Size() == 2) {
				output ~= "push word 0\n";
			}
			else if (var.Size() == 4) {
				output ~= "push dword 0\n";
			}
			else if (var.Size() == 8) {
				output ~= "push qword 0\n";
			}
			else {
				output ~= format("sub rsp, %d\n", var.Size());
			}

			if (var.type.hasInit) { // call constructor
				output ~= "mov [r15], rsp\n";
				output ~= "add r15, 8\n";
				output ~= format("call __type_init_%s\n", Sanitise(var.type.name));
			}
		}
		else {
			Global global;
			global.type        = GetType(node.varType);
			global.array       = node.array;
			global.arraySize   = node.arraySize;
			globals[node.name] = global;

			if (global.type.hasInit) { // call constructor
				output ~= format(
					"mov qword [r15], qword __global_%s\n", node.name.Sanitise()
				);
				output ~= "add r15, 8\n";
				output ~= format("call __type_init_%s\n", global.type.name.Sanitise());
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

		if (!TypeExists(node.arrayType)) {
			Error(node.error, "Type '%s' doesn't exist", node.arrayType);
		}

		array.type    = GetType(node.arrayType);
		array.global  = !inScope || node.constant;
		arrays       ~= array;

		if (!inScope || node.constant) {
			output ~= format("mov qword [r15], __array_%d_meta\n", arrays.length - 1);
			output ~= "add r15, 8\n";
		}
		else {
			// allocate a copy of this array
			output ~= format("sub rsp, %d\n", array.Size());
			output ~= "mov rax, rsp\n";
			output ~= format("mov rsi, __array_%d\n", arrays.length - 1);
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
			var.type   = GetType("Array");
			var.offset = 0;
			var.array  = false;

			foreach (ref var2 ; variables) {
				var2.offset += var.Size();
			}

			variables ~= var;

			output ~= "mov rax, rsp\n";
			output ~= format("sub rsp, %d\n", 8 * 3); // size of Array structure
			output ~= format("mov qword [rsp], %d\n", array.values.length); // length
			output ~= format("mov qword [rsp + 8], %d\n", array.type.size); // member size
			output ~= "mov [rsp + 16], rax\n"; // elements

			// push metadata address
			output ~= "mov [r15], rsp\n";
			output ~= "add r15, 8\n";
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

		output ~= format("jmp __while_%d_end\n", currentLoop);
	}

	override void CompileContinue(WordNode node) {
		if (!inWhile) {
			Error(node.error, "Not in while loop");
		}

		output ~= format("jmp __while_%d_next\n", currentLoop);
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
				break;
			}
		}

		if (word.type != WordType.Callisto) {
			output ~= format("extern %s\n", node.func);
		}

		words[node.func] = word;
	}

	override void CompileCall(WordNode node) {
		output ~= "sub r15, 8\n";
		output ~= "mov rax, [r15]\n";
		output ~= "call rax\n";
	}

	override void CompileFuncAddr(FuncAddrNode node) {
		if (node.func !in words) {
			Error(node.error, "Function '%s' doesn't exist");
		}

		auto   word   = words[node.func];
		string symbol = word.type == WordType.Callisto?
			format("__func__%s", node.func.Sanitise()) : node.func;

		output ~= format("mov rax, %s\n", symbol);
		output ~= "mov [r15], rax\n";
		output ~= "add r15, 8\n";
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

		foreach (ref inode ; node.nodes) {
			compiler.CompileNode(inode);
		}

		size_t scopeSize;
		foreach (ref var ; variables) {
			scopeSize += var.Size();

			if (var.type.hasDeinit) {
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

		output    ~= "ret\n";
		inScope    = false;
		variables  = [];
	}
}
