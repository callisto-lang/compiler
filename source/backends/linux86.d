module callisto.backends.linux86;

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

class BackendLinux86 : CompilerBackend {
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
		types["u64"]   = Type(8);
		types["i64"]   = Type(8);
		types["addr"]  = Type(8);
		types["size"]  = Type(8);
		types["usize"] = Type(8);
		types["cell"]  = Type(8);

		// built in structs
		types["Array"] = Type(24, true, [
			StructEntry("usize" in types, "length"),
			StructEntry("usize" in types, "memberSize"),
			StructEntry("addr" in types,  "elements")
		]);
		NewConst("Array.length",     0);
		NewConst("Array.memberSize", 8);
		NewConst("Array.elements",   16);
		NewConst("Array.sizeof",     8 * 3);

		foreach (name, ref type ; types) {
			NewConst(format("%s.sizeof", name), cast(long) type.size);
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

	size_t GetStackSize() {
		// old
		//return variables.empty()? 0 : variables[0].offset + variables[0].type.size;

		size_t size;
		foreach (ref var ; variables) {
			size += var.Size();
		}

		return size;
	}

	override string[] GetVersions() => ["Linux86", "Linux", "LittleEndian", "16Bit"];

	override string[] FinalCommands() => [
		format("mv %s %s.asm", compiler.outFile, compiler.outFile),
		useDebug?
			format(
				"nasm -f elf64 %s.asm -o %s.o -F dwarf -g", compiler.outFile,
				compiler.outFile
			) :
			format("nasm -f elf64 %s.asm -o %s.o", compiler.outFile, compiler.outFile),
		format("ld %s.o -o %s", compiler.outFile, compiler.outFile),
		format("rm %s.asm %s.o", compiler.outFile, compiler.outFile)
	];

	override void BeginMain() {
		output ~= "__calmain:\n";
	}

	override void Init() {
		output ~= "global _start\n";
		output ~= "section .text\n";
		output ~= "_start:\n";

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
				output ~= format("call __func__%s\n", node.name.Sanitise());
			}
		}
		else if (VariableExists(node.name)) {
			auto var = GetVariable(node.name);

			output ~= "mov rdi, rsp\n";
			output ~= format("add rdi, %d\n", var.offset);
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
			words[node.name] = Word(true, node.nodes);
		}
		else {
			assert(!inScope);
			inScope = true;

			words[node.name] = Word(false, []);

			if (exportSymbols) {
				output ~= format("global __func__%s\n", node.name.Sanitise());
			}

			output ~= format("__func__%s:\n", node.name.Sanitise());

			foreach (ref inode ; node.nodes) {
				compiler.CompileNode(inode);
			}

			//output ~= format("__func_return__%s:\n", node.name.Sanitise());

			size_t scopeSize;
			foreach (ref var ; variables) {
				scopeSize += var.Size();
			}
			output ~= format("add rsp, %d\n", scopeSize);

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
		if (node.varType !in types) {
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
			var.type      = types[node.varType];
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
		}
		else {
			Global global;
			global.type        = types[node.varType];
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

		if (node.arrayType !in types) {
			Error(node.error, "Type '%s' doesn't exist", node.arrayType);
		}

		array.type    = types[node.arrayType];
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
			var.type   = types["Array"];
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

		if (node.name in types) {
			Error(node.error, "Type '%s' defined multiple times", node.name);
		}

		StructEntry[] entries;
		string[]      members;

		if (node.inherits) {
			if (node.inheritsFrom !in types) {
				Error(node.error, "Type '%s' doesn't exist", node.inheritsFrom);
			}

			if (!types[node.inheritsFrom].isStruct) {
				Error(node.error, "Type '%s' is not a structure", node.inheritsFrom);
			}

			entries = types[node.inheritsFrom].structure;

			foreach (ref member ; types[node.inheritsFrom].structure) {
				members ~= member.name;
			}
		}

		foreach (ref member ; node.members) {
			if (member.type !in types) {
				Error(node.error, "Type '%s' doesn't exist", member.type);
			}
			if (members.canFind(member.name)) {
				Error(node.error, "Duplicate member '%s'", member.name);
			}

			entries ~= StructEntry(
				member.type in types, member.name, member.array, member.size
			);
			members ~= member.name;
		}

		foreach (ref member ; entries) {
			NewConst(format("%s.%s", node.name, member.name), offset);
			offset += member.array? member.type.size * member.size : member.type.size;
		}

		NewConst(format("%s.sizeof", node.name), offset);
		types[node.name] = Type(offset, true, entries);
	}
	
	override void CompileReturn(WordNode node) {
		if (!inScope) {
			Error(node.error, "Return used outside of function");
		}

		size_t scopeSize;
		foreach (ref var ; variables) {
			scopeSize += var.Size();
		}
		output ~= format("add rsp, %d\n", scopeSize);
		output ~= "ret\n";
	}

	override void CompileConst(ConstNode node) {
		if (node.name in consts) {
			Error(node.error, "Constant '%s' already defined", node.name);
		}
		
		NewConst(node.name, node.value);
	}

	override void CompileEnum(EnumNode node) {
		if (node.enumType !in types) {
			Error(node.error, "Enum base type '%s' doesn't exist", node.enumType);
		}
		if (node.name in types) {
			Error(node.error, "Enum name is already used by type '%s'", node.enumType);
		}

		types[node.name] = types[node.enumType];

		foreach (i, ref name ; node.names) {
			NewConst(format("%s.%s", node.name, name), node.values[i]);
		}

		NewConst(format("%s.min", node.name), node.values.minElement());
		NewConst(format("%s.max", node.name), node.values.maxElement());
		NewConst(format("%s.sizeof", node.name), types[node.name].size);
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

		output ~= format("jmp __while_%d_condition\n", currentLoop);
	}

	override void CompileUnion(UnionNode node) {
		size_t maxSize = 0;

		if (node.name in types) {
			Error(node.error, "Type '%s' already exists", node.name);
		}

		string[] unionTypes;

		foreach (ref type ; node.types) {
			if (unionTypes.canFind(type)) {
				Error(node.error, "Union type '%s' defined twice", type);
			}
			unionTypes ~= type;

			if (type !in types) {
				Error(node.error, "Type '%s' doesn't exist", type);
			}

			if (types[type].size > maxSize) {
				maxSize = types[type].size;
			}
		}

		types[node.name] = Type(maxSize);
		NewConst(format("%s.sizeof", node.name), cast(long) maxSize);
	}

	override void CompileAlias(AliasNode node) {
		if (node.from !in types) {
			Error(node.error, "Type '%s' doesn't exist", node.from);
		}
		if ((node.to in types) && !node.overwrite) {
			Error(node.error, "Type '%s' already defined", node.to);
		}

		types[node.to] = types[node.from];
		NewConst(format("%s.sizeof", node.to), cast(long) types[node.to].size);
	}
}
