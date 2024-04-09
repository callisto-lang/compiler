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


private struct Type {
	ulong size;
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
	Variable[]       variables;
	Global[string]   globals;

	this() {
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
		types["Array"] = Type(24);

		foreach (name, ref type ; types) {
			NewConst(format("%s.sizeof", name), cast(long) type.size);
		}
	}

	void NewConst(string name, long value, ErrorInfo error = ErrorInfo.init) {
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
		return variables.empty()? 0 : variables[0].offset + variables[0].type.size;
	}

	override string[] GetVersions() => ["Linux86", "Linux"];

	override string[] FinalCommands() => [
		format("mv %s %s.asm", compiler.outFile, compiler.outFile),
		format("nasm -f elf64 %s.asm -o %s.o", compiler.outFile, compiler.outFile),
		format("ld %s.o -o %s", compiler.outFile, compiler.outFile)
	];

	override void Init() {
		output ~= "global _start\n";
		output ~= "section .text\n";
		output ~= "_start:\n";

		// allocate data stack
		output ~= "sub rsp, 4096\n"; // 512 cells
		output ~= "mov r15, rsp\n";
	}
	
	override void End() {
		output ~= "mov rax, 60\n";
		output ~= "mov rdi, 0\n";
		output ~= "syscall\n";

		output ~= "section .bss\n";

		foreach (name, var ; globals) {
			output ~= format("__global_%s: resb %d\n", name, var.Size());
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
		output ~= format("mov qword [r15], %d\n", node.value);
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

			output ~= format("jmp __func_end__%s\n", node.name.Sanitise());
			output ~= format("__func__%s:\n", node.name.Sanitise());

			foreach (ref inode ; node.nodes) {
				compiler.CompileNode(inode);
			}

			output ~= format("__func_return__%s:\n", node.name.Sanitise());

			size_t scopeSize;
			foreach (ref var ; variables) {
				scopeSize += var.Size();
			}
			output ~= format("add sp, %d\n", scopeSize);

			output    ~= "ret\n";
			output    ~= format("__func_end__%s:\n", node.name.Sanitise());
			inScope    = false;
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
				output ~= format("add sp, %d\n", GetStackSize() - oldSize);
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
				output ~= format("add sp, %d\n", GetStackSize() - oldSize);
			}
			variables = oldVars;
		}

		output ~= format("__if_%d_end:\n", blockNum);
	}
	
	override void CompileWhile(WhileNode node) {
		++ blockCounter;
		uint blockNum = blockCounter;

		output ~= format("jmp __while_%d_condition\n", blockNum);
		output ~= format("__while_%d:\n", blockNum);

		// make scope
		auto oldVars = variables.dup;
		auto oldSize = GetStackSize();

		foreach (ref inode ; node.doWhile) {
			inWhile = true;
			compiler.CompileNode(inode);
		}

		// restore scope
		if (GetStackSize() - oldSize > 0) {
			output ~= format("add sp, %d\n", GetStackSize() - oldSize);
		}
		variables = oldVars;

		output ~= format("__while_%d_condition:\n", blockNum);
		
		foreach (ref inode ; node.condition) {
			inWhile = true;
			compiler.CompileNode(inode);
		}

		inWhile = false;

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
			foreach (ref var ; variables) {
				var.offset += types[node.varType].size; // rm86 said fix this but idfk
			}

			Variable var;
			var.name      = node.name;
			var.type      = types[node.varType];
			var.offset    = 0;
			var.array     = node.array;
			var.arraySize = node.arraySize;

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
				output ~= format("sub sp, %d\n", var.Size());
			}
		}
		else {
			Global global;
			global.type        = types[node.varType];
			globals[node.name] = global;
		}
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
}
