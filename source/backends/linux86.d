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

	override string[] GetVersions() => ["Linux86", "Linux"];

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
		++ blockCounter;
		uint blockNum = blockCounter;

		output ~= format("jmp __while_%d_condition\n", blockNum);
		output ~= format("__while_%d:\n", blockNum);

		foreach (ref inode ; node.doWhile) {
			inWhile = true;
			compiler.CompileNode(inode);
		}

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
}
