module callisto.backends.y16;

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

private struct Constant {
	Node value;
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

class BackendY16 : CompilerBackend {
	Word[string]     words;
	Type[string]     types;
	bool             inScope;
	Constant[string] consts;
	uint             blockCounter;
	Variable[]       variables;
	Global[string]   globals;

	override string[] GetVersions() => ["Y16"];

	this() {
		types["u8"]    = Type(1);
		types["i8"]    = Type(1);
		types["u16"]   = Type(2);
		types["i16"]   = Type(2);
		types["addr"]  = Type(3);
		types["size"]  = Type(2);
		types["usize"] = Type(2);
		types["cell"]  = Type(2);
		types["Array"] = Type(7);

		foreach (name, ref type ; types) {
			NewConst(format("%s.sizeof", name), cast(long) type.size);
		}

		// struct Array
		//     usize length
		//     usize memberSize
		//     addr  elements
		// end
		NewConst("Array.length",     0);
		NewConst("Array.memberSize", 2);
		NewConst("Array.elements",   4);
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

	override void Init() {
		output ~= "cpp sr bs\n";
		output ~= "ldi a __stack\n";
		output ~= "addp sr a\n";
	}

	override void End() {
		output ~= "hlt\n";

		foreach (name, var ; globals) {
			output ~= format("__global_%s: fill %d 0\n", name, var.Size());
		}

		output ~= "__stack: fill 1024 0\n"; // 512 cell stack
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
				output ~= format("callb __func__%s\n", node.name.Sanitise());
			}
		}
		else if (VariableExists(node.name)) {
			auto var = GetVariable(node.name);

			output ~= "cpp gh sp\n";
			output ~= format("ldi a %d\n", var.offset);
			output ~= "wrw sr g\n";
			output ~= "ldsi b 2\n";
			output ~= "addp sr b\n";
			output ~= "wrw sr h\n";
			output ~= "addp sr b\n";
		}
		else if (node.name in globals) {
			auto var = globals[node.name];

			output ~= "cpp gh bs\n";
			output ~= format("ldi a __global_%s\n", node.name.Sanitise());
			output ~= "addp gh a\n";
			output ~= "wrw sr g\n";
			output ~= "ldsi b 2\n";
			output ~= "addp sr b\n";
			output ~= "wrw sr h\n";
			output ~= "addp sr b\n";
		}
		else if (node.name in consts) {
			compiler.CompileNode(consts[node.name].value);
		}
		else {
			Error(node.error, "Undefined identifier '%s'", node.name);
		}
	}
	
	override void CompileInteger(IntegerNode node) {
		output ~= format("ldi a %d\n", node.value);
		output ~= "wrw sr a\n";
		output ~= "ldsi a 2\n";
		output ~= "addp sr a\n";
	}
	
	override void CompileFuncDef(FuncDefNode node) {
		if (node.inline) {
			words[node.name] = Word(true, node.nodes);
		}
		else {
			assert(!inScope);
			inScope = true;

			words[node.name] = Word(false, []);

			output ~= format("jmpb __func_end__%s\n", node.name.Sanitise());
			output ~= format("__func__%s:\n", node.name.Sanitise());

			foreach (ref inode ; node.nodes) {
				compiler.CompileNode(inode);
			}

			output ~= format("__func_return__%s:\n", node.name.Sanitise());

			output  ~= "ret\n";
			output  ~= format("__func_end__%s:\n", node.name.Sanitise());
			inScope  = false;
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

			output ~= "ldsi b 2\n";
			output ~= "subp sr b\n";
			output ~= "rdw a sr\n";
			output ~= "ldsi b 0\n";
			output ~= "cmp a b\n";
			output ~= format("jzb __if_%d_%d\n", blockNum, condCounter + 1);

			foreach (ref inode ; node.doIf[i]) {
				compiler.CompileNode(inode);
			}

			output ~= format("jmpb __if_%d_end\n", blockNum);

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

		output ~= format("jmpb __while_%d_condition\n", blockNum);
		output ~= format("__while_%d:\n", blockNum);

		foreach (ref inode ; node.doWhile) {
			compiler.CompileNode(inode);
		}

		output ~= format("__while_%d_condition:\n", blockNum);

		foreach (ref inode ; node.condition) {
			compiler.CompileNode(inode);
		}

		output ~= "ldsi b 2\n";
		output ~= "subp sr b\n";
		output ~= "rdw a sr\n";
		output ~= "ldsi b 0\n";
		output ~= "cmp a b\n";
		output ~= format("jnzb __while_%d\n", blockNum);
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
				var.offset += types[node.varType].size;
				// idk but RM86 had a comment here saying TODO: fix this
				// i have no memory of what needs to be fixed
				// but RM86 works fine so i guess its ok
			}

			Variable var;
			var.name      = node.name;
			var.type      = types[node.varType];
			var.offset    = 0;
			var.array     = node.array;
			var.arraySize = node.arraySize;

			variables ~= var;

			if (var.Size() == 2) {
				output ~= "ldsi a 0\n";
				output ~= "push a\n";
			}
			else {
				output ~= format("ldi a %d\n", var.Size());
				output ~= "subp sp a\n";
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
