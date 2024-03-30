module callisto.backends.y16;

import std.format;
import callisto.util;
import callisto.error;
import callisto.parser;
import callisto.compiler;

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

class BackendY16 : CompilerBackend {
	Word[string]     words;
	Type[string]     types;
	bool             inScope;
	Constant[string] consts;
	uint             blockCounter;

	override string[] GetVersions() => ["Y16"];

	this() {
		types["u8"]    = Type(1);
		types["i8"]    = Type(1);
		types["u16"]   = Type(2);
		types["i16"]   = Type(2);
		types["addr"]  = Type(2);
		types["size"]  = Type(2);
		types["usize"] = Type(2);
		types["cell"]  = Type(2);
		types["Array"] = Type(6);

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

	override void Init() {
		output ~= "cpp sr bs\n";
		output ~= "ldi a __stack\n";
		output ~= "addp sr a\n";
	}

	override void End() {
		output ~= "hlt\n";
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
}
