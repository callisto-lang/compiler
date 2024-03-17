module callisto.backends.rm86;

import std.format;
import callisto.parser;
import callisto.compiler;

private struct Word {
	bool   inline;
	Node[] inlineNodes;
}

class BackendRM86 : CompilerBackend {
	Word[string] words;

	override void Init() {
		output ~= format("org 0x%.4X\n", org);
		output ~= "mov si, __stack\n";
	}

	override void End() {
		output ~= "ret\n__stack: times 512 dw 0\n";
	}

	override void CompileWord(WordNode node) {
		if (node.name !in words) {
			Error(node.error, "Undefined word '%s'", node.name);
		}

		auto word = words[node.name];

		if (word.inline) {
			foreach (inode ; word.inlineNodes) {
				compiler.CompileNode(inode);
			}
		}
		else {
			output ~= format("call __func__%s\n", node.name);
		}
	}

	override void CompileInteger(IntegerNode node) {
		output ~= format("mov word [si], %d\n", node.value);
		output ~= "add si, 2\n";
	}

	override void CompileFuncDef(FuncDefNode node) {
		if (node.inline) {
			words[node.name] = Word(true, node.nodes);
		}
		else {
			output ~= format("jmp __func_end__%s\n", node.name);
			output ~= format("__func__%s:\n", node.name);

			foreach (ref inode ; node.nodes) {
				compiler.CompileNode(inode);
			}

			output ~= "ret\n";
			output ~= format("__func_end__%s:\n", node.name);

			words[node.name] = Word(false, []);
		}
	}
}
