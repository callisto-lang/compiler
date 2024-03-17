module callisto.backends.rm86;

import std.format;
import callisto.util;
import callisto.parser;
import callisto.compiler;

private struct Word {
	bool   inline;
	Node[] inlineNodes;
}

class BackendRM86 : CompilerBackend {
	Word[string] words;
	uint         blockCounter; // used for block statements

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
			output ~= format("call __func__%s\n", node.name.Sanitise());
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
			output ~= format("jmp __func_end__%s\n", node.name.Sanitise());
			output ~= format("__func__%s:\n", node.name.Sanitise());

			foreach (ref inode ; node.nodes) {
				compiler.CompileNode(inode);
			}

			output ~= "ret\n";
			output ~= format("__func_end__%s:\n", node.name.Sanitise());

			words[node.name] = Word(false, []);
		}
	}

	override void CompileIf(IfNode node) {
		++ blockCounter;
		uint condCounter;

		foreach (i, ref condition ; node.condition) {
			foreach (ref inode ; condition) {
				compiler.CompileNode(inode);
			}
			output ~= "sub si, 2\n";
			output ~= "mov ax, [si]\n";
			output ~= "cmp ax, 0\n";
			output ~= format("je __if_%d_%d\n", blockCounter, condCounter + 1);

			foreach (ref inode ; node.doIf[i]) {
				compiler.CompileNode(inode);
			}

			output ~= format("jmp __if_%d_end\n", blockCounter);

			++ condCounter;
			output ~= format("__if_%d_%d:\n", blockCounter, condCounter);
		}

		if (node.hasElse) {
			foreach (ref inode ; node.doElse) {
				compiler.CompileNode(inode);
			}
		}

		output ~= format("__if_%d_end:\n", blockCounter);
	}
}
