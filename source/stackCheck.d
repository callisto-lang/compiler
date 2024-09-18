module callisto.stackCheck;

import std.conv;
import std.range;
import std.stdio;
import std.format;
import callisto.error;
import callisto.parser;

struct Effect {
	size_t push;
	size_t pop;
}

struct Word {
	Effect effect;
}

struct StackCell {
	Node node;
}

class StackCheckerError : Exception {
	this() {
		super("", "", 0);
	}
}

class StackChecker {
	Word[string] words;
	StackCell[]  stack;

	this() {
		
	}

	void ErrorNoThrow(Char, A...)(ErrorInfo error, in Char[] fmt, A args) {
		ErrorBegin(error);
		stderr.writeln(format(fmt, args));
		PrintErrorLine(error);
	}

	void Error(Char, A...)(ErrorInfo error, in Char[] fmt, A args) {
		ErrorBegin(error);
		stderr.writeln(format(fmt, args));
		PrintErrorLine(error);
		throw new StackCheckerError();
	}

	void StackOverflow(Node node, size_t start) {
		ErrorNoThrow(node.error, "Stack overflow");

		for (size_t i = start; i < stack.length; ++ i) {
			auto   cell = stack[i];
			string cellStr;

			switch (cell.node.type) {
				case NodeType.Integer:
				case NodeType.Word:
				case NodeType.Array:
				case NodeType.String:
				case NodeType.Addr:
				case NodeType.Set: cellStr = cell.node.toString(); break;
				default:           cellStr = text(cell.node.type); break;
			}

			stderr.writefln(
				" '%s' -> %s:%d:%d", cellStr, cell.node.error.file,
				cell.node.error.line + 1, cell.node.error.col + 1
			);
		}

		throw new StackCheckerError();
	}

	void Push(Node node, size_t n) {
		foreach (i ; 0 .. n) {
			stack ~= StackCell(node);
		}
	}

	void Pop(Node node, size_t n) {
		foreach (i ; 0 .. n) {
			if (stack.length == 0) {
				Error(node.error, "Stack underflow");
				return;
			}

			stack = stack[0 .. $ - 1];
		}
	}

	void EvaluateWord(WordNode node) {
		if (node.name in words) {
			auto word = words[node.name];

			Pop(node, word.effect.pop);
			Push(node, word.effect.push);
		}
		else {
			Error(node.error, "Unknown word '%s'", node.name);
		}
	}

	void EvaluateFuncDef(FuncDefNode node) {
		words[node.name] = Word(Effect(node.numReturns, node.params.length));

		if (node.unsafe) return;

		auto oldStack = stack;
		stack = [];

		if (node.manual) {
			Push(node, node.params.length);
		}

		Evaluate(node.nodes);

		if (stack.length > node.numReturns) {
			StackOverflow(node, node.numReturns);
		}
	}

	void EvaluateIf(IfNode node) {
		size_t blockStack;

		if (node.hasElse) {
			auto oldStack = stack.dup;
			Evaluate(node.doElse);
			stack = oldStack;
		}

		foreach (i, ref cond ; node.condition) {
			auto oldStack = stack.dup;

			Evaluate(cond);
			Evaluate(node.doIf[i]);

			if (i == 0) {
				blockStack = stack.length;
			}
			else if (stack.length != blockStack) {
				ErrorInfo error = node.doIf[i].empty()?
					node.error : node.doIf[i][$ - 1].error;

				Error(error, "If statement has inconsistent stack effects");
			}

			stack = oldStack;
		}
	}
	void EvaluateNode(Node node) {
		switch (node.type) {
			case NodeType.Word:    EvaluateWord(cast(WordNode) node); break;
			case NodeType.Integer: Push(node, 1); break;
			case NodeType.FuncDef: EvaluateFuncDef(cast(FuncDefNode) node); break;
			case NodeType.Asm: {
				Error(node.error, "Inline assembly can only be used in unsafe code");
				break;
			}
			case NodeType.If:        EvaluateIf(cast(IfNode) node); break;
			case NodeType.While:     assert(0);
			case NodeType.Let:       assert(0);
			case NodeType.Array:     Push(node, 1); break;
			case NodeType.String:    Push(node, 1); break;
			case NodeType.Const:     assert(0);
			case NodeType.Extern:    assert(0);
			case NodeType.Addr:      Push(node, 1); break;
			case NodeType.Implement: assert(0);
			case NodeType.Set:       Pop(node, 1); break;
			case NodeType.TryCatch:  assert(0);
			default: break;
		}
	}

	void Evaluate(Node[] nodes) {
		foreach (ref node ; nodes) {
			EvaluateNode(node);
		}
	}
}
