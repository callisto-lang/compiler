module callisto.codeRemover;

import std.stdio;
import std.format;
import std.algorithm;
import callisto.error;
import callisto.parser;

class CodeRemover {
	Node[]         res;
	string[]       usedFunctions;
	Node[][string] functions;
	string[]       funcStack;
	bool           success = true;

	this() {
		usedFunctions = [
			"__x86_64_program_init",
			"__x86_64_program_exit",
			"__rm86_program_init",
			"__rm86_program_exit",
			"uxn_program_init",
			"uxn_program_end"
		];
	}

	final void Error(Char, A...)(ErrorInfo error, in Char[] fmt, A args) {
		ErrorBegin(error);
		stderr.writeln(format(fmt, args));
		PrintErrorLine(error);
		success = false;
	}

	void FindFunctions(Node[] nodes) {
		foreach (ref inode ; nodes) {
			switch (inode.type) {
				case NodeType.Word: {
					auto node = cast(WordNode) inode;

					usedFunctions ~= node.name;

					if (node.name !in functions) {
						continue;
					}

					if (funcStack.canFind(node.name)) {
						continue;
					}

					funcStack ~= node.name;
					FindFunctions(functions[node.name]);
					funcStack = funcStack[0 .. $ - 1];
					break;
				}
				case NodeType.Addr: {
					auto node = cast(AddrNode) inode;

					usedFunctions ~= node.func;

					if (node.func !in functions) {
						continue;
					}

					if (funcStack.canFind(node.func)) {
						continue;
					}

					funcStack ~= node.func;
					FindFunctions(functions[node.func]);
					funcStack = funcStack[0 .. $ - 1];
					break;
				}
				case NodeType.If: {
					auto node = cast(IfNode) inode;

					Node[][] blocks = node.condition ~ node.doIf ~ node.doElse;

					foreach (ref block ; blocks) {
						FindFunctions(block);
					}
					break;
				}
				case NodeType.While: {
					auto node = cast(WhileNode) inode;

					Node[] blocks = node.condition ~ node.doWhile;

					FindFunctions(blocks);
					break;
				}
				case NodeType.Implement: {
					auto node = cast(ImplementNode) inode;

					FindFunctions(node.nodes);
					break;
				}
				default: break;
			}
		}
	}

	void Run(Node[] nodes) {
		// create function defs first
		foreach (ref inode ; nodes) {
			if (inode.type == NodeType.FuncDef) {
				FuncDefNode node = cast(FuncDefNode) inode;

				if (node.name in functions) {
					Error(
						node.error, "Function '%s' defined multiple times", node.name
					);
				}

				functions[node.name] = node.nodes;
			}
		}

		FindFunctions(nodes);

		res = [];

		foreach (ref inode ; nodes) {
			if (inode.type == NodeType.FuncDef) {
				auto node = cast(FuncDefNode) inode;

				if (!usedFunctions.canFind(node.name)) {
					continue;
				}
			}
			res ~= inode;
		}
	}
}
