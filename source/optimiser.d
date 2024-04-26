module callisto.optimiser;

import std.stdio;
import std.format;
import std.algorithm;
import callisto.error;
import callisto.parser;

class OptimiserError : Exception {
	this() {
		super("", "", 0);
	}
}

class Optimiser {
	Node[]         res;
	string[]       usedFunctions;
	Node[][string] functions;

	this() {
		
	}

	final void Error(Char, A...)(ErrorInfo error, in Char[] fmt, A args) {
		ErrorBegin(error);
		stderr.writeln(format(fmt, args));
		throw new OptimiserError();
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

					FindFunctions(functions[node.name]);
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
