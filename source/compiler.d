module callisto.compiler;

import std.stdio;
import std.format;
import callisto.util;
import callisto.error;
import callisto.parser;
import callisto.language;

class CompilerBackend {
	string   output;
	ulong    org;
	Compiler compiler;

	abstract void Init();
	abstract void End();
	abstract void CompileWord(WordNode node);
	abstract void CompileInteger(IntegerNode node);
	abstract void CompileFuncDef(FuncDefNode node);
	abstract void CompileIf(IfNode node);

	final void Error(Char, A...)(ErrorInfo error, in Char[] fmt, A args) {
		ErrorBegin(error);
		stderr.writeln(format(fmt, args));
		throw new CompilerError();
	}
}

class CompilerError : Exception {
	this() {
		super("", "", 0);
	}
}

class Compiler {
	CompilerBackend backend;

	this() {
		
	}

	void CompileNode(Node inode) {
		switch (inode.type) {
			case NodeType.Word: {
				backend.CompileWord(cast(WordNode) inode);
				break;
			}
			case NodeType.Integer: {
				backend.CompileInteger(cast(IntegerNode) inode);
				break;
			}
			case NodeType.FuncDef: {
				backend.CompileFuncDef(cast(FuncDefNode) inode);
				break;
			}
			case NodeType.Include: {
				auto node  = cast(IncludeNode) inode;
				auto nodes = ParseFile(node.path);

				foreach (inode2 ; nodes) {
					CompileNode(inode2);
				}
				break;
			}
			case NodeType.Asm: {
				auto node       = cast(AsmNode) inode;
				backend.output ~= node.code;
				break;
			}
			case NodeType.If: {
				backend.CompileIf(cast(IfNode) inode);
				break;
			}
			default: assert(0);
		}
	}

	void Compile(Node[] nodes) {
		assert(backend !is null);

		backend.compiler = this;
		backend.Init();

		foreach (ref node ; nodes) {
			CompileNode(node);
		}

		backend.End();
	}
}
