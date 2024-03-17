module callisto.parser;

import std.conv;
import std.stdio;
import std.format;
import callisto.error;
import callisto.lexer;

enum NodeType {
	Null,
	Word,
	Integer,
	FuncDef,
	Include,
	Asm
}

class Node {
	NodeType  type;
	ErrorInfo error;

	this() {

	}
}

class WordNode : Node {
	string name;

	this(ErrorInfo perror) {
		type  = NodeType.Word;
		error = perror;
	}

	this(ErrorInfo perror, string pname) {
		type  = NodeType.Word;
		error = perror;
		name  = pname;
	}

	override string toString() => name;
}

class IntegerNode : Node {
	long value;

	this(ErrorInfo perror) {
		type  = NodeType.Integer;
		error = perror;
	}

	this(ErrorInfo perror, long pvalue) {
		type  = NodeType.Integer;
		value = pvalue;
	}

	override string toString() => format("%d", value);
}

class FuncDefNode : Node {
	string name;
	Node[] nodes;
	bool   inline;

	this(ErrorInfo perror) {
		type  = NodeType.FuncDef;
		error = perror;
	}

	override string toString() {
		string ret = format("func %s\n", name);

		foreach (ref node ; nodes) {
			ret ~= "    " ~ node.toString() ~ '\n';
		}

		return ret ~ "\nend";
	}
}

class IncludeNode : Node {
	string path;

	this(ErrorInfo perror) {
		type  = NodeType.Include;
		error = perror;
	}

	override string toString() => format("include \"path\"", path);
}

class AsmNode : Node {
	string code;

	this(ErrorInfo perror) {
		type  = NodeType.Asm;
		error = perror;
	}

	this(ErrorInfo perror, string pcode) {
		type  = NodeType.Asm;
		error = perror;
		code  = pcode;
	}
}

class ParserError : Exception {
	this() {
		super("", "", 0);
	}
}

class Parser {
	Token[] tokens;
	size_t  i;
	Node[]  nodes;

	this() {
		
	}

	ErrorInfo GetError() {
		return ErrorInfo(tokens[i].file, tokens[i].line, tokens[i].col);
	}

	void Error(Char, A...)(in Char[] fmt, A args) {
		ErrorBegin(GetError());
		stderr.writeln(format(fmt, args));
		throw new ParserError();
	}

	void Next() {
		++ i;

		if (i >= tokens.length) {
			-- i;
			Error("Unexpected EOF");
		}
	}

	void Expect(TokenType type) {
		if (tokens[i].type != type) {
			Error("Expected %s, got %s", type, tokens[i].type);
		}
	}

	Node ParseFuncDef(bool inline) {
		auto ret   = new FuncDefNode(GetError());
		ret.inline = inline;

		Next();
		Expect(TokenType.Identifier);
		ret.name = tokens[i].contents;

		Next();
		Expect(TokenType.Identifier);
		if (tokens[i].contents != "begin") {
			Error("Expected begin keyword"); // TODO: add parameters
		}
		Next();

		while (true) {
			if (
				(tokens[i].type == TokenType.Identifier) &&
				(tokens[i].contents == "end")
			) {
				break;
			}

			ret.nodes ~= ParseStatement();

			if (ret.nodes[$ - 1].type == NodeType.FuncDef) {
				Error("Function definitions can't be nested");
			}

			Next();
		}

		return ret;
	}

	Node ParseInclude() {
		auto ret = new IncludeNode(GetError());

		Next();
		Expect(TokenType.String);

		ret.path = tokens[i].contents;
		return ret;
	}

	Node ParseAsm() {
		auto ret = new AsmNode(GetError());
		Next();

		while (true) {
			switch (tokens[i].type) {
				case TokenType.Identifier: {
					if (tokens[i].contents != "end") {
						Error("Unexpected identifier");
					}

					goto end;
				}
				case TokenType.String: {
					ret.code ~= tokens[i].contents ~ '\n';
					break;
				}
				default: {
					Error("Unexpected %s", tokens[i].type);
				}
			}
			Next();
		}

		end:
		return ret;
	}

	Node ParseStatement() {
		switch (tokens[i].type) {
			case TokenType.Integer: {
				return new IntegerNode(GetError(), parse!long(tokens[i].contents));
			}
			case TokenType.Identifier: {
				switch (tokens[i].contents) {
					case "func":    return ParseFuncDef(false);
					case "inline":  return ParseFuncDef(true);
					case "include": return ParseInclude();
					case "asm":     return ParseAsm();
					default: return new WordNode(GetError(), tokens[i].contents);
				}
			}
			default: {
				Error("Unexpected %s", tokens[i].type);
			}
		}

		assert(0);
	}

	void Parse() {
		for (i = 0; i < tokens.length; ++ i) {
			nodes ~= ParseStatement();
		}
	}
}
