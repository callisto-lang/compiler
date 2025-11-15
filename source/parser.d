module callisto.parser;

import std.conv;
import std.range;
import std.stdio;
import std.format;
import std.algorithm;
import callisto.error;
import callisto.lexer;

enum NodeType {
	Null,
	Type,
	Word,
	Integer,
	SignedInt,
	FuncDef,
	Include,
	Asm,
	If,
	While,
	Let,
	Enable,
	Requires,
	Version,
	Array,
	String,
	Struct,
	Const,
	Enum,
	Restrict,
	Union,
	Alias,
	Extern,
	Addr,
	Implement,
	Set,
	TryCatch,
	Unsafe,
	Anon,
	Import,
	Module
}

class Node {
	NodeType  type;
	ErrorInfo error;

	this() {

	}
}

class TypeNode : Node {
	string name;
	bool   ptr;

	this(ErrorInfo perror) {
		type  = NodeType.Type;
		error = perror;
	}

	this(ErrorInfo perror, string pname, bool pptr) {
		type  = NodeType.Type;
		name  = pname;
		ptr   = pptr;
		error = perror;
	}

	override string toString() => format("%s%s", ptr? "ptr " : "", name);
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

class SignedIntNode : Node {
	long value;

	this(ErrorInfo perror) {
		type  = NodeType.SignedInt;
		error = perror;
	}

	this(ErrorInfo perror, long pvalue) {
		error = perror;
		type  = NodeType.SignedInt;
		value = pvalue;
	}

	override string toString() => format("%d", value);
}

class IntegerNode : Node {
	ulong value;

	this(ErrorInfo perror) {
		type  = NodeType.Integer;
		error = perror;
	}

	this(ErrorInfo perror, ulong pvalue) {
		error = perror;
		type  = NodeType.Integer;
		value = pvalue;
	}

	override string toString() => format("%d", value);
}

class FuncDefNode : Node {
	string     name;
	Node[]     nodes;
	bool       inline;
	bool       raw;
	TypeNode[] paramTypes;
	string[]   params;
	bool       errors;
	bool       manual;
	bool       unsafe;
	TypeNode[] returnTypes;

	this(ErrorInfo perror) {
		type  = NodeType.FuncDef;
		error = perror;
	}

	override string toString() {
		string ret = format("func %s%s", errors? "error " : "", name);

		foreach (i, ref param ; params) {
			ret ~= format(" %s %s", paramTypes[i], param);
		}
		ret ~= " ->";

		foreach (ref type ; returnTypes) {
			ret ~= format(" %s <ret>", type);
		}

		ret ~= " begin\n";

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

	override string toString() => format("include \"%s\"", path);
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

	override string toString() {
		string str = "asm\n";

		foreach (ref line ; code.split("\n")) {
			str ~= format("    \"%s\"\n", line);
		}

		return str ~ "end\n";
	}
}

class IfNode : Node {
	Node[][] condition;
	Node[][] doIf;
	bool     hasElse;
	Node[]   doElse;

	this(ErrorInfo perror) {
		type  = NodeType.If;
		error = perror;
	}

	override string toString() {
		string ret;

		auto condition2 = condition;
		auto doIf2      = doIf;

		foreach (i, ref icondition ; condition) {
			ret ~= i == 0? "if " : "elseif ";

			foreach (ref node ; icondition) {
				ret ~= node.toString ~ ' ';
			}
			ret ~= "then\n";
			
			foreach (ref node ; doIf[i]) {
				ret ~= node.toString() ~ '\n';
			}
		}

		if (hasElse) {
			ret ~= "else\n";

			foreach (ref node ; doElse) {
				ret ~= node.toString() ~ '\n';
			}
		}

		return ret ~ "end";
	}
}

class WhileNode : Node {
	Node[] condition;
	Node[] doWhile;

	this(ErrorInfo perror) {
		error = perror;
		type  = NodeType.While;
	}

	override string toString() {
		string ret = "while ";

		foreach (ref node ; condition) {
			ret ~= node.toString() ~ ' ';
		}

		ret ~= "do\n";

		foreach (ref node ; doWhile) {
			ret ~= node.toString() ~ '\n';
		}

		return ret ~ "end";
	}
}

class LetNode : Node {
	TypeNode varType;
	string   name;
	bool     array;
	size_t   arraySize;

	this(ErrorInfo perror) {
		type  = NodeType.Let;
		error = perror;
	}

	override string toString() => array?
		format("let array %d %s %s", arraySize, varType, name) :
		format("let %s %s", varType, name);
}

class EnableNode : Node {
	string ver;

	this(ErrorInfo perror) {
		type  = NodeType.Enable;
		error = perror;
	}

	override string toString() => format("enable %s", ver);
}

class RequiresNode : Node {
	string ver;

	this(ErrorInfo perror) {
		type  = NodeType.Requires;
		error = perror;
	}

	override string toString() => format("requires %s", ver);
}

class VersionNode : Node {
	string ver;
	bool   not;
	Node[] block;

	this(ErrorInfo perror) {
		type  = NodeType.Version;
		error = perror;
	}

	override string toString() {
		string ret = format("version %s\n", ver);

		foreach (ref node ; block) {
			ret ~= format("    %s\n", node);
		}

		return ret ~ "end";
	}
}

class ArrayNode : Node {
	TypeNode arrayType;
	Node[]   elements;
	bool     constant;

	this(ErrorInfo perror) {
		type  = NodeType.Array;
		error = perror;
	}

	override string toString() {
		string ret = constant? "c[" : "[";

		ret ~= format("%s%s ", arrayType.ptr? "ptr " : "", arrayType.name);

		foreach (ref node ; elements) {
			ret ~= node.toString() ~ ' ';
		}

		return ret ~ ']';
	}
}

class StringNode : Node {
	string value;
	bool   constant;

	this(ErrorInfo perror) {
		type  = NodeType.String;
		error = perror;
	}

	override string toString() => format("%s\"%s\"", constant? "c" : "", value);
}

struct StructMember {
	TypeNode type;
	string   name;
	bool     array;
	size_t   size;

	string toString() {
		return array?
			format("array %d %s %s", size, type, name) :
			format("%s %s", type, name);
	}
}

class StructNode : Node {
	string         name;
	StructMember[] members;
	bool           inherits;
	string         inheritsFrom;

	this(ErrorInfo perror) {
		type  = NodeType.Struct;
		error = perror;
	}

	override string toString() {
		string ret = inherits?
			format("struct %s : %s\n", name, inheritsFrom) :
			format("struct %s\n", name);

		foreach (ref member ; members) {
			ret ~= "    " ~ member.toString() ~ '\n';
		}

		return ret ~ "end";
	}
}

class ConstNode : Node {
	string name;
	long   value;

	this(ErrorInfo perror) {
		type  = NodeType.Const;
		error = perror;
	}

	override string toString() => format("const %s %d", name, value);
}

class EnumNode : Node {
	string   name;
	string   enumType;
	string[] names;
	long[]   values;

	this(ErrorInfo perror) {
		type  = NodeType.Enum;
		error = perror;
	}

	override string toString() {
		string ret = format("enum %s : %s\n", name, enumType);

		foreach (i, ref name ; names) {
			ret ~= format("    %s = %d\n", name, values[i]);
		}

		return ret ~ "end\n";
	}
}

class RestrictNode : Node {
	string ver;

	this(ErrorInfo perror) {
		type  = NodeType.Restrict;
		error = perror;
	}

	override string toString() => format("restrict %s", ver);
}

class UnionNode : Node {
	string   name;
	string[] types;

	this(ErrorInfo perror) {
		type  = NodeType.Union;
		error = perror;
	}

	override string toString() {
		auto ret = format("union %s\n", name);

		foreach (ref type ; types) {
			ret ~= format("    %s\n", type);
		}

		return ret ~ "end";
	}
}

class AliasNode : Node {
	string to;
	string from;
	bool   overwrite;

	this(ErrorInfo perror) {
		type  = NodeType.Alias;
		error = perror;
	}

	override string toString() => format("alias %s %s", to, from);
}

enum ExternType {
	Callisto,
	Raw,
	C
}

class ExternNode : Node {
	string     func;
	ExternType externType;
	string     asName;

	// for C extern
	TypeNode[] types;
	TypeNode   retType;

	this(ErrorInfo perror) {
		type  = NodeType.Extern;
		error = perror;
	}

	override string toString() {
		final switch (externType) {
			case ExternType.Callisto: return format("extern %s", func);
			case ExternType.Raw:      return format("extern raw %s", func);
			case ExternType.C: {
				string ret = format("extern C %s %s", retType, func);

				foreach (i, ref type ; types) {
					ret ~= format(" %s", type);
				}

				return ret ~ " end";
			}
		}
	}
}

class AddrNode : Node {
	string func;

	this(ErrorInfo perror) {
		type  = NodeType.Addr;
		error = perror;
	}

	override string toString() => format("&%s", func);
}

class ImplementNode : Node {
	string structure;
	string method;
	Node[] nodes;

	this(ErrorInfo perror) {
		type  = NodeType.Implement;
		error = perror;
	}

	override string toString() {
		string ret = format("implement %s %s\n", structure, method);

		foreach (ref node ; nodes) {
			ret ~= format("    %s\n", node.toString());
		}

		return ret ~ "end";
	}
}

class SetNode : Node {
	string var;

	this(ErrorInfo perror) {
		type  = NodeType.Set;
		error = perror;
	}

	override string toString() => format("-> %s", var);
}

class TryCatchNode : Node {
	string func;
	Node[] catchBlock;

	this(ErrorInfo perror) {
		type  = NodeType.TryCatch;
		error = perror;
	}

	override string toString() {
		string ret = format("try %s catch\n", func);

		foreach (ref node ; catchBlock) {
			ret ~= node.toString() ~ '\n';
		}

		return ret;
	}
}

class UnsafeNode : Node {
	string[] paramTypes;
	string[] retTypes;
	Node[]   nodes;

	this(ErrorInfo perror) {
		type  = NodeType.Unsafe;
		error = perror;
	}

	override string toString() {
		string ret = "unsafe";

		foreach (ref param ; paramTypes) {
			ret ~= format(" %s _", param);
		}
		ret ~= " ->";
		foreach (ref type ; retTypes) {
			ret ~= format(" %s _", type);
		}
		ret ~= " begin\n";

		foreach (ref node ; nodes) {
			ret ~= node.toString() ~ '\n';
		}

		return ret ~ "end";
	}
}

class AnonNode : Node {
	TypeNode varType;
	bool     array;
	size_t   arraySize;

	this(ErrorInfo perror) {
		type  = NodeType.Anon;
		error = perror;
	}

	override string toString() => array?
		format("anon array %d %s", arraySize, varType) :
		format("anon %s", varType);
}

class ImportNode : Node {
	string mod;
	bool   pub;

	this(ErrorInfo perror) {
		type    = NodeType.Import;
		error   = perror;
	}

	this(ErrorInfo perror, string pmod) {
		type  = NodeType.Import;
		error = perror;
		mod   = pmod;
	}

	override string toString() => format("import %s", mod);
}

class ModuleNode : Node {
	string modType;
	bool   pub;

	this(ErrorInfo perror) {
		type    = NodeType.Module;
		error   = perror;
	}

	this(ErrorInfo perror, string ptype) {
		type    = NodeType.Module;
		error   = perror;
		modType = ptype;
	}

	override string toString() => format("module %s", modType);
}

class ParserError : Exception {
	this() {
		super("", "", 0);
	}
}

class Parser {
	Token[]  tokens;
	size_t   i;
	Node[]   nodes;
	NodeType parsing;
	Token    parseStart;

	this() {
		
	}

	ErrorInfo GetError() {
		return ErrorInfo(
			tokens[i].file, tokens[i].line, tokens[i].col, tokens[i].contents.length
		);
	}

	void Error(Char, A...)(in Char[] fmt, A args) {
		ErrorBegin(GetError());
		stderr.writeln(format(fmt, args));
		PrintErrorLine(GetError());
		throw new ParserError();
	}

	void Next() {
		++ i;

		if (i >= tokens.length) {
			-- i;
			Error("Unexpected EOF while parsing %s at %s", parsing, parseStart.GetError());
		}
	}

	void Expect(TokenType type) {
		if (tokens[i].type != type) {
			Error("Expected %s, got %s while parsing %s", type, tokens[i].type, parsing);
		}
	}

	void ExpectWord(string word) {
		if ((tokens[i].type != TokenType.Identifier) || (tokens[i].contents != word)) {
			Error("Expected '%s' while parsing %s", word, parsing);
		}
	}

	bool IsIdentifier(string identifier) {
		return
			(tokens[i].type == TokenType.Identifier) &&
			(tokens[i].contents == identifier);
	}

	Node ParseType() {
		auto ret = new TypeNode(GetError());
		Expect(TokenType.Identifier);

		if (IsIdentifier("ptr")) {
			ret.ptr = true;

			Next();
			Expect(TokenType.Identifier);
			ret.name = tokens[i].contents;
		}
		else {
			ret.name = tokens[i].contents;
		}

		return ret;
	}

	Node ParseFuncDef(bool inline) {
		auto ret   = new FuncDefNode(GetError());
		ret.inline = inline;
		parsing    = NodeType.FuncDef;
		parseStart = tokens[i];

		bool readingAttr = true;
		while (readingAttr) {
			Next();
			Expect(TokenType.Identifier);

			switch (tokens[i].contents) {
				case "error":  ret.errors  = true; break;
				case "raw":    ret.raw     = true; break;
				case "man":    ret.manual  = true; break;
				case "unsafe": ret.unsafe  = true; break;
				default:       readingAttr = false;
			}
		}

		ret.name = tokens[i].contents;

		Next();

		while (!IsIdentifier("begin") && !IsIdentifier("->")) {
			ret.paramTypes ~= cast(TypeNode) ParseType();
			Next();
			Expect(TokenType.Identifier);
			ret.params ~= tokens[i].contents;
			Next();
		}

		if (IsIdentifier("->")) {
			Next();

			while (!IsIdentifier("begin")) {
				ret.returnTypes ~= cast(TypeNode) ParseType(); // return type

				Next();
				Expect(TokenType.Identifier); // return name, ignored

				if (tokens[i].contents == "begin") {
					Error("Begin in return type");
				}
				Next();
			}
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
		parsing  = NodeType.Include;
		parseStart = tokens[i];

		Next();
		Expect(TokenType.String);

		ret.path = tokens[i].contents;
		return ret;
	}

	Node ParseAsm() {
		auto ret = new AsmNode(GetError());
		parsing  = NodeType.Asm;
		parseStart = tokens[i];
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

	Node ParseIf() {
		auto ret       = new IfNode(GetError());
		ret.condition ~= new Node[](0);
		ret.doIf      ~= new Node[](0);
		parsing        = NodeType.If;
		parseStart     = tokens[i];
		Next();

		while (true) {
			parsing = NodeType.If;
			if (!ret.hasElse) {
				while (true) {
					parsing = NodeType.If;
					if (
						(tokens[i].type == TokenType.Identifier) &&
						(tokens[i].contents == "then")
					) {
						break;
					}

					ret.condition[$ - 1] ~= ParseStatement();
					Next();
				}
			}

			Next();

			while (true) {
				parsing = NodeType.If;
				if (
					(tokens[i].type == TokenType.Identifier) &&
					(tokens[i].contents == "elseif")
				) {
					ret.condition ~= new Node[](0);
					ret.doIf      ~= new Node[](0);
					Next();
					break;
				}
				if (
					(tokens[i].type == TokenType.Identifier) &&
					(tokens[i].contents == "else")
				) {
					ret.hasElse = true;

					Next();
					while (true) {
						if (
							(tokens[i].type == TokenType.Identifier) &&
							(tokens[i].contents == "end")
						) {
							return ret;
						}

						ret.doElse ~= ParseStatement();
						Next();
					}
				}
				if (
					(tokens[i].type == TokenType.Identifier) &&
					(tokens[i].contents == "end")
				) {
					return ret;
				}

				ret.doIf[$ - 1] ~= ParseStatement();
				Next();
			}
		}

		assert(0);
	}

	Node ParseWhile() {
		auto ret = new WhileNode(GetError());
		parsing  = NodeType.While;
		parseStart = tokens[i];
		Next();

		while (true) {
			if (
				(tokens[i].type == TokenType.Identifier) &&
				(tokens[i].contents == "do")
			) {
				break;
			}

			ret.condition ~= ParseStatement();
			Next();
			parsing = NodeType.While;
		}

		Next();

		while (true) {
			parsing = NodeType.While;
			if (
				(tokens[i].type == TokenType.Identifier) &&
				(tokens[i].contents == "end")
			) {
				break;
			}

			ret.doWhile ~= ParseStatement();
			Next();
			parsing  = NodeType.While;
		}

		return ret;
	}

	Node ParseLet() {
		auto ret = new LetNode(GetError());
		parsing  = NodeType.Let;
		parseStart = tokens[i];

		Next();
		Expect(TokenType.Identifier);

		if (tokens[i].contents == "array") {
			Next();
			Expect(TokenType.Integer);

			ret.array     = true;
			ret.arraySize = parse!size_t(tokens[i].contents);
			Next();
			Expect(TokenType.Identifier);
		}

		ret.varType = cast(TypeNode) ParseType();
		Next();
		Expect(TokenType.Identifier);
		ret.name = tokens[i].contents;

		return ret;
	}

	Node ParseEnable() {
		auto ret = new EnableNode(GetError());
		parsing  = NodeType.Enable;
		parseStart = tokens[i];

		Next();
		Expect(TokenType.Identifier);
		ret.ver = tokens[i].contents;

		return ret;
	}

	Node ParseRequires() {
		auto ret = new RequiresNode(GetError());
		parsing  = NodeType.Requires;
		parseStart = tokens[i];

		Next();
		Expect(TokenType.Identifier);
		ret.ver = tokens[i].contents;

		return ret;
	}

	Node ParseArray() {
		auto ret = new ArrayNode(GetError());
		parsing  = NodeType.Array;
		parseStart = tokens[i];

		switch (tokens[i].contents) {
			case "c": {
				ret.constant = true;
				break;
			}
			case "": break;
			default: {
				Error("Unknown attribute '%s'", tokens[i].contents);
			}
		}

		const NodeType[] allowedNodes = [
			NodeType.Word, NodeType.Integer
		];

		Next();
		Expect(TokenType.Identifier);
		ret.arrayType = cast(TypeNode) ParseType();
		Next();

		while (tokens[i].type != TokenType.RSquare) {
			parsing       = NodeType.Array;
			ret.elements ~= ParseStatement();
			Next();
		}

		return ret;
	}

	Node ParseString() {
		auto ret = new StringNode(GetError());
		parsing  = NodeType.String;
		parseStart = tokens[i];

		switch (tokens[i].extra) {
			case "c": {
				ret.constant = true;
				break;
			}
			case "": break;
			default: {
				Error("Invalid string type: '%s'", tokens[i].extra);
			}
		}

		ret.value = tokens[i].contents;
		return ret;
	}

	Node ParseStruct() {
		auto ret = new StructNode(GetError());
		parsing  = NodeType.Struct;
		parseStart = tokens[i];

		Next();
		Expect(TokenType.Identifier);
		ret.name = tokens[i].contents;
		Next();

		if ((tokens[i].type == TokenType.Identifier) && (tokens[i].contents == ":")) {
			Next();
			Expect(TokenType.Identifier);

			ret.inherits     = true;
			ret.inheritsFrom = tokens[i].contents;
			Next();
		}

		while (true) {
			if (
				(tokens[i].type == TokenType.Identifier) &&
				(tokens[i].contents == "end")
			) {
				break;
			}

			/*Expect(TokenType.Identifier);
			ret.types ~= tokens[i].contents;
			Next();
			Expect(TokenType.Identifier);
			ret.names ~= tokens[i].contents;
			Next();*/

			StructMember member;
			Expect(TokenType.Identifier);

			if (tokens[i].contents == "array") {
				Next();
				Expect(TokenType.Integer);
				member.array = true;
				member.size  = parse!size_t(tokens[i].contents);

				Next();
				Expect(TokenType.Identifier);
			}
			
			member.type = cast(TypeNode) ParseType();
			Next();
			Expect(TokenType.Identifier);
			member.name = tokens[i].contents;

			ret.members ~= member;

			Next();
			Expect(TokenType.Identifier);
		}

		return ret;
	}

	Node ParseVersion() {
		auto ret = new VersionNode(GetError());
		parsing  = NodeType.Version;
		parseStart = tokens[i];

		Next();
		Expect(TokenType.Identifier);

		if (tokens[i].contents == "not") {
			ret.not = true;
			Next();
			Expect(TokenType.Identifier);
		}

		ret.ver = tokens[i].contents;
		Next();

		while (true) {
			if (
				(tokens[i].type == TokenType.Identifier) &&
				(tokens[i].contents == "end")
			) {
				break;
			}

			ret.block ~= ParseStatement();
			Next();
		}

		return ret;
	}

	Node ParseConst() {
		auto ret = new ConstNode(GetError());
		parsing  = NodeType.Const;
		parseStart = tokens[i];

		Next();
		Expect(TokenType.Identifier);
		ret.name = tokens[i].contents;

		Next();
		Expect(TokenType.Integer);
		ret.value = parse!long(tokens[i].contents);

		return ret;
	}

	Node ParseEnum() {
		auto ret = new EnumNode(GetError());
		parsing  = NodeType.Enum;
		parseStart = tokens[i];

		Next();
		Expect(TokenType.Identifier);
		ret.name = tokens[i].contents;
		ret.enumType = "cell";

		Next();
		Expect(TokenType.Identifier);

		if (tokens[i].contents == ":") {
			Next();
			Expect(TokenType.Identifier);
			ret.enumType = tokens[i].contents;

			Next();
			Expect(TokenType.Identifier);
		}

		while (true) {
			if (tokens[i].contents == "end") break;

			ret.names ~= tokens[i].contents;
			Next();
			Expect(TokenType.Identifier);

			if (tokens[i].contents == "=") {
				Next();
				Expect(TokenType.Integer);

				ret.values ~= parse!long(tokens[i].contents);
				Next();
				Expect(TokenType.Identifier);
			}
			else {
				ret.values ~= ret.values.empty()? 0 : ret.values[$ - 1] + 1;
			}
		}

		return ret;
	}

	Node ParseRestrict() {
		auto ret = new RestrictNode(GetError());
		parsing  = NodeType.Restrict;
		parseStart = tokens[i];

		Next();
		Expect(TokenType.Identifier);
		ret.ver = tokens[i].contents;

		return ret;
	}

	Node ParseUnion() {
		auto ret = new UnionNode(GetError());
		parsing  = NodeType.Union;
		parseStart = tokens[i];

		Next();
		Expect(TokenType.Identifier);
		ret.name = tokens[i].contents;
		Next();
		Expect(TokenType.Identifier);

		while (true) {
			if ((tokens[i].type == TokenType.Identifier) && (tokens[i].contents == "end")) {
				break;
			}

			ret.types ~= tokens[i].contents;
			Next();
			Expect(TokenType.Identifier);
		}

		return ret;
	}

	Node ParseAlias() {
		auto ret = new AliasNode(GetError());
		parsing  = NodeType.Alias;
		parseStart = tokens[i];

		Next();
		Expect(TokenType.Identifier);

		if (tokens[i].contents == "overwrite") {
			ret.overwrite = true;
			Next();
			Expect(TokenType.Identifier);
		}

		ret.to = tokens[i].contents;

		Next();
		Expect(TokenType.Identifier);
		ret.from = tokens[i].contents;

		return ret;
	}

	Node ParseExtern() {
		auto ret = new ExternNode(GetError());
		parsing  = NodeType.Extern;
		parseStart = tokens[i];

		Next();
		Expect(TokenType.Identifier);

		if (tokens[i].contents == "raw") {
			ret.externType = ExternType.Raw;

			Next();
			Expect(TokenType.Identifier);
			ret.func = tokens[i].contents;
		}
		else if (tokens[i].contents == "C") {
			ret.externType = ExternType.C;

			Next();
			Expect(TokenType.Identifier);
			ret.retType = cast(TypeNode) ParseType();

			Next();
			Expect(TokenType.Identifier);
			ret.func = tokens[i].contents;

			while (true) {
				Next();
				Expect(TokenType.Identifier);

				if (tokens[i].contents == "end") {
					break;
				}
				else if (tokens[i].contents == "as") {
					Next();
					Expect(TokenType.Identifier);

					ret.asName = tokens[i].contents;
					break;
				}

				ret.types ~= cast(TypeNode) ParseType();
			}
		}
		else {
			ret.externType = ExternType.Callisto;
			ret.func       = tokens[i].contents;
		}

		return ret;
	}

	Node ParseAddr() {
		auto ret = new AddrNode(GetError());
		parsing  = NodeType.Addr;
		parseStart = tokens[i];

		Next();
		Expect(TokenType.Identifier);
		ret.func = tokens[i].contents;

		return ret;
	}

	Node ParseImplement() {
		auto ret = new ImplementNode(GetError());
		parsing  = NodeType.Implement;
		parseStart = tokens[i];

		Next();
		Expect(TokenType.Identifier);
		ret.structure = tokens[i].contents;

		Next();
		Expect(TokenType.Identifier);
		ret.method = tokens[i].contents;

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

	Node ParseSet() {
		auto ret = new SetNode(GetError());
		parsing  = NodeType.Set;
		parseStart = tokens[i];

		Next();
		Expect(TokenType.Identifier);
		ret.var = tokens[i].contents;

		return ret;
	}

	Node ParseTryCatch() {
		auto ret = new TryCatchNode(GetError());
		parsing  = NodeType.TryCatch;
		parseStart = tokens[i];

		Next();
		Expect(TokenType.Identifier);
		ret.func = tokens[i].contents;

		Next();
		ExpectWord("catch");

		Next();

		while (!IsIdentifier("end")) {
			ret.catchBlock ~= ParseStatement();
			Next();
		}

		return ret;
	}

	Node ParseUnsafe() {
		auto ret = new UnsafeNode(GetError());
		parsing  = NodeType.Unsafe;
		parseStart = tokens[i];

		Next();

		while (!IsIdentifier("begin") && !IsIdentifier("->")) {
			Expect(TokenType.Identifier);
			ret.paramTypes ~= tokens[i].contents;
			Next();
			Expect(TokenType.Identifier);
			Next();
		}

		if (IsIdentifier("->")) {
			Next();

			while (!IsIdentifier("begin")) {
				Expect(TokenType.Identifier);
				ret.retTypes ~= tokens[i].contents; // return type

				Next();
				Expect(TokenType.Identifier); // return name, ignored

				if (tokens[i].contents == "begin") {
					Error("Begin in return name");
				}
				Next();
			}
		}

		Next();

		while (!IsIdentifier("end")) {
			ret.nodes ~= ParseStatement();
			Next();
		}

		return ret;
	}

	Node ParseAnon() {
		auto ret   = new AnonNode(GetError());
		parsing    = NodeType.Anon;
		parseStart = tokens[i];

		Next();
		Expect(TokenType.Identifier);

		if (tokens[i].contents == "array") {
			Next();
			Expect(TokenType.Integer);

			ret.array     = true;
			ret.arraySize = parse!size_t(tokens[i].contents);
			Next();
			Expect(TokenType.Identifier);
		}

		ret.varType = cast(TypeNode) ParseType();
		return ret;
	}

	Node ParseImport() {
		auto ret   = new ImportNode(GetError());
		parsing    = NodeType.Import;
		parseStart = tokens[i];
		Next();
		Expect(TokenType.Identifier);

		if (tokens[i].contents == "public") {
			ret.pub = true;

			Next();
			Expect(TokenType.Identifier);
		}

		ret.mod = tokens[i].contents;
		return ret;
	}

	Node ParseModule() {
		auto ret   = new ModuleNode(GetError());
		parsing    = NodeType.Module;
		parseStart = tokens[i];
		Next();
		Expect(TokenType.Identifier);

		if (tokens[i].contents == "public") {
			ret.pub = true;

			Next();
			Expect(TokenType.Identifier);
		}

		ret.modType = tokens[i].contents;
		return ret;
	}

	Node ParseStatement() {
		switch (tokens[i].type) {
			case TokenType.Integer: {
				try {
					return new SignedIntNode(GetError(), parse!long(tokens[i].contents));
				}
				catch (ConvOverflowException) {
					return new IntegerNode(GetError(), parse!ulong(tokens[i].contents));
				}
			}
			case TokenType.Identifier: {
				switch (tokens[i].contents) {
					case "func":       return ParseFuncDef(false);
					case "inline":     return ParseFuncDef(true);
					case "include":    return ParseInclude();
					case "asm":        return ParseAsm();
					case "if":         return ParseIf();
					case "while":      return ParseWhile();
					case "let":        return ParseLet();
					case "enable":     return ParseEnable();
					case "requires":   return ParseRequires();
					case "struct":     return ParseStruct();
					case "version":    return ParseVersion();
					case "const":      return ParseConst();
					case "enum":       return ParseEnum();
					case "restrict":   return ParseRestrict();
					case "union":      return ParseUnion();
					case "alias":      return ParseAlias();
					case "extern":     return ParseExtern();
					case "implement":  return ParseImplement();
					case "try":        return ParseTryCatch();
					case "->":         return ParseSet();
					case "unsafe":     return ParseUnsafe();
					case "anon":       return ParseAnon();
					case "import":     return ParseImport();
					case "module":     return ParseModule();
					default: return new WordNode(GetError(), tokens[i].contents);
				}
			}
			case TokenType.LSquare:   return ParseArray();
			case TokenType.String:    return ParseString();
			case TokenType.Ampersand: return ParseAddr();
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
