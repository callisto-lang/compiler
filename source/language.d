module callisto.language;

import std.file;
import std.stdio;
import callisto.util;
import callisto.lexer;
import callisto.parser;

class Language {
	static const string[] bannedNames = ["return"];
}

Node[] ParseText(string txt, string file = "UNKNOWN") {
	auto lexer  = new Lexer();
	auto parser = new Parser();

	lexer.file = file;
	lexer.code = txt;

	lexer.Lex();
	if (!lexer.success) exit(1);

	parser.tokens = lexer.tokens;

	try {
		parser.Parse();
	}
	catch (ParserError) {
		exit(1);
	}

	return parser.nodes;
}

Node[] ParseFile(string path) {
	string code;

	try {
		code = readText(path);
	}
	catch (Exception e) {
		stderr.writefln("Failed to read '%s': %s", path, e.msg);
		exit(1);
	}

	return ParseText(code, path);
}
