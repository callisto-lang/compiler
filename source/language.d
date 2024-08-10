module callisto.language;

import std.file;
import std.stdio;
import callisto.util;
import callisto.lexer;
import callisto.parser;

class Language {
	static const string[] bannedNames = ["return"];
}

Node[] ParseFile(string path) {
	auto lexer  = new Lexer();
	auto parser = new Parser();

	lexer.file = path;

	try {
		lexer.code = readText(path);
	}
	catch (Exception e) {
		stderr.writefln("Failed to read '%s': %s", path, e.msg);
		exit(1);
	}

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
