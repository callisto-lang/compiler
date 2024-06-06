module callisto.lexer;

import std.conv;
import std.stdio;
import std.format;
import std.string;
import std.algorithm;
import callisto.util;
import callisto.error;

enum TokenType {
	Null,
	Integer,
	String,
	Identifier,
	LSquare,
	RSquare,
	Ampersand
}

struct Token {
	TokenType type;
	string    contents;
	string    extra;
	string    file;
	size_t    line;
	size_t    col;
}

class LexerError : Exception {
	this() {
		super("", "", 0);
	}
}

class Lexer {
	Token[] tokens;
	size_t  i;
	string  code;
	string  file;
	size_t  line;
	size_t  col;
	bool    inString;
	string  reading;
	string  extra;

	char[char] escapes;

	this() {
		escapes = [
			'n':  '\n',
			'r':  '\r',
			't':  '\t',
			'e':  '\x1b',
			'"':  '"',
			'\\': '\\',
			'0':  '\0'
		];
	}

	ErrorInfo GetError() {
		return ErrorInfo(file, line, col);
	}

	void Error(Char, A...)(in Char[] fmt, A args) {
		ErrorBegin(GetError());
		stderr.writeln(format(fmt, args));
		throw new LexerError();
	}

	void AddToken(TokenType type) {
		tokens  ~= Token(type, reading, extra, file, line, col);
		reading  = "";
		extra    = "";
	}

	void AddReading() {
		if (reading.strip() == "") {
			reading = "";
		}
		else if (reading.isNumeric()) {
			AddToken(TokenType.Integer);
		}
		else if (reading.startsWith("0x")) {
			if (!reading.OnlyContains("0123456789abcdefABCDEF")) {
				Error("Invalid literal");
			}

			reading = format("%d", reading[2 .. $].to!long(16));
			AddToken(TokenType.Integer);
		}
		else if (reading.startsWith("0b")) {
			if (!reading.OnlyContains("01")) {
				Error("Invalid binary literal");
			}

			reading = format("%d", reading[2 .. $].to!long(2));
			AddToken(TokenType.Integer);
		}
		else if (reading.startsWith("0o")) {
			if (!reading.OnlyContains("01234567")) {
				Error("Invalid octal literal");
			}

			reading = format("%d", reading[2 .. $].to!long(8));
			AddToken(TokenType.Integer);
		}
		else {
			AddToken(TokenType.Identifier);
		}
	}

	void HandleColLine() {
		if (code[i] == '\n') {
			++ line;
			col = 0;
		}
		else {
			++ col;
		}
	}

	void Next() {
		++ i;

		if (i > code.length) {
			Error("Unexpected EOF");
		}
	}

	void ExpectChar(char ch) {
		if (code[i] != ch) {
			Error("Expected character %c, got %c", ch, code[i]);
		}
	}

	void Lex() {
		for (i = 0; i < code.length; ++ i) {
			if (inString) {
				switch (code[i]) {
					case '\\': {
						++ i;

						HandleColLine();

						if (i >= code.length) {
							Error("Unexpected EOF");
						}

						if (code[i] !in escapes) {
							Error("Invalid escape character: %c", code[i]);
						}

						reading ~= escapes[code[i]];
						break;
					}
					case '"': {
						AddToken(TokenType.String);
						inString = false;
						break;
					}
					default: {
						reading ~= code[i];
					}
				}
			}
			else {
				switch (code[i]) {
					case ' ':
					case '\t':
					case '\n': {
						AddReading();
						break;
					}
					case '\r': break;
					case '"': {
						extra    = reading;
						reading  = "";
						inString = true;
						break;
					}
					case '[': {
						AddToken(TokenType.LSquare);
						break;
					}
					case ']': {
						AddReading();
						AddToken(TokenType.RSquare);
						break;
					}
					case '#': {
						AddReading();

						while (true) {
							++ i;

							if ((i >= code.length) || (code[i] == '\n')) {
								break;
							}
						}

						++ line;
						col = 0;
						break;
					}
					case '&': {
						AddReading();
						AddToken(TokenType.Ampersand);
						break;
					}
					case '\'': {
						Next();
						char ch = code[i];

						if (ch == '\\') {
							Next();

							if (code[i] !in escapes) {
								Error("Invalid escape character: %c", code[i]);
							}

							ch = escapes[code[i]];
						}

						Next();
						ExpectChar('\'');

						reading = format("%d", ch);
						break;
					}
					default: {
						reading ~= code[i];
					}
				}
			}

			HandleColLine();
		}
	}
}
