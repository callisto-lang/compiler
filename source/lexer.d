module callisto.lexer;

import std.stdio;
import std.format;
import std.string;
import callisto.error;

enum TokenType {
	Null,
	Integer,
	String,
	Identifier,
	LSquare,
	RSquare
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

	this() {
		
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

	void Lex() {
		char[char] escapes = [
			'n': '\n',
			'r': '\r',
			't': '\t',
			'e': '\x1b'
		];

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
					default: {
						reading ~= code[i];
					}
				}
			}

			HandleColLine();
		}
	}
}
