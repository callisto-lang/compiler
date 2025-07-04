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

	ErrorInfo GetError() {
		return ErrorInfo(file, line, col);
	}
}

class Lexer {
	Token[]   tokens;
	size_t    i;
	string    code;
	string    file;
	size_t    line;
	size_t    col;
	bool      inString;
	string    reading;
	string    extra;
	ErrorInfo tokenError;
	bool      success = true;

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
		PrintErrorLine(GetError());
		success = false;
	}

	void AddToken(TokenType type) {
		tokens     ~= Token(type, reading, extra, tokenError.file, tokenError.line, tokenError.col);
		reading     = "";
		extra       = "";
		tokenError  = GetError();
	}

	void AddReading() {
		if (reading.strip() == "") {
			reading    = "";
			tokenError = GetError();
		}
		else if (reading.isNumeric()) {
			AddToken(TokenType.Integer);
		}
		else if (reading.startsWith("0x")) {
			if (!reading.OnlyContains("0123456789abcdefABCDEF")) {
				Error("Invalid literal");
			}

			reading = format("%d", reading[2 .. $].to!ulong(16));
			AddToken(TokenType.Integer);
		}
		else if (reading.startsWith("0b")) {
			if (!reading.OnlyContains("01")) {
				Error("Invalid binary literal");
			}

			reading = format("%d", reading[2 .. $].to!ulong(2));
			AddToken(TokenType.Integer);
		}
		else if (reading.startsWith("0o")) {
			if (!reading.OnlyContains("01234567")) {
				Error("Invalid octal literal");
			}

			reading = format("%d", reading[2 .. $].to!ulong(8));
			AddToken(TokenType.Integer);
		}
		else {
			AddToken(TokenType.Identifier);
		}
	}

	void Next(bool error = true) {
		++ i;

		if (i >= code.length) {
			if (error) Error("Unexpected EOF");
			else       return;
		}

		if (code[i] == '\n') {
			++ line;
			col = 0;
		}
		else {
			++ col;
		}
	}

	void ExpectChar(char ch) {
		if (code[i] != ch) {
			Error("Expected character %c, got %c", ch, code[i]);
		}
	}

	void Lex() {
		tokenError = GetError();

		for (i = 0; i < code.length; Next(false)) {
			if (inString) {
				switch (code[i]) {
					case '\\': {
						Next();

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
					case '\r':
					case '\n': {
						AddReading();
						break;
					}
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
							Next(false);

							if ((i >= code.length) || (code[i] == '\n')) {
								break;
							}
						}
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
					case '(': {
						while (code[i] != ')') {
							Next();
						}
						break;
					}
					case '{':
					case '}':
					case ',': { // ignored characters
						AddReading();
						break;
					}
					default: {
						reading ~= code[i];
					}
				}
			}
		}
		// TODO: here
		// what??
		// 2025: i know what this is for

		if (reading.strip() != "") {
			AddReading();
		}
	}
}
