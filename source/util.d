module callisto.util;

import std.algorithm;

public import core.stdc.stdlib : exit;

string Sanitise(string str) {
	string ret;

	foreach (ref ch ; str) {
		switch (ch) {
			case '!':  ret ~= "__exclaim__"; break;
			case '"':  ret ~= "__double_quote__"; break;
			case '£':  ret ~= "__pound__"; break;
			case '$':  ret ~= "__dollar__"; break;
			case '%':  ret ~= "__percent__"; break;
			case '^':  ret ~= "__up_arrow__"; break;
			case '&':  ret ~= "__ampersand__"; break;
			case '*':  ret ~= "__star__"; break;
			case '(':  ret ~= "__lparen__"; break;
			case ')':  ret ~= "__rparen__"; break;
			case '-':  ret ~= "__dash__"; break;
			case '_':  ret ~= "__underscore__"; break;
			case '+':  ret ~= "__plus__"; break;
			case '=':  ret ~= "__equal__"; break;
			case '[':  ret ~= "__lsquare__"; break;
			case '{':  ret ~= "__lcurly__"; break;
			case ']':  ret ~= "__rsquare__"; break;
			case '}':  ret ~= "__rcurly__"; break;
			case ';':  ret ~= "__semicolon__"; break;
			case ':':  ret ~= "__colon__"; break;
			case '\'': ret ~= "__single_quote__"; break;
			case '@':  ret ~= "__at__"; break;
			case '#':  ret ~= "__hashtag__"; break;
			case '~':  ret ~= "__tilde__"; break;
			case '\\': ret ~= "__backslash__"; break;
			case '|':  ret ~= "__pipe__"; break;
			case ',':  ret ~= "__comma__"; break;
			case '<':  ret ~= "__left__"; break;
			case '.':  ret ~= "__dot__"; break;
			case '>':  ret ~= "__right__"; break;
			case '/':  ret ~= "__slash__"; break;
			case '?':  ret ~= "__question__"; break;
			default:   ret ~= ch; break;
		}
	}

	return ret;
}
