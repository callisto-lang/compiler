module callisto.util;

import std.file;
import std.array;
import std.format;
import std.algorithm;

public import core.stdc.stdlib : exit;

string Sanitise(string str) {
	string ret;

	foreach (ref ch ; str) {
		switch (ch) {
			case '!':  ret ~= "__exc__"; break;
			case '"':  ret ~= "__dq__"; break;
			case '£':  ret ~= "__pnd__"; break;
			case '$':  ret ~= "__dlr__"; break;
			case '%':  ret ~= "__pcnt__"; break;
			case '^':  ret ~= "__up__"; break;
			case '&':  ret ~= "__amp__"; break;
			case '*':  ret ~= "__star__"; break;
			case '(':  ret ~= "__lp__"; break;
			case ')':  ret ~= "__r__"; break;
			case '-':  ret ~= "__dash__"; break;
			case '_':  ret ~= "__us__"; break;
			case '+':  ret ~= "__plus__"; break;
			case '=':  ret ~= "__equ__"; break;
			case '[':  ret ~= "__ls__"; break;
			case '{':  ret ~= "__lc__"; break;
			case ']':  ret ~= "__rs__"; break;
			case '}':  ret ~= "__rc__"; break;
			case ';':  ret ~= "__scn__"; break;
			case ':':  ret ~= "__cn__"; break;
			case '\'': ret ~= "__sq__"; break;
			case '@':  ret ~= "__at__"; break;
			case '#':  ret ~= "__hg__"; break;
			case '~':  ret ~= "__tld__"; break;
			case '\\': ret ~= "__bs__"; break;
			case '|':  ret ~= "__pipe__"; break;
			case ',':  ret ~= "__comma__"; break;
			case '<':  ret ~= "__left__"; break;
			case '.':  ret ~= "__dot__"; break;
			case '>':  ret ~= "__right__"; break;
			case '/':  ret ~= "__slash__"; break;
			case '?':  ret ~= "__qstn__"; break;
			default:   ret ~= ch; break;
		}
	}

	return ret;
}

bool OnlyContains(string str, string chars) => str.any!(ch => !chars.canFind(ch));

string GetFileLine(string fileName, size_t line) {
	try {
		return readText(fileName).split("\n")[line];
	}
	catch (FileException e) {
		return format("Failed to read: %s", e.msg);
	}
}
