module callisto.error;

import std.file;
import std.path;
import std.array;
import std.stdio;
import std.format;
import core.stdc.stdlib : exit;
import callisto.util;

struct ErrorInfo {
	string file;
	size_t line;
	size_t col;
	size_t length;
}

private string FixPath(string path) => path.asRelativePath(getcwd()).array;

void ErrorBegin(ErrorInfo info) {
	version (Windows) {
		stderr.writef(
			"%s:%d:%d: error: ", info.file.FixPath(), info.line + 1, info.col + 1
		);
	}
	else {
		stderr.writef(
			"\x1b[1m%s:%d:%d: \x1b[31merror:\x1b[0m ", info.file.FixPath(),
			info.line + 1, info.col + 1
		);
	}
}

void WarningBegin(ErrorInfo info) {
	version (Windows) {
		stderr.writef(
			"%s:%d:%d: warning: ", info.file.FixPath(), info.line + 1, info.col + 1
		);
	}
	else {
		stderr.writef(
			"\x1b[1m%s:%d:%d: \x1b[33mwarning:\x1b[0m ", info.file.FixPath(),
			info.line + 1, info.col + 1
		);
	}
}

void WarnNoInfo(Char, A...)(in Char[] fmt, A args) {
	version (Windows) {
		stderr.writefln("warning: %s", format(fmt, args));
	}
	else {
		stderr.writefln("\x1b[33mwarning:\x1b[0m %s", format(fmt, args));
	}
}

void ErrorNoInfo(Char, A...)(in Char[] fmt, A args) {
	version (Windows) {
		stderr.writefln("error: ", format(fmt, args));
	}
	else {
		stderr.writefln("\x1b[31merror:\x1b[0m ", format(fmt, args));
	}
	exit(1);
}

void PrintErrorLine(ErrorInfo info) {
	writef(" %d | ", info.line + 1);

	string line = GetFileLine(info.file, info.line);

	foreach (i, ref ch ; line) {
		if (i == info.col) {
			writef("\x1b[31m-> ");
		}

		writef("%c", ch);

		if (i == info.col + info.length - 1) {
			writef(" <-\x1b[0m");
		}
	}
	writeln("\n");
}
