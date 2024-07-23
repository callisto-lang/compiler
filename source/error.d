module callisto.error;

import std.stdio;
import std.format;
import core.stdc.stdlib : exit;

struct ErrorInfo {
	string file;
	size_t line;
	size_t col;
}

void ErrorBegin(ErrorInfo info) {
	version (Windows) {
		stderr.writef("%s:%d:%d: error: ", info.file, info.line + 1, info.col + 1);
	}
	else {
		stderr.writef(
			"\x1b[1m%s:%d:%d: \x1b[31merror:\x1b[0m ", info.file, info.line + 1,
			info.col + 1
		);
	}
}

void WarningBegin(ErrorInfo info) {
	version (Windows) {
		stderr.writef("%s:%d:%d: warning: ", info.file, info.line + 1, info.col + 1);
	}
	else {
		stderr.writef(
			"\x1b[1m%s:%d:%d: \x1b[33mwarning:\x1b[0m ", info.file, info.line + 1,
			info.col + 1
		);
	}
}

void WarnNoInfo(Char, A...)(in Char[] fmt, A args) {
	version (Windows) {
		stderr.writeln("warning: %s", format(fmt, args));
	}
	else {
		stderr.writeln("\x1b[33mwarning:\x1b[0m %s", format(fmt, args));
	}
}

void ErrorNoInfo(Char, A...)(in Char[] fmt, A args) {
	version (Windows) {
		stderr.writeln("error: ", format(fmt, args));
	}
	else {
		stderr.writeln("\x1b[31merror:\x1b[0m ", format(fmt, args));
	}
	exit(1);
}
