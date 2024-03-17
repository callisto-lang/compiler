module callisto.error;

import std.stdio;
import core.stdc.stdlib;

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
