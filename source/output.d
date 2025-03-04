module callisto.output;

import std.file;
import std.stdio;
import std.format;
import std.string;
import std.algorithm;
import core.stdc.stdlib : exit;

// this will eventually be used for generating ASMMod files
// but for now i'm using it because GNU Assembler is the worst piece of software to
// ever exist
class Output {
	string         output;
	string         outFile;
	string[string] macros;

	this() {
		
	}

	void Error(Char, A...)(string source, in Char[] fmt, A args) {
		stderr.writefln("Output error from source: %s", source);
		stderr.writeln(format(fmt, args));
		exit(1);
	}

	void opOpAssign(string op: "~")(char ch) {
		output ~= ch;
	}

	void opOpAssign(string op: "~")(string text) {
		for (size_t i = 0; i < text.length; ++ i) {
			if (text[i .. $].startsWith("$(")) {
				if (text[i .. $].length == 2) {
					Error(text, "Incomplete macro invocation");
				}

				string macroName = text[i + 2 .. $];

				if (!macroName.canFind(')')) {
					Error(text, "Incomplete macro invocation");
				}

				macroName = macroName[0 .. macroName.indexOf(')')];

				if (macroName !in macros) {
					Error(text, "Macro '%s' doesn't exist", macroName);
				}

				output ~= macros[macroName];
				i = macroName.length + i + 2;
			}
			else {
				output ~= text[i];
			}
		}
	}

	void Finish() {
		std.file.write(outFile, output);
	}
}


