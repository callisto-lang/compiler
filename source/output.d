module callisto.output;

import std.file;
import std.stdio;
import std.format;
import std.string;
import std.algorithm;
import std.exception;
import core.stdc.stdlib : exit;
import callisto.util;
import callisto.mod.mod;
import callisto.mod.sections;

class OutputException : Exception {
	mixin basicExceptionCtors;
}

// this will eventually be used for generating ASMMod files
// but for now i'm using it because GNU Assembler is the worst piece of software to
// ever exist
class Output {
	bool           useMod;
	string         output;
	string         outFile;
	string[string] macros;

	// module stuff
	WriteModule mod;
	Section     sect;

	this(string dest) {
		outFile = dest;
	}

	this(ModCPU cpu, ModOS os, string source, string dest) {
		useMod  = true;
		outFile = dest;

		mod = new WriteModule(cpu, os, source, dest);
	}

	void Error(Char, A...)(string source, in Char[] fmt, A args) {
		stderr.writefln("Output error from source: %s", source);
		stderr.writeln(format(fmt, args));
		exit(1);
	}

	void StartSection(SectionType type) {
		if (!useMod) return;

		if (sect !is null) {
			throw new ModuleException("Unfinished section");
		}

		final switch (type) {
			case SectionType.TopLevel:  sect = new TopLevelSection();  break;
			case SectionType.FuncDef:   sect = new FuncDefSection();   break;
			case SectionType.Import:    sect = new ImportSection();    break;
			case SectionType.Enable:    sect = new EnableSection();    break;
			case SectionType.Const:     sect = new ConstSection();     break;
			case SectionType.Enum:      sect = new EnumSection();      break;
			case SectionType.Restrict:  sect = new RestrictSection();  break;
			case SectionType.Union:     sect = new UnionSection();     break;
			case SectionType.Alias:     sect = new AliasSection();     break;
			case SectionType.Implement: sect = new ImplementSection(); break;
			case SectionType.Let:       sect = new LetSection();       break;
			case SectionType.Struct:    sect = new StructSection();    break;
		}
	}

	void AddCall(string call) {
		if (!useMod) return;

		sect.AddCall(call);
	}

	void FinishSection() {
		if (!useMod) return;

		mod.Add(sect);
	}

	void opOpAssign(string op: "~")(char ch) {
		if (useMod) {
			sect.WriteAsm(cast(string) [ch]);
		}
		else {
			output ~= ch;
		}
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

				if (useMod) sect.WriteAsm(macros[macroName]);
				else        output ~= macros[macroName];
				i = macroName.length + i + 2;
			}
			else if (text[i .. $].startsWith("${")) {
				if (text[i .. $].length == 2) {
					Error(text, "Incomplete sanitise statement");
				}

				string input = text[i + 2 .. $];

				if (!input.canFind('}')) {
					Error(text, "Incomplete sanitise statement");
				}

				input = input[0 .. input.indexOf('}')];

				if (useMod) sect.WriteAsm(input.Sanitise());
				else        output ~= input.Sanitise();
				i = input.length + i + 2;
			}
			else if (useMod) {
				sect.WriteAsm([text[i]]);
			}
			else {
				output ~= text[i];
			}
		}
	}

	void Finish() {
		if (useMod) {
			mod.Finish();
		}
		else {
			std.file.write(outFile, output);
		}
	}
}
