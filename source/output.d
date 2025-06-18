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
import callisto.compiler;
import callisto.mod.sections;

class OutputException : Exception {
	mixin basicExceptionCtors;
}

enum OutputMode {
	None,
	Assembly,
	Module
}

// this will eventually be used for generating ASMMod files
// but for now i'm using it because GNU Assembler is the worst piece of software to
// ever exist
class Output {
	OutputMode     mode;
	string         output;
	string         outFile;
	string[string] macros;

	// module stuff
	WriteModule mod;
	Section     sect;

	this() {
		mode = OutputMode.None;
	}

	this(string dest) {
		outFile = dest;
		mode    = OutputMode.Assembly;
	}

	this(string inFile, ModCPU cpu, ModOS os, string source, string dest) {
		mode    = OutputMode.Module;
		outFile = dest;

		mod = new WriteModule(inFile, cpu, os, source, dest);
	}

	void Error(Char, A...)(string source, in Char[] fmt, A args) {
		stderr.writefln("Output error from source: %s", source);
		stderr.writeln(format(fmt, args));
		exit(1);
	}

	string GetModPrefix() {
		return mode == OutputMode.Module? format("%s__sep__", mod.name) : "";
	}

	void StartSection(SectionType type) {
		if (mode != OutputMode.Module) return;

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
			case SectionType.BSS:       sect = new BSSSection();       break;
			case SectionType.Data:      sect = new DataSection();      break;
		}
	}

	void StartSection(Section psect) {
		if (mode != OutputMode.Module) return;

		if (sect !is null) {
			throw new ModuleException("Unfinished section");
		}

		sect = psect;
	}

	void AddCall(string call) {
		if (mode != OutputMode.Module) return;

		sect.AddCall(call);
	}

	void FinishSection() {
		if (mode != OutputMode.Module) return;

		mod.Add(sect);
		sect = null;
	}

	T ThisSection(T)() => cast(T) sect;

	void AddSection(Section psect) {
		if (mode != OutputMode.Module) return;

		if (sect !is null) {
			throw new ModuleException("Unfinished section");
		}

		mod.Add(psect);
	}

	void AddGlobal(Global global) {
		if (mode != OutputMode.Module) return;

		auto section = new LetSection();
		section.array = global.array;
		section.size  = global.type.size;
		section.ptr   = global.type.ptr;
		section.type  = global.type.name;
		section.name  = global.name;
		AddSection(section);
	}

	void opOpAssign(string op: "~")(char ch) {
		final switch (mode) {
			case OutputMode.None:     return;
			case OutputMode.Assembly: output ~= ch; return;
			case OutputMode.Module:   sect.WriteAsm(cast(string) [ch]); return;
		}
	}

	void opOpAssign(string op: "~")(string text) {
		if (mode == OutputMode.None) return;

		bool useMod = mode == OutputMode.Module;

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
			else if (text[i .. $].startsWith("&{")) {
				if (text[i .. $].length == 2) {
					Error(text, "Incomplete identifier statement");
				}

				string input = text[i + 2 .. $];

				if (!input.canFind('}')) {
					Error(text, "Incomplete identifier statement");
				}

				input = input[0 .. input.indexOf('}')];

				auto parts = input.split();
				if (parts.length != 2) {
					Error(text, "Identifier statement requires 2 parts");
				}

				switch (parts[0]) {
					case "global": {
						if (mode == OutputMode.Module) {
							sect.WriteAsm(format(
								"%s%s%s", "__global_", GetModPrefix(),
								parts[1].Sanitise()
							));
						}
						else {
							output ~= format("__global_%s", parts[1].Sanitise());
						}
						break;
					}
					default: {
						Error(
							text, "Identifier statement: unknown operation '%s'",
							parts[0]
						);
					}
				}

				i = input.length + i + 2;
			}
			else if (mode == OutputMode.Module) {
				sect.WriteAsm([text[i]]);
			}
			else {
				output ~= text[i];
			}
		}
	}

	void Finish() {
		final switch (mode) {
			case OutputMode.None:     return;
			case OutputMode.Assembly: std.file.write(outFile, output); return;
			case OutputMode.Module:   mod.Finish(); return;
		}
	}
}
