module callisto.linkers.arm64;

import std.stdio;
import std.format;
import std.process;
import std.algorithm;
import callisto.util;
import callisto.error;
import callisto.linker;
import callisto.output;
import callisto.mod.mod;
import callisto.compiler;
import callisto.mod.sections;
import callisto.backends.arm64;

private struct Func {
	string mod;
	string name;
	bool   inline;

	string Label() => format("__func__%s__sep__%s", mod, name.Sanitise());
}

// TOOD: lift common code into Linker?
class LinkerARM64 : Linker {
	string tlcAsm;
	string funcAsm;
	string bssAsm;
	string dataAsm;
	bool   useLibc;

	Func[] funcs;

	bool FuncExists(string name) {
		foreach (ref func ; funcs) {
			if (func.name == name) return true;
		}

		return false;
	}

	size_t CountFuncs(string name) {
		size_t ret;

		foreach (ref func ; funcs) {
			if (func.name == name) ++ ret;
		}

		return ret;
	}

	Func GetFunc(string name) {
		foreach (ref func ; funcs) {
			if (func.name == name) return func;
		}
		assert(0);
	}

	override bool HandleOption(string opt) {
		switch (opt) {
			case "use-libc": {
				useLibc = true;
				return true;
			}
			default: return false;
		}
	}

	override void Add(Module mod) {
		foreach (ref isect ; mod.sections) {
			switch (isect.GetType()) {
				case SectionType.TopLevel: {
					auto sect  = cast(TopLevelSection) isect;
					tlcAsm    ~= sect.assembly;
					break;
				}
				case SectionType.FuncDef: {
					auto sect = cast(FuncDefSection) isect;

					if (!sect.inline) {
						funcAsm ~= sect.assembly;
					}

					Func func;
					func.mod    = mod.name;
					func.name   = sect.name;
					func.inline = sect.inline;
					funcs     ~= func;
					break;
				}
				case SectionType.Implement: {
					auto sect  = cast(ImplementSection) isect;
					funcAsm   ~= sect.assembly;
					break;
				}
				case SectionType.BSS: {
					auto sect  = cast(BSSSection) isect;
					bssAsm    ~= sect.assembly;
					break;
				}
				case SectionType.Data: {
					auto sect  = cast(DataSection) isect;
					dataAsm   ~= sect.assembly;
					break;
				}
				default: break;
			}
		}
	}

	void CheckForOneFunc(string name) {
		if (CountFuncs(name) != 1) {
			ErrorNoInfo("Linker requires one function named `%s`", name);
		}
		if (GetFunc(name).inline) {
			ErrorNoInfo("Linker requires that the function '%s' is not inline", name);
		}
	}

	override void Link() {
		auto file = File(outFile, "w");

		file.writeln(".text");

		if (os == ModOS.macOS) {
			file.writeln(".global _main");
			file.writeln("_main:");
		}
		else if (useLibc) {
			file.writeln(".global main");
			file.writeln("main:");
		}
		else {
			file.writeln(".global _start");
			file.writeln("_start:");
		}

		// allocate data stack
		file.writeln("sub x20, sp, 4096"); // 512 cells
		file.writeln("mov x19, x20");

		CheckForOneFunc("__arm64_program_init");
		file.writefln("bl %s", GetFunc("__arm64_program_init").Label());

		// run top level code
		file.writeln("__calmain:");
		file.write(tlcAsm);

		// run exit function
		CheckForOneFunc("__arm64_program_exit");
		file.writefln("bl %s", GetFunc("__arm64_program_exit").Label());

		// write function assembly
		file.write(funcAsm);

		// data
		file.writeln(".bss");
		file.write(bssAsm);
		file.writeln(".lcomm __exception, 32");

		file.writeln(".data");
		file.write(dataAsm);
		file.flush();
		file.close();

		// run final commands
		auto backend             = new BackendARM64();
		backend.compiler         = new Compiler();
		backend.compiler.outFile = outFile;
		backend.output           = new Output("");
		backend.useLibc          = useLibc;
		switch (os) {
			case ModOS.macOS:
				backend.os = "osx";
				break;
			case ModOS.Linux:
				backend.os = "linux";
				break;
			case ModOS.None:
				backend.os = "bare-metal";
				break;
			default: break;
		}
		auto commands            = backend.FinalCommands();

		foreach (cmd ; commands) {
			writeln(cmd);
			auto res = executeShell(cmd);

			if (res.status != 0) {
				stderr.writefln("Error running '%s': %s", cmd, res.output);
				exit(1);
			}
		}
	}
}
