module callisto.linkers.x86_64;

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
import callisto.backends.x86_64;

private struct Func {
	string mod;
	string name;
	bool   inline;

	string Label() => format("__func__%s__sep__%s", mod, name.Sanitise());
}

// TODO: link time optimisation
class LinkerX86_64 : Linker {
	string   tlcAsm;
	string   funcAsm;
	string   bssAsm;
	string   dataAsm;
	string[] externs;
	bool     useGas;
	bool     useLibc;
	bool     keepAsm;
	bool     debugInfo;

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
			case "use-gas": {
				useGas = true;
				return true;
			}
			case "use-libc": {
				useLibc = true;
				return true;
			}
			case "keep-asm": {
				keepAsm = true;
				return true;
			}
			case "debug": {
				debugInfo = true;
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
				case SectionType.Extern: {
					auto sect  = cast(ExternSection) isect;
					externs   ~= sect.symbolName;
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

		if (useGas) {
			file.writeln(".intel_syntax noprefix");
			file.writeln(".section .text");
		}
		else {
			file.writeln("section .text");
		}

		foreach (ref ext ; externs) {
			file.writefln("%sextern %s", useGas? "." : "", ext);
		}

		if (os == ModOS.macOS) {
			if (useGas) {
				ErrorNoInfo("Cannot use GNU Assembler on x86_64 macOS");
			}

			file.writeln("default rel");
			file.writeln("global _main");
			file.writeln("_main:");
		}
		else if (useLibc) {
			if (useGas) {
				file.writeln(".global main");
			}
			else {
				file.writeln("global main");
			}

			file.writeln("main:");
		}
		else {
			if (useGas) {
				file.writeln(".global _start");
			}
			else {
				file.writeln("global _start");
			}

			file.writeln("_start:");
		}

		CheckForOneFunc("__x86_64_program_init");
		file.writefln("call %s", GetFunc("__x86_64_program_init").Label());

		// allocate data stack
		file.writeln("sub rsp, 4096"); // 512 cells
		file.writeln("mov r15, rsp");

		// run top level code
		file.writeln("__calmain:");
		file.write(tlcAsm);

		// run exit function
		CheckForOneFunc("__x86_64_program_exit");
		file.writefln("call %s", GetFunc("__x86_64_program_exit").Label());

		// write function assembly
		file.write(funcAsm);

		// data
		if (useGas) {
			file.writeln(".section .bss");
		}
		else {
			file.writeln("section .bss");
		}
		file.write(bssAsm);

		// create exception
		file.writeln(useGas? "__exception: .skip 32" : "__exception: resb 32");

		if (useGas) {
			file.writeln(".section .data");
		}
		else {
			file.writeln("section .data");
		}
		file.write(dataAsm);
		file.flush();
		file.close();

		// run final commands
		auto backend             = new BackendX86_64();
		backend.compiler         = new Compiler();
		backend.compiler.outFile = outFile;
		backend.output           = new Output("");
		backend.useGas           = useGas;
		backend.useLibc          = useLibc;
		backend.keepAssembly     = keepAsm;
		backend.useDebug         = debugInfo;

		final switch (os) {
			case ModOS.None:    backend.os = "bare-metal"; break;
			case ModOS.Linux:   backend.os = "linux"; break;
			case ModOS.macOS:   backend.os = "osx"; break;
			case ModOS.Windows: backend.os = "windows"; break;
			case ModOS.DOS:     backend.os = "dos"; break;
			case ModOS.FreeBSD: backend.os = "freebsd"; break;
			case ModOS.Varvara: backend.os = "varvara"; break;
			case ModOS.Fox32OS: backend.os = "fox32os"; break;
		}

		if (useLibc) {
			backend.link ~= "c";
		}

		auto commands = backend.FinalCommands();

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
