module callisto.linkers.uxn;

import std.file;
import std.stdio;
import std.format;
import std.string;
import std.process;
import std.algorithm;
import callisto.util;
import callisto.error;
import callisto.linker;
import callisto.output;
import callisto.mod.mod;
import callisto.compiler;
import callisto.mod.sections;
import callisto.backends.uxn;

// TODO: link time optimisation
class LinkerUXN : Linker {
	string assembler = "uxncli";
	string headerFile;
	bool   keepAsm;

	override bool HandleOption(string opt) {
		switch (opt) {
			case "keep-asm": {
				keepAsm = true;
				return true;
			}
			default: {
				if (opt.canFind('=')) {
					string key = opt[0 .. opt.indexOf('=')];
					string val = opt[opt.indexOf('=') + 1 .. $];

					switch (key) {
						case "asm":    assembler  = val; return true;
						case "header": headerFile = val; return true;
						default: return false;
					}
				}
				else {
					return false;
				}
			}
		}
	}

	override void Link() {
		auto file    = File(outFile, "w");
		auto backend = new BackendUXN();

		if (headerFile == "") {
			file.writeln(backend.DefaultHeader());
		}
		else {
			file.writeln(readText(headerFile));
		}

		file.writeln("|0 @vsp $2 @arraySrc $2 @arrayDest $2 @temp $2");
		file.writeln("|100");
		file.writeln("#ffff .vsp STZ2");

		CheckForOneFunc("__uxn_program_init");
		file.writefln("%s", GetFunc("__uxn_program_init").Label(false));

		// run top level code
		file.writeln("@calmain");
		file.write(tlcAsm);

		// run exit function
		CheckForOneFunc("__uxn_program_exit");
		file.writefln("%s", GetFunc("__uxn_program_exit").Label(false));

		// write function assembly
		file.write(funcAsm);

		// bss, even though it doesn't exist on this backend!
		file.write(bssAsm);

		// create exception and other stuff
		file.writeln("@memcpy  01");
		file.writeln("&length  0000");
		file.writeln("&srcBank 0000");
		file.writeln("&srcAddr 0000");
		file.writeln("&dstBank 0000");
		file.writeln("&dstAddr 0000");
		file.writeln("@exception $8");
		file.writeln("@program_end");

		// data
		file.write(dataAsm);
		file.flush();
		file.close();

		// run final commands
		backend.compiler         = new Compiler();
		backend.compiler.outFile = outFile;
		backend.output           = new Output("");
		backend.keepAssembly     = keepAsm;
		backend.assembler        = assembler;

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
