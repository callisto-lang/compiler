module callisto.linkers.rm86;

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
import callisto.backends.rm86;

// TODO: link time optimisation
class LinkerRM86 : Linker {
	bool keepAsm;

	override bool HandleOption(string opt) {
		switch (opt) {
			case "keep-asm": {
				keepAsm = true;
				return true;
			}
			default: return false;
		}
	}

	override void Link() {
		auto file = File(outFile, "w");

		file.writefln("org 0x%.4X\n", 0x100); // TODO: make this replace-able
		file.writeln("mov si, __stack\n");

		CheckForOneFunc("__rm86_program_init");
		file.writefln("call %s", GetFunc("__rm86_program_init").Label());

		// run top level code
		file.writeln("__calmain:");
		file.write(tlcAsm);

		// run exit function
		CheckForOneFunc("__rm86_program_exit");
		file.writefln("call %s", GetFunc("__rm86_program_exit").Label());

		// write function assembly
		file.write(funcAsm);

		// data
		file.write(bssAsm);
		file.write(dataAsm);

		// create exception and stack
		file.writeln("__stack: times 512 dw 0");
		file.writeln("__exception: times 8 db 0");

		file.flush();
		file.close();

		// run final commands
		auto backend             = new BackendRM86();
		backend.compiler         = new Compiler();
		backend.compiler.outFile = outFile;
		backend.output           = new Output("");
		backend.keepAssembly     = keepAsm;

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
