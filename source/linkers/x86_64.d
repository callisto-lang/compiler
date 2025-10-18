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

// TODO: link time optimisation
class LinkerX86_64 : Linker {
	bool useGas;
	bool useLibc;
	bool keepAsm;
	bool debugInfo;

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
