module callisto.linkers.lua;

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
import callisto.backends.lua;

// TODO: link time optimisation
class LinkerLua : Linker {
	override bool HandleOption(string opt) => false;

	override void Link() {
		auto file    = File(outFile, "w");
		auto backend = new BackendLua();

		file.writeln("mem = {};");
		file.writeln("for i = 1, 1048576 do");
		file.writeln("    table.insert(mem, 0);");
		file.writeln("end");
		file.writeln("dsp = 1;");
		file.writeln("vsp = 1048576;");
		file.writeln("gsp = 524288;");
		file.writeln("regA = 0;");
		file.writeln("regB = 0;");
		file.writeln("dspRestore = 0;");

		file.writeln("
			function cal_pop()
				dsp = dsp - 1;
				return mem[dsp];
			end
		");

		// run top level code
		file.writeln("function calmain()");
		file.write(tlcAsm);
		file.writeln("end");

		// write function assembly
		file.write(funcAsm);

		// bss, even though it doesn't exist on this backend!
		file.write(bssAsm);

		file.write(dataAsm);

		file.writeln("regA = 0\n");
		file.writeln("calmain();");
		file.flush();
		file.close();

		// run final commands
		backend.compiler         = new Compiler();
		backend.compiler.outFile = outFile;
		backend.output           = new Output("");

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
