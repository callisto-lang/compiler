module callisto.linkers.x86_64;

import std.stdio;
import callisto.error;
import callisto.linker;
import callisto.mod.mod;
import callisto.mod.sections;

// TODO: link time optimisation
class LinkerX86_64 : Linker {
	string tlcAsm;
	string funcAsm;
	string bssAsm;
	string dataAsm;
	bool   useGas;
	bool   useLibc;

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
			default: return false;
		}
	}

	override void Add(Module mod) {
		foreach (ref isect ; mod.sections) {
			writeln(isect.GetType()); /*
			switch (isect.GetType()) {
				case SectionType.TopLevel: {
					auto sect  = cast(TopLevelSection) isect;
					tlcAsm    ~= sect.assembly;
					break;
				}
				case SectionType.FuncDef: {
					auto sect  = cast(FuncDefSection) isect;
					funcAsm   ~= sect.assembly;
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
			}*/
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

		if (os == ModOS.macOS) {
			if (useGas) {
				ErrorNoInfo("Cannot use GNU Assembler on x86_64 macOS");
			}

			file.writeln("default rel\n");
			file.writeln("global _main\n");
			file.writeln("_main:\n");
		}
		else if (useLibc) {
			if (useGas) {
				file.writeln(".global main\n");
			}
			else {
				file.writeln("global main\n");
			}

			file.writeln("main:\n");
		}
		else {
			if (useGas) {
				file.writeln(".global _start\n");
			}
			else {
				file.writeln("global _start\n");
			}

			file.writeln("_start:\n");
		}

		file.writeln("call __init\n");

		// allocate data stack
		file.writeln("sub rsp, 4096\n"); // 512 cells
		file.writeln("mov r15, rsp\n");

		// jump to main
		file.writeln("jmp __calmain\n");

		file.write(funcAsm);
		file.writeln("__calmain:");
		file.write(tlcAsm);
		// TODO: what do i do about the init/exit words??
		file.writeln("__init:\n");
		file.writeln("ret\n");
	}
}
