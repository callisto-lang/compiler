module callisto.backends.win64;

import std.conv;
import std.stdio;
import std.range;
import std.format;
import std.algorithm;
import callisto.util;
import callisto.error;
import callisto.parser;
import callisto.compiler;
import callisto.language;
import callisto.backends.linux86;

class BackendWin64 : BackendLinux86 {
	this() {
		super();
	}

	override string[] GetVersions() => ["Win64", "Windows"];

	version (Windows) {
		override string[] FinalCommands() => [
			format("mv %s %s.asm", compiler.outFile, compiler.outFile),
			format("nasm -f win64 %s.asm -o %s.obj", compiler.outFile, compiler.outFile),
			format(
				"link %s.obj /subsystem:console /entry:main /out:%s", compiler.outFile,
				compiler.outFile
			)
		];
	}
	else {
		override string[] FinalCommands() => [
			format("mv %s %s.asm", compiler.outFile, compiler.outFile),
			format("nasm -f win64 %s.asm -o %s.obj", compiler.outFile, compiler.outFile),
			format(
				"x86_64-w64-mingw32-gcc %s.obj -o %s -nostdlib -lkernel32",
				compiler.outFile, compiler.outFile
			)
		];
	}

	override void Init() {
		output ~= "extern ExitProcess\n";
		output ~= "global _start\n";
		output ~= "section .text\n";
		output ~= "_start:\n";

		// allocate data stack
		output ~= "sub rsp, 4096\n"; // 512 cells
		output ~= "mov r15, rsp\n";

		// copy static array constants
		output ~= "call __copy_arrays\n";

		// jump to main
		output ~= "jmp __calmain\n";
	}

	override void End() {
		output ~= "xor rax, rax\n";
		output ~= "call ExitProcess\n";

		// create copy arrays function
		output ~= "__copy_arrays:\n";

		foreach (i, ref array ; arrays) {
			output ~= format("mov rsi, __array_src_%d\n", i);
			output ~= format("mov rdi, __array_%d\n", i);
			output ~= format("mov rcx, %d\n", array.Size());
			output ~= "rep movsb\n";
		}

		output ~= "ret\n";

		// create global variables
		output ~= "section .bss\n";

		foreach (name, var ; globals) {
			output ~= format("__global_%s: resb %d\n", name.Sanitise(), var.Size());
		}

		foreach (i, ref array ; arrays) {
			output ~= format("__array_%d: resb %d\n", i, array.Size());
		}

		// create array source
		output ~= "section .text\n";
		foreach (i, ref array ; arrays) {
			output ~= format("__array_src_%d: ", i);

			switch (array.type.size) {
				case 1:  output ~= "db "; break;
				case 2:  output ~= "dw "; break;
				case 4:  output ~= "dd "; break;
				case 8:  output ~= "dq "; break;
				default: assert(0);
			}

			foreach (j, ref element ; array.values) {
				output ~= element ~ (j == array.values.length - 1? "" : ", ");
			}

			output ~= '\n';
		}
	}
}
