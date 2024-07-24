module callisto.app;

import std.conv;
import std.file;
import std.stdio;
import std.string;
import std.process;
import std.algorithm;
import callisto.compiler;
import callisto.language;
import callisto.codeRemover;
import callisto.preprocessor;
import callisto.backends.uxn;
import callisto.backends.rm86;
import callisto.backends.x86_64;

const static string usage = "
Usage: %s FILE [FLAGS]

Flags:
	-o FILE    - Sets the output assembly file to FILE (out.asm by default)
	--org ADDR - Sets ORG value for compiler backend's assembly, ADDR is hex
	-i PATH    - Adds PATH to the list of include directories
	-O         - Enables optimisation (only works properly with programs without errors)
	-v VER     - Enables VER as a version
	-b BACKEND - Uses the given backend (backends listed below), default is linux86
	-a         - Automatically runs commands to turn the output assembly into an
	             executable file (set by default)
	-na        - Disables the -a flag
	--version  - Shows the callisto version
	-dp        - Prints parser output
	-es        - Export all Callisto symbols (and add util functions for interacting
	             with the Callisto stack)
	-d         - Enables debug symbols (if available)
	-l LIB     - Links LIB with the linker (if available)
	-dv VER    - Disables VER
	-h FILE    - Puts the contents of FILE at the top of the assembly output
	-bo OPT    - Backend option, see below
	-ka        - Keep assembly
	-al        - Print assembly line numbers for callisto nodes
	-os OS     - Set operating system for backend - see below

Backends and their operating systems:
	rm86   - Real mode x86, for bare-metal, dos
	x86_64 - 64-bit x86, for bare-metal, linux
	uxn    - Varvara/Uxn

Backend options:
	rm86:
		no-dos - Disables DOS-specific features
	linux86:
		use-libc - Makes Callisto use the C runtime and links libc
";

int main(string[] args) {
	if (args.length == 0) {
		writeln("what");
		return 1;
	}
	if (args.length == 1) {
		writefln(usage.strip(), args[0]);
		return 0;
	}

	string          file;
	string          outFile = "out";
	ulong           org = 0xFFFF;
	bool            orgSet;
	string[]        includeDirs;
	bool            optimise;
	string[]        versions;
	bool            runFinal = true;
	CompilerBackend backend = new BackendX86_64();
	bool            doDebug;
	bool            debugParser;
	bool            exportSymbols;
	string[]        link;
	string[]        disabled;
	string          header;
	string[]        backendOpts;
	bool            keepAssembly;
	bool            assemblyLines;
	string          os = "DEFAULT";

	for (size_t i = 1; i < args.length; ++ i) {
		if (args[i][0] == '-') {
			switch (args[i]) {
				case "-o": {
					++ i;

					if (i >= args.length) {
						stderr.writeln("-o requires FILE parameter");
						return 1;
					}
					if (outFile != "out") {
						stderr.writeln("Output file set multiple times");
						return 1;
					}

					outFile = args[i];
					break;
				}
				case "--org": {
					++ i;

					if (i >= args.length) {
						stderr.writeln("--org requires ADDR parameter");
						return 1;
					}

					try {
						org = args[i].to!ulong(16);
					}
					catch (ConvException) {
						stderr.writeln("--org parameter must be hexadecimal");
						return 1;
					}
					orgSet = true;
					break;
				}
				case "-i": {
					++ i;

					if (i >= args.length) {
						stderr.writeln("-i requires PATH parameter");
						return 1;
					}

					includeDirs ~= args[i];
					break;
				}
				case "-O": {
					optimise = true;
					break;
				}
				case "-v": {
					++ i;
					if (i >= args.length) {
						stderr.writeln("-v requires VER parameter");
						return 1;
					}

					versions ~= args[i];
					return 0;
				}
				case "-b": {
					++ i;
					if (i >= args.length) {
						stderr.writeln("-b requires BACKEND parameter");
						return 1;
					}

					switch (args[i]) {
						case "rm86": {
							backend = new BackendRM86();
							break;
						}
						case "x86_64": {
							backend = new BackendX86_64();
							break;
						}
						case "uxn": {
							backend = new BackendUXN();
							break;
						}
						default: {
							stderr.writefln("Unknown backend '%s'", args[i]);
							return 1;
						}
					}
					break;
				}
				case "--version": {
					writeln("Callisto compiler beta 0.6.0");
					return 0;
				}
				case "-a": {
					runFinal = true;
					break;
				}
				case "-na": {
					runFinal = false;
					break;
				}
				case "-d": {
					doDebug = true;
					break;
				}
				case "-dp": {
					debugParser = true;
					break;
				}
				case "-es": {
					exportSymbols = true;
					break;
				}
				case "-l": {
					++ i;
					if (i >= args.length) {
						stderr.writeln("-l expects LIB argument");
						return 1;
					}

					link ~= args[i];
					break;
				}
				case "-dv": {
					++ i;
					if (i >= args.length) {
						stderr.writeln("-dv expects VER argument");
						return 1;
					}

					disabled ~= args[i];
					break;
				}
				case "-h": {
					++ i;
					if (i >= args.length) {
						stderr.writeln("-h expects FILE argument");
						return 1;
					}

					if (header != "") {
						stderr.writeln("Header set multiple times");
						return 1;
					}

					header = args[i];
					break;
				}
				case "-bo": {
					++ i;
					if (i >= args.length) {
						stderr.writeln("-h expects FILE argument");
						return 1;
					}

					backendOpts ~= args[i];
					break;
				}
				case "-ka": {
					keepAssembly = true;
					break;
				}
				case "-al": {
					assemblyLines = true;
					break;
				}
				case "-os": {
					++ i;
					if (i >= args.length) {
						stderr.writeln("-os expects OS argument");
						return 1;
					}

					os = args[i];
					break;
				}
				default: {
					stderr.writefln("Unknown flag '%s'", args[i]);
					return 1;
				}
			}
		}
		else {
			if (file != "") {
				stderr.writeln("Source file set multiple times");
				return 1;
			}

			file = args[i];
		}
	}

	if (os == "DEFAULT") {
		os = backend.defaultOS;
	}

	foreach (ref opt ; backendOpts) {
		if (!backend.HandleOption(opt, versions)) {
			stderr.writefln("Unknown option '%s'", opt);
			return 1;
		}
	}

	if (header == "") {
		header = backend.DefaultHeader();
	}
	else {
		try {
			header = readText(header);
		}
		catch (Exception e) {
			stderr.writefln("%s: %s", header, e.msg);
			return 1;
		}
	}

	backend.output = header ~ '\n';

	if (file == "") {
		stderr.writeln("No source files");
		return 1;
	}

	string[] included;
	auto     nodes = ParseFile(file);

	if (debugParser) {
		foreach (ref node ; nodes) {
			writeln(node);
		}

		return 0;
	}

	auto compiler                   = new Compiler();
	compiler.backend                = backend;
	compiler.backend.org            = org;
	compiler.backend.orgSet         = orgSet;
	compiler.backend.useDebug       = doDebug;
	compiler.backend.exportSymbols  = exportSymbols;
	compiler.backend.link          ~= link;
	compiler.backend.os             = os;
	backend.keepAssembly            = keepAssembly;
	compiler.assemblyLines          = assemblyLines;
	
	versions ~= compiler.backend.GetVersions();

	bool removing = true;
	while (removing) {
		foreach (ref ver ; disabled) {
			if (versions.canFind(ver)) {
				versions = versions.remove(versions.countUntil(ver));
				goto next;
			}
		}

		removing = false;
		next:
	}

	auto preproc        = new Preprocessor();
	preproc.includeDirs = includeDirs;
	preproc.versions    = versions;

	try {
		nodes = preproc.Run(nodes);
	}
	catch (PreprocessorError) {
		return 1;
	}

	if (optimise) {
		auto codeRemover = new CodeRemover();
		codeRemover.Run(nodes);
		nodes = codeRemover.res;
	}

	compiler.versions = preproc.versions;
	
	try {
		compiler.Compile(nodes);
	}
	catch (CompilerError) {
		return 1;
	}

	std.file.write(outFile, compiler.backend.output);

	if (runFinal) {
		compiler.outFile   = outFile;
		auto finalCommands = compiler.backend.FinalCommands();

		foreach (cmd ; finalCommands) {
			writeln(cmd);
			auto res = executeShell(cmd);

			if (res.status != 0) {
				stderr.writefln("Error running '%s': %s", cmd, res.output);
				return 1;
			}
		}
	}

	return 0;
}
