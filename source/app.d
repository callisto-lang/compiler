module callisto.app;

import std.conv;
import std.file;
import std.path;
import std.stdio;
import std.string;
import std.process;
import std.algorithm;
import callisto.test;
import callisto.stub;
import callisto.error;
import callisto.output;
import callisto.linker;
import callisto.summary;
import callisto.mod.mod;
import callisto.compiler;
import callisto.language;
import callisto.profiler;
import callisto.stackCheck;
import callisto.codeRemover;
import callisto.preprocessor;
import callisto.mod.sections;
import callisto.backends.lua;
import callisto.backends.uxn;
import callisto.backends.rm86;
import callisto.backends.arm64;
import callisto.backends.x86_64;

const static string appVersion = "Beta 0.12.7";

const static string usage = "
Callisto Compiler
=================

Usage: %s FILE [FLAGS]

Flags:
  -o FILE     - Sets the output assembly file to FILE (default: out.asm)
  --org ADDR  - Sets ORG value for compiler backend's assembly, ADDR is hex
  -i PATH     - Adds PATH to the list of include directories
  -O          - Enables optimization (only works properly with error-free programs)
  -v VER      - Enables VER as a version
  -b BACKEND  - Uses the specified backend (default is linux86)
  -a          - Automatically runs commands to create an executable file (enabled by default)
  -na         - Disables the -a flag
  --version   - Shows the Callisto version
  -dp         - Prints parser output
  -es         - Exports all Callisto symbols and adds utility functions for interacting with the Callisto stack
  -d          - Enables debug symbols (if available)
  -l LIB      - Links LIB with the linker (if available)
  -dv VER     - Disables VER
  -h FILE     - Puts the contents of FILE at the top of the assembly output
  -bo OPT     - Backend option, see below
  -ka         - Keeps the generated assembly
  -al         - Prints assembly line numbers for Callisto nodes
  -os OS      - Sets the operating system for the backend (see below)
  -sc         - Stop after stack check
  -scf        - Show functions in stack checker
  --help      - Shows this help text
  -m          - Generates a module file instead of an executable
  -stub       - Generates a stub module
  -p          - Print profiler results

Backends and their operating systems:
  rm86   - Real mode x86, for bare-metal, DOS
  x86_64 - 64-bit x86, for bare-metal, linux, osx, freebsd
  arm64  - 64-bit ARM, for linux
  uxn    - Varvara/Uxn
  lua    - Lua, uses subset CallistoScript

Backend options:
  rm86:
    no-dos - Disables DOS-specific features
  x86_64:
    use-libc  - Makes Callisto use the C runtime and links libc
    frame-ptr - Makes Callisto use both rbp (frame pointer) and rsp for stack frames
    use-gas   - Makes Callisto use GNU Assembler instead of nasm

Programs:
  cac link <MODULES...> - Links MODULES... together into one executable
    Flags:
      -ps     - Print all sections in the first given module
      -o FILE - Sets executable path
      -lo OPT - Adds binutil linker option
    Linker options:
      x86_64:
        use-gas  - Makes Callisto use GNU Assembler instead of nasm
        use-libc - Makes Callisto use the C runtime and links libc
        keep-asm - Keep assembly
        debug    - Add debug info
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
	string          outFile = "DEFAULT";
	bool            orgSet;
	string[]        includeDirs;
	bool            optimise;
	string[]        versions;
	bool            runFinal = true;
	CompilerBackend backend;
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
	bool            onlyStackCheck = false;
	bool            noStackCheck;
	bool            stackCheckerFunctions = false;
	bool            makeMod;
	ModCPU          modCPU;
	ModOS           modOS;
	bool            makeStub;
	bool            printProfiler;

	// choose default backend
	version (X86_64) {
		backend = new BackendX86_64();
		modCPU  = ModCPU.x86_64;
	}
	else version (AArch64) {
		backend = new BackendARM64();
		modCPU  = ModCPU.ARM64;
	}
	else {
		WarnNoInfo("No default backend for your system");
	}

	if (args.length > 1) {
		switch (args[1]) {
			case "link": return LinkerProgram(args[2 .. $]);
			case "test": return TestProgram();
			default: break;
		}
	}

	for (size_t i = 1; i < args.length; ++ i) {
		if (args[i][0] == '-') {
			switch (args[i]) {
				case "-o": {
					++ i;

					if (i >= args.length) {
						stderr.writeln("-o requires FILE parameter");
						return 1;
					}
					if (outFile != "DEFAULT") {
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
						backend.org = args[i].to!ulong(16);
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
							modCPU  = ModCPU.RM86;
							break;
						}
						case "x86_64": {
							backend = new BackendX86_64();
							modCPU  = ModCPU.x86_64;
							break;
						}
						case "arm64": {
							backend = new BackendARM64();
							modCPU  = ModCPU.ARM64;
							break;
						}
						case "uxn": {
							backend = new BackendUXN();
							modCPU  = ModCPU.Uxn;
							break;
						}
						case "lua": {
							writeln("Language subset 'CallistoScript' in use");
							backend = new BackendLua();
							modCPU  = ModCPU.None;
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
					writeln("The Glorious Callisto Compilation System");
					writeln(appVersion);
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
				case "-sc":   onlyStackCheck = true;        break;
				case "-nsc":  noStackCheck = true;          break;
				case "-scf":  stackCheckerFunctions = true; break;
				case "-m":    makeMod = true;               break;
				case "-stub": makeStub = true;              break;
				case "-p":    printProfiler = true;         break;
				case "--help": {
					writeln(usage.strip());
					return 0;
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

	if (makeMod) {
		versions ~= "Module";
	}

	if (os == "DEFAULT") {
		os = backend.defaultOS;

		switch (os) {
			case "linux":      modOS = ModOS.Linux;   break;
			case "osx":        modOS = ModOS.macOS;   break;
			case "bare-metal": modOS = ModOS.None;    break;
			case "dos":        modOS = ModOS.DOS;     break;
			case "freebsd":    modOS = ModOS.FreeBSD; break;
			default:           break;
		}
	}

	if (makeStub && !makeMod) {
		stderr.writeln("Pass -m to make a stub module");
		return 1;
	}

	if (makeStub) {
		if (outFile == "DEFAULT") {
			outFile = baseName(file).stripExtension();
		}

		backend.output = new Output();
	}
	else if (makeMod) {
		if (outFile == "DEFAULT") {
			outFile = baseName(file).stripExtension();
		}

		backend.output = new Output(file, modCPU, modOS, file, outFile ~ ".mod");
	}
	else {
		if (outFile == "DEFAULT") {
			outFile = "out";
		}

		backend.output = new Output(outFile);
	}

	if (backend is null) {
		ErrorNoInfo("No backend selected");
	}
	backend.output.outFile = outFile;

	auto preproc = new Preprocessor();
	if (makeMod) {
		preproc.thisMod = outFile;
	}

	foreach (ref opt ; backendOpts) {
		if (!backend.HandleOption(opt, versions, preproc)) {
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

	if (!makeMod) backend.output ~= header ~ '\n';

	if (file == "") {
		stderr.writeln("No source files");
		return 1;
	}

	auto     profiler = new Profiler();
	string[] included;

	profiler.Begin();
	auto nodes = ParseFile(file);
	profiler.End("parsing");

	if (debugParser) {
		foreach (ref node ; nodes) {
			writeln(node);
		}

		return 0;
	}

	auto compiler                   = new Compiler();
	compiler.backend                = backend;
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

	preproc.disabled    ~= disabled;
	preproc.includeDirs ~= includeDirs;
	preproc.versions    ~= versions;
	preproc.stub         = makeStub;

	if (makeMod) preproc.versions ~= "Module";

	profiler.Begin();
	nodes = preproc.Run(nodes);
	profiler.End("preprocessor");
	if (!preproc.success) return 1;

	if (makeMod && makeStub) {
		auto stubComp = new StubCompiler(file, modCPU, modOS, file, outFile ~ ".mod");
		profiler.Begin();
		stubComp.Compile(nodes);
		profiler.End("stub compiler");
		return 0;
	}

	if (optimise) {
		if (makeMod) {
			ErrorNoInfo("Dead code removal on modules should be done at link time");
			return 1;
		}
		auto codeRemover = new CodeRemover();

		profiler.Begin();
		codeRemover.Run(nodes);
		profiler.End("dead code remover");

		nodes = codeRemover.res;
		if (!codeRemover.success) return 1;
	}

	auto stackChecker     = new StackChecker();
	stackChecker.preproc  = preproc;
	stackChecker.profiler = profiler;

	if (makeMod) {
		stackChecker.mod = baseName(outFile);
	}

	try {
		if (!noStackCheck) stackChecker.Run(nodes);
	}
	catch (StackCheckerError) {
		stderr.writeln("Error occured during stack checker stage");
		return 1;
	}

	if (stackChecker.stack.length > 0) {
		try {
			stackChecker.StackOverflow(nodes[$ - 1], 0);
		}
		catch (StackCheckerError) {
			return 1;
		}
	}

	if (stackCheckerFunctions) {
		stackChecker.DumpFunctions();
	}

	if (onlyStackCheck) {
		writeln("no errors");
		return 0;
	}

	compiler.backend.summary = preproc.summary;
	compiler.versions        = preproc.versions;

	profiler.Begin();
	compiler.Compile(nodes);
	profiler.End("compilation");
	if (!compiler.success) return 1;

	// std.file.write(outFile, compiler.backend.output);
	compiler.backend.output.Finish();

	if (runFinal) {
		profiler.Begin();
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

		profiler.End("final commands");
	}

	if (printProfiler) {
		profiler.PrintResults();
	}

	return 0;
}
