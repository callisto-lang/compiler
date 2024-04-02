module callisto.app;

import std.conv;
import std.file;
import std.stdio;
import std.string;
import callisto.compiler;
import callisto.language;
import callisto.optimiser;
import callisto.preprocessor;
import callisto.backends.y16;
import callisto.backends.rm86;

const static string usage = "
Usage: %s FILE [FLAGS]

Flags:
	-o FILE    - Sets the output assembly file to FILE (out.asm by default)
	--org ADDR - Sets ORG value for compiler backend's assembly, ADDR is hex
	-i PATH    - Adds PATH to the list of include directories
	-O         - Enables optimisation (only works properly with programs without errors)
	-v VER     - Enables VER as a version
	-b BACKEND - Uses the given backend (backends listed below)

Backends:
	rm86 - Real mode x86
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
	string          outFile = "out.asm";
	ulong           org;
	bool            orgSet;
	string[]        includeDirs;
	bool            optimise;
	string[]        versions;
	CompilerBackend backend = new BackendRM86();

	writeln(backend.GetVersions());

	for (size_t i = 1; i < args.length; ++ i) {
		if (args[i][0] == '-') {
			switch (args[i]) {
				case "-o": {
					++ i;

					if (i >= args.length) {
						stderr.writeln("-o requires FILE parameter");
						return 1;
					}
					if (outFile != "") {
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
					break;
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
						case "y16": {
							goto default;
						}
						default: {
							stderr.writefln("Unknown backend '%s'", args[i]);
						}
					}
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

	writeln(backend.GetVersions());

	if (file == "") {
		stderr.writeln("No source files");
		return 1;
	}

	string[] included;
	auto     nodes = ParseFile(file);

	auto compiler           = new Compiler();
	compiler.backend        = backend;
	compiler.backend.org    = org;
	compiler.backend.orgSet = orgSet;
	
	versions ~= compiler.backend.GetVersions();
	
	writeln(backend.GetVersions());

	auto preproc        = new Preprocessor();
	preproc.includeDirs = includeDirs;
	preproc.versions    = versions;
	nodes               = preproc.Run(nodes);
	
	if (optimise) {
		auto optimiser  = new Optimiser();
		optimiser.Run(nodes);
		nodes = optimiser.res;
	}

	try {
		compiler.Compile(nodes);
	}
	catch (CompilerError) {
		return 1;
	}

	std.file.write(outFile, compiler.backend.output);

	return 0;
}
