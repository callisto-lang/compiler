module callisto.linker;

import std.path;
import std.stdio;
import callisto.mod.mod;
import callisto.mod.sections;
import callisto.linkers.x86_64;

// NOT an object file linker, this is for linking module files

class Linker {
	ModOS  os;
	string outFile;

	abstract bool HandleOption(string opt);
	abstract void Add(Module mod);
	abstract void Link();
}

int LinkerProgram(string[] args) {
	string[] modules;
	string   outFile;

	for (size_t i = 0; i < args.length; ++ i) {
		if (args[i][0] == '-') {
			switch (args[i]) {
				case "-o": {
					++ i;

					if (i >= args.length) {
						stderr.writeln("-o expects FILE parameter");
						return 1;
					}

					outFile = args[i];
					break;
				}
				default: {
					stderr.writefln("Unknown flag: %s", args[i]);
					return 1;
				}
			}
		}
		else {
			modules ~= args[i];
		}
	}

	ModCPU cpu;
	ModOS  os;

	foreach (i, ref path ; modules) {
		auto mod = new Module();
		mod.ReadHeader(path);

		writeln(mod.header);

		if (i == 0) {
			cpu = mod.header.cpu;
			os  = mod.header.os;
		}
		else {
			if ((mod.header.cpu != cpu) || (mod.header.os != os)) {
				stderr.writefln(
					"Module '%s' architecture does not match other modules",
					path.baseName()
				);
				return 1;
			}
		}
	}

	Linker linker;

	switch (cpu) {
		case ModCPU.x86_64: linker = new LinkerX86_64(); break;
		default: {
			stderr.writefln("Unsupported architecture '%s'", cpu);
			return 1;
		}
	}

	linker.os = os;

	foreach (ref path ; modules) {
		auto mod = new Module();
		mod.Read(path, false);
		linker.Add(mod);
	}

	linker.Link();
	return 0;
}
