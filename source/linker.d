module callisto.linker;

// NOT an object file linker, this is for linking module files

class Linker {
	string[] modules;
	string   outFile;

	abstract void Link();
}

int LinkerProgram(string[] args) {
	if (args.length == 0) {
		stderr.writeln("No modules to link");
		return 1;
	}

	
}
