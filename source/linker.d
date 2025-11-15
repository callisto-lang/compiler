module callisto.linker;

import std.path;
import std.stdio;
import std.format;
import std.algorithm;
import callisto.util;
import callisto.error;
import callisto.mod.mod;
import callisto.mod.sections;
import callisto.linkers.uxn;
import callisto.linkers.lua;
import callisto.linkers.rm86;
import callisto.linkers.arm64;
import callisto.linkers.x86_64;

// NOT an object file linker, this is for linking module files

struct Func {
	string mod;
	string name;
	bool   inline;

	string Label(bool prefix = true) {
		return format(
			"%sfunc__%s__sep__%s", prefix? "__" : "", mod, name.Sanitise()
		);
	}
}

class Linker {
	ModOS          os;
	string         outFile;
	Module[string] mods;
	string[]       added;

	string   tlcAsm;
	string   funcAsm;
	string   bssAsm;
	string   dataAsm;
	string[] externs;
	string[] calls;
	Func[]   funcs;

	FuncDefSection[] funcDefs;

	bool FuncExists(string name) {
		foreach (ref func ; funcs) {
			if (func.name == name) return true;
		}

		return false;
	}

	size_t CountFuncs(string name) {
		size_t ret;

		foreach (ref func ; funcs) {
			if (func.name == name) ++ ret;
		}

		return ret;
	}

	Func GetFunc(string name) {
		foreach (ref func ; funcs) {
			if (func.name == name) return func;
		}
		assert(0);
	}

	void CheckForOneFunc(string name) {
		if (CountFuncs(name) != 1) {
			ErrorNoInfo("Linker requires one function named `%s`", name);
		}
		if (GetFunc(name).inline) {
			ErrorNoInfo("Linker requires that the function '%s' is not inline", name);
		}
	}

	void Add(Module mod) {
		foreach (ref isect ; mod.sections) {
			switch (isect.GetType()) {
				case SectionType.TopLevel: {
					auto sect  = cast(TopLevelSection) isect;
					tlcAsm    ~= sect.assembly;

					foreach (ref call ; sect.calls) {
						if (!calls.canFind(call)) {
							calls ~= call;
						}
					}
					break;
				}
				case SectionType.FuncDef: {
					auto sect = cast(FuncDefSection) isect;

					if (!sect.inline) {
						funcAsm ~= sect.assembly;
					}

					Func func;
					func.mod     = mod.name;
					func.name    = sect.name;
					func.inline  = sect.inline;
					funcs       ~= func;
					funcDefs    ~= sect;

					foreach (ref call ; sect.calls) {
						if (!calls.canFind(call)) {
							calls ~= call;
						}
					}
					break;
				}
				case SectionType.Implement: {
					auto sect  = cast(ImplementSection) isect;
					funcAsm   ~= sect.assembly;

					foreach (ref call ; sect.called) {
						if (!calls.canFind(call)) {
							calls ~= call;
						}
					}
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
				case SectionType.Extern: {
					auto sect  = cast(ExternSection) isect;
					externs   ~= sect.symbolName;
					break;
				}
				default: break;
			}
		}
	}

	abstract bool HandleOption(string opt);
	abstract void Link();

	void AddImports(Module mod, string name) {
		added ~= name;

		foreach (ref isect ; mod.sections) {
			if (isect.GetType() == SectionType.Import) {
				auto sect = cast(ImportSection) isect;

				if (!added.canFind(sect.mod)) {
					AddImports(mods[sect.mod], sect.mod);
				}
			}
		}

		Add(mod);
	}
}

int LinkerProgram(string[] args) {
	string[] modules;
	string   outFile = "out";
	string[] linkerOptions;
	bool     printSections;

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
				case "-lo": {
					++ i;

					if (i >= args.length) {
						stderr.writeln("-lo expects OPTION parameter");
						return 1;
					}

					linkerOptions ~= args[i];
					break;
				}
				case "-ps": {
					printSections = true;
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

	Module[string] mods;
	string         main;

	foreach (i, ref path ; modules) {
		auto mod = new Module();
		mod.Read(path, false);

		if (printSections) {
			foreach (ref sect ; mod.sections) {
				writeln(sect);
			}

			return 0;
		}

		if (mod.header.stub) {
			ErrorNoInfo("Module '%s' is a stub and cannot be linked", path.baseName());
		}

		if (i == 0) {
			cpu = mod.header.cpu;
			os  = mod.header.os;
		}
		else {
			if ((mod.header.cpu != cpu) || (mod.header.os != os)) {
				ErrorNoInfo(
					"Module '%s' architecture does not match other modules",
					path.baseName()
				);
				return 1;
			}
		}

		string name = path.baseName().stripExtension();

		mods[name] = mod;

		if (mod.header.main) {
			main = name;
		}
	}

	if (main == "") {
		ErrorNoInfo("None of the given modules are the main module");
	}

	Linker linker;

	switch (cpu) {
		case ModCPU.x86_64: linker = new LinkerX86_64(); break;
		case ModCPU.RM86:   linker = new LinkerRM86();   break;
		case ModCPU.ARM64:  linker = new LinkerARM64();  break;
		case ModCPU.Uxn:    linker = new LinkerUXN();    break;
		case ModCPU.Lua:    linker = new LinkerLua();    break;
		default: {
			stderr.writefln("Unsupported architecture '%s'", cpu);
			return 1;
		}
	}

	linker.os      = os;
	linker.outFile = outFile;
	linker.mods    = mods;

	foreach (ref opt ; linkerOptions) {
		if (!linker.HandleOption(opt)) {
			stderr.writefln("Linker: unknown option '%s'", opt);
			return 1;
		}
	}

	// link modules
	linker.AddImports(mods[main], main);

	linker.Link();
	return 0;
}
