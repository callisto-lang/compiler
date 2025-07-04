module callisto.mod.mod;

import std.path;
import std.stdio;
import std.bitmanip;
import std.algorithm;
import std.exception;
import core.stdc.stdlib;
import callisto.mod.sections;

class ModuleException : Exception {
	mixin basicExceptionCtors;
}

class Module {
	string        name;
	HeaderSection header;
	Section[]     sections;

	private File file;

	this() {
		header = new HeaderSection();
	}

	void ReadHeader(string path) {
		if (!path.endsWith(".mod")) {
			throw new ModuleException("Not a module file");
		}
		name = path.baseName().stripExtension();

		file = File(path, "rb");

		header = new HeaderSection();
		header.Read(file, false);
	}

	void Read(string path, bool skip) {
		ReadHeader(path);

		foreach (i ; 0 .. header.sectionNum) {
			Section sect;
			auto    type = file.rawRead(new ubyte[1])[0];

			switch (type) {
				case SectionType.TopLevel:  sect = new TopLevelSection();  break;
				case SectionType.FuncDef:   sect = new FuncDefSection();   break;
				case SectionType.Import:    sect = new ImportSection();    break;
				case SectionType.Enable:    sect = new EnableSection();    break;
				case SectionType.Const:     sect = new ConstSection();     break;
				case SectionType.Enum:      sect = new EnumSection();      break;
				case SectionType.Restrict:  sect = new RestrictSection();  break;
				case SectionType.Union:     sect = new UnionSection();     break;
				case SectionType.Alias:     sect = new AliasSection();     break;
				case SectionType.Implement: sect = new ImplementSection(); break;
				case SectionType.Let:       sect = new LetSection();       break;
				case SectionType.Struct:    sect = new StructSection();    break;
				case SectionType.BSS:       sect = new BSSSection();       break;
				case SectionType.Data:      sect = new DataSection();      break;
				default: {
					stderr.writefln("Invalid section type: %.2X", type);
					throw new ModuleException("Invalid section type");
				}
			}

			sect.Read(file, skip);
			sect.inMod  = name;
			sections   ~= sect;
		}
	}

	void Write(string path) {
		auto file = File(path, "wb");

		header.ver        = 0x0000;
		header.sectionNum = cast(SectionInt) sections.length;
		header.Write(file);

		foreach (ref sect ; sections) {
			sect.Write(file);
		}
	}
}

// smaller version that uses less memory, just used for writing
class WriteModule {
	string name;
	bool   main;

	private SectionInt sectionNum;
	private File       file;

	this(string path, ModCPU cpu, ModOS os, string source, string dest) {
		file = File(dest, "wb+");
		name = path.baseName().stripExtension();

		auto header   = new HeaderSection();
		header.cpu    = cpu;
		header.os     = os;
		header.source = source;
		header.Write(file);
	}

	void Add(Section sect) {
		file.rawWrite(cast(ubyte[]) [sect.GetType()]);
		sect.Write(file);
		++ sectionNum;
	}

	void Finish() {
		file.flush();

		if (main) {
			file.seek(3, SEEK_SET);
			file.rawWrite(cast(ubyte[]) [2]);
		}

		auto bytes = nativeToLittleEndian(sectionNum);
		file.seek(28, SEEK_SET);
		file.rawWrite(bytes);
		file.flush();
		file.close();
	}
}
