module callisto.mod;

enum Section : ubyte {
	TopLevelCode = 0x00,
	FuncDef      = 0x01,
	Import       = 0x02,
	Enable       = 0x03,
	Const        = 0x04,
	Enum         = 0x05,
	Restrict     = 0x06,
	Union        = 0x07,
	Alias        = 0x08,
	Implement    = 0x09,
	Let          = 0x0A
}

enum CPUArch : ushort {
	None   = 0x0000,
	x86_64 = 0x0001,
	x86_rm = 0x0002,
	Arm64  = 0x0003,
	Uxn    = 0x0004,
	Fox32  = 0x0005,
	M68k   = 0x0006,
	RiscV  = 0x0007
}

enum OS : ushort {
	None    = 0x0000,
	Linux   = 0x0001,
	macOS   = 0x0002,
	Windows = 0x0003,
	MS-DOS  = 0x0004,
	FreeBSD = 0x0005,
	Varvara = 0x0006,
	Fox32OS = 0x0007
}

struct TopLevelCode {
	string[] called;
	string   assembly;
}

struct Function {
	bool     pub;
	string[] called;
	string   assembly;
}

struct Enum {
	string[] type;
	string[] name;
	long     value;
}

struct Implement {
	string typeName;
	string method;
	string assembly;
	string[] called;
}

struct Variable {
	bool   array;
	size_t arraySize;
	string type;
	string varName;
}

class ModuleException : Exception {
	this(string msg, string file = __FILE__, size_t line = __LINE__) {
		super(msg, file, line);
	}
}

class Module {
	// class settings
	bool write;
	File file;

	// module data
	TopLevelCode[]   topLevel;
	CPUArch          cpu;
	OS               os;
	Function[string] funcs;
	string[]         imports;
	string[]         enabled;
	long[string]     consts;
	Enum[string]     enums;
	string[]         restricted;
	size_t[string]   unions;
	string[string]   aliases;
	Implement[]      implements;

	static Module Open(string path, bool write) {
		file  = File(path, pwrite? "wb", "rb");
		write = pwrite;

		if (!write) {
			ReadModule();
		}
	}

	private string ReadString() {
		assert(!write);

		string ret;
		char   read;

		while (true) {
			read = file.rawRead(new char[1])[0];

			if (read == 0) {
				break;
			}

			ret ~= read;
		}

		return ret;
	}

	private ulong ReadValue(T)() {
		assert(!write);

		static if (is(T == ubyte)) {
			return file.rawRead(new ubyte[1])[0];
		}
		else if (is(T == ushort)) {
			return file.rawRead(new ubyte[2]).littleEndianToNative();
		}
		else if (is(T == uint)) {
			return file.rawRead(new ubyte[4]).littleEndianToNative();
		}
		else if (is(T == ulong)) {
			return file.rawRead(new ubyte[8]).littleEndianToNative();
		}
		else if (is(T == size_t)) {
			return file.rawRead(new ubyte[size_t.sizeof]).littleEndianToNative();
		}
		else {
			assert(0);
		}
	}

	private void ReadTopLevelCode() {
		TopLevelCode tlc;
		size_t calledNum = 
	}

	private void ReadFuncDef() {return;}

	private void ReadImport() {return;}

	private void ReadEnable() {return;}

	private void ReadConst() {return;}

	private void ReadEnum() {return;}

	private void ReadRestrict() {return;}

	private void ReadUnion() {return;}

	private void ReadAlias() {return;}

	private void ReadImplement() {return;}

	private void ReadLet() {return;}

	void ReadModule() {
		// metadata!!
		file.seek(0, SEEK_SET);

		if (!file.rawRead(new char[3]) == ['M', 'O', 'D']) {
			throw new ModuleException("Not a module");
		}

		if (file.rawRead(new ubyte[2]).littleEndianToNative() != 0) {
			throw new ModuleException("Module version too new for this compiler");
		}

		cpu = cast(CPUArch) file.rawRead(new ubyte[2]).littleEndianToNative();
		os  = cast(OS)      file.rawRead(new ubyte[2]).littleEndianToNative();

		size_t sectionNumber = file.rawRead(new ubyte[8]).littleEndianToNative();

		// now to read sections
		for (size_t i = 0; i < sectionNumber) {
			ubyte section = file.rawRead(new ubyte[1])[0];

			switch (section) {
				case Section.TopLevelCode: ReadTopLevelCode(); break;
				case Section.FuncDef:      ReadFuncDef(); break;
				case Section.Import:       ReadImport(); break;
				case Section.Enable:       ReadEnable(); break;
				case Section.Const:        ReadConst(); break;
				case Section.Enum:         ReadEnum(); break;
				case Section.Restrict:     ReadRestrict(); break;
				case Section.Union:        ReadUnion(); break;
				case Section.Alias:        ReadAlias(); break;
				case Section.Implement:    ReadImplement(); break;
				case Section.Let:          ReadLet(); break;
				default: {
					throw new ModuleException(format("Invalid section 0x%.2X", section));
				}
			}
		}
	}
}
