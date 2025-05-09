module callisto.mod.sections;

import std.conv;
import std.stdio;
import std.bitmanip;

enum SectionType : ubyte {
	TopLevel  = 0x00,
	FuncDef   = 0x01,
	Import    = 0x02,
	Enable    = 0x03,
	Const     = 0x04,
	Enum      = 0x05,
	Restrict  = 0x06,
	Union     = 0x07,
	Alias     = 0x08,
	Implement = 0x09,
	Let       = 0x0A

	// TODO: structures
}

alias SectionInt = ulong;

class SectionException : Exception {
	this(string msg, string file = __FILE__, size_t line = __LINE__) {
		super(msg, file, line);
	}
}

class Section {
	abstract SectionType GetType();
	abstract void        Write(File file);
	abstract void        Read(File file, bool skip);
}

private SectionInt ReadInt(File file) {
	ubyte[SectionInt.sizeof] res = file.rawRead(new ubyte[SectionInt.sizeof]);
	return littleEndianToNative!SectionInt(res);
}

private ubyte ReadByte(File file) {
	return file.rawRead(new ubyte[1])[0];
}

private string ReadString(File file) {
	string res;

	while (true) {
		auto ch = file.rawRead(new ubyte[1])[0];

		if (ch == 0) return res;
		else         res ~= ch;
	}
}

private void SkipString(File file) {
	while (true) {
		auto ch = file.rawRead(new ubyte[1])[0];

		if (ch == 0) return;
	}
}

private string ReadOrSkipString(File file, bool skip) {
	if (skip) {
		file.SkipString();
		return "";
	}

	return file.ReadString();
}

private string[] ReadStringArray(File file) {
	auto     length = ReadInt(file);
	string[] res;

	foreach (i ; 0 .. length) {
		res ~= ReadString(file);
	}

	return res;
}

private void SkipStringArray(File file) {
	auto length = ReadInt(file);

	foreach (i ; 0 .. length) {
		SkipString(file);
	}
}

private void WriteString(File file, string str) {
	file.rawWrite(str);
	file.rawWrite(cast(ubyte[]) [0]);
}

private void WriteStringArray(File file, string[] arr) {
	auto len = cast(SectionInt) arr.length;
	file.rawWrite(nativeToLittleEndian(len));

	foreach (ref str ; arr) {
		WriteString(file, str);
	}
}

private void WriteInt(File file, SectionInt value) {
	file.rawWrite(nativeToLittleEndian(value));
}

private void WriteByte(File file, ubyte value) {
	file.rawWrite([value]);
}

class TopLevelSection : Section {
	string[] calls;
	string   assembly;

	override SectionType GetType() => SectionType.TopLevel;

	override void Write(File file) {
		file.WriteStringArray(calls);
		file.WriteString(assembly);
	}

	override void Read(File file, bool skip) {
		calls    = file.ReadStringArray();
		assembly = file.ReadOrSkipString(skip);
	}
}

class FuncDefSection : Section {
	bool       pub;
	string[]   calls;
	string     assembly;
	string     name;
	SectionInt params;
	SectionInt ret;

	override SectionType GetType() => SectionType.FuncDef;

	override void Write(File file) {
		file.WriteByte(pub? 1 : 0);
		file.WriteStringArray(calls);
		file.WriteString(assembly);
		file.WriteString(name);
		file.WriteInt(params);
		file.WriteInt(ret);
	}

	override void Read(File file, bool skip) {
		pub      = file.ReadByte() != 0;
		calls    = file.ReadStringArray();
		assembly = file.ReadOrSkipString(skip);
		name     = file.ReadString();
		params   = file.ReadInt();
		ret      = file.ReadInt();
	}
}

class ImportSection : Section {
	string mod;

	override SectionType GetType() => SectionType.Import;
	override void        Write(File file) => file.WriteString(mod);
	override void        Read(File file, bool skip) => cast(void) (mod = file.ReadString());
}

class EnableSection : Section {
	string ver;

	override SectionType GetType() => SectionType.Enable;
	override void        Write(File file) => file.WriteString(ver);
	override void        Read(File file, bool skip) => cast(void) (ver = file.ReadString());
}

class ConstSection : Section {
	SectionInt value;
	string     name;

	override SectionType GetType() => SectionType.Const;

	override void Write(File file) {
		file.WriteInt(value);
		file.WriteString(name);
	}

	override void Read(File file, bool skip) {
		value = file.ReadInt();
		name  = file.ReadString();
	}
}

struct ModEnumEntry {
	SectionInt value;
	string     name;
}

class EnumSection : Section {
	string         name;
	ModEnumEntry[] entries;

	override SectionType GetType() => SectionType.Enum;

	override void Write(File file) {
		file.WriteInt(cast(SectionInt) entries.length);
		file.WriteString(name);

		foreach (ref entry ; entries) {
			file.WriteInt(entry.value);
			file.WriteString(entry.name);
		}
	}

	override void Read(File file, bool skip) {
		auto length = file.ReadInt();
		name = file.ReadString();

		foreach (i ; 0 .. length) {
			ModEnumEntry entry;
			entry.value = file.ReadInt();
			entry.name = file.ReadString();
		}
	}
}

class RestrictSection : Section {
	
}
