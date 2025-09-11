module callisto.mod.sections;

import std.conv;
import std.array;
import std.stdio;
import std.format;
import std.bitmanip;
import std.algorithm;
import callisto.parser;
import callisto.compiler;

alias SectionInt = ulong;

enum ModCPU : SectionInt {
	None    = 0x0000,
	x86_64  = 0x0001,
	RM86    = 0x0002,
	ARM64   = 0x0003,
	Uxn     = 0x0004,
	Fox32   = 0x0005,
	M68k    = 0x0006,
	RISCV   = 0x0007
}

enum ModOS : SectionInt {
	None            = 0x0000,
	Linux           = 0x0001,
	macOS           = 0x0002,
	Windows         = 0x0003,
	DOS             = 0x0004,
	FreeBSD         = 0x0005,
	Varvara         = 0x0006,
	Fox32OS         = 0x0007
}

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
	Let       = 0x0A,
	Struct    = 0x0B,
	BSS       = 0x0C,
	Data      = 0x0D
}

class SectionException : Exception {
	this(string msg, string file = __FILE__, size_t line = __LINE__) {
		super(msg, file, line);
	}
}

class Section {
	string inMod;

	abstract SectionType GetType();
	abstract void        Write(File file);
	abstract void        Read(File file, bool skip);

	void WriteAsm(string code) {
		throw new SectionException("Module doesn't contain assembly code");
	}

	void AddCall(string call) {
		throw new SectionException("Module doesn't contain called function list");
	}
}

private SectionInt ReadInt(File file) {
	ubyte[SectionInt.sizeof] res = file.rawRead(new ubyte[SectionInt.sizeof]);
	return littleEndianToNative!SectionInt(res);
}

private ubyte ReadByte(File file) {
	auto res = file.rawRead(new ubyte[1]);

	if (res.length == 0) {
		throw new SectionException("Unexpected EOF while reading " ~ file.name);
	}
	
	return res[0];
}

private string ReadString(File file) {
	string res;

	while (true) {
		auto ch = file.ReadByte();

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

class HeaderSection : Section {
	SectionInt ver;
	ModCPU     cpu;
	ModOS      os;
	SectionInt sectionNum;
	string     source;
	bool       stub;
	bool       main;

	override SectionType GetType() => assert(0);

	ubyte FlagsByte() => (stub? 1 : 0) | (main? 2 : 0);

	override void Write(File file) {
		file.rawWrite(cast(ubyte[]) "MOD");
		file.WriteByte(FlagsByte());
		file.WriteInt(ver);
		file.WriteInt(cast(SectionInt) cpu);
		file.WriteInt(cast(SectionInt) os);
		file.WriteInt(sectionNum);
		file.WriteString(source);
	}

	override void Read(File file, bool skip) {
		file.rawRead(new ubyte[3]);
		auto flags = file.ReadByte();
		stub       = (flags & 1)? true : false;
		main       = (flags & 2)? true : false;
		ver        = file.ReadInt();
		cpu        = cast(ModCPU) file.ReadInt();
		os         = cast(ModOS)  file.ReadInt();
		sectionNum = file.ReadInt();
		source     = file.ReadString();
	}

	override string toString() {
		string str;
		str ~= "==== MODULE HEADER ====\n";
		str ~= format("Version:  %s\n", ver);
		str ~= format("CPU:      %s\n", cpu);
		str ~= format("OS:       %s\n", os);
		str ~= format("Stub:     %s\n", stub);
		str ~= format("Sections: %d\n", sectionNum);
		str ~= format("Source:   %s\n", source);
		str ~= format("Stub:     %s\n", stub);
		str ~= format("Main:     %s\n", main);
		return str;
	}
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

	override void WriteAsm(string code) => cast(void) (assembly ~= code);

	override void AddCall(string call) {
		if (!calls.canFind(call)) calls ~= call;
	}

	override string toString() {
		string str;
		str ~= "==== TOP LEVEL CODE ====\n";
		str ~= format("Calls: %s\n", calls);
		str ~= "Assembly:\n";
		str ~= '\t' ~ assembly.replace("\n", "\n\t") ~ '\n';
		return str;
	}
}

class FuncDefSection : Section {
	bool       pub;
	bool       inline;
	bool       error;
	string[]   calls;
	string     assembly;
	string     name;
	SectionInt params;
	SectionInt ret;

	override SectionType GetType() => SectionType.FuncDef;

	static FuncDefSection FromNode(FuncDefNode node) {
		auto ret   = new FuncDefSection();
		ret.pub    = true; // TODO: public functions
		ret.inline = node.inline;
		ret.error  = node.errors;
		ret.name   = node.name;
		ret.params = node.paramTypes.length;
		ret.ret    = node.returnTypes.length;
		return ret;
	}

	override void Write(File file) {
		file.WriteByte((pub? 1 : 0) | (inline? 2 : 0) | (error? 4 : 0));
		file.WriteStringArray(calls);
		file.WriteString(assembly);
		file.WriteString(name);
		file.WriteInt(params);
		file.WriteInt(ret);
	}

	override void Read(File file, bool skip) {
		auto flags = file.ReadByte();

		pub      = (flags & 1) != 0;
		inline   = (flags & 2) != 0;
		error    = (flags & 4) != 0;
		calls    = file.ReadStringArray();
		assembly = file.ReadOrSkipString(inline? false : skip);
		name     = file.ReadString();
		params   = file.ReadInt();
		ret      = file.ReadInt();
	}

	override void WriteAsm(string code) => cast(void) (assembly ~= code);

	override void AddCall(string call) {
		if (!calls.canFind(call)) calls ~= call;
	}

	override string toString() {
		string str;
		str ~= "==== FUNCTION DEFINITION ====\n";
		str ~= format("Public: %s\n", pub);
		str ~= format("Inline: %s\n", inline);
		str ~= format("Calls:  %s\n", calls);
		str ~= format("Name:   %s\n", name);
		str ~= format("Params: %s\n", params);
		str ~= format("Ret:    %s\n", ret);
		str ~= "Assembly:\n";
		str ~= '\t' ~ assembly.replace("\n", "\n\t") ~ '\n';
		return str;
	}
}

class ImportSection : Section {
	string mod;
	bool   pub;

	static ImportSection FromNode(ImportNode node) {
		auto ret = new ImportSection();
		ret.mod  = node.mod;
		ret.pub  = node.pub;
		return ret;
	}

	override SectionType GetType() => SectionType.Import;

	override void Write(File file) {
		file.WriteByte(pub? 1 : 0);
		file.WriteString(mod);
	}

	override void Read(File file, bool skip) {
		pub = file.ReadByte() != 0;
		mod = file.ReadString();
	}

	override string toString() {
		return format("==== IMPORT %s %s ====", pub? "public" : "private", mod);
	}
}

class EnableSection : Section {
	string ver;

	static EnableSection FromNode(EnableNode node) {
		auto ret = new EnableSection();
		ret.ver  = node.ver;
		return ret;
	}

	override SectionType GetType() => SectionType.Enable;
	override void        Write(File file) => file.WriteString(ver);
	override void        Read(File file, bool skip) => cast(void) (ver = file.ReadString());
	override string      toString() => format("==== ENABLE %s ====", ver);
}

class ConstSection : Section {
	SectionInt value;
	string     name;

	static ConstSection FromNode(ConstNode node) {
		auto ret  = new ConstSection();
		ret.value = cast(SectionInt) node.value;
		ret.name  = node.name;
		return ret;
	}

	override SectionType GetType() => SectionType.Const;

	override void Write(File file) {
		file.WriteInt(value);
		file.WriteString(name);
	}

	override void Read(File file, bool skip) {
		value = file.ReadInt();
		name  = file.ReadString();
	}

	override string toString() => format("==== CONST %s = %s ====", name, value);
}

struct ModEnumEntry {
	SectionInt value;
	string     name;

	string toString() => format("%s = %s", name, value);
}

class EnumSection : Section {
	string         name;
	string         enumType;
	ModEnumEntry[] entries;

	static EnumSection FromNode(EnumNode node) {
		auto ret     = new EnumSection();
		ret.name     = node.name;
		ret.enumType = node.enumType;

		foreach (i, ref name ; node.names) {
			ret.entries ~= ModEnumEntry(cast(SectionInt) node.values[i], name);
		}
		return ret;
	}

	override SectionType GetType() => SectionType.Enum;

	override void Write(File file) {
		file.WriteInt(cast(SectionInt) entries.length);
		file.WriteString(name);
		file.WriteString(enumType);

		foreach (ref entry ; entries) {
			file.WriteInt(entry.value);
			file.WriteString(entry.name);
		}
	}

	override void Read(File file, bool skip) {
		auto length = file.ReadInt();
		name        = file.ReadString();
		enumType    = file.ReadString();

		foreach (i ; 0 .. length) {
			ModEnumEntry entry;
			entry.value  = file.ReadInt();
			entry.name   = file.ReadString();
			entries     ~= entry;
		}
	}

	override string toString() {
		string str;
		str ~= format("==== ENUM %s : %s ====\n", name, enumType);

		foreach (ref entry ; entries) {
			str ~= entry.toString() ~ '\n';
		}

		return str;
	}
}

class RestrictSection : Section {
	string ver;

	static RestrictSection FromNode(RestrictNode node) {
		auto ret = new RestrictSection();
		ret.ver  = node.ver;
		return ret;
	}

	override SectionType GetType() => SectionType.Restrict;
	override void        Write(File file) => file.WriteString(ver);
	override void        Read(File file, bool skip) => cast(void) (ver = file.ReadString());	
	override string      toString() => format("==== RESTRICT %s ====", ver);
}

class UnionSection : Section {
	string   name;
	string[] types;

	override SectionType GetType() => SectionType.Union;

	override void Write(File file) {
		file.WriteString(name);
		file.WriteStringArray(types);
	}

	override void Read(File file, bool skip) {
		name  = file.ReadString();
		types = file.ReadStringArray();
	}

	override string toString() => format("==== UNION %s ====\n%s", name, types.join("\n"));
}

class AliasSection : Section {
	string original;
	string newName;

	this() {}

	this(string original, string newName) {
		this.original = original;
		this.newName  = newName;
	}

	static AliasSection FromNode(AliasNode node) {
		assert(!node.overwrite);

		auto ret     = new AliasSection();
		ret.original = node.from;
		ret.newName  = node.to;
		return ret;
	}

	override SectionType GetType() => SectionType.Alias;

	override void Write(File file) {
		file.WriteString(original);
		file.WriteString(newName);
	}

	override void Read(File file, bool skip) {
		original = file.ReadString();
		newName  = file.ReadString();
	}

	override string toString() => format("==== ALIAS %s = %s ====", newName, original);
}

class ImplementSection : Section {
	string   type;
	string   method;
	string[] called;
	string   assembly;

	static ImplementSection FromNode(ImplementNode node) {
		auto ret   = new ImplementSection();
		ret.type   = node.structure;
		ret.method = node.method;
		return ret;
	}

	override SectionType GetType() => SectionType.Implement;

	override void Write(File file) {
		file.WriteString(type);
		file.WriteString(method);
		file.WriteStringArray(called);
		file.WriteString(assembly);
	}

	override void Read(File file, bool skip) {
		type     = file.ReadString();
		method   = file.ReadString();
		called   = file.ReadStringArray();
		assembly = file.ReadOrSkipString(skip);
	}

	override void WriteAsm(string code) => cast(void) (assembly ~= code);

	override void AddCall(string call) {
		if (!called.canFind(call)) called ~= call;
	}

	override string toString() {
		string str;
		str ~= "==== FUNCTION DEFINITION ====\n";
		str ~= format("Calls:  %s\n", called);
		str ~= format("Type:   %s\n", type);
		str ~= format("Method: %s\n", method);
		str ~= "Assembly:\n";
		str ~= '\t' ~ assembly.replace("\n", "\n\t") ~ '\n';
		return str;
	}
}

class LetSection : Section {
	bool       array;
	SectionInt size;
	bool       ptr;
	string     type;
	string     name;

	static LetSection FromGlobal(Global var) {
		auto ret  = new LetSection();
		ret.array = var.array;
		ret.size  = var.Size();
		ret.ptr   = var.type.ptr;
		ret.type  = var.type.name;
		ret.name  = ret.name;
		return ret;
	}

	override SectionType GetType() => SectionType.Let;

	override void Write(File file) {
		file.WriteByte(array? 1 : 0);
		file.WriteInt(size);
		file.WriteByte(ptr? 1 : 0);
		file.WriteString(type);
		file.WriteString(name);
	}

	override void Read(File file, bool skip) {
		array = file.rawRead(new ubyte[1])[0] != 0;
		size  = file.ReadInt();
		ptr   = file.rawRead(new ubyte[1])[0] != 0;
		type  = file.ReadString();
		name  = file.ReadString();
	}

	override string toString() {
		string str;
		str ~= "==== LET ====\n";
		str ~= format("Array: %s\n", array);
		str ~= format("Size:  %s\n", size);
		str ~= format("Ptr:   %s\n", ptr);
		str ~= format("Type:  %s\n", type);
		str ~= format("Name:  %s\n", name);
		return str;
	}
}

struct ModStructEntry {
	bool       ptr;
	string     type;
	bool       array;
	SectionInt size;
	string     name;

	string toString() => format(
		"MEMBER %s %s %s %s %s",
		array? "array" : "", array? text(size) : "", ptr? "ptr" : "", type, name
	);
}

class StructSection : Section {
	string           name;
	string           inherits;
	ModStructEntry[] entries;

	override SectionType GetType() => SectionType.Struct;

	private static void WriteEntry(File file, ref ModStructEntry entry) {
		file.WriteByte(entry.ptr? 1 : 0);
		file.WriteString(entry.type);
		file.WriteByte(entry.array? 1 : 0);
		file.WriteInt(entry.size);
		file.WriteString(entry.name);
	}

	private static ModStructEntry ReadEntry(File file) {
		ModStructEntry ret;

		ret.ptr    = file.ReadByte() != 0;
		ret.type   = file.ReadString();
		ret.array  = file.ReadByte() != 0;
		ret.size   = file.ReadInt();
		ret.name   = file.ReadString();

		return ret;
	}

	override void Read(File file, bool skip) {
		name     = file.ReadString();
		inherits = file.ReadString();

		auto num = file.ReadInt();
		foreach (i ; 0 .. num) {
			entries ~= ReadEntry(file);
		}
	}

	override void Write(File file) {
		file.WriteString(name);
		file.WriteString(inherits);
		file.WriteInt(cast(SectionInt) entries.length);

		foreach (ref entry ; entries) {
			WriteEntry(file, entry);
		}
	}

	override string toString() {
		string str;
		str ~= format("==== STRUCT %s ====\n", name);

		foreach (ref entry ; entries) {
			str ~= entry.toString() ~ "\n";
		}
		return str;
	}
}

class BSSSection : Section {
	string assembly;

	override SectionType GetType() => SectionType.BSS;

	override void Write(File file) {
		file.WriteString(assembly);
	}

	override void Read(File file, bool skip) {
		assembly = file.ReadOrSkipString(skip);
	}

	override void WriteAsm(string code) => cast(void) (assembly ~= code);

	override string toString() {
		string str;
		str ~= "==== BSS ====\n";
		str ~= "Assembly:\n";
		str ~= '\t' ~ assembly.replace("\n", "\n\t") ~ '\n';
		return str;
	}
}

class DataSection : Section {
	string assembly;

	override SectionType GetType() => SectionType.Data;

	override void Write(File file) {
		file.WriteString(assembly);
	}

	override void Read(File file, bool skip) {
		assembly = file.ReadOrSkipString(skip);
	}

	override void WriteAsm(string code) => cast(void) (assembly ~= code);

	override string toString() {
		string str;
		str ~= "==== DATA ====\n";
		str ~= "Assembly:\n";
		str ~= '\t' ~ assembly.replace("\n", "\n\t") ~ '\n';
		return str;
	}
}
