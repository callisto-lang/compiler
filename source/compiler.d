module callisto.compiler;

import std.file;
import std.path;
import std.array;
import std.stdio;
import std.format;
import std.algorithm;
import callisto.util;
import callisto.error;
import callisto.parser;
import callisto.output;
import callisto.summary;
import callisto.language;
import callisto.preprocessor;
import callisto.mod.sections;

ubyte addrSize; // in bytes, set by backend

struct StructEntry {
	UsedType type;
	string   name;
	bool     array;
	size_t   size;
	size_t   offset;

	size_t Size() => array? size * type.Size() : type.Size();
}

struct StructVariable {
	size_t size;
	size_t offset; // Relative to root struct, unlike StructEntry
	bool   structure; // includes defined structs and arrays
}

struct Type {
	string        mod;
	string        name;
	ulong         size;
	bool          isSigned;
	bool          isStruct;
	StructEntry[] structure;
	bool          hasInit;
	bool          hasDeinit;

	string FullName() => format("%s.%s", mod, name);
}

struct UsedType {
	Type type;
	bool ptr;

	alias type this;

	size_t Size() => ptr? addrSize : type.size;
}

struct Variable {
	string   name;
	UsedType type;
	uint     offset; // SP + offset to access
	bool     array;
	ulong    arraySize;
	size_t   stackSize; // use this instead of Size() if >0

	size_t Size() => array? arraySize * type.Size() : type.Size();
	size_t StackSize() => stackSize > 0? stackSize : Size();
}

struct Global {
	bool     pub;
	string   mod;
	string   name;
	UsedType type;
	bool     array;
	ulong    arraySize;
	void*    extra;

	size_t Size() => array? arraySize * type.Size() : type.Size();
}

struct Constant {
	string mod;
	string name;
	Node   value;
}

struct Array {
	string[] values;
	UsedType type;
	bool     global;
	void*    extra;

	size_t Size() => type.Size() * values.length;
}

enum WordType {
	Callisto,
	Raw,
	C,
	Lua
}

struct Word {
	string     mod;
	string     name;
	WordType   type;
	bool       raw; // TODO: only used by rm86, not sure what the purpose is
	bool       inline;
	Node[]     inlineNodes;
	bool       error;
	UsedType[] params;

	// for C words, if supported
	UsedType ret;
	bool     isVoid;
	string   symbolName;

	string FullName() => format("%s.%s", mod, name);
}

class CompilerBackend {
	Word[]           words;
	string           thisFunc;
	bool             inScope;
	uint             blockCounter; // used for block statements
	bool             inWhile;
	uint             currentLoop;
	Output           output;
	ulong            org = 0xFFFFFFFFFFFFFFFF;
	bool             orgSet;
	Compiler         compiler;
	bool             useDebug;
	bool             exportSymbols;
	string[]         link;
	bool             keepAssembly;
	string           os;
	string           defaultOS;
	Variable[]       variables;
	Global[]         globals;
	Array[]          arrays;
	Type[]           types;
	Constant[]       consts;
	Summary          summary;
	bool             symbolPrefix = true; // prefix with __?

	abstract string[] GetVersions();
	abstract string[] FinalCommands();
	abstract long     MaxInt();
	abstract string   ExecExt();
	abstract string   DefaultHeader();
	abstract bool     HandleOption(string opt, ref string[] versions, Preprocessor preproc);

	void NewConst(string name, long value, ErrorInfo error = ErrorInfo.init) {
		consts ~= Constant(output.GetModName(), name, new SignedIntNode(error, value));
	}

	void NewConst(string mod, string name, long value, ErrorInfo error = ErrorInfo.init) {
		consts ~= Constant(mod, name, new SignedIntNode(error, value));
	}

	void BeforeCompile(Node node) {}

	abstract void BeginMain();

	abstract void Init();
	abstract void End();
	abstract void CompileWord(WordNode node);
	abstract void CompileSignedInt(SignedIntNode node);
	abstract void CompileInteger(IntegerNode node);
	abstract void CompileFuncDef(FuncDefNode node);
	abstract void CompileIf(IfNode node);
	abstract void CompileWhile(WhileNode node);
	abstract void CompileLet(LetNode node);
	abstract void CompileArray(ArrayNode node);
	abstract void CompileString(StringNode node);
	abstract void CompileReturn(WordNode node);
	abstract void CompileBreak(WordNode node);
	abstract void CompileContinue(WordNode node);
	abstract void CompileExtern(ExternNode node);
	abstract void CompileCall(WordNode node);
	abstract void CompileAddr(AddrNode node);
	abstract void CompileImplement(ImplementNode node);
	abstract void CompileSet(SetNode node);
	abstract void CompileTryCatch(TryCatchNode node);
	abstract void CompileThrow(WordNode node);

	void CompileStruct(StructNode node) {
		size_t offset;

		if (TypeExistsHere(node.name)) {
			Error(node.error, "Type '%s' defined multiple times", node.name);
		}

		StructEntry[] entries;
		string[]      members;

		output.StartSection(SectionType.Struct);
		if (output.mode == OutputMode.Module) {
			auto sect = output.ThisSection!StructSection();
			sect.name = node.name;

			if (node.inherits) {
				sect.inherits = node.inheritsFrom;
			}
		}

		if (node.inherits) {
			if (!TypeExists(node.inheritsFrom)) {
				Error(node.error, "Type '%s' doesn't exist", node.inheritsFrom);
			}
			if (CountTypes(node.inheritsFrom) > 1) {
				Error(node.error, "Multiple matches for type '%s'", node.inheritsFrom);
			}

			if (!GetType(node.inheritsFrom).isStruct) {
				Error(node.error, "Type '%s' is not a structure", node.inheritsFrom);
			}

			entries = GetType(node.inheritsFrom).structure;

			foreach (ref member ; GetType(node.inheritsFrom).structure) {
				members ~= member.name;
			}

			offset = GetType(node.inheritsFrom).size;
		}

		UsedType*[] copiesOfMe;

		foreach (ref member ; node.members) {
			bool canUseMyself = false;

			if ((member.type.name == node.name) && member.type.ptr) {
				canUseMyself = true;
			}

			if (!canUseMyself && !TypeExists(member.type.name)) {
				Error(node.error, "Type '%s' doesn't exist", member.type.name);
			}
			if (members.canFind(member.name)) {
				Error(node.error, "Duplicate member '%s'", member.name);
			}

			UsedType memberType;
			if (canUseMyself) {
				memberType = UsedType(
					Type.init, member.type.ptr
				);
			}
			else {
				memberType = UsedType(
					GetType(member.type.name), member.type.ptr
				);
			}
			memberType.ptr = member.type.ptr;

			auto newMember = StructEntry(
				memberType, member.name, member.array, member.size, offset
			);
			entries    ~= newMember;
			members    ~= member.name;

			if (canUseMyself) {
				copiesOfMe ~= &entries[$ - 1].type;
			}

			offset += newMember.Size();

			if (output.mode == OutputMode.Module) {
				auto sect = output.ThisSection!StructSection();
				sect.entries ~= ModStructEntry(
					member.type.ptr, member.type.name, member.array,
					cast(SectionInt) member.size, member.name
				);
			}
		}

		foreach (ref member ; entries) {
			NewConst(format("%s.%s", node.name, member.name), member.offset);
		}

		NewConst(format("%s.sizeOf", node.name), offset);
		types ~= Type(output.GetModName(), node.name, offset, false, true, entries);

		foreach (ref me ; copiesOfMe) {
			*me = UsedType(types[$ - 1], true);
		}

		output.FinishSection();
	}

	void CompileEnum(EnumNode node, string mod) {
		if (!TypeExists(node.enumType)) {
			Error(node.error, "Enum base type '%s' doesn't exist", node.enumType);
		}
		if (CountTypes(node.enumType) > 1) {
			Error(node.error, "Multiple types named '%s' exist", node.enumType);
		}
		if (TypeExistsHere(node.name)) {
			Error(node.error, "Enum name is already used by type '%s'", node.enumType);
		}

		auto baseType  = GetType(node.enumType);
		baseType.mod   = mod;
		baseType.name  = node.name;
		types         ~= baseType;

		output.StartSection(SectionType.Enum);
		if (output.mode == OutputMode.Module) {
			auto sect     = output.ThisSection!EnumSection();
			sect.name     = node.name;
			sect.enumType = node.enumType;
		}

		foreach (i, ref name ; node.names) {
			NewConst(mod, format("%s.%s", node.name, name), node.values[i]);

			if (output.mode == OutputMode.Module) {
				auto sect     = output.ThisSection!EnumSection();
				sect.entries ~= ModEnumEntry(node.values[i], name);
			}
		}

		NewConst(mod, format("%s.min", node.name), node.values.minElement());
		NewConst(mod, format("%s.max", node.name), node.values.maxElement());
		NewConst(mod, format("%s.sizeOf", node.name), GetType(node.name).size);
		output.FinishSection();
	}

	void CompileConst(ConstNode node) {
		if (ConstExists(node.name)) {
			Error(node.error, "Constant '%s' already defined", node.name);
		}
		
		NewConst(node.name, node.value);

		output.StartSection(SectionType.Const);
		if (output.mode == OutputMode.Module) {
			auto sect  = output.ThisSection!ConstSection();
			sect.value = cast(SectionInt) node.value;
			sect.name  = node.name;
		}
		output.FinishSection();
	}

	void CompileUnion(UnionNode node, string mod) {
		if (TypeExistsHere(node.name)) {
			Error(node.error, "Type '%s' already exists", node.name);
		}

		size_t maxSize = 0;

		string[] unionTypes;

		output.StartSection(SectionType.Union);
		if (output.mode == OutputMode.Module) {
			auto sect = output.ThisSection!UnionSection();
			sect.name = node.name;
		}

		foreach (ref type ; node.types) {
			if (unionTypes.canFind(type)) {
				Error(node.error, "Union type '%s' defined twice", type);
			}
			unionTypes ~= type;

			AssertTypeMatch(node.error, type);

			if (GetType(type).size > maxSize) {
				maxSize = GetType(type).size;
			}

			if (output.mode == OutputMode.Module) {
				auto sect   = output.ThisSection!UnionSection();
				sect.types ~= type;
			}
		}

		types ~= Type(mod, node.name, maxSize);
		NewConst(mod, format("%s.sizeOf", node.name), cast(long) maxSize);
		output.FinishSection();
	}

	void CompileAlias(AliasNode node) {
		if (!TypeExists(node.from)) {
			Error(node.error, "Type '%s' doesn't exist", node.from);
		}
		if ((TypeExists(node.to)) && !node.overwrite) {
			Error(node.error, "Type '%s' already defined", node.to);
		}

		auto baseType  = GetType(node.from);
		baseType.mod   = output.GetModName();
		baseType.name  = node.to;
		types         ~= baseType;

		NewConst(format("%s.sizeOf", node.to), cast(long) GetType(node.to).size);

		output.AddSection(new AliasSection(node.from, node.to));
	}

	final void Error(Char, A...)(ErrorInfo error, in Char[] fmt, A args) {
		ErrorBegin(error);
		stderr.writeln(format(fmt, args));
		PrintErrorLine(error);
		compiler.success = false;
	}

	final void Warn(Char, A...)(ErrorInfo error, in Char[] fmt, A args) {
		WarningBegin(error);
		stderr.writeln(format(fmt, args));
		PrintErrorLine(error);
	}

	string Label(string label) {
		return format("%s%s", output.GetModPrefix(), label);
	}

	string Label(string prefix, string label) {
		return format("%s%s%s", prefix, output.GetModPrefix(), label);
	}

	string Label(Char, A...)(string prefix, in Char[] fmt, A args) {
		return Label(prefix, format(fmt, args));
	}

	string ExtLabel(string mod, string prefix, string label) {
		auto modPrefix = output.mode == OutputMode.Module?
			format("%s__sep__", Sanitise(mod)) : "";
		return format("%s%s%s", prefix, modPrefix, label);
	}

	string ExtLabel(Char, A...)(string mod, string prefix, in Char[] fmt, A args) {
		return ExtLabel(mod, prefix, format(fmt, args));
	}

	string Label(Word word) {
		if (symbolPrefix) {
			return ExtLabel(word.mod, "__func__", "%s", Sanitise(word.name));
		}
		else {
			return ExtLabel(word.mod, "func__", "%s", Sanitise(word.name));
		}
	}

	final size_t CountWords(string name) {
		size_t ret = 0;

		foreach (ref word ; words) {
			if (MatchMod(word.mod, word.name, name)) ++ ret;
		}

		return ret;
	}

	final size_t CountAll(string name) {
		return
			CountWords(name) + CountTypes(name) + CountGlobals(name) + CountConsts(name);
	}

	final void PrintAll(string name) {
		foreach (ref word ; words) {
			if (word.name == name) writefln("%s\n", word);
		}

		foreach (ref type ; types) {
			if (type.name == name) writefln("%s\n", word);
		}

		foreach (ref global ; globals) {
			if (global.name == name) writefln("%s\n", global);
		}

		foreach (ref con ; consts) {
			if (con.name == name) writefln("%s\n", con);
		}
	}

	final Word GetWord(string name) {
		foreach (ref word ; words) {
			if (MatchMod(word.mod, word.name, name)) return word;
		}

		stderr.writeln("FATAL! Word '%s' does not exist", name);
		assert(0);
	}

	final bool WordExists(string name) {
		return words.any!(v => MatchMod(v.mod, v.name, name));
	}

	final bool WordExistsHere(string name) {
		return words.any!(v => (v.mod == output.GetModName()) && (v.name == name));
	}

	final bool FullWordExists(string name) {
		return words.any!(v => format("%s.%s", v.mod, v.name) == name);
	}

	final string[] WordMatches(string name) {
		string[] ret;

		foreach (ref word ; words) {
			if (MatchMod(word.mod, word.name, name)) ret ~= word.name;
		}

		return ret;
	}

	final bool VariableExists(string name) => variables.any!(v => v.name == name);

	final Variable GetVariable(string name) {
		foreach (ref var ; variables) {
			if (var.name == name) {
				return var;
			}
		}

		assert(0);
	}

	final bool TypeExists(string name) {
		return types.any!(v => MatchMod(v.mod, v.name, name));
	}

	final bool TypeExistsHere(string name) {
		if (output.mode == OutputMode.Module) {
			foreach (ref type ; types) {
				if ((type.mod == output.GetModName()) && (type.name == name)) {
					return true;
				}
			}

			return false;
		}
		else {
			return TypeExists(name);
		}
	}

	final bool MatchMod(string thisMod, string thisName, string toMatch) {
		return (thisName == toMatch) || (format("%s.%s", thisMod, thisName) == toMatch);
	}

	final Type GetType(string name) {
		foreach (ref type ; types) {
			if (MatchMod(type.mod, type.name, name)) {
				return type;
			}
		}

		assert(0);
	}

	final size_t CountTypes(string name) {
		size_t ret;

		foreach (ref type ; types) {
			if (MatchMod(type.mod, type.name, name)) {
				++ ret;
			}
		}

		return ret;
	}

	final void AssertTypeMatch(ErrorInfo error, string type) {
		size_t num = CountTypes(type);

		if (num > 1) {
			Error(error, "More than one matches for type '%s'", type);
		}
		if (num == 0) {
			Error(error, "No matches for type '%s'", type);
		}
	}

	final void SetType(string name, Type ptype) {
		foreach (i, ref type ; types) {
			if (MatchMod(type.mod, type.name, name)) {
				types[i] = ptype;
				return;
			}
		}

		assert(0);
	}

	final bool GlobalExists(string name) {
		return globals.any!(v => MatchMod(v.mod, v.name, name));
	}

	final bool GlobalExistsHere(string name) {
		return globals.any!(v => (v.mod == output.GetModName()) && (v.name == name));
	}

	final Global GetGlobal(string name) {
		foreach (ref global ; globals) {
			if (MatchMod(global.mod, global.name, name)) {
				return global;
			}
		}

		assert(0);
	}

	final size_t CountGlobals(string name) {
		size_t ret;

		foreach (ref global ; globals) {
			if (MatchMod(global.mod, global.name, name)) {
				++ ret;
			}
		}

		return ret;
	}

	final bool ConstExists(string name) {
		return consts.any!(v => MatchMod(v.mod, v.name, name));
	}

	final Constant GetConst(string name) {
		foreach (ref v ; consts) {
			if (MatchMod(v.mod, v.name, name)) {
				return v;
			}
		}

		assert(0);
	}

	final size_t CountConsts(string name) {
		size_t ret;

		foreach (ref v ; consts) {
			if (MatchMod(v.mod, v.name, name)) {
				++ ret; // TODO: try using an std.algorithm function for all these
				// count functions
			}
		}

		return ret;
	}

	final bool IsStructMember(string identifier) {
		string[] parts = identifier.split(".");

		if (parts.length < 2) return false;

		// this is not gonna work on globals in other modules ughh
		// TODO: i guess?
		if (VariableExists(parts[0]))    return GetVariable(parts[0]).type.isStruct;
		else if (GlobalExists(parts[0])) return GetGlobal(parts[0]).type.isStruct;
		else                             return false;
	}

	final StructVariable GetStructVariable(Node node, string identifier) {
		string[] parts = identifier.split(".");

		StructEntry[] structure;

		if (VariableExists(parts[0])) {
			structure = GetVariable(parts[0]).type.structure;
		}
		else if (GlobalExists(parts[0])) {
			structure = GetGlobal(parts[0]).type.structure;
		}
		else {
			Error(node.error, "Structure '%s' doesn't exist");
		}

		parts = parts[1 .. $];

		size_t offset;

		while (parts.length > 1) {
			ptrdiff_t index = structure.countUntil!(a => a.name == parts[0]);

			if (index == -1) {
				Error(node.error, "Member '%s' doesn't exist", parts[0]);
			}
			if (!structure[index].type.isStruct) {
				Error(node.error, "Member '%s' is not a structure", parts[0]);
			}

			offset    += structure[index].offset;
			structure  = structure[index].type.structure;
			parts      = parts[1 .. $];
		}

		ptrdiff_t index = structure.countUntil!(a => a.name == parts[0]);

		if (index == -1) {
			Error(node.error, "Member '%s' doesn't exist", parts[0]);
		}

		offset += structure[index].offset;
		size_t size = structure[index].type.Size();

		return StructVariable(
			size, offset, structure[index].array || structure[index].type.isStruct
		);
	}

	final size_t GetStackSize() {
		// old
		//return variables.empty()? 0 : variables[0].offset + variables[0].type.size;

		size_t size;
		foreach (ref var ; variables) {
			size += var.StackSize();
		}

		return size;
	}
}

class CompilerError : Exception {
	this() {
		super("", "", 0);
	}
}

class Compiler {
	CompilerBackend backend;
	string[]        includeDirs;
	string[]        included;
	string          outFile;
	string[]        versions;
	bool            assemblyLines;
	bool            success = true;
	StructSection[] inheritedStructs;

	this() {
		
	}

	void CompileNode(Node inode) {
		backend.BeforeCompile(inode);

		switch (inode.type) {
			case NodeType.Word: {
				auto node = cast(WordNode) inode;

				switch (node.name) {
					case "return":   backend.CompileReturn(node);   break;
					case "continue": backend.CompileContinue(node); break;
					case "break":    backend.CompileBreak(node);    break;
					case "call":     backend.CompileCall(node);     break;
					case "throw":    backend.CompileThrow(node);    break;
					case "error":    backend.Error(node.error, "Error thrown by code"); break;
					default:         backend.CompileWord(node);
				}
				break;
			}
			case NodeType.SignedInt: {
				backend.CompileSignedInt(cast(SignedIntNode) inode);
				break;
			}
			case NodeType.Integer: backend.CompileInteger(cast(IntegerNode) inode); break;
			case NodeType.FuncDef: backend.CompileFuncDef(cast(FuncDefNode) inode); break;
			case NodeType.Include: {
				auto node  = cast(IncludeNode) inode;
				auto path  = format("%s/%s", dirName(node.error.file), node.path);

				if (!exists(path)) {
					bool found;
					
					foreach (ref ipath ; includeDirs) {
						path = format("%s/%s", ipath, node.path);

						if (exists(path)) {
							found = true;
							break;
						}
					}

					if (!found) {
						backend.Error(node.error, "Can't find file '%s'", node.path);
					}
				}

				if (included.canFind(path)) {
					break;
				}

				included ~= path;

				auto nodes = ParseFile(path);

				foreach (inode2 ; nodes) {
					CompileNode(inode2);
				}
				break;
			}
			case NodeType.Asm: {
				auto node       = cast(AsmNode) inode;
				backend.output ~= node.code;
				break;
			}
			case NodeType.If: backend.CompileIf(cast(IfNode) inode); break;
			case NodeType.While: {
				auto node = cast(WhileNode) inode;

				NodeType[] allowedTypes = [
					NodeType.Word, NodeType.SignedInt, NodeType.Integer, NodeType.Addr
				];

				foreach (ref inode2 ; node.condition) {
					if (!allowedTypes.canFind(inode2.type)) {
						backend.Error(
							inode2.error, "While conditions can't contain %s",
							inode2.type
						);
					}
				}

				backend.CompileWhile(node);
				break;
			}
			case NodeType.Let: backend.CompileLet(cast(LetNode) inode); break;
			case NodeType.Requires: {
				auto node = cast(RequiresNode) inode;

				if (!versions.canFind(node.ver)) {
					backend.Error(node.error, "Version '%s' required", node.ver);
				}
				break;
			}
			case NodeType.Array:     backend.CompileArray(cast(ArrayNode) inode); break;
			case NodeType.String:    backend.CompileString(cast(StringNode) inode); break;
			case NodeType.Struct:    backend.CompileStruct(cast(StructNode) inode); break;
			case NodeType.Const:     backend.CompileConst(cast(ConstNode) inode); break;
			case NodeType.Enum: {
				backend.CompileEnum(cast(EnumNode) inode, backend.output.GetModName());
				break;
			}
			case NodeType.Union: {
				backend.CompileUnion(cast(UnionNode) inode, backend.output.GetModName());
				break;
			}
			case NodeType.Alias:     backend.CompileAlias(cast(AliasNode) inode); break;
			case NodeType.Extern:    backend.CompileExtern(cast(ExternNode) inode); break;
			case NodeType.Addr:      backend.CompileAddr(cast(AddrNode) inode); break;
			case NodeType.Implement: backend.CompileImplement(cast(ImplementNode) inode); break;
			case NodeType.Set:       backend.CompileSet(cast(SetNode) inode); break;
			case NodeType.TryCatch:  backend.CompileTryCatch(cast(TryCatchNode) inode); break;
			case NodeType.Unsafe: {
				auto node = cast(UnsafeNode) inode;

				foreach (ref inode2 ; node.nodes) {
					CompileNode(inode2);
				}
				break;
			}
			case NodeType.Anon: {
				auto node = cast(AnonNode) inode;
				auto let  = new LetNode(inode.error);

				let.varType   = node.varType;
				let.name      = "";
				let.array     = node.array;
				let.arraySize = node.arraySize;
				backend.CompileLet(let);
				break;
			}
			case NodeType.Import: {
				if (backend.output.mode != OutputMode.Module) {
					backend.Error(
						inode.error,
						"Import statement used in a program that is not being compiled to a module file"
					);
				}

				backend.output.AddSection(
					ImportSection.FromNode(cast(ImportNode) inode)
				);
				break;
			}
			case NodeType.Module: {
				if (backend.output.mode != OutputMode.Module) {
					backend.Error(
						inode.error,
						"Module statement used in a program that is not being compiled to a module file"
					);
				}

				if (backend.output.mod.main) {
					backend.Error(inode.error, "Main module set multiple times");
				}

				backend.output.mod.main = true;
				break;
			}
			default: {
				backend.Error(inode.error, "Unimplemented node '%s'", inode.type);
			}
		}

		if (assemblyLines) {
			//backend.output ~= "; " ~ inode.toString().replace("\n", "\n; ") ~ '\n';
			size_t line = backend.output.output.count!((ch) => ch == '\n');
			writefln(
				"%s:%d:%d - line %d, node %s", inode.error.file, inode.error.line + 1,
				inode.error.col + 1, line, inode.type
			);
		}
	}

	void ErrorSect(Char, A...)(Section sect, in Char[] fmt, A args) {
		ErrorNoInfo("In module '%s': %s\n%s", sect.inMod, format(fmt, args), sect);
	}

	void ImportFuncDef(FuncDefSection sect) {
		backend.words ~= Word(
			sect.inMod, sect.name, WordType.Callisto, false, sect.inline,
			sect.inline? ParseText(sect.assembly) : [], sect.error,
			replicate([UsedType.init], sect.params)
		);
	}

	void ImportConst(ConstSection sect) {
		backend.consts ~= Constant(
			sect.inMod, sect.name, new SignedIntNode(ErrorInfo.init, sect.value)
		);
	}

	void ImportEnum(EnumSection sect) {
		auto enumNode     = new EnumNode(ErrorInfo.init);
		enumNode.name     = sect.name;
		enumNode.enumType = sect.enumType;

		foreach (ref entry ; sect.entries) {
			enumNode.names  ~= entry.name;
			enumNode.values ~= cast(long) entry.value;
		}

		backend.CompileEnum(enumNode, sect.inMod);
	}

	void ImportUnion(UnionSection sect) {
		Type type;
		type.mod  = sect.inMod;
		type.name = sect.name;

		foreach (ref typeName ; sect.types) {
			if (!backend.TypeExists(typeName)) {
				ErrorSect(sect, "Error on import: Type '%s' does not exist", typeName);
			}
			if (backend.CountTypes(typeName) > 1) {
				ErrorSect(sect, "Error on import: Multiple matches for type '%s'", typeName);
			}

			auto memberType = backend.GetType(typeName);
			if (memberType.size > type.size) {
				type.size = memberType.size;
			}
		}

		backend.types ~= type;
		backend.NewConst(
			sect.inMod, format("%s.sizeOf", type.name), cast(long) type.size
		);
	}

	void ImportAlias(AliasSection sect) {
		if (!backend.TypeExists(sect.original)) {
			ErrorSect(sect, "Error on import: Type '%s' does not exist", sect.original);
		}
		if (backend.CountTypes(sect.original) > 1) {
			ErrorSect(
				sect, "Error on import: Multiple matches for type '%s'", sect.original
			);
		}

		auto oldType   = backend.GetType(sect.original);
		oldType.mod    = sect.inMod;
		oldType.name   = sect.newName;
		backend.types ~= oldType;

		backend.NewConst(
			sect.inMod, format("%s.sizeOf", sect.newName), cast(long) oldType.size
		);
	}

	void ImportImplement(ImplementSection sect) {
		size_t amount = backend.CountTypes(sect.type);
		if (amount == 0) {
			ErrorSect(sect, "Error on import: Type '%s' does not exist", sect.type);
		}
		else if (amount > 1) {
			ErrorSect(sect, "Error on import: Multiple matches for type '%s'", sect.type);
		}

		auto type = backend.GetType(sect.type);
		if (type.mod != sect.inMod) {
			ErrorSect(sect, "Error on import: Implement section implements method for type outside of this module");
		}

		switch (sect.method) {
			case "init":   type.hasInit   = true; break;
			case "deinit": type.hasDeinit = true; break;
			default: {
				ErrorNoInfo("Error on import: Unknown implement method '%s'", sect.method);
			}
		}

		backend.SetType(sect.type, type);
	}

	void ImportLet(LetSection sect) {
		size_t amount = backend.CountTypes(sect.type);
		if (amount == 0) {
			ErrorSect(sect, "Error on import: Type '%s' does not exist", sect.type);
		}
		else if (amount > 1) {
			ErrorSect(sect, "Error on import: Multiple matches for type '%s'", sect.type);
		}

		auto type = backend.GetType(sect.type);

		backend.globals ~= Global(
			sect.inMod, sect.name, UsedType(type, sect.ptr), sect.array,
			cast(ulong) sect.size, null
		);
	}

	void ImportStruct(StructSection sect) {
		Type type;
		type.mod      = sect.inMod;
		type.name     = sect.name;
		type.isStruct = true;

		foreach (ref entry ; sect.entries) {
			Type membType;

			if (entry.type == sect.name) {
				membType = type;
			}
			else {
				size_t amount = backend.CountTypes(entry.type);

				if (amount == 0) {
					ErrorSect(sect, "Error on import: Type '%s' does not exist", entry.type);
				}
				else if (amount > 1) {
					ErrorSect(sect, "Error on import: Multiple matches for type '%s'", entry.type);
				}

				membType = backend.GetType(entry.type);
			}

			type.structure ~= StructEntry(
				UsedType(membType, entry.ptr), entry.name, entry.array,
				cast(size_t) entry.size, 0 // this is overwritten later
			);
		}

		if (sect.inherits != "") {
			inheritedStructs ~= sect;
		}

		backend.types ~= type;
	}

	void ImportExtern(ExternSection sect) {
		if (sect.type == ExternSectType.Callisto) {
			ErrorSect(sect,
				"Callisto externs currently should not be used until they are updated"
			);
		}
		if (sect.type == ExternSectType.Raw) {
			ErrorSect(sect, "Raw externs are deprecated");
		}

		assert(sect.type == ExternSectType.C);

		Word word;
		word.mod        = sect.inMod;
		word.name       = sect.funcName;
		word.symbolName = sect.symbolName;
		word.type       = WordType.C;

		foreach (ref param ; sect.params) {
			if (!backend.TypeExists(param.type)) {
				ErrorSect(sect, "Unknown type '%s'", param.type);
			}

			UsedType type  = UsedType(backend.GetType(param.type), param.ptr);
			word.params   ~= type;
		}

		if (sect.returns.length == 1) {
			if (!backend.TypeExists(sect.returns[0].type)) {
				ErrorSect(sect, "Unknown type '%s'", sect.returns[0].type);
			}

			word.ret = UsedType(
				backend.GetType(sect.returns[0].type), sect.returns[0].ptr
			);
		}
		else if (sect.returns.length == 0) {
			word.isVoid = true;
		}
		else  {
			ErrorSect(sect, "C externs must only have 0 or 1 return value");
		}

		backend.words ~= word;
	}

	void Compile(Node[] nodes) {
		assert(backend !is null);
		assert(addrSize != 0);

		backend.compiler = this;
		backend.Init();

		// import summary
		if (backend.output.mode == OutputMode.Module) {
			foreach (ref sect ; backend.summary.sections) {
				switch (sect.GetType()) {
					case SectionType.FuncDef: {
						ImportFuncDef(cast(FuncDefSection) sect);
						break;
					}
					case SectionType.Const: ImportConst(cast(ConstSection) sect); break;
					case SectionType.Enum:  ImportEnum(cast(EnumSection) sect); break;
					case SectionType.Union: ImportUnion(cast(UnionSection) sect); break;
					case SectionType.Alias: ImportAlias(cast(AliasSection) sect); break;
					case SectionType.Implement: {
						ImportImplement(cast(ImplementSection) sect);
						break;
					}
					case SectionType.Let:    ImportLet(cast(LetSection) sect); break;
					case SectionType.Struct: ImportStruct(cast(StructSection) sect); break;
					case SectionType.Extern: ImportExtern(cast(ExternSection) sect); break;
					default: break;
				}
			}

			foreach (ref sect ; inheritedStructs) {
				auto type = backend.GetType(format("%s.%s", sect.inMod, sect.name));

				if (!backend.TypeExists(sect.inherits)) {
					ErrorSect(
						sect, "Parent type '%s' not found, try importing it publicly?",
						sect.inherits
					);
				}

				auto inherited = backend.GetType(sect.inherits);
				type.structure = inherited.structure ~ type.structure;

				// create offsets
				size_t offset;
				foreach (ref member ; type.structure) {
					member.offset  = offset;
					offset        += member.Size();
				}

				type.size = offset;

				backend.SetType(format("%s.%s", sect.inMod, sect.name), type);
			}
		}

		backend.NewConst("true",  backend.MaxInt(), ErrorInfo.init);
		backend.NewConst("false", 0, ErrorInfo.init);

		Node[] header;
		Node[] main;

		foreach (ref node ; nodes) {
			switch (node.type) {
				case NodeType.FuncDef:
				case NodeType.Include:
				case NodeType.Let:
				case NodeType.Enable:
				case NodeType.Requires:
				case NodeType.Struct:
				case NodeType.Const:
				case NodeType.Enum:
				case NodeType.Union:
				case NodeType.Alias:
				case NodeType.Extern:
				case NodeType.Implement:
				case NodeType.Import:
				case NodeType.Module: {
					header ~= node;
					break;
				}
				default: main ~= node;
			}
		}

		foreach (ref node ; header) {
			CompileNode(node);
		}

		backend.BeginMain();

		foreach (ref node ; main) {
			CompileNode(node);
		}

		backend.End();
	}
}
