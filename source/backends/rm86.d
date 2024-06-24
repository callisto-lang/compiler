module callisto.backends.rm86;

import std.conv;
import std.stdio;
import std.range;
import std.format;
import std.algorithm;
import callisto.util;
import callisto.error;
import callisto.parser;
import callisto.compiler;
import callisto.language;

private struct Word {
	bool   raw;
	bool   inline;
	Node[] inlineNodes;
}

private struct StructEntry {
	Type*  type;
	string name;
	bool   array;
	size_t size;
}

private struct Type {
	ulong         size;
	bool          isStruct;
	StructEntry[] structure;
}

private struct Variable {
	string name;
	Type   type;
	uint   offset; // SP + offset to access
	bool   array;
	ulong  arraySize;

	size_t Size() => array? arraySize * type.size : type.size;
}

private struct Global {
	Type  type;
	bool  array;
	ulong arraySize;

	size_t Size() => array? arraySize * type.size : type.size;
}

private struct Constant {
	Node value;
}

private struct Array {
	string[] values;
	Type     type;
	bool     global;

	size_t Size() => type.size * values.length;
}

private struct RM86Opts {
	bool noDos;
}

class BackendRM86 : CompilerBackend {
	Word[string]     words;
	uint             blockCounter; // used for block statements
	Type[string]     types;
	Variable[]       variables;
	Global[string]   globals;
	Constant[string] consts;
	bool             inScope;
	Array[]          arrays;
	string           thisFunc;
	bool             inWhile;
	uint             currentLoop;
	RM86Opts         opts;

	this() {
		types["u8"]    = Type(1);
		types["i8"]    = Type(1);
		types["u16"]   = Type(2);
		types["i16"]   = Type(2);
		types["addr"]  = Type(2);
		types["size"]  = Type(2);
		types["usize"] = Type(2);
		types["cell"]  = Type(2);

		// built in structs
		types["Array"] = Type(6, true, [
			StructEntry("usize" in types, "length"),
			StructEntry("usize" in types, "memberSize"),
			StructEntry("addr" in types,  "elements")
		]);
		NewConst("Array.length",     0);
		NewConst("Array.memberSize", 2);
		NewConst("Array.elements",   4);
		NewConst("Array.sizeof",     2 * 3);

		foreach (name, ref type ; types) {
			NewConst(format("%s.sizeof", name), cast(long) type.size);
		}

		if (!opts.noDos) {
			globals["__rm86_argv"] = Global(types["addr"], false, 0);
			globals["__rm86_arglen"] = Global(types["cell"], false, 0);
		}
	}

	override void NewConst(string name, long value, ErrorInfo error = ErrorInfo.init) {
		consts[name] = Constant(new IntegerNode(error, value));
	}

	bool VariableExists(string name) => variables.any!(v => v.name == name);

	Variable GetVariable(string name) {
		foreach (ref var ; variables) {
			if (var.name == name) {
				return var;
			}
		}

		assert(0);
	}

	size_t GetStackSize() {
		// old
		//return variables.empty()? 0 : variables[0].offset + variables[0].type.size;

		size_t size;
		foreach (ref var ; variables) {
			size += var.Size();
		}

		return size;
	}

	override string[] GetVersions() => [
		// platform
		"RM86", "LittleEndian", "16Bit",
		// features
		"IO", "Args"
	];

	override string[] FinalCommands() => [
		format("mv %s %s.asm", compiler.outFile, compiler.outFile),
		format("nasm -f bin %s.asm -o %s", compiler.outFile, compiler.outFile),
		format("rm %s.asm", compiler.outFile)
	];

	override long MaxInt() => 0xFFFF;

	override string DefaultHeader() => "";

	override bool HandleOption(string opt, ref string[] versions) {
		switch (opt) {
			case "no-dos": {
				opts.noDos = true;
				return true;
			}
			default: return false;
		}
	}

	override void BeginMain() {
		output ~= "__calmain:\n";
	}

	override void Init() {
		output ~= format("org 0x%.4X\n", org);

		if (!opts.noDos) {
			// get argv and argc
			output ~= "xor ah, ah\n";
			output ~= "mov al, [0x80]\n";
			output ~= format("mov [__global_%s], ax\n", Sanitise("__rm86_arglen"));
			output ~= "mov ax, 0x81\n";
			output ~= format("mov [__global_%s], ax\n", Sanitise("__rm86_argv"));
		}
		
		output ~= "mov si, __stack\n";
		output ~= "jmp __calmain\n";
	}

	override void End() {
		output ~= "ret\n";

		foreach (name, var ; globals) {
			output ~= format("__global_%s: times %d db 0\n", name.Sanitise(), var.Size());
		}

		foreach (i, ref array ; arrays) {
			output ~= format("__array_%d: ", i);

			switch (array.type.size) {
				case 1:  output ~= "db "; break;
				case 2:  output ~= "dw "; break;
				default: assert(0);
			}

			foreach (j, ref element ; array.values) {
				output ~= element ~ (j == array.values.length - 1? "" : ", ");
			}

			output ~= '\n';

			if (array.global) {
				output ~= format(
					"__array_%d_meta: dw %d, %d, __array_%d\n", i,
					array.values.length,
					array.type.size,
					i
				);
			}
		}

		output ~= "__stack: times 512 dw 0\n";
	}

	override void CompileWord(WordNode node) {
		if (node.name in words) {
			auto word = words[node.name];

			if (word.inline) {
				foreach (inode ; word.inlineNodes) {
					compiler.CompileNode(inode);
				}
			}
			else {
				if (word.raw) {
					output ~= format("call %s\n", node.name);
				}
				else {
					output ~= format("call __func__%s\n", node.name.Sanitise());
				}
			}
		}
		else if (VariableExists(node.name)) {
			auto var = GetVariable(node.name);

			output ~= "mov di, sp\n";
			if (var.offset > 0) {
				output ~= format("add di, %d\n", var.offset);
			}
			output ~= "mov [si], di\n";
			output ~= "add si, 2\n";
		}
		else if (node.name in globals) {
			auto var = globals[node.name];

			output ~= format("mov word [si], __global_%s\n", node.name.Sanitise());
			output ~= "add si, 2\n";
		}
		else if (node.name in consts) {
			compiler.CompileNode(consts[node.name].value);
		}
		else {
			Error(node.error, "Undefined identifier '%s'", node.name);
		}
	}

	override void CompileInteger(IntegerNode node) {
		output ~= format("mov word [si], %d\n", node.value);
		output ~= "add si, 2\n";
	}

	override void CompileFuncDef(FuncDefNode node) {
		if ((node.name in words) || VariableExists(node.name)) {
			Error(node.error, "Function name '%s' already used", node.name);
		}
		if (Language.bannedNames.canFind(node.name)) {
			Error(node.error, "Name '%s' can't be used", node.name);
		}

		thisFunc = node.name;

		if (node.inline) {
			words[node.name] = Word(false, true, node.nodes);
		}
		else {
			assert(!inScope);
			inScope = true;

			words[node.name] = Word(node.raw, false, []);

			string symbol =
				node.raw? node.name : format("__func__%s", node.name.Sanitise());

			output ~= format("%s:\n", symbol);

			foreach (ref inode ; node.nodes) {
				compiler.CompileNode(inode);
			}

			//output ~= format("__func_return__%s:\n", node.name.Sanitise());

			size_t scopeSize;
			foreach (ref var ; variables) {
				scopeSize += var.Size();
			}
			if (scopeSize == 1) {
				output ~= "inc sp\n";
			}
			else {
				output ~= format("add sp, %d\n", scopeSize);
			}

			output    ~= "ret\n";
			//output    ~= format("__func_end__%s:\n", node.name.Sanitise());
			variables  = [];
			inScope    = false;
		}
	}

	override void CompileIf(IfNode node) {
		++ blockCounter;
		auto blockNum = blockCounter;
		uint condCounter;

		foreach (i, ref condition ; node.condition) {
			foreach (ref inode ; condition) {
				compiler.CompileNode(inode);
			}
			output ~= "sub si, 2\n";
			output ~= "mov ax, [si]\n";
			output ~= "cmp ax, 0\n";
			output ~= format("je __if_%d_%d\n", blockNum, condCounter + 1);

			// create scope
			auto oldVars = variables.dup;
			auto oldSize = GetStackSize();

			foreach (ref inode ; node.doIf[i]) {
				compiler.CompileNode(inode);
			}

			// remove scope
			if (GetStackSize() - oldSize > 0) {
				output ~= format("add sp, %d\n", GetStackSize() - oldSize);
			}
			variables = oldVars;

			output ~= format("jmp __if_%d_end\n", blockNum);

			++ condCounter;
			output ~= format("__if_%d_%d:\n", blockNum, condCounter);
		}

		if (node.hasElse) {
			// create scope
			auto oldVars = variables.dup;
			auto oldSize = GetStackSize();

			foreach (ref inode ; node.doElse) {
				compiler.CompileNode(inode);
			}

			// remove scope
			if (GetStackSize() - oldSize > 0) {
				output ~= format("add sp, %d\n", GetStackSize() - oldSize);
			}
			variables = oldVars;
		}

		output ~= format("__if_%d_end:\n", blockNum);
	}

	override void CompileWhile(WhileNode node) {
		++ blockCounter;
		uint blockNum = blockCounter;
		currentLoop   = blockNum;

		output ~= format("jmp __while_%d_condition\n", blockNum);
		output ~= format("__while_%d:\n", blockNum);

		// make scope
		auto oldVars = variables.dup;
		auto oldSize = GetStackSize();
		
		foreach (ref inode ; node.doWhile) {
			inWhile = true;
			compiler.CompileNode(inode);

			currentLoop = blockNum;
		}

		// restore scope
		if (GetStackSize() - oldSize > 0) {
			output ~= format("add sp, %d\n", GetStackSize() - oldSize);
		}
		variables = oldVars;

		inWhile = false;

		output ~= format("__while_%d_condition:\n", blockNum);
		
		foreach (ref inode ; node.condition) {
			compiler.CompileNode(inode);
		}

		output ~= "sub si, 2\n";
		output ~= "mov ax, [si]\n";
		output ~= "cmp ax, 0\n";
		output ~= format("jne __while_%d\n", blockNum);
		output ~= format("__while_%d_end:\n", blockNum);
	}

	override void CompileLet(LetNode node) {
		if (node.varType !in types) {
			Error(node.error, "Undefined type '%s'", node.varType);
		}
		if (VariableExists(node.name) || (node.name in words)) {
			Error(node.error, "Variable name '%s' already used", node.name);
		}
		if (Language.bannedNames.canFind(node.name)) {
			Error(node.error, "Name '%s' can't be used", node.name);
		}

		if (inScope) {
			Variable var;
			var.name      = node.name;
			var.type      = types[node.varType];
			var.offset    = 0;
			var.array     = node.array;
			var.arraySize = node.arraySize;

			foreach (ref ivar ; variables) {
				ivar.offset += var.Size();
			}

			variables ~= var;

			if (var.Size() == 2) {
				output ~= "push 0\n";
			}
			else {
				output ~= format("sub sp, %d\n", var.Size());
			}
		}
		else {
			Global global;
			global.type        = types[node.varType];
			globals[node.name] = global;

			if (!orgSet) {
				Warn(node.error, "Declaring global variables without a set org value");
			}
		}
	} 

	override void CompileArray(ArrayNode node) {
		Array array;

		foreach (ref elem ; node.elements) {
			switch (elem.type) {
				case NodeType.Integer: {
					auto node2    = cast(IntegerNode) elem;
					array.values ~= node2.value.text();
					break;
				}
				default: {
					Error(elem.error, "Type '%s' can't be used in array literal");
				}
			}
		}

		if (node.arrayType !in types) {
			Error(node.error, "Type '%s' doesn't exist", node.arrayType);
		}

		array.type    = types[node.arrayType];
		array.global  = !inScope || node.constant;
		arrays       ~= array;

		if (!inScope || node.constant) {
			if (!orgSet) {
				Warn(node.error, "Using array literals without a set org value");
			}

			output ~= format("mov word [si], __array_%d_meta\n", arrays.length - 1);
			output ~= "add si, 2\n";
		}
		else {
			// allocate a copy of the array
			output ~= "mov ax, ds\n";
			output ~= "mov es, ax\n";
			output ~= format("sub sp, %d\n", array.Size());
			output ~= "mov ax, sp\n";
			output ~= "push si\n";
			output ~= format("mov si, __array_%d\n", arrays.length - 1);
			output ~= "mov di, ax\n";
			output ~= format("mov cx, %d\n", array.Size());
			output ~= "rep movsb\n";
			output ~= "pop si\n";

			Variable var;
			var.type      = array.type;
			var.offset    = 0;
			var.array     = true;
			var.arraySize = array.values.length;

			foreach (ref var2 ; variables) {
				var2.offset += var.Size();
			}

			variables ~= var;

			// create metadata variable
			var.type   = types["Array"];
			var.offset = 0;
			var.array  = false;

			foreach (ref var2 ; variables) {
				var2.offset += var.Size();
			}

			variables ~= var;

			output ~= "mov ax, sp\n";
			output ~= format("sub sp, %d\n", 2 * 3); // size of Array structure
			output ~= "mov bx, sp\n";
			output ~= format("mov word [bx], %d\n", array.values.length); // length
			output ~= format("mov word [bx + 2], %d\n", array.type.size); // member size
			output ~= "mov [bx + 4], ax\n"; // elements

			// push metadata address
			output ~= "mov [si], sp\n";
			output ~= "add si, 2\n";
		}
	}

	override void CompileString(StringNode node) {
		auto arrayNode = new ArrayNode(node.error);

		arrayNode.arrayType = "u8";
		arrayNode.constant  = node.constant;

		foreach (ref ch ; node.value) {
			arrayNode.elements ~= new IntegerNode(node.error, cast(long) ch);
		}

		CompileArray(arrayNode);
	}

	override void CompileStruct(StructNode node) {
		size_t offset;

		if (node.name in types) {
			Error(node.error, "Type '%s' defined multiple times", node.name);
		}

		StructEntry[] entries;
		string[]      members;

		if (node.inherits) {
			if (node.inheritsFrom !in types) {
				Error(node.error, "Type '%s' doesn't exist", node.inheritsFrom);
			}

			if (!types[node.inheritsFrom].isStruct) {
				Error(node.error, "Type '%s' is not a structure", node.inheritsFrom);
			}

			entries = types[node.inheritsFrom].structure;

			foreach (ref member ; types[node.inheritsFrom].structure) {
				members ~= member.name;
			}
		}

		foreach (ref member ; node.members) {
			if (member.type !in types) {
				Error(node.error, "Type '%s' doesn't exist", member.type);
			}
			if (members.canFind(member.name)) {
				Error(node.error, "Duplicate member '%s'", member.name);
			}

			entries ~= StructEntry(
				member.type in types, member.name, member.array, member.size
			);
			members ~= member.name;
		}

		foreach (ref member ; entries) {
			NewConst(format("%s.%s", node.name, member.name), offset);
			offset += member.array? member.type.size * member.size : member.type.size;
		}

		NewConst(format("%s.sizeof", node.name), offset);
		types[node.name] = Type(offset, true, entries);
	}

	override void CompileReturn(WordNode node) {
		if (!inScope) {
			Error(node.error, "Return used outside of function");
		}

		size_t scopeSize;
		foreach (ref var ; variables) {
			scopeSize += var.Size();
		}
		if (scopeSize == 1) {
			output ~= "inc sp\n";
		}
		else {
			output ~= format("add sp, %d\n", scopeSize);
		}

		output ~= "ret\n";
	}

	override void CompileConst(ConstNode node) {
		if (node.name in consts) {
			Error(node.error, "Constant '%s' already defined", node.name);
		}
		
		NewConst(node.name, node.value);
	}

	override void CompileEnum(EnumNode node) {
		if (node.enumType !in types) {
			Error(node.error, "Enum base type '%s' doesn't exist", node.enumType);
		}
		if (node.name in types) {
			Error(node.error, "Enum name is already used by type '%s'", node.enumType);
		}

		types[node.name] = types[node.enumType];

		foreach (i, ref name ; node.names) {
			NewConst(format("%s.%s", node.name, name), node.values[i]);
		}

		NewConst(format("%s.min", node.name), node.values.minElement());
		NewConst(format("%s.max", node.name), node.values.maxElement());
		NewConst(format("%s.sizeof", node.name), types[node.name].size);
	}

	override void CompileBreak(WordNode node) {
		if (!inWhile) {
			Error(node.error, "Not in while loop");
		}

		output ~= format("jmp __while_%d_end\n", currentLoop);
	}

	override void CompileContinue(WordNode node) {
		if (!inWhile) {
			Error(node.error, "Not in while loop");
		}

		output ~= format("jmp __while_%d_condition\n", currentLoop);
	}

	override void CompileUnion(UnionNode node) {
		size_t maxSize = 0;

		if (node.name in types) {
			Error(node.error, "Type '%s' already exists", node.name);
		}

		string[] unionTypes;

		foreach (ref type ; node.types) {
			if (unionTypes.canFind(type)) {
				Error(node.error, "Union type '%s' defined twice", type);
			}
			unionTypes ~= type;

			if (type !in types) {
				Error(node.error, "Type '%s' doesn't exist", type);
			}

			if (types[type].size > maxSize) {
				maxSize = types[type].size;
			}
		}

		types[node.name] = Type(maxSize);
		NewConst(format("%s.sizeof", node.name), cast(long) maxSize);
	}

	override void CompileAlias(AliasNode node) {
		if (node.from !in types) {
			Error(node.error, "Type '%s' doesn't exist", node.from);
		}
		if ((node.to in types) && !node.overwrite) {
			Error(node.error, "Type '%s' already defined", node.to);
		}

		types[node.to] = types[node.from];
		NewConst(format("%s.sizeof", node.to), cast(long) types[node.to].size);
	}

	override void CompileExtern(ExternNode node) {
		Word word;
		word.raw         = node.externType == ExternType.Raw;
		words[node.func] = word;
	}

	override void CompileCall(WordNode node) {
		output ~= "sub si, 2\n";
		output ~= "mov ax, [si]\n";
		output ~= "call ax\n";
	}

	override void CompileFuncAddr(FuncAddrNode node) {
		if (node.func !in words) {
			Error(node.error, "Function '%s' doesn't exist");
		}

		auto   word   = words[node.func];
		string symbol =
			word.raw? node.func : format("__func__%s", node.func.Sanitise());

		output ~= format("mov ax, %s\n", symbol);
		output ~= "mov [si], ax\n";
		output ~= "add si, 2\n";
	}
}
