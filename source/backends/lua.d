module callisto.backends.lua;

import std.conv;
import std.file;
import std.path;
import std.stdio;
import std.range;
import std.format;
import std.algorithm;
import callisto.util;
import callisto.error;
import callisto.parser;
import callisto.compiler;
import callisto.language;

private enum WordType {
	Callisto,
	Lua
}

private struct Word {
	WordType type;
	bool     inline;
	Node[]   inlineNodes;
}

private struct StructEntry {
	Type   type;
	string name;
	bool   array;
	size_t size;
}

private struct Type {
	string        name;
	ulong         size;
	bool          isStruct;
	StructEntry[] structure;
	bool          hasInit;
	bool          hasDeinit;
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
	string name;
	Type   type;
	ulong  addr;
	bool   array;
	ulong  arraySize;

	size_t Size() => array? arraySize * type.size : type.size;
}

private struct Constant {
	Node value;
}

private struct Array {
	string[] values;
	Type     type;
	bool     global;
	ulong    metaAddr;
	ulong    elements;

	size_t Size() => type.size * values.length;
}

class BackendLua : CompilerBackend {
	Word[string]     words;
	Type[]           types;
	string           thisFunc;
	Constant[string] consts;
	bool             inScope;
	uint             blockCounter;
	bool             inWhile;
	uint             currentLoop;
	Variable[]       variables;
	Global[]         globals;
	bool             useLibc;
	ulong            globalStack = 524288;
	Array[]          arrays;
	ulong            arrayStack = 524287;

	this() {
		// built in integer types
		types ~= Type("addr",  1);
		types ~= Type("size",  1);
		types ~= Type("usize", 1);
		types ~= Type("cell",  1);
		types ~= Type("bool",  1);

		// built in structs
		types ~= Type("Array", 3, true, [
			StructEntry(GetType("usize"), "length"),
			StructEntry(GetType("usize"), "memberSize"),
			StructEntry(GetType("addr"),  "elements")
		]);
		NewConst("Array.length",     0);
		NewConst("Array.memberSize", 1);
		NewConst("Array.elements",   2);
		NewConst("Array.sizeof",     3);

		types ~= Type("Exception", 3 + 1, true, [
			StructEntry(GetType("bool"),  "error"),
			StructEntry(GetType("Array"), "msg")
		]);
		NewConst("Exception.bool",   0);
		NewConst("Exception.msg",    1);
		NewConst("Exception.sizeof", 3 + 1);

		foreach (ref type ; types) {
			NewConst(format("%s.sizeof", type.name), cast(long) type.size);
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

	bool GlobalExists(string name) => globals.any!(v => v.name == name);

	Global GetGlobal(string name) {
		foreach (ref var ; globals) {
			if (var.name == name) {
				return var;
			}
		}

		assert(0);
	}

	bool TypeExists(string name) => types.any!(v => v.name == name);

	Type GetType(string name) {
		foreach (ref type ; types) {
			if (type.name == name) {
				return type;
			}
		}

		assert(0);
	}

	void SetType(string name, Type ptype) {
		foreach (i, ref type ; types) {
			if (type.name == name) {
				types[i] = ptype;
				return;
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

	override string[] GetVersions() => ["Lua", "CallistoScript", "IO", "Time", "Exit"];

	override string[] FinalCommands() => [];

	override long MaxInt() => -1;

	override string DefaultHeader() => "";

	override bool HandleOption(string opt, ref string[] versions) => false;

	override void Init() {
		output ~= "mem = {};\n";
		output ~= "for i = 1, 1048576 do\n";
		output ~= "    table.insert(mem, 0);\n";
		output ~= "end\n";
		output ~= "dsp = 1;\n";
		output ~= "vsp = 1048576;\n";
		output ~= "gsp = 524288;\n";
		output ~= "regA = 0;\n";
		output ~= "regB = 0;\n";

		output ~= "
			function cal_pop()
				dsp = dsp - 1;
				return mem[dsp];
			end
		";
	}

	override void End() {
		// call destructors
		foreach (name, global ; globals) {
			output ~= format("mem[dsp] = %d\n", global.addr);
			output ~= "dsp = dsp + 1\n";
			output ~= format("type_deinit_%s()\n", global.type.name.Sanitise());
		}

		output ~= "end\n";

		// create arrays
		foreach (ref array ; arrays) {
			// create metadata
			output ~= format("mem[%d] = %d\n", array.metaAddr, array.values.length);
			output ~= format("mem[%d + 1] = %d\n", array.metaAddr, array.type.size);
			output ~= format("mem[%d + 2] = %d\n", array.metaAddr, array.elements);

			// create array contents
			output ~= "regA = {";

			foreach (i, ref value ; array.values) {
				output ~= value;

				if (i < array.values.length - 1) {
					output ~= ",";
				}
			}

			output ~= "}\n";
			output ~= format("
				for i = 1, %d do
					mem[%d + (i - 1)] = regA[i]
				end
			", array.values.length, array.elements);
		}

		output ~= "regA = 0\n";
		output ~= "calmain();\n";
	}

	override void BeginMain() {
		output ~= "function calmain()\n";
	}

	override void CompileWord(WordNode node) {
		if (node.name in words) {
			auto word = words[node.name];

			if (word.inline) {
				foreach (ref inode ; word.inlineNodes) {
					compiler.CompileNode(inode);
				}
			}
			else {
				output ~= format("func__%s();\n", node.name.Sanitise());
			}
		}
		else if (VariableExists(node.name)) {
			auto var = GetVariable(node.name);

			if (var.type.isStruct) {
				Error(node.error, "Can't push value of struct");
			}

			if (var.type.size != 1) {
				Error(node.error, "Bad variable type size");
			}

			output ~= format("mem[dsp] = mem[vsp + %d]\n", var.offset);
			output ~= "dsp = dsp + 1\n";
		}
		else if (GlobalExists(node.name)) {
			auto var = GetGlobal(node.name);

			if (var.type.isStruct) {
				Error(node.error, "Can't push value of struct");
			}

			if (var.type.size != 1) {
				Error(node.error, "Bad variable type size");
			}

			output ~= format("mem[dsp] = mem[%d]\n", var.addr);
			output ~= "dsp = dsp + 1\n";
		}
		else if (node.name in consts) {
			auto value  = consts[node.name].value;
			value.error = node.error;

			compiler.CompileNode(consts[node.name].value);
		}
		else {
			Error(node.error, "Unknown identifier '%s'", node.name);
		}
	}

	override void CompileInteger(IntegerNode node) {
		output ~= format("mem[dsp] = %d;\n", node.value);
		output ~= "dsp = dsp + 1;\n";
	}

	override void CompileFuncDef(FuncDefNode node) {
		if (node.name in words) {
			Error(node.error, "Function name '%s' already used", node.name);
		}
		if (Language.bannedNames.canFind(node.name)) {
			Error(node.error, "Name '%s' can't be used", node.name);
		}

		thisFunc = node.name;

		if (node.inline) {
			words[node.name] = Word(WordType.Callisto, true, node.nodes);
		}
		else {
			assert(!inScope);
			inScope = true;

			words[node.name] = Word(WordType.Callisto, false);

			output ~= format("function func__%s()\n", node.name.Sanitise());

			// allocate parameters
			size_t paramSize = node.params.length;
			foreach (ref type ; node.paramTypes) {
				if (!TypeExists(type)) {
					Error(node.error, "Type '%s' doesn't exist", type);
				}
				if (GetType(type).isStruct) {
					Error(node.error, "Structures cannot be used in function parameters");
				}
			}
			if (paramSize > 0) {
				output ~= format("vsp = vsp - %d\n", paramSize);
				foreach (ref var ; variables) {
					var.offset += paramSize;
				}

				size_t offset;
				foreach (i, ref type ; node.paramTypes) {
					auto     param = node.params[i];
					Variable var;

					var.name      = param;
					var.type      = GetType(type);
					var.offset    = cast(uint) offset;
					offset       += var.Size();
					variables    ~= var;
				}

				// copy data to parameters
				output ~= format("dsp = dsp - %d\n", paramSize);
				output ~= format("
					for i = 1, %d do
						mem[vsp + (i - 1)] = mem[dsp + (i - 1)]
					end
				", paramSize);
			}

			foreach (ref inode ; node.nodes) {
				compiler.CompileNode(inode);
			}

			size_t scopeSize;
			foreach (ref var ; variables) {
				scopeSize += var.Size();

				if (var.type.hasDeinit) {
					output ~= format("mem[dsp] = vsp + %d\n", var.offset);
					output ~= "dsp = dsp + 1\n";
					output ~= format("type_deinit_%s()\n", var.type.name.Sanitise());
				}
			}
			output ~= format("vsp = vsp + %d\n", scopeSize);
			output ~= "end\n";

			inScope   = false;
			variables = [];
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

			output ~= "if cal_pop() == 0 then\n";
			output ~= format("goto if_%d_%d\n", blockNum, condCounter + 1);
			output ~= "end\n";

			// create scope
			auto oldVars = variables.dup;
			auto oldSize = GetStackSize();

			foreach (ref inode ; node.doIf[i]) {
				compiler.CompileNode(inode);
			}

			// remove scope
			foreach (ref var ; variables) {
				if (oldVars.canFind(var)) continue;
				if (!var.type.hasDeinit)  continue;

				output ~= format("mem[dsp] = vsp + %d\n", var.offset);
				output ~= "dsp = dsp + 1\n";
				output ~= format("type_deinit_%s()\n", var.type.name.Sanitise());
			}
			if (GetStackSize() - oldSize > 0) {
				output ~= format("vsp = vsp + %d\n", GetStackSize() - oldSize);
			}
			variables = oldVars;

			output ~= format("goto if_%d_end\n", blockNum);

			++ condCounter;
			output ~= format("::if_%d_%d::\n", blockNum, condCounter);
		}

		if (node.hasElse) {
			// create scope
			auto oldVars = variables.dup;
			auto oldSize = GetStackSize();

			foreach (ref inode ; node.doElse) {
				compiler.CompileNode(inode);
			}

			// remove scope
			foreach (ref var ; variables) {
				if (oldVars.canFind(var)) continue;
				if (!var.type.hasDeinit)  continue;

				output ~= format("mem[dsp] = vsp + %d\n", var.offset);
				output ~= "dsp = dsp + 1\n";
				output ~= format("type_deinit_%s()\n", var.type.name.Sanitise());
			}
			if (GetStackSize() - oldSize > 0) {
				output ~= format("vsp = vsp + %d\n", GetStackSize() - oldSize);
			}
			variables = oldVars;
		}

		output ~= format("::if_%d_end::\n", blockNum);
	}

	override void CompileWhile(WhileNode node) {
		++ blockCounter;
		uint blockNum = blockCounter;
		currentLoop   = blockNum;

		output ~= format("goto while_%d_condition\n", blockNum);
		output ~= format("::while_%d::\n", blockNum);

		// make scope
		auto oldVars = variables.dup;
		auto oldSize = GetStackSize();

		foreach (ref inode ; node.doWhile) {
			inWhile = true;
			compiler.CompileNode(inode);

			currentLoop = blockNum;
		}

		// restore scope
		output ~= format("::while_%d_next::\n", blockNum);
		foreach (ref var ; variables) {
			if (oldVars.canFind(var)) continue;
			if (!var.type.hasDeinit)  continue;

			output ~= format("mem[dsp] = vsp + %d\n", var.offset);
			output ~= "dsp = dsp + 1\n";
			output ~= format("type_deinit_%s()\n", var.type.name.Sanitise());
		}
		if (GetStackSize() - oldSize > 0) {
			output ~= format("vsp = vsp + %d\n", GetStackSize() - oldSize);
		}
		variables = oldVars;

		inWhile  = false;
		output  ~= format("::while_%d_condition::\n", blockNum);

		foreach (ref inode ; node.condition) {
			compiler.CompileNode(inode);
		}

		output ~= "if cal_pop() ~= 0 then\n";
		output ~= format("goto while_%d;\n", blockNum);
		output ~= "end\n";
		output ~= format("::while_%d_end::\n", blockNum);
	}

	override void CompileLet(LetNode node) {
		if (!TypeExists(node.varType)) {
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
			var.type      = GetType(node.varType);
			var.offset    = 0;
			var.array     = node.array;
			var.arraySize = node.arraySize;

			foreach (ref ivar ; variables) {
				ivar.offset += var.Size();
			}

			variables ~= var;

			output ~= format("vsp = vsp - %d\n", var.Size());

			if (var.Size() == 1) {
				output ~= "mem[vsp] = 0\n";
			}

			if (var.type.hasInit) { // call constructor
				output ~= "mem[dsp] = vsp\n";
				output ~= "dsp = dsp + 1\n";
				output ~= format("type_init_%s()\n", var.type.name.Sanitise());
			}
		}
		else {
			Global global;
			global.name       = node.name;
			global.type       = GetType(node.varType);
			global.array      = node.array;
			global.arraySize  = node.arraySize;
			global.addr       = globalStack;
			globals          ~= global;

			globalStack += global.Size();

			if (global.type.hasInit) { // call constructor
				output ~= format("mem[dsp] = %d\n", global.addr);
				output ~= "dsp = dsp + 1\n";
				output ~= format("type_init_%s()\n", global.type.name.Sanitise());
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

		if (!TypeExists(node.arrayType)) {
			Error(node.error, "Type '%s' doesn't exist", node.arrayType);
		}

		array.type    = GetType(node.arrayType);
		array.global  = !inScope || node.constant;

		if (!inScope || node.constant) {
			arrayStack     -= array.Size();
			array.elements  = arrayStack;
			arrayStack     -= 3;
			array.metaAddr  = arrayStack;

			output ~= format("mem[dsp] = %d\n", array.metaAddr);
			output ~= "dsp = dsp + 1\n";
		}
		else {
			// allocate a copy of this array
			output ~= format("vsp = vsp - %d\n", array.Size());
			output ~= "regA = {";

			foreach (i, ref value ; array.values) {
				output ~= value;

				if (i < array.values.length - 1) {
					output ~= ",";
				}
			}

			output ~= "}\n";
			output ~= format("
				for i = 1, %d do
					mem[vsp + (i - 1)] = regA[i]
				end
			", array.values.length);

			output ~= "regA = 0\n";

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
			var.type   = GetType("Array");
			var.offset = 0;
			var.array  = false;

			foreach (ref var2 ; variables) {
				var2.offset += var.Size();
			}

			variables ~= var;

			output ~= "regA = vsp\n";
			output ~= "vsp = vsp - 3\n";
			output ~= format("mem[vsp] = %d\n", array.values.length);
			output ~= format("mem[vsp + 1] = %d\n", array.type.size);
			output ~= "mem[vsp + 2] = regA\n";

			// push metadata address
			output ~= "mem[dsp] = vsp\n";
			output ~= "dsp = dsp + 1\n";
		}

		arrays ~= array;
	}

	override void CompileString(StringNode node) {
		auto arrayNode = new ArrayNode(node.error);

		arrayNode.arrayType = "cell";
		arrayNode.constant  = node.constant;

		foreach (ref ch ; node.value) {
			arrayNode.elements ~= new IntegerNode(node.error, cast(long) ch);
		}

		CompileArray(arrayNode);
	}

	override void CompileStruct(StructNode node) {
		size_t offset;

		if (TypeExists(node.name)) {
			Error(node.error, "Type '%s' defined multiple times", node.name);
		}

		StructEntry[] entries;
		string[]      members;

		if (node.inherits) {
			if (!TypeExists(node.inheritsFrom)) {
				Error(node.error, "Type '%s' doesn't exist", node.inheritsFrom);
			}

			if (!GetType(node.inheritsFrom).isStruct) {
				Error(node.error, "Type '%s' is not a structure", node.inheritsFrom);
			}

			entries = GetType(node.inheritsFrom).structure;

			foreach (ref member ; GetType(node.inheritsFrom).structure) {
				members ~= member.name;
			}
		}

		foreach (ref member ; node.members) {
			if (!TypeExists(member.type)) {
				Error(node.error, "Type '%s' doesn't exist", member.type);
			}
			if (members.canFind(member.name)) {
				Error(node.error, "Duplicate member '%s'", member.name);
			}

			entries ~= StructEntry(
				GetType(member.type), member.name, member.array, member.size
			);
			members ~= member.name;
		}

		foreach (ref member ; entries) {
			NewConst(format("%s.%s", node.name, member.name), offset);
			offset += member.array? member.type.size * member.size : member.type.size;
		}

		NewConst(format("%s.sizeof", node.name), offset);
		types ~= Type(node.name, offset, true, entries);
	}

	override void CompileReturn(WordNode node) {
		if (!inScope) {
			Error(node.error, "Return used outside of function");
		}

		size_t scopeSize;
		foreach (ref var ; variables) {
			scopeSize += var.Size();

			if (var.type.hasDeinit) {
				output ~= format("mem[dsp] = vsp + %d\n", var.offset);
				output ~= "dsp = dsp + 1\n";
				output ~= format("type_deinit_%s()\n", var.type.name.Sanitise());
			}
		}
		output ~= format("vsp = vsp + %d\n", scopeSize);

		output ~= "do return end\n";
	}

	override void CompileConst(ConstNode node) {
		if (node.name in consts) {
			Error(node.error, "Constant '%s' already defined", node.name);
		}
		
		NewConst(node.name, node.value);
	}

	override void CompileEnum(EnumNode node) {
		if (!TypeExists(node.enumType)) {
			Error(node.error, "Enum base type '%s' doesn't exist", node.enumType);
		}
		if (TypeExists(node.name)) {
			Error(node.error, "Enum name is already used by type '%s'", node.enumType);
		}

		auto baseType  = GetType(node.enumType);
		baseType.name  = node.name;
		types         ~= baseType;

		foreach (i, ref name ; node.names) {
			NewConst(format("%s.%s", node.name, name), node.values[i]);
		}

		NewConst(format("%s.min", node.name), node.values.minElement());
		NewConst(format("%s.max", node.name), node.values.maxElement());
		NewConst(format("%s.sizeof", node.name), GetType(node.name).size);
	}

	override void CompileBreak(WordNode node) {
		if (!inWhile) {
			Error(node.error, "Not in while loop");
		}

		output ~= format("goto ::while_%d_end::\n", currentLoop);
	}

	override void CompileContinue(WordNode node) {
		if (!inWhile) {
			Error(node.error, "Not in while loop");
		}

		output ~= format("goto ::while_%d_next::\n", currentLoop);
	}

	override void CompileUnion(UnionNode node) {
		size_t maxSize = 0;

		if (TypeExists(node.name)) {
			Error(node.error, "Type '%s' already exists", node.name);
		}

		string[] unionTypes;

		foreach (ref type ; node.types) {
			if (unionTypes.canFind(type)) {
				Error(node.error, "Union type '%s' defined twice", type);
			}
			unionTypes ~= type;

			if (!TypeExists(type)) {
				Error(node.error, "Type '%s' doesn't exist", type);
			}

			if (GetType(type).size > maxSize) {
				maxSize = GetType(type).size;
			}
		}

		types ~= Type(node.name, maxSize);
		NewConst(format("%s.sizeof", node.name), cast(long) maxSize);
	}

	override void CompileAlias(AliasNode node) {
		if (!TypeExists(node.from)) {
			Error(node.error, "Type '%s' doesn't exist", node.from);
		}
		if ((TypeExists(node.to)) && !node.overwrite) {
			Error(node.error, "Type '%s' already defined", node.to);
		}

		auto baseType  = GetType(node.from);
		baseType.name  = node.to;
		types         ~= baseType;

		NewConst(format("%s.sizeof", node.to), cast(long) GetType(node.to).size);
	}

	override void CompileExtern(ExternNode node) {
		Word word;

		switch (node.externType) {
			case ExternType.Callisto: word.type = WordType.Callisto; break;
			default: {
				Error(node.error, "Can't use extern type %s", node.externType);
			}
		}

		words[node.func] = word;
	}

	override void CompileCall(WordNode node) {
		Error(node.error, "Call can't be used in CallistoScript");
	}

	override void CompileAddr(AddrNode node) {
		if (node.func in words) {
			Error(node.error, "Can't get addresses of functions in CallistoScript");
		}
		else if (GlobalExists(node.func)) {
			auto var = GetGlobal(node.func);

			output ~= format("mem[dsp] = %d\n", var.addr);
		}
		else if (VariableExists(node.func)) {
			auto var = GetVariable(node.func);

			output ~= format("mem[dsp] = vsp + %d\n", var.offset);
		}
		else {
			Error(node.error, "Undefined identifier '%s'", node.func);
		}

		output ~= "dsp = dsp + 1\n";
	}

	override void CompileImplement(ImplementNode node) {
		if (!TypeExists(node.structure)) {
			Error(node.error, "Type '%s' doesn't exist", node.structure);
		}
		auto type = GetType(node.structure);

		string labelName;

		switch (node.method) {
			case "init": {
				if (GetType(node.structure).hasInit) {
					Error(node.error, "Already implemented in type");
				}

				type.hasInit = true;
				labelName = format("type_init_%s", Sanitise(node.structure));
				break;
			}
			case "deinit": {
				if (GetType(node.structure).hasDeinit) {
					Error(node.error, "Already implemented in type");
				}

				type.hasDeinit = true;
				labelName = format("type_deinit_%s", Sanitise(node.structure));
				break;
			}
			default: Error(node.error, "Unknown method '%s'", node.method);
		}

		SetType(type.name, type);

		assert(!inScope);
		inScope = true;

		output ~= format("function %s()\n", labelName);

		foreach (ref inode ; node.nodes) {
			compiler.CompileNode(inode);
		}

		size_t scopeSize;
		foreach (ref var ; variables) {
			scopeSize += var.Size();

			if (var.type.hasDeinit) {
				output ~= format("lea rax, [rsp + %d\n]", var.offset);
				output ~= "mov [r15], rax\n";
				output ~= "add r15, 8\n";
				output ~= format("call __type_deinit_%s\n", Sanitise(var.type.name));
			}
		}
		output ~= format("vsp = vsp + %d\n", scopeSize);
		output ~= "end\n";

		inScope    = false;
		variables  = [];
	}

	override void CompileSet(SetNode node) {
		if (VariableExists(node.var)) {
			auto var = GetVariable(node.var);

			if (var.type.isStruct) {
				Error(node.error, "Can't set struct value");
			}

			output ~= format("mem[vsp + %d] = cal_pop()\n", var.offset);
		}
		else if (GlobalExists(node.var)) {
			auto global = GetGlobal(node.var);

			if (global.type.isStruct) {
				Error(node.error, "Can't set struct value");
			}

			output ~= format("mem[%d] = cal_pop()\n", global.addr);
		}
		else {
			Error(node.error, "Variable '%s' doesn't exist", node.var);
		}
	}
}
