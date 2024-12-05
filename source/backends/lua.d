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

private struct GlobalExtra {
	ulong addr;
}

private struct ArrayExtra {
	ulong metaAddr;
	ulong elements;
}

private enum WordType {
	Callisto,
	Lua
}

private struct Word {
	WordType   type;
	bool       inline;
	Node[]     inlineNodes;
	bool       error;
	UsedType[] params;
}

class BackendLua : CompilerBackend {
	Word[string] words;
	string       thisFunc;
	bool         inScope;
	uint         blockCounter;
	bool         inWhile;
	uint         currentLoop;
	bool         useLibc;
	ulong        globalStack = 524288;
	ulong        arrayStack = 524287;

	this() {
		addrSize = 1;

		// built in integer types
		types ~= Type("addr",  1);
		types ~= Type("size",  1);
		types ~= Type("usize", 1);
		types ~= Type("cell",  1);
		types ~= Type("bool",  1);

		// built in structs
		types ~= Type("Array", 3, true, [
			StructEntry(UsedType(GetType("usize"), false), "length"),
			StructEntry(UsedType(GetType("usize"), false), "memberSize"),
			StructEntry(UsedType(GetType("addr"), false),  "elements")
		]);
		NewConst("Array.length",     0);
		NewConst("Array.memberSize", 1);
		NewConst("Array.elements",   2);
		NewConst("Array.sizeof",     3);

		types ~= Type("Exception", 3 + 1, true, [
			StructEntry(UsedType(GetType("bool"), false),  "error"),
			StructEntry(UsedType(GetType("Array"), false), "msg")
		]);
		NewConst("Exception.bool",   0);
		NewConst("Exception.msg",    1);
		NewConst("Exception.sizeof", 3 + 1);

		foreach (ref type ; types) {
			NewConst(format("%s.sizeof", type.name), cast(long) type.size);
		}

		globals ~= Global(
			"_cal_exception", UsedType(GetType("Exception"), false), false, 0,
			new GlobalExtra(globalStack)
		);
		globalStack += globals[$ - 1].Size();
	}

	override void NewConst(string name, long value, ErrorInfo error = ErrorInfo.init) {
		consts[name] = Constant(new IntegerNode(error, value));
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
			if (!global.type.hasDeinit) continue;
			auto globalExtra = cast(GlobalExtra*) global.extra;

			output ~= format("mem[dsp] = %d\n", globalExtra.addr);
			output ~= "dsp = dsp + 1\n";
			output ~= format("type_deinit_%s()\n", global.type.name.Sanitise());
		}

		output ~= "end\n";

		// create arrays
		foreach (ref array ; arrays) {
			// create metadata
			auto arrayExtra = cast(ArrayExtra*) array.extra;

			output ~= format("mem[%d] = %d\n", arrayExtra.metaAddr, array.values.length);
			output ~= format("mem[%d + 1] = %d\n", arrayExtra.metaAddr, array.type.size);
			output ~= format("mem[%d + 2] = %d\n", arrayExtra.metaAddr, arrayExtra.elements);

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
			", array.values.length, arrayExtra.elements);
		}

		output ~= "regA = 0\n";
		output ~= "calmain();\n";
	}

	override void BeginMain() {
		output ~= "function calmain()\n";
	}

	void PushVariableValue(
		Node node, Variable var, size_t size = 0, size_t offset = 0,
		bool member = false, bool deref = false
	) {
		if (size == 0) {
			size = var.type.Size();
		}

		if (size != 1) {
			Error(node.error, "Bad variable type size (%d)", size);
		}

		if (deref) {
			output ~= format("mem[dsp] = mem[mem[vsp + %d] + %d]\n", var.offset, offset);
		}
		else {
			output ~= format("mem[dsp] = mem[vsp + %d]\n", var.offset + offset);
		}
		output ~= "dsp = dsp + 1\n";
	}

	void PushGlobalValue(
		Node node, Global var, size_t size = 0, size_t offset = 0, bool member = false,
		bool deref = false
	) {
		if (size == 0) {
			size = var.type.Size();
		}

		auto extra = cast(GlobalExtra*) var.extra;

		if (size != 1) {
			Error(node.error, "Bad variable type size (%d)", size);
		}

		if (deref) {
			output ~= format("mem[dsp] = mem[mem[%d] + %d]\n", extra.addr, offset);
		}
		else {
			output ~= format("mem[dsp] = mem[%d]\n", extra.addr + offset);
		}
		output ~= "dsp = dsp + 1\n";
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
				if (word.error && words[thisFunc].error) {
					output ~= "vsp = vsp - 1\n";
					output ~= format("mem[vsp] = dsp - %d\n", word.params.length);
				}

				output ~= format("func__%s();\n", node.name.Sanitise());

				if (word.error && words[thisFunc].error) {
					output ~= "dsp = mem[vsp]\n";
					output ~= "vsp = vsp + 1\n";
				}
			}

			if (word.error) {
				if ("__lua_exception" in words) {
					auto exception = GetGlobal("_cal_exception");
					auto extra     = cast(GlobalExtra*) exception.extra;

					output ~= format("if mem[%d] ~= 0 then\n", extra.addr);
					output ~= format("func__%s()\n", Sanitise("__lua_exception"));
					output ~= "end\n";
				}
				else {
					Warn(node.error, "No exception handler");
				}
			}
		}
		else if (VariableExists(node.name)) {
			auto var = GetVariable(node.name);

			if (var.type.isStruct && !var.type.ptr) {
				Error(node.error, "Can't push value of structures");
			}

			PushVariableValue(node, GetVariable(node.name));
		}
		else if (GlobalExists(node.name)) {
			auto var = GetGlobal(node.name);

			if (var.type.isStruct && !var.type.ptr) {
				Error(node.error, "Can't push value of structures");
			}

			PushGlobalValue(node, GetGlobal(node.name));
		}
		else if (IsStructMember(node.name)) {
			string name    = node.name[0 .. node.name.countUntil(".")];
			auto structVar = GetStructVariable(node, node.name);

			if (GlobalExists(name)) {
				auto var = GetGlobal(name);

				PushGlobalValue(
					node, var, structVar.size, structVar.offset, true, var.type.ptr
				);
			}
			else if (VariableExists(name)) {
				auto var = GetVariable(name);

				PushVariableValue(
					node, var, structVar.size, structVar.offset, true, var.type.ptr
				);
			}
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

		UsedType[] params;

		foreach (ref type ; node.paramTypes) {
			if (!TypeExists(type.name)) {
				Error(node.error, "Type '%s' doesn't exist", type.name);
			}

			params ~= UsedType(GetType(type.name), type.ptr);
		}

		if (node.inline) {
			auto globalExtra = cast(GlobalExtra*) GetGlobal("_cal_exception").extra;
			output ~= format("mem[%d] = 0\n", globalExtra.addr);

			words[node.name] = Word(
				WordType.Callisto, true, node.nodes, node.errors, params
			);
		}
		else {
			assert(!inScope);
			inScope = true;

			words[node.name] = Word(WordType.Callisto, false, [], node.errors, params);

			output ~= format("function func__%s()\n", node.name.Sanitise());

			auto exceptionExtra = cast(GlobalExtra*) GetGlobal("_cal_exception").extra;
			output ~= format("mem[%d] = 0\n", exceptionExtra.addr);

			// allocate parameters
			size_t paramSize = node.params.length;

			foreach (ref type ; node.paramTypes) {
				if (!TypeExists(type.name)) {
					Error(node.error, "Type '%s' doesn't exist", type.name);
				}
				if (GetType(type.name).isStruct && !type.ptr) {
					Error(node.error, "Structures cannot be used in function parameters");
				}
			}

			if ((paramSize > 0) && !node.manual) {
				output ~= format("vsp = vsp - %d\n", paramSize);
				foreach (ref var ; variables) {
					var.offset += paramSize;
				}

				size_t offset;
				foreach (i, ref type ; node.paramTypes) {
					auto     param = node.params[i];
					Variable var;

					var.name      = param;
					var.type.type = GetType(type.name);
					var.type.ptr  = type.ptr;
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
		if (!TypeExists(node.varType.name)) {
			Error(node.error, "Undefined type '%s'", node.varType.name);
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
			var.type.type = GetType(node.varType.name);
			var.type.ptr  = node.varType.ptr;
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
			if (GlobalExists(node.name)) {
				Error(node.error, "Global '%s' already exists", node.name);
			}

			GlobalExtra* extra = new GlobalExtra();
			extra.addr         = globalStack;

			Global global;
			global.name       = node.name;
			global.type.type  = GetType(node.varType.name);
			global.type.ptr   = node.varType.ptr;
			global.array      = node.array;
			global.arraySize  = node.arraySize;
			global.extra      = extra;
			globals          ~= global;

			globalStack += global.Size();

			if (global.type.hasInit) { // call constructor
				output ~= format("mem[dsp] = %d\n", extra.addr);
				output ~= "dsp = dsp + 1\n";
				output ~= format("type_init_%s()\n", global.type.name.Sanitise());
			}
		}
	}

	override void CompileArray(ArrayNode node) {
		Array       array;
		ArrayExtra* extra = new ArrayExtra();
		array.extra       = extra;

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

		if (!TypeExists(node.arrayType.name)) {
			Error(node.error, "Type '%s' doesn't exist", node.arrayType.name);
		}

		array.type.type = GetType(node.arrayType.name);
		array.type.ptr  = node.arrayType.ptr;
		array.global    = !inScope || node.constant;

		if (!inScope || node.constant) {
			arrayStack     -= array.Size();
			extra.elements  = arrayStack;
			arrayStack     -= 3;
			extra.metaAddr  = arrayStack;

			output ~= format("mem[dsp] = %d\n", extra.metaAddr);
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
			var.type   = UsedType(GetType("Array"), false);
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

		arrayNode.arrayType = new TypeNode(node.error, "cell", false);
		arrayNode.constant  = node.constant;

		foreach (ref ch ; node.value) {
			arrayNode.elements ~= new IntegerNode(node.error, cast(long) ch);
		}

		CompileArray(arrayNode);
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
			auto var   = GetGlobal(node.func);
			auto extra = cast(GlobalExtra*) var.extra;

			output ~= format("mem[dsp] = %d\n", extra.addr);
		}
		else if (VariableExists(node.func)) {
			auto var = GetVariable(node.func);

			output ~= format("mem[dsp] = vsp + %d\n", var.offset);
		}
		else if (IsStructMember(node.func)) {
			string name      = node.func[0 .. node.func.countUntil(".")];
			auto   structVar = GetStructVariable(node, node.func);
			size_t offset    = structVar.offset;

			if (GlobalExists(name)) {
				auto var   = GetGlobal(name);
				auto extra = cast(GlobalExtra*) var.extra;

				output ~= format("mem[dsp] = %d\n", extra.addr + offset);
				output ~= "dsp = dsp + 1\n";
			}
			else if (VariableExists(node.func)) {
				auto var = GetVariable(name);

				output ~= format("mem[dsp] = vsp + %d\n", var.offset + offset);
				output ~= "dsp = dsp + 1";
			}
			else assert(0);
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
				output ~= format("mem[dsp] = vsp + %d\n", var.offset);
				output ~= "dsp = dsp + 1\n";
				output ~= format("type_deinit_%s()\n", var.type.name.Sanitise());
			}
		}
		output ~= format("vsp = vsp + %d\n", scopeSize);
		output ~= "end\n";

		inScope    = false;
		variables  = [];
	}

	void SetGlobal(
		Node node, Global global, size_t size = 0, size_t offset = 0,
		bool member = false, bool deref = false
	) {
		if (size == 0) {
			size = global.type.Size();
		}

		if (size != 1) {
			Error(node.error, "Bad variable type size");
		}

		auto extra = cast(GlobalExtra*) global.extra;

		if (deref) {
			output ~= format("mem[mem[%d] + %d] = cal_pop()\n", extra.addr, offset);
		}
		else {
			output ~= format("mem[%d] = cal_pop()\n", extra.addr + offset);
		}
	}

	void SetVariable(
		Node node, Variable var, size_t size = 0, size_t offset = 0, bool member = false,
		bool deref = false
	) {
		if (size == 0) {
			size = var.type.Size();
		}

		if (size != 1) {
			Error(node.error, "Bad variable type size");
		}

		if (deref) {
			output ~= format("mem[mem[vsp + %d] + %d] = cal_pop()\n", var.offset, offset);
		}
		else {
			output ~= format("mem[vsp + %d] = cal_pop()\n", var.offset + offset);
		}
	}

	override void CompileSet(SetNode node) {
		if (VariableExists(node.var)) {
			auto var = GetVariable(node.var);

			if (var.type.isStruct && !var.type.ptr) {
				Error(node.error, "Can't set struct value");
			}

			SetVariable(node, var);
		}
		else if (GlobalExists(node.var)) {
			auto var = GetGlobal(node.var);

			if (var.type.isStruct && !var.type.ptr) {
				Error(node.error, "Can't set struct value");
			}

			SetGlobal(node, GetGlobal(node.var));
		}
		else if (IsStructMember(node.var)) {
			string name    = node.var[0 .. node.var.countUntil(".")];
			auto structVar = GetStructVariable(node, node.var);

			if (VariableExists(name)) {
				auto var = GetVariable(name);

				SetVariable(
					node, var, structVar.size, structVar.offset, true, var.type.ptr
				);
			}
			else if (GlobalExists(name)) {
				auto var = GetGlobal(name);

				SetGlobal(
					node, var, structVar.size, structVar.offset, true, var.type.ptr
				);
			}
		}
		else {
			Error(node.error, "Variable '%s' doesn't exist", node.var);
		}
	}

	override void CompileTryCatch(TryCatchNode node) {
		if (node.func !in words) {
			Error(node.error, "Function '%s' doesn't exist", node.func);
		}

		auto word = words[node.func];

		if (!word.error) {
			Error(node.error, "Function '%s' doesn't throw", node.func);
		}
		if (word.type != WordType.Callisto) {
			Error(node.error, "Non-callisto functions can't throw");
		}

		output ~= "vsp = vsp - 1\n";
		output ~= format("mem[vsp] = dsp - %d\n", word.params.length);

		if (word.inline) {
			foreach (inode ; word.inlineNodes) {
				compiler.CompileNode(inode);
			}
		}
		else {
			output ~= format("func__%s()\n", node.func.Sanitise());
		}

		output ~= "regA = mem[vsp]\n";
		output ~= "vsp = vsp + 1\n";

		++ blockCounter;

		auto global      = GetGlobal("_cal_exception");
		auto globalExtra = cast(GlobalExtra*) global.extra;

		output ~= format("if mem[%d] == 0 then\n", globalExtra.addr);
		output ~= format("goto catch_%d_end\n", blockCounter);
		output ~= "end\n";

		// function errored, assume that all it did was consume parameters
		output ~= "dsp = regA\n";

		// create scope
		auto oldVars = variables.dup;
		auto oldSize = GetStackSize();

		foreach (inode ; node.catchBlock) {
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

		output ~= format("::catch_%d_end::\n", blockCounter);
	}

	override void CompileThrow(WordNode node) {
		if (!inScope || (!words[thisFunc].error)) {
			Error(node.error, "Not in a function that can throw");
		}
		if (words[thisFunc].inline) {
			Error(node.error, "Can't use throw in an inline function");
		}

		auto global = GetGlobal("_cal_exception");
		auto extra  = cast(GlobalExtra*) global.extra;

		// set exception error
		output ~= format("mem[%d] = -1\n", extra.addr);

		// copy exception message
		output ~= "dsp = dsp - 1\n";
		output ~= "for i = 1, 3 do\n";
		output ~= format(
			"mem[%d + i] = mem[mem[dsp] + (i - 1)]\n", extra.addr
		);
		output ~= "end\n";

		CompileReturn(node);
	}
}
