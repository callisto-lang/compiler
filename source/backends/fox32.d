module callisto.backends.fox32;

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

private enum WordType {
	Callisto,
	Raw
}

private struct Word {
	WordType   type;
	bool       inline;
	Node[]     inlineNodes;
	bool       error;
	UsedType[] params;
}

class BackendFox32 : CompilerBackend {
	Word[string] words;
	uint         blockCounter; // used for block statements
	bool         inScope;
	string       thisFunc;
	bool         makeImage;
	uint         currentLoop;
	bool         inWhile;

	this() {
		addrSize = 8;

		version (linux) {
			defaultOS = "linux";
		}
		else version (OSX) {
			defaultOS = "osx";
		}
		else version (FreeBSD) {
			defaultOS = "freebsd";
		}
		else {
			defaultOS = "bare-metal";
			WarnNoInfo("Default operating system, defaulting to bare-metal OS");
		}

		// built in integer types
		types ~= Type("u8",    1);
		types ~= Type("i8",    1);
		types ~= Type("u16",   2);
		types ~= Type("i16",   2);
		types ~= Type("u32",   4);
		types ~= Type("i32",   4);
		types ~= Type("addr",  4);
		types ~= Type("size",  4);
		types ~= Type("usize", 4);
		types ~= Type("cell",  4);
		types ~= Type("bool",  4);

		// built in structs
		types ~= Type("Array", 12, true, [
			StructEntry(UsedType(GetType("usize"), false), "length", false, 4, 0),
			StructEntry(UsedType(GetType("usize"), false), "memberSize", false, 4, 4),
			StructEntry(UsedType(GetType("addr"), false), "elements", false, 4, 8)
		]);
		NewConst("Array.length",     0);
		NewConst("Array.memberSize", 4);
		NewConst("Array.elements",   8);
		NewConst("Array.sizeOf",     4 * 3);

		types ~= Type("Exception", 12 + 8, true, [
			StructEntry(UsedType(GetType("bool"), false),  "error", false, 4, 0),
			StructEntry(UsedType(GetType("Array"), false), "msg", false, 12, 4)
		]);
		NewConst("Exception.error",  0);
		NewConst("Exception.msg",    4);
		NewConst("Exception.sizeOf", 12 + 8);

		foreach (ref type ; types) {
			NewConst(format("%s.sizeOf", type.name), cast(long) type.size);
		}

		globals ~= Global(
			"_cal_exception", UsedType(GetType("Exception"), false), false, 0
		);
	}

	override string[] GetVersions() => [
		// CPU features
		"Fox32", "LittleEndian", "16Bit", "32Bit",

		// OS features
		"Fox32OS", "IO", "Exit"
	];

	override string[] FinalCommands() {
		string[] ret = [
			format("mv %s %s.asm", compiler.outFile, compiler.outFile),
			format("fox32asm %s.asm %s", compiler.outFile, compiler.outFile)
		];

		if (!keepAssembly) {
			ret ~= format("rm %s.asm", compiler.outFile);
		}

		if (makeImage) {
			ret ~= format("ryfs.py -s 16777216 -l cal create %s.img", compiler.outFile);
			ret ~= format("ryfs.py add %s.img %s", compiler.outFile, compiler.outFile);
		}

		return ret;
	}

	override long MaxInt() => 0xFFFFFFFF;

	override void NewConst(string name, long value, ErrorInfo error = ErrorInfo.init) {
		consts[name] = Constant(new IntegerNode(error, value));
	}

	override string DefaultHeader() => "";

	override bool HandleOption(string opt, ref string[] versions) {
		switch (opt) {
			case "make-img": {
				makeImage = true;
				return true;
			}
			default: return false;
		}
	}

	override void BeginMain() {
		output ~= "__calmain:\n";
	}

	void CallFunction(string name) {
		auto word = words[name];

		if (word.inline) {
			foreach (inode ; word.inlineNodes) {
				compiler.CompileNode(inode);
			}
		}
		else if (word.type == WordType.Raw) {
			output ~= format("call %s\n", name);
		}
		else {
			output ~= format("call __func__%s\n", name.Sanitise());
		}
	}

	override void Init() {
		output ~= "call __init\n";

		// allocate data stack
		output ~= "sub rsp, 2048\n"; // 512 cells
		output ~= "mov r30, rsp\n";

		// jump to main
		output ~= "jmp __calmain\n";
	}

	override void End() {
		// exit program
		if ("__fox32_program_exit" in words) {
			CallFunction("__fox32_program_exit");
		}
		else {
			WarnNoInfo("No exit function available, expect bugs");
		}

		// create init function
		output ~= "__init:\n";
		if ("__fox32_program_init" in words) {
			CallFunction("__fox32_program_init");
		}
		else {
			WarnNoInfo("No program init function available");
		}
		output ~= "ret\n";

		// create global variables
		foreach (var ; globals) {
			auto sanitised = var.name.Sanitise();

			foreach (i ; 0 .. var.Size()) {
				output ~= format("__global_%s_%d: data.8 0\n", sanitised, i);
			}
		}

		output ~= "#include \"fox32rom.def\"\n";
		output ~= "#include \"fox32os.def\"\n";
	}

	void PushGlobalValue(
		Node node, Global var, size_t size = 0, size_t offset = 0, bool member = false,
		bool deref = false
	) {
		if (size == 0) {
			size = var.type.Size();
		}

		if (size != 8) {
			output ~= "xor r0, r0\n";
		}

		string symbol = format("__global_%s", var.name.Sanitise());

		if (deref) {
			output ~= format("mov.32 r1, [%s_0]\n", symbol);

			switch (size) {
				case 1: output ~= format("mov.8 r0, [r1 + %d]\n", offset); break;
				case 2: output ~= format("mov.16 r0, [r1 + %d]\n", offset); break;
				case 4: output ~= format("mov.32 r0, [r1 + %d]\n", offset); break;
				default: Error(node.error, "Bad variable type size");
			}
		}
		else {
			switch (size) {
				case 1: output ~= format("mov.8 r0, [%s_%d]\n", symbol, offset); break;
				case 2: output ~= format("mov.16 r0, [%s_%d]\n", symbol, offset); break;
				case 4: output ~= format("mov.32 r0, [%s_%d]\n", symbol, offset); break;
				default: Error(node.error, "Bad variable type size");
			}
		}

		output ~= "mov [r30], r0\n";
		output ~= "inc r30, 4\n";
	}

	void PushVariableValue(
		Node node, Variable var, size_t size = 0, size_t offset = 0, bool member = false,
		bool deref = false
	) {
		if (size == 0) {
			size = var.type.Size();
		}

		if (size != 8) {
			output ~= "xor r0, r0\n";
		}

		if (deref) {
			output ~= format("mov r1, [rsp + %d]\n", var.offset);

			switch (size) {
				case 1: output ~= format("mov.8 r0, [r1 + %d]\n", offset); break;
				case 2: output ~= format("mov.16 r0, [r1 + %d]\n", offset); break;
				case 4: output ~= format("mov.32 r0, [r1 + %d]\n", offset); break;
				default: Error(node.error, "Bad variable type size");
			}
		}
		else {
			switch (size) {
				case 1: output ~= format("mov.8 r0, [rsp + %d]\n", offset + var.offset); break;
				case 2: output ~= format("mov.16 r0, [rsp + %d]\n", offset + var.offset); break;
				case 4: output ~= format("mov.32 r0, [rsp + %d]\n", offset + var.offset); break;
				default: Error(node.error, "Bad variable type size");
			}
		}

		output ~= "mov [r30], r0\n";
		output ~= "add r30, 4\n";
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
				output ~= format("call __func__%s\n", node.name.Sanitise());
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
			Error(node.error, "Undefined identifier '%s'", node.name);
		}
	}

	override void CompileInteger(IntegerNode node) {
		output ~= format("mov.32 [r30], %d\n", node.value);
		output ~= "add r30, 4\n";
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

			words[node.name] = Word(node.raw? WordType.Raw : WordType.Callisto , false, []);

			string symbol =
				node.raw? node.name : format("__func__%s", node.name.Sanitise());

			output ~= format("%s:\n", symbol);

			// allocate parameters
			size_t paramSize = node.params.length * 4;
			foreach (ref type ; node.paramTypes) {
				if (!TypeExists(type.name)) {
					Error(node.error, "Type '%s' doesn't exist", type.name);
				}
				if (GetType(type.name).isStruct && !type.ptr) {
					Error(node.error, "Structures cannot be used in function parameters");
				}
			}
			if ((paramSize > 0) && !node.manual) {
				output ~= format("sub rsp, %d\n", paramSize);
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
				if (node.params.length > 10) {
					output ~= format("sub r30, %d\n", paramSize);
					output ~= "mov r0, r30\n";
					output ~= "mov r1, rsp\n";
					output ~= format("mov r2, %d\n", paramSize / 4);
					output ~= "call copy_memory_bytes\n";
				}
				else {
					foreach_reverse (ref param ; node.params) {
						auto setNode = new SetNode(node.error);
						setNode.var  = param;
						CompileSet(setNode);
					}
				}
			}

			foreach (ref inode ; node.nodes) {
				compiler.CompileNode(inode);
			}

			size_t scopeSize;
			foreach (ref var ; variables) {
				scopeSize += var.Size();
			}

			switch (scopeSize) {
				case 1:  output ~= "inc rsp\n"; break;
				case 2:  output ~= "inc rsp, 2\n"; break;
				case 4:  output ~= "inc rsp, 4\n"; break;
				default: output ~= format("add rsp, %d\n", scopeSize);
			}

			output    ~= "ret\n";
			inScope    = false;
			variables  = [];
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

			output ~= "sub r30, 4\n";
			output ~= "cmp [r30], 0\n";
			output ~= format("ifz jmp __if_%d_%d\n", blockNum, condCounter + 1);

			// create scope
			auto oldVars = variables.dup;
			auto oldSize = GetStackSize();

			foreach (ref inode ; node.doIf[i]) {
				compiler.CompileNode(inode);
			}

			// remove scope
			if (GetStackSize() - oldSize > 0) {
				output ~= format("add rsp, %d\n", GetStackSize() - oldSize);
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
				output ~= format("add rsp, %d\n", GetStackSize() - oldSize);
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

			output ~= "push r30\n";
			output ~= "call save_state_and_yield_task\n";
			output ~= "pop r30\n";
		}

		// restore scope
		output ~= format("__while_%d_next:\n", blockNum);
		if (GetStackSize() - oldSize > 0) {
			output ~= format("add rsp, %d\n", GetStackSize() - oldSize);
		}
		variables = oldVars;

		inWhile = false;

		output ~= format("__while_%d_condition:\n", blockNum);
		
		foreach (ref inode ; node.condition) {
			compiler.CompileNode(inode);
		}

		output ~= "sub r30, 4\n";
		output ~= "mov r0, [r30]\n";
		output ~= "cmp r0, 0\n";
		output ~= format("ifnz jmp __while_%d\n", blockNum);
		output ~= format("__while_%d_end:\n", blockNum);
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

			if (var.Size() == 4) {
				output ~= "push.32 0\n";
			}
			else {
				output ~= format("sub rsp, %d\n", var.Size());
			}
		}
		else {
			if (GlobalExists(node.name)) {
				Error(node.error, "Global '%s' already exists", node.name);
			}

			Global global;
			global.type.type   = GetType(node.varType.name);
			global.type.ptr    = node.varType.ptr;
			global.array       = node.array;
			global.arraySize   = node.arraySize;
			global.name        = node.name;
			globals           ~= global;
		}
	}

	override void CompileArray(ArrayNode node) {}

	override void CompileString(StringNode node) {}

	override void CompileReturn(WordNode node) {
		if (!inScope) {
			Error(node.error, "Return used outside of function");
		}

		size_t scopeSize;
		foreach (ref var ; variables) {
			scopeSize += var.Size();
		}

		switch (scopeSize) {
			case 1:  output ~= "inc rsp\n"; break;
			case 2:  output ~= "inc rsp, 2\n"; break;
			case 4:  output ~= "inc rsp, 4\n"; break;
			default: output ~= format("add rsp, %d\n", scopeSize);
		}

		output ~= "ret\n";
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

		output ~= format("jmp __while_%d_next\n", currentLoop);
	}

	override void CompileExtern(ExternNode node) {}

	override void CompileCall(WordNode node) {}

	override void CompileAddr(AddrNode node) {}

	override void CompileImplement(ImplementNode node) {}

	void SetVariable(
		Node node, Variable var, size_t size = 0, size_t offset = 0, bool member = false,
		bool deref = false
	) {
		if (size == 0) {
			size = var.type.Size();
		}

		output ~= "dec r30, 4\n";
		output ~= "mov r0, [r30]\n";

		if (deref) {
			output ~= format("mov r1, [rsp + %d]\n", var.offset);

			switch (size) {
				case 1: output ~= format("mov.8 [r1 + %d], r0\n", offset); break;
				case 2: output ~= format("mov.16 [r1 + %d], r0\n", offset); break;
				case 4: output ~= format("mov.32 [r1 + %d], r0\n", offset); break;
				default: Error(node.error, "Bad variable type size");
			}
		}
		else {
			switch (size) {
				case 1: output ~= format("mov.8 [rsp + %d], r0\n", var.offset + offset); break;
				case 2: output ~= format("mov.16 [rsp + %d], r0\n", var.offset + offset); break;
				case 4: output ~= format("mov.32 [rsp + %d], r0\n", var.offset + offset); break;
				default: Error(node.error, "Bad variable type size");
			}
		}
	}

	void SetGlobal(
		Node node, Global global, size_t size = 0, size_t offset = 0,
		bool member = false, bool deref = false
	) {
		if (size == 0) {
			size = global.type.Size();
		}

		output ~= "dec r30, 4\n";
		output ~= "mov r0, [r30]\n";

		string symbol = format("__global_%s", global.name.Sanitise());

		if (deref) {
			output ~= format("mov r1, [%s_0]\n", symbol);

			switch (size) {
				case 1: output ~= format("mov.8 [r1 + %d], r0\n", offset); break;
				case 2: output ~= format("mov.16 [r1 + %d], r0\n", offset); break;
				case 4: output ~= format("mov.32 [r1 + %d], r0\n", offset); break;
				default: Error(node.error, "Bad variable type size");
			}
		}
		else {
			switch (size) {
				case 1: output ~= format("mov.8 [%s_%d], r0\n", symbol, offset); break;
				case 2: output ~= format("mov.16 [%s_%d], r0\n", symbol, offset); break;
				case 4: output ~= format("mov.32 [%s_%d], r0\n", symbol, offset); break;
				default: Error(node.error, "Bad variable type size");
			}
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

	override void CompileTryCatch(TryCatchNode node) {}

	override void CompileThrow(WordNode node) {}
}
