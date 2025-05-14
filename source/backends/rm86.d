module callisto.backends.rm86;

import std.conv;
import std.stdio;
import std.range;
import std.format;
import std.algorithm;
import callisto.util;
import callisto.error;
import callisto.parser;
import callisto.output;
import callisto.compiler;
import callisto.language;
import callisto.preprocessor;

private struct Word {
	bool       raw;
	bool       inline;
	Node[]     inlineNodes;
	bool       error;
	UsedType[] params;
}

private struct RM86Opts {
	bool noDos;
}

class BackendRM86 : CompilerBackend {
	Word[string]     words;
	uint             blockCounter; // used for block statements
	bool             inScope;
	string           thisFunc;
	bool             inWhile;
	uint             currentLoop;
	RM86Opts         opts;
	uint             tempLabelNum;

	this() {
		addrSize  = 2;
		defaultOS = "dos";

		types ~= Type("u8",    1);
		types ~= Type("i8",    1);
		types ~= Type("u16",   2);
		types ~= Type("i16",   2);
		types ~= Type("addr",  2);
		types ~= Type("size",  2);
		types ~= Type("usize", 2);
		types ~= Type("cell",  2);
		types ~= Type("bool",  2);

		// built in structs
		types ~= Type("Array", 6, true, [
			StructEntry(UsedType(GetType("usize"), false), "length", false, 2, 0),
			StructEntry(UsedType(GetType("usize"), false), "memberSize", false, 2, 2),
			StructEntry(UsedType(GetType("addr"), false),  "elements", false, 2, 4)
		]);
		NewConst("Array.length",     0);
		NewConst("Array.memberSize", 2);
		NewConst("Array.elements",   4);
		NewConst("Array.sizeOf",     2 * 3);

		types ~= Type("Exception", 6 + 2, true, [
			StructEntry(UsedType(GetType("bool"), false),  "error", false, 2, 0),
			StructEntry(UsedType(GetType("Array"), false), "msg", false, 6, 2)
		]);
		NewConst("Exception.bool",   0);
		NewConst("Exception.msg",    2);
		NewConst("Exception.sizeOf", 6 + 2);

		foreach (ref type ; types) {
			NewConst(format("%s.sizeOf", type.name), cast(long) type.size);
		}

		globals ~= Global(
			"_cal_exception", UsedType(GetType("Exception"), false), false, 0
		);
	}

	string TempLabel() {
		++ tempLabelNum;
		return format("__temp_%d", tempLabelNum);
	}

	override void NewConst(string name, long value, ErrorInfo error = ErrorInfo.init) {
		consts[name] = Constant(new IntegerNode(error, value));
	}

	override string[] GetVersions() => [
		// platform
		"RM86", "LittleEndian", "16Bit",
		// features
		"IO"
	] ~ (os == "dos"? ["DOS", "Args", "Exit"] : os == "bare-metal"? ["BareMetal"] : []);

	override string[] FinalCommands() => output.useMod? [] : [
		format("mv %s %s.asm", compiler.outFile, compiler.outFile),
		format("nasm -f bin %s.asm -o %s", compiler.outFile, compiler.outFile),
		keepAssembly? "" : format("rm %s.asm", compiler.outFile)
	];

	override long MaxInt() => 0xFFFF;

	override string DefaultHeader() => "";

	override bool HandleOption(string opt, ref string[] versions, Preprocessor preproc) {
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

		// call globals
		// what?
		foreach (global ; globals) {
			if (global.type.hasInit && !global.type.ptr) {
				output ~= format(
					"mov word [si], word __global_%s\n", global.name.Sanitise()
				);
				output ~= "add si, 2\n";
				output ~= format("call __type_init_%s\n", global.type.name.Sanitise());
			}
		}
	}

	void CallFunction(string name) {
		auto word = words[name];

		if (word.inline) {
			foreach (inode ; word.inlineNodes) {
				compiler.CompileNode(inode);
			}
		}
		else {
			if (word.raw) {
				output ~= format("call %s\n", name);
			}
			else {
				output ~= format("call __func__%s\n", name.Sanitise());
			}
		}
	}

	override void Init() {
		if (org == 0xFFFF) {
			switch (os) {
				case "dos": org = 0x100; break;
				default: {
					WarnNoInfo("No org seems to be set");
				}
			}
		}

		string[] oses = ["bare-metal", "dos"];
		if (!oses.canFind(os)) {
			ErrorNoInfo("Backend doesn't support operating system '%s'", os);
		}

		output ~= format("org 0x%.4X\n", org);
		output ~= "call __init\n";
		output ~= "mov si, __stack\n";
		output ~= "jmp __calmain\n";
	}

	override void End() {
		foreach (global ; globals) {
			if (global.type.hasDeinit && !global.type.ptr) {
				output ~= format("mov word [si], word __global_%s\n", Sanitise(global.name));
				output ~= "add si, 2\n";
				output ~= format("call __type_deinit_%s\n", Sanitise(global.type.name));
			}
		}

		if ("__rm86_program_exit" in words) {
			CallFunction("__rm86_program_exit");
		}
		else {
			WarnNoInfo("No exit function available, expect bugs");
		}

		// create init function
		output ~= "__init:\n";
		if ("__rm86_program_init" in words) {
			CallFunction("__rm86_program_init");
		}
		else {
			WarnNoInfo("No program init function available");
		}
		output ~= "ret\n";

		foreach (var ; globals) {
			output ~= format("__global_%s: times %d db 0\n", var.name.Sanitise(), var.Size());
		}

		foreach (i, ref array ; arrays) {
			output ~= format("__array_%d: ", i);

			switch (array.type.Size()) {
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
					array.type.Size(),
					i
				);
			}
		}

		output ~= "__stack: times 512 dw 0\n";
	}

	void PushVariableValue(
		Node node, Variable var, size_t size = 0, size_t offset = 0, bool member = false,
		bool deref = false
	) {
		if (size == 0) {
			size = var.type.Size();
		}

		if (size != 2) {
			output ~= "xor ax, ax\n";
		}

		output ~= "mov di, sp\n";

		if (deref) {
			output ~= format("mov bx, [di + %d]\n", var.offset);

			switch (size) {
				case 1: output ~= format("mov al, [bx + %d]\n", offset); break;
				case 2: output ~= format("mov ax, [bx + %d]\n", offset); break;
				default: Error(node.error, "Bad variable type size");
			}
		}
		else {
			switch (size) {
				case 1: output ~= format("mov al, [di + %d]\n", var.offset + offset); break;
				case 2: output ~= format("mov ax, [di + %d]\n", var.offset + offset); break;
				default: Error(node.error, "Bad variable type size");
			}
		}

		output ~= "mov [si], ax\n";
		output ~= "add si, 2\n";
	}

	void PushGlobalValue(
		Node node, Global var, size_t size = 0, size_t offset = 0, bool member = false,
		bool deref = false
	) {
		if (size == 0) {
			size = var.type.Size();
		}

		if (size != 2) {
			output ~= "xor ax, ax\n";
		}

		string symbol = format("__global_%s", var.name.Sanitise());

		if (deref) {
			output ~= format("mov bx, [%s]\n", symbol);

			switch (size) {
				case 1: output ~= format("mov al, [bx + %d]\n", offset); break;
				case 2: output ~= format("mov ax, [bx + %d]\n", offset); break;
				default: Error(node.error, "Bad variable type size");
			}
		}
		else {
			switch (size) {
				case 1: output ~= format("mov al, [%s + %d]\n", symbol, offset); break;
				case 2: output ~= format("mov ax, [%s + %d]\n", symbol, offset); break;
				default: Error(node.error, "Bad variable type size");
			}
		}

		output ~= "mov [si], ax\n";
		output ~= "add si, 2\n";
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
					if (word.error && words[thisFunc].error) {
						size_t paramSize = word.params.length * 2;

						if (paramSize != 0) {
							output ~= format("lea ax, [si - %d]\n", paramSize);
							output ~= "push ax\n";
						}
						else {
							output ~= "push si\n";	
						}
					}
					
					output ~= format("call __func__%s\n", node.name.Sanitise());

					if (word.error && words[thisFunc].error) {
						output ~= "pop di\n";
					}
				}
			}

			if (word.error) {
				if ("__rm86_exception" in words) {
					bool crash;

					if (inScope) {
						crash = !words[thisFunc].error;
					}
					else {
						crash = true;
					}

					if (crash) {
						output ~= format("mov bx, __global_%s\n", Sanitise("_cal_exception"));
						output ~= "mov ax, [bx]\n";
						output ~= "cmp ax, 0\n";
						output ~= format("jne __func__%s\n", Sanitise("__rm86_exception"));
					}
					else {
						string temp = TempLabel();

						output ~= format("cmp word [__global_%s], 0\n", Sanitise("_cal_exception"));
						output ~= format("je %s\n", temp);
						output ~= "mov si, di\n";
						CompileReturn(node);
						output ~= format("%s:\n", temp);
					}
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

			if (structVar.structure) {
				Error(node.error, "Can't push the value of an array or structure");
			}

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

	override void CompileSignedInt(SignedIntNode node) {
		output ~= format("mov word [si], %d\n", cast(short) node.value);
		output ~= "add si, 2\n";
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

		UsedType[] params;

		foreach (ref type ; node.paramTypes) {
			if (!TypeExists(type.name)) {
				Error(node.error, "Type '%s' doesn't exist", type.name);
			}

			params ~= UsedType(GetType(type.name), type.ptr);
		}

		if (node.inline) {
			if (node.errors) {
				output ~= format("mov word [__global_%s], 0\n", Sanitise("_cal_exception"));
			}

			words[node.name] = Word(false, true, node.nodes, node.errors, params);
		}
		else {
			assert(!inScope);
			inScope = true;

			words[node.name] = Word(node.raw, false, [], node.errors, params);

			string symbol =
				node.raw? node.name : format("__func__%s", node.name.Sanitise());

			output ~= format("%s:\n", symbol);

			if (node.errors) {
				output ~= format("mov word [__global_%s], 0\n", Sanitise("_cal_exception"));
			}

			// allocate parameters
			size_t paramSize = node.params.length * 2;
			foreach (ref type ; node.paramTypes) {
				if (!TypeExists(type.name)) {
					Error(node.error, "Type '%s' doesn't exist", type.name);
				}
				if (GetType(type.name).isStruct && !type.ptr) {
					Error(node.error, "Structures cannot be used in function parameters");
				}
			}
			if ((paramSize > 0) && !node.manual) {
				output ~= format("sub sp, %d\n", paramSize);
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
					var.stackSize = 2;
					offset       += var.StackSize();
					variables    ~= var;
				}

				// copy data to parameters
				output ~= format("sub si, %d\n", paramSize);
				output ~= "mov ax, si\n";
				output ~= "mov di, sp\n";
				output ~= format("mov cx, %d\n", paramSize);
				output ~= "rep movsb\n";
				output ~= "mov si, ax\n";
			}

			foreach (ref inode ; node.nodes) {
				compiler.CompileNode(inode);
			}

			//output ~= format("__func_return__%s:\n", node.name.Sanitise());

			size_t scopeSize;
			foreach (ref var ; variables) {
				scopeSize += var.StackSize();

				if (var.type.hasDeinit && !var.type.ptr) {
					output ~= format("lea ax, [sp + %d\n]", var.offset);
					output ~= "mov [si], ax\n";
					output ~= "add si, 2\n";
					output ~= format("call __type_deinit_%s\n", Sanitise(var.type.name));
				}
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
			foreach (ref var ; variables) {
				if (oldVars.canFind(var)) continue;
				if (!var.type.hasDeinit || var.type.ptr) continue;

				output ~= format("lea ax, [sp + %d\n]", var.offset);
				output ~= "mov [si], ax\n";
				output ~= "add si, 2\n";
				output ~= format("call __type_deinit_%s\n", Sanitise(var.type.name));
			}
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
			foreach (ref var ; variables) {
				if (oldVars.canFind(var)) continue;
				if (!var.type.hasDeinit || var.type.ptr) continue;

				output ~= format("lea ax, [sp + %d\n]", var.offset);
				output ~= "mov [si], ax\n";
				output ~= "add si, 2\n";
				output ~= format("call __type_deinit_%s\n", Sanitise(var.type.name));
			}
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
		output ~= format("__while_%d_next:\n", blockNum);
		foreach (ref var ; variables) {
			if (oldVars.canFind(var)) continue;
			if (!var.type.hasDeinit || var.type.ptr) continue;

			output ~= format("lea ax, [sp + %d\n]", var.offset);
			output ~= "mov [si], ax\n";
			output ~= "add si, 2\n";
			output ~= format("call __type_deinit_%s\n", Sanitise(var.type.name));
		}
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
		if (!TypeExists(node.varType.name)) {
			Error(node.error, "Undefined type '%s'", node.varType.name);
		}
		if (node.name != "") {
			if (VariableExists(node.name) || (node.name in words)) {
				Error(node.error, "Variable name '%s' already used", node.name);
			}
			if (Language.bannedNames.canFind(node.name)) {
				Error(node.error, "Name '%s' can't be used", node.name);
			}
		}

		if (inScope) {
			Variable var;
			var.name      = node.name;
			var.type      = UsedType(GetType(node.varType.name), node.varType.ptr);
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

			if (var.name == "") {
				output ~= "mov [si], sp\n";
				output ~= "add si, 2\n";
			}

			if (var.type.hasInit && !var.type.ptr) { // call constructor
				output ~= "mov [si], sp\n";
				output ~= "add si, 2\n";
				output ~= format("call __type_init_%s\n", Sanitise(var.type.name));
			}
		}
		else {
			if (GlobalExists(node.name)) {
				Error(node.error, "Global '%s' already exists", node.name);
			}

			Global global;
			global.type        = UsedType(GetType(node.varType.name), node.varType.ptr);
			global.array       = node.array;
			global.arraySize   = node.arraySize;
			global.name        = node.name;
			globals           ~= global;

			if (global.name == "") {
				Error(
					node.error,
					"Anonymous variables can only be created inside a function"
				);
			}

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

		if (!TypeExists(node.arrayType.name)) {
			Error(node.error, "Type '%s' doesn't exist", node.arrayType.name);
		}

		array.type    = UsedType(GetType(node.arrayType.name), node.arrayType.ptr);
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
			var.type   = UsedType(GetType("Array"), false);
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
			output ~= format("mov word [bx + 2], %d\n", array.type.Size()); // member size
			output ~= "mov [bx + 4], ax\n"; // elements

			// push metadata address
			output ~= "mov [si], sp\n";
			output ~= "add si, 2\n";
		}
	}

	override void CompileString(StringNode node) {
		auto arrayNode = new ArrayNode(node.error);

		arrayNode.arrayType = new TypeNode(node.error, "u8", false);
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

			if (var.type.hasDeinit && !var.type.ptr) {
				output ~= format("lea ax, [sp + %d\n]", var.offset);
				output ~= "mov [si], ax\n";
				output ~= "add si, 2\n";
				output ~= format("call __type_deinit_%s\n", Sanitise(var.type.name));
			}
		}
		if (scopeSize == 1) {
			output ~= "inc sp\n";
		}
		else {
			output ~= format("add sp, %d\n", scopeSize);
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

	override void CompileExtern(ExternNode node) {
		if (node.externType == ExternType.C) {
			Error(node.error, "This backend doesn't support C externs");
		}

		Word word;
		word.raw         = node.externType == ExternType.Raw;
		words[node.func] = word;
	}

	override void CompileCall(WordNode node) {
		output ~= "sub si, 2\n";
		output ~= "mov ax, [si]\n";
		output ~= "call ax\n";
	}

	override void CompileAddr(AddrNode node) {
		if (node.func in words) {
			auto   word   = words[node.func];
			string symbol =
				word.raw? node.func : format("__func__%s", node.func.Sanitise());

			output ~= format("mov ax, %s\n", symbol);
			output ~= "mov [si], ax\n";
			output ~= "add si, 2\n";
		}
		else if (GlobalExists(node.func)) {
			auto var = GetGlobal(node.func);

			output ~= format(
				"mov [si], word __global_%s\n", node.func.Sanitise()
			);
			output ~= "add si, 2\n";
		}
		else if (VariableExists(node.func)) {
			auto var = GetVariable(node.func);

			output ~= "mov di, sp\n";
			if (var.offset > 0) {
				output ~= format("add di, %d\n", var.offset);
			}
			output ~= "mov [si], di\n";
			output ~= "add si, 2\n";
		}
		else if (IsStructMember(node.func)) {
			string name    = node.func[0 .. node.func.countUntil(".")];
			auto structVar = GetStructVariable(node, node.func);
			size_t offset  = structVar.offset;

			if (GlobalExists(name)) {
				auto var = GetGlobal(name);

				if (var.type.ptr) {
					output ~= format("mov bx, [__global_%s]\n", name.Sanitise());
					output ~= format("lea ax, [bx + %d]\n", offset);
				}
				else {
					output ~= format(
						"lea ax, [__global_%s + %d]\n", name.Sanitise(), offset
					);
				}

				output ~= "mov [si], ax\n";
				output ~= "add si, 2\n";
			}
			else if (VariableExists(name)) {
				auto var = GetVariable(name);

				if (var.type.ptr) {
					output ~= "mov di, sp\n";
					output ~= format("mov bx, [di + %d]\n", var.offset);
					output ~= format("lea ax, [bx + %d]\n", offset);
				}
				else {
					output ~= "mov ax, sp\n";
					if (var.offset > 0) {
						output ~= format("add ax, %d\n", var.offset + offset);
					}
				}

				output ~= "mov [si], ax\n";
				output ~= "add si, 2\n";
			}
			else {
				Error(node.error, "Variable '%s' does not exist", name);
			}
		}
		else {
			Error(node.error, "Undefined identifier '%s'", node.func);
		}
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
				labelName = format("__type_init_%s", Sanitise(node.structure));
				break;
			}
			case "deinit": {
				if (GetType(node.structure).hasDeinit) {
					Error(node.error, "Already implemented in type");
				}

				type.hasDeinit = true;
				labelName = format("__type_deinit_%s", Sanitise(node.structure));
				break;
			}
			default: Error(node.error, "Unknown method '%s'", node.method);
		}

		SetType(type.name, type);

		assert(!inScope);
		inScope = true;

		output ~= format("%s:\n", labelName);

		foreach (ref inode ; node.nodes) {
			compiler.CompileNode(inode);
		}

		size_t scopeSize;
		foreach (ref var ; variables) {
			scopeSize += var.StackSize();

			if (var.type.hasDeinit) {
				output ~= format("lea ax, [sp + %d\n]", var.offset);
				output ~= "mov [si], ax\n";
				output ~= "add si, 2\n";
				output ~= format("call __type_deinit_%s\n", Sanitise(var.type.name));
			}
		}
		if (scopeSize == 1) {
			output ~= "inc sp\n";
		}
		else if (scopeSize > 0) {
			output ~= format("add sp, %d\n", scopeSize);
		}

		output    ~= "ret\n";
		inScope    = false;
		variables  = [];
	}

	void SetVariable(
		Node node, Variable var, size_t size = 0, size_t offset = 0, bool member = false,
		bool deref = false
	) {
		if (size == 0) {
			size = var.type.Size();
		}

		output ~= "mov di, sp\n";

		if (deref) {
			output ~= format("mov bx, [di + %d]\n", var.offset);

			switch (size) {
				case 1: output ~= format("mov [bx + %d], al\n", offset); break;
				case 2: output ~= format("mov [bx + %d], ax\n", offset); break;
				default: Error(node.error, "Bad variable type size");
			}
		}
		else {
			switch (size) {
				case 1: output ~= format("mov [di + %d], al\n", var.offset + offset); break;
				case 2: output ~= format("mov [di + %d], ax\n", var.offset + offset); break;
				default: Error(node.error, "Bad variable type size");
			}
		}
	}

	void SetGlobal(
		Node node, Global global, size_t size = 0, size_t offset = 0, bool member = false,
		bool deref = false
	) {
		if (size == 0) {
			size = global.type.Size();
		}

		string symbol = format("__global_%s", global.name.Sanitise());

		if (deref) {
			output ~= format("mov bx, [%s]\n", symbol);

			switch (size) {
				case 1: output ~= format("mov [bx + %d], al\n", offset); break;
				case 2: output ~= format("mov [bx + %d], ax\n", offset); break;
				default: Error(node.error, "Bad variable type size");
			}
		}
		else {
			switch (size) {
				case 1: output ~= format("mov [%s], al\n", symbol); break;
				case 2: output ~= format("mov [%s], ax\n", symbol); break;
				default: Error(node.error, "Bad variable type size");
			}
		}
	}

	override void CompileSet(SetNode node) {
		output ~= "sub si, 2\n";
		output ~= "mov ax, [si]\n";

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

			if (structVar.structure) {
				Error(node.error, "Can't push the value of an array or structure");
			}

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
		if (word.raw) {
			Error(node.error, "Non-callisto functions can't throw");
		}

		if (word.params.length > 0) {
			output ~= format("lea di, [si - %d]\n", word.params.length * 2);
			output ~= "push di\n";
		}
		else {
			output ~= "push si\n";
		}

		++ blockCounter;

		if (word.inline) {
			foreach (inode ; word.inlineNodes) {
				compiler.CompileNode(inode);
			}
		}
		else {
			output ~= format("call __func__%s\n", node.func.Sanitise());
		}

		output ~= "pop di\n";

		output ~= format("mov ax, [__global_%s]\n", Sanitise("_cal_exception"));
		output ~= "cmp ax, 0\n";
		output ~= format("je __catch_%d_end\n", blockCounter);
		output ~= "mov si, di\n";

		// create scope
		auto oldVars = variables.dup;
		auto oldSize = GetStackSize();

		foreach (inode ; node.catchBlock) {
			compiler.CompileNode(inode);
		}

		// remove scope
		foreach (ref var ; variables) {
			if (oldVars.canFind(var)) continue;
			if (!var.type.hasDeinit || var.type.ptr) continue;

			output ~= format("lea ax, [sp + %d\n]", var.offset);
			output ~= "mov [si], ax\n";
			output ~= "add si, 2\n";
			output ~= format("call __type_deinit_%s\n", Sanitise(var.type.name));
		}
		if (GetStackSize() - oldSize > 0) {
			output ~= format("add sp, %d\n", GetStackSize() - oldSize);
		}
		variables = oldVars;

		output ~= format("__catch_%d_end:\n", blockCounter);
	}

	override void CompileThrow(WordNode node) {
		if (!inScope || (!words[thisFunc].error)) {
			Error(node.error, "Not in a function that can throw");
		}
		if (words[thisFunc].inline) {
			Error(node.error, "Can't use throw in an inline function");
		}

		// set exception error
		output ~= format("mov word [__global_%s], 0xFFFF\n", Sanitise("_cal_exception"));

		// copy exception message
		output ~= "sub si, 2\n";
		output ~= "mov bx, si\n";
		output ~= "mov si, [bx]\n";
		output ~= format("lea di, [__global_%s + 2]\n", Sanitise("_cal_exception"));
		output ~= "mov cx, 3\n";
		output ~= "rep movsw\n";
		output ~= "mov si, bx\n";

		CompileReturn(node);
	}
}
