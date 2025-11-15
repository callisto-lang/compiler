module callisto.backends.uxn;

import std.conv;
import std.stdio;
import std.range;
import std.format;
import std.string;
import std.algorithm;
import callisto.util;
import callisto.error;
import callisto.parser;
import callisto.output;
import callisto.compiler;
import callisto.language;
import callisto.preprocessor;
import callisto.mod.sections;

class BackendUXN : CompilerBackend {
	uint   tempLabelNum;
	string assembler = "uxnasm";

	this() {
		org          = 0x100;
		addrSize     = 2;
		symbolPrefix = false;

		types ~= Type("core", "u8",    1, false);
		types ~= Type("core", "i8",    1, true);
		types ~= Type("core", "u16",   2, false);
		types ~= Type("core", "i16",   2, true);
		types ~= Type("core", "addr",  2, false);
		types ~= Type("core", "isize", 2, true);
		types ~= Type("core", "usize", 2, false);
		types ~= Type("core", "cell",  2, false);
		types ~= Type("core", "icell", 2, true);
		types ~= Type("core", "bool",  2, false);

		// built in structs
		types ~= Type("core", "Array", 6, false, true, [
			StructEntry(UsedType(GetType("core.usize"), false), "length", false, 2, 0),
			StructEntry(UsedType(GetType("core.usize"), false), "memberSize", false, 2, 2),
			StructEntry(UsedType(GetType("core.addr"), false),  "elements", false, 2, 4)
		]);
		NewConst("core", "Array.length",     0);
		NewConst("core", "Array.memberSize", 2);
		NewConst("core", "Array.elements",   4);
		NewConst("core", "Array.sizeOf",     2 * 3);

		types ~= Type("core", "Exception", 6 + 2, false, true, [
			StructEntry(UsedType(GetType("core.bool"), false),  "error", false, 2, 0),
			StructEntry(UsedType(GetType("core.Array"), false), "msg", false, 6, 0)
		]);
		NewConst("core", "Exception.bool",   0);
		NewConst("core", "Exception.msg",    2);
		NewConst("core", "Exception.sizeOf", 6 + 2);

		foreach (ref type ; types) {
			NewConst("core", format("%s.sizeOf", type.name), cast(long) type.size);
		}
	}

	string TempLabel() {
		++ tempLabelNum;
		return Label("temp_", "%d", tempLabelNum);
	}

	override string[] GetVersions() => [
		// platform
		"UXN", "BigEndian", "16Bit",
		// features
		"IO", "Exit"
	];

	override string[] FinalCommands() => output.mode == OutputMode.Module? [] : [
		format("mv %s %s.tal", compiler.outFile, compiler.outFile),
		format("%s %s.tal %s", assembler, compiler.outFile, compiler.outFile),
		keepAssembly? "" : format("rm %s.tal", compiler.outFile)
	];

	override long   MaxInt()  => 0xFFFF;
	override string ExecExt() => ".rom";

	override string DefaultHeader() => "
		|00 @System &vector $2 &expansion $2 &wst $1 &rst $1 &metadata $2 &r $2 &g $2 &b $2 &debug $1 &state $1
		|10 @Console &vector $2 &read $5 &type $1 &write $1 &error $1
		|20 @Screen/vector $2 &width $2 &height $2 &auto $2 &x $2 &y $2 &addr $2 &pixel $1 &sprite $1
		|30 @Audio0/vector $2 &position $2 &output $1 &pad $3 &adsr $2 &length $2 &addr $2 &volume $1 &pitch $1
		|40 @Audio1/vector $2 &position $2 &output $1 &pad $3 &adsr $2 &length $2 &addr $2 &volume $1 &pitch $1
		|50 @Audio2/vector $2 &position $2 &output $1 &pad $3 &adsr $2 &length $2 &addr $2 &volume $1 &pitch $1
		|60 @Audio3/vector $2 &position $2 &output $1 &pad $3 &adsr $2 &length $2 &addr $2 &volume $1 &pitch $1
		|80 @Controller/vector $2 &button $1 &key $1
		|90 @Mouse/vector $2 &x $2 &y $2 &state $5 &scrolly &scrolly-hb $1 &scrolly-lb $1
		|a0 @File1/vector $2 &success $2 &stat $2 &delete $1 &append $1 &name $2 &length $2 &read $2 &write $2
		|b0 @File2/vector $2 &success $2 &stat $2 &delete $1 &append $1 &name $2 &length $2 &read $2 &write $2
		|c0 @DateTime &year $2 &month $1 &day $1 &hour $1 &minute $1 &second $1 &dotw $1 &doty $2 &isdst $1
	";

	override bool HandleOption(string opt, ref string[] versions, Preprocessor preproc) {
		if (opt.canFind('=')) {
			string key = opt[0 .. opt.indexOf('=')];
			string val = opt[opt.indexOf('=') + 1 .. $];

			switch (key) {
				case "asm": {
					assembler = val;
					return true;
				}
				default: return false;
			}
		}
		else {
			return false;
		}
	}

	override void BeginMain() {
		output.StartSection(SectionType.TopLevel);
		if (output.mode != OutputMode.Module) {
			output ~= "@calmain\n";
		}

		foreach (global ; globals) {
			if (global.mod != output.GetModName()) continue;

			if (global.type.hasInit && !global.type.ptr) {
				output ~= format(
					";%s\n", Label("global_", "%s", global.name.Sanitise())
				);
				output ~= format(
					"%s\n", Label("type_init_", "%s", global.type.name.Sanitise())
				);
			}
		}
	}

	void CallFunction(string name) {
		auto word = GetWord(name);

		if (word.inline) {
			foreach (inode ; word.inlineNodes) {
				compiler.CompileNode(inode);
			}
		}
		else {
			if (word.raw) {
				output ~= format("%s\n", name);
			}
			else {
				output ~= format("%s\n", Label(word));
			}
		}
	}

	override void Init() {
		if (output.mode == OutputMode.Module) return;

		output ~= "|0 @vsp $2 @arraySrc $2 @arrayDest $2 @temp $2\n";

		if (org != 0x100) {
			output ~= "|100\n";
			output ~= format("#%.4x JMP2\n", org);
		}

		output ~= format("|%x\n", org);
		output ~= "@on-reset\n";
		output ~= "    #ffff .vsp STZ2\n";
		output ~= "    init\n";
		output ~= "    BRK\n";
	}

	override void End() {
		// call destructors
		foreach (global ; globals) {
			if (global.type.hasDeinit && !global.type.ptr) {
				if (output.GetModName() != global.mod) continue;

				output ~= format(
					";%s\n", Label("global_", "%s", global.name.Sanitise())
				);
				output ~= format(
					"%s\n", Label("type_deinit_", "%s", global.type.name.Sanitise())
				);
			}
		}

		if (output.mode != OutputMode.Module) {
			if (WordExists("__uxn_program_exit")) {
				CallFunction("__uxn_program_exit");
			}
			else {
				WarnNoInfo("No exit function available, expect bugs");
			}

			// create init function
			output ~= "@init\n";
			if (WordExists("__uxn_program_init")) {
				CallFunction("__uxn_program_init");
			}
			else {
				WarnNoInfo("No program init function available");
			}
			output ~= "JMP2r\n";
		}

		// end top level code
		output.FinishSection();

		output.StartSection(SectionType.Data);
		foreach (var ; globals) {
			if (var.mod != output.GetModName()) continue;
			output ~= format("@%s", Label("global_", "%s", var.name.Sanitise()));

			foreach (i ; 0 .. var.Size()) {
				output ~= " 00";
			}

			output ~= "\n";
		}

		foreach (i, ref array ; arrays) {
			output ~= format("@%s ", Label("array_", "%d", i));

			foreach (j, ref element ; array.values) {
				output ~= element ~ (j == array.values.length - 1? "" : " ");
			}

			output ~= '\n';

			if (array.global) {
				output ~= format(
					"@%s %.4x %.4x =%s\n",
					Label("array_", "%d_meta", i),
					array.values.length, array.type.Size(),
					Label("array_", "%d", i)
				);
			}
		}

		output.FinishSection();

		if (output.mode != OutputMode.Module) {
			output ~= "@memcpy  01\n";
			output ~= "&length  0000\n";
			output ~= "&srcBank 0000\n";
			output ~= "&srcAddr 0000\n";
			output ~= "&dstBank 0000\n";
			output ~= "&dstAddr 0000\n";
			output ~= "@exception $8\n";

			output ~= "@program_end\n";

			// pad for the stack
			output ~= "|e0000\n";
		}
	}

	void PushVariableValue(
		Node node, Variable var, size_t size = 0, size_t offset = 0, bool member = false,
		bool deref = false
	) {
		if (size == 0) {
			size = var.type.Size();
		}

		if (deref) {
			if (var.offset == 0) {
				output ~= ".vsp LDZ2\n";
			}
			else {
				output ~= format(".vsp LDZ2 #%.4x ADD2\n", var.offset);
			}

			output ~= "LDA2\n";

			if (offset != 0) {
				output ~= format("#%.4x ADD2\n", offset);
			}
		}
		else {
			if (var.offset + offset == 0) {
				output ~= ".vsp LDZ2\n";
			}
			else {
				output ~= format(".vsp LDZ2 #%.4x ADD2\n", var.offset + offset);
			}
		}

		switch (size) {
			case 1: output ~= "LDA\n"; break;
			case 2: output ~= "LDA2\n"; break;
			default: Error(node.error, "Bad variable type size");
		}

		if ((size == 1) && var.type.isSigned) {
			output ~= "DUP #7f GTH #ff MUL SWP\n";
		}
		else if (size == 1) {
			output ~= "#00 SWP\n";
		}
	}

	void PushGlobalValue(
		Node node, Global var, size_t size = 0, size_t offset = 0, bool member = false,
		bool deref = false
	) {
		if (size == 0) {
			size = var.type.Size();
		}

		output ~= format(
			";%s\n", ExtLabel(var.mod, "global_", "%s", var.name.Sanitise())
		);

		if (deref) {
			output ~= "LDA2\n";
		}

		if (offset != 0) {
			output ~= format("#%.4x ADD2\n", offset);
		}

		switch (size) {
			case 1: output ~= "LDA\n"; break;
			case 2: output ~= "LDA2\n"; break;
			default: Error(node.error, "Bad variable type size");
		}

		if ((size == 1) && var.type.isSigned) {
			output ~= "DUP #7f GTH #ff MUL SWP\n";
		}
		else if (size == 1) {
			output ~= "#00 SWP\n";
		}
	}

	override void CompileWord(WordNode node) {
		if (CountAll(node.name) > 1) {
			Error(node.error, "Multiple matches for identifier '%s'", node.name);
		}

		if (WordExists(node.name)) {
			auto word = GetWord(node.name);

			if (word.inline) {
				foreach (inode ; word.inlineNodes) {
					compiler.CompileNode(inode);
				}
			}
			else {
				if (word.raw) {
					output ~= format("%s\n", node.name);
				}
				else {
					if (word.error && GetWord(thisFunc).error) {
						size_t paramSize = word.params.length * 2;

						if (paramSize != 0) {
							output ~= format(
								"LITr -System/wst DEIr LITr %.2x SUBr\n", paramSize
							);
						}
						else {
							output ~= "LITr -System/wst DEIr\n";
						}
					}
				
					output ~= format("%s\n", Label(word));

					if (word.error && GetWord(thisFunc).error) {
						output ~= "LITr -temp STZr\n";
					}
				}
			}

			if (word.error) {
				if (WordExists("__uxn_exception")) {
					bool crash;

					if (inScope) {
						crash = !GetWord(thisFunc).error;
					}
					else {
						crash = true;
					}

					if (crash) {
						output ~= ";exception LDA2\n";
						output ~= "#0000 NEQ2\n";
						output ~= format(
							"?%s\n", Label(GetWord("__uxn_exception"))
						);
					}
					else {
						string temp = TempLabel();

						output ~= ";exception LDA2\n";
						output ~= format("#0000 EQU2 ?%s\n", temp);
						output ~= ".temp LDZ .System/wst DEO\n";
						CompileReturn(node);
						output ~= format("@%s\n", temp);
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
			
			if (CountAll(name)) {
				Error(node.error, "Multiple matches for identifier '%s'", name);
			}

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
		else if (ConstExists(node.name)) {
			auto value  = GetConst(node.name).value;
			value.error = node.error;

			compiler.CompileNode(value);
		}
		else {
			Error(node.error, "Undefined identifier '%s'", node.name);
		}
	}

	override void CompileSignedInt(SignedIntNode node) {
		if (node.value > 0xFFFF) {
			Error(node.error, "Value is too big for 16-bit target");
		}

		output ~= format("#%.4x\n", cast(short) node.value);
	}

	override void CompileInteger(IntegerNode node) {
		if (node.value > 0xFFFF) {
			Error(node.error, "Value is too big for 16-bit target");
		}

		output ~= format("#%.4x\n", node.value);
	}

	override void CompileFuncDef(FuncDefNode node) {
		if ((WordExistsHere(node.name)) || VariableExists(node.name)) {
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

		output.StartSection(SectionType.FuncDef);
		if (output.mode == OutputMode.Module) {
			auto sect   = output.ThisSection!FuncDefSection();
			sect.pub    = true; // TODO: add private functions
			sect.inline = node.inline;
			sect.calls  = GetCalls(node.nodes);
			sect.name   = node.name;
			sect.params = params.length;
			sect.ret    = node.returnTypes.length;
		}

		if (node.inline) {
			if (node.errors) {
				output ~= "#0000 ;exception STA2\n";
			}

			words ~= Word(
				output.GetModName(), node.name, WordType.Callisto, false, true,
				node.nodes, node.errors, params
			);

			if (output.mode == OutputMode.Module) {
				auto sect = output.ThisSection!FuncDefSection();

				foreach (ref inode ; node.nodes) {
					sect.assembly ~= inode.toString() ~ '\n';
				}
			}
		}
		else {
			assert(!inScope);
			inScope = true;

			words ~= Word(
				output.GetModName(), node.name, WordType.Callisto, node.raw,
				false, [], node.errors, params
			);

			string symbol =
				node.raw? node.name : Label("func__", "%s", node.name.Sanitise());

			output ~= format("@%s\n", symbol);

			if (node.errors) {
				output ~= "#0000 ;exception STA2\n";
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
				output ~= format(".vsp LDZ2 #%.4x SUB2 .vsp STZ2\n", paramSize);
				foreach (ref var ; variables) {
					var.offset += paramSize;
				}

				size_t offset;
				foreach (i, ref type ; node.paramTypes) {
					auto     param = node.params[i];
					Variable var;

					var.name      = param;
					var.type      = UsedType(GetType(type.name), type.ptr);
					var.offset    = cast(uint) offset;
					var.stackSize = 2;
					offset       += var.Size();
					variables    ~= var;
				}

				// copy all parameters
				foreach_reverse (ref param ; node.params) {
					auto setNode = new SetNode(node.error);
					setNode.var  = param;
					CompileSet(setNode);
				}
			}

			foreach (ref inode ; node.nodes) {
				compiler.CompileNode(inode);
			}

			size_t scopeSize;
			foreach (ref var ; variables) {
				scopeSize += var.StackSize();
				
				if (var.type.hasDeinit && !var.type.ptr) {
					output ~= format(".vsp LDZ2 #.2x ADD2", var.offset);
					output ~= format("%s\n", ExtLabel(
						var.type.mod, "type_deinit_", "%s",
						var.type.name.Sanitise()
					));
				}
			}
			//output ~= format(".vsp LDZ2 #%.4x ADD2 .vsp STZ2\n", scopeSize);
			if (scopeSize > 0) {
				output ~= ".vsp LDZ2 ";

				switch (scopeSize) {
					case 1:  output ~= "INC2 "; break;
					case 2:  output ~= "INC2 INC2 "; break;
					case 3:  output ~= "INC2 INC2 INC2 "; break;
					default: output ~= format("#%.4x ADD2 ", scopeSize); break;
				}

				output ~= ".vsp STZ2\n";
			}

			output    ~= "JMP2r\n";
			variables  = [];
			inScope    = false;
		}

		output.FinishSection();
	}

	override void CompileIf(IfNode node) {
		++ blockCounter;
		auto blockNum = blockCounter;
		uint condCounter;

		foreach (i, ref condition ; node.condition) {
			foreach (ref inode ; condition) {
				compiler.CompileNode(inode);
			}
			output ~= format("#0000 EQU2 ;%s JCN2\n",
				Label("if_", "%d_%d", blockNum, condCounter + 1)
			);

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

				output ~= format(".vsp LDZ2 #.2x ADD2", var.offset);
				output ~= format("%s\n", ExtLabel(
					var.type.mod, "type_deinit_", "%s",
					var.type.name.Sanitise()
				));
			}
			if (GetStackSize() - oldSize > 0) {
				output ~= format(
					".vsp LDZ2 #%.4x ADD2 .vsp STZ2\n", GetStackSize() - oldSize
				);
			}
			variables = oldVars;

			output ~= format(";%s JMP2\n", Label("if_", "%d_end", blockNum));

			++ condCounter;
			output ~= format("@%s\n", Label("if_", "%d_%d", blockNum, condCounter));
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

				output ~= format(".vsp LDZ2 #.2x ADD2", var.offset);
				output ~= format("%s\n", ExtLabel(
					var.type.mod, "type_deinit_", "%s",
					var.type.name.Sanitise()
				));
			}
			if (GetStackSize() - oldSize > 0) {
				output ~= format(
					".vsp LDZ2 #%.4x ADD2 .vsp STZ2\n", GetStackSize() - oldSize
				);
			}
			variables = oldVars;
		}

		output ~= format("@%s\n", Label("if_", "%d_end", blockNum));
	}

	override void CompileWhile(WhileNode node) {
		++ blockCounter;
		uint blockNum = blockCounter;
		currentLoop   = blockNum;

		output ~= format(";%s JMP2\n", Label("while_", "%d_condition", blockNum));
		output ~= format("@%s\n", Label("while_", "%d", blockNum));

		// make scope
		auto oldVars = variables.dup;
		auto oldSize = GetStackSize();

		foreach (ref inode ; node.doWhile) {
			inWhile = true;
			compiler.CompileNode(inode);

			currentLoop = blockNum;
		}

		// remove scope
		output ~= format("@%s\n", Label("while_", "%d_next", blockNum));
		foreach (ref var ; variables) {
			if (oldVars.canFind(var)) continue;
			if (!var.type.hasDeinit || var.type.ptr) continue;

			output ~= format(".vsp LDZ2 #.2x ADD2", var.offset);
			output ~= format("%s\n", ExtLabel(
				var.type.mod, "type_deinit_", "%s",
				var.type.name.Sanitise()
			));
		}
		if (GetStackSize() - oldSize > 0) {
			output ~= format(
				".vsp LDZ2 #%.4x ADD2 .vsp STZ2\n", GetStackSize() - oldSize
			);
		}
		variables = oldVars;

		inWhile = false;

		output ~= format("@%s\n", Label("while_", "%d_condition", blockNum));
		
		foreach (ref inode ; node.condition) {
			compiler.CompileNode(inode);
		}

		output ~= "#0000 NEQ2\n";
		output ~= format(";%s JCN2\n", Label("while_", "%d", blockNum));
		output ~= format("@%s\n", Label("while_", "%d_end", blockNum));
	}

	override void CompileLet(LetNode node) {
		if (!TypeExists(node.varType.name)) {
			Error(node.error, "Undefined type '%s'", node.varType.name);
		}
		if (node.name != "") {
			if (VariableExists(node.name) || WordExistsHere(node.name)) {
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

			output ~= format(".vsp LDZ2 #%.4x SUB2 .vsp STZ2\n", var.Size());

			if (var.Size() == 1) {
				output ~= format("#00 .vsp LDZ2 STA\n");
			}
			else if (var.Size() == 2) {
				output ~= format("#0000 .vsp LDZ2 STA2\n");
			}

			if (var.name == "") {
				output ~= ".vsp LDZ2\n";
			}

			if (var.type.hasInit && !var.type.ptr) {
				output ~= format(
					".vsp LDZ2 %s\n",
					ExtLabel(var.type.mod, "type_init_", "%s", Sanitise(var.type.name))
				);
			}
		}
		else {
			Global global;
			global.mod         = output.GetModName();
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

			output.AddGlobal(global);
		}
	}

	override void CompileArray(ArrayNode node) {
		Array array;

		if (!TypeExists(node.arrayType.name)) {
			Error(node.error, "Type '%s' doesn't exist", node.arrayType.name);
		}
		if (CountTypes(node.arrayType.name) > 1) {
			Error(node.error, "Multiple matches for type '%s'", node.arrayType.name);
		}

		array.type = UsedType(GetType(node.arrayType.name), node.arrayType.ptr);

		foreach (ref elem ; node.elements) {
			switch (elem.type) {
				case NodeType.SignedInt:{
					auto node2    = cast(SignedIntNode) elem;

					final switch (array.type.Size()) {
						case 1: {
							array.values ~= format("%.2x", cast(ubyte) (cast(byte) node2.value));
							break;
						}
						case 2: {
							array.values ~= format("%.4x", cast(ushort) (cast(short) node2.value));
							break;
						}
					}
					break;
				}
				case NodeType.Integer: {
					auto node2    = cast(IntegerNode) elem;

					//array.values ~= node2.value.text();
					final switch (array.type.Size()) {
						case 1: array.values ~= format("%.2x", node2.value); break;
						case 2: array.values ~= format("%.4x", node2.value); break;
					}
					break;
				}
				default: {
					Error(elem.error, "Type '%s' can't be used in array literal", elem.type);
				}
			}
		}
		array.global  = !inScope || node.constant;
		arrays       ~= array;

		if (!inScope || node.constant) {
			output ~= format(";%s\n", Label("array_", "%d_meta", arrays.length - 1));
		}
		else {
			// allocate a copy of the array
			output ~= format(".vsp LDZ2 #%.4x SUB2 .vsp STZ2\n", array.Size());

			// copy array contents
			output ~= format("#%.4x ;memcpy/length STA2\n", array.Size());
			output ~= "#0000 ;memcpy/srcBank STA2\n";
			output ~= format(
				";%s ;memcpy/srcAddr STA2\n",
				Label("array_", "%d", arrays.length - 1)
			);
			output ~= "#0000 ;memcpy/dstBank STA2\n";
			output ~= ".vsp LDZ2 ;memcpy/dstAddr STA2\n";
			output ~= ";memcpy .System/expansion DEO2\n";

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
			var.type   = UsedType(GetType("core.Array"), false);
			var.offset = 0;
			var.array  = false;

			foreach (ref var2 ; variables) {
				var2.offset += var.Size();
			}

			variables ~= var;

			// save array address for later
			output ~= ".vsp LDZ2\n";
			// allocate metadata
			output ~= format(".vsp LDZ2 #%.4x SUB2 .vsp STZ2\n", 2 * 3);
			// length
			output ~= format("#%.4x .vsp LDZ2 STA2\n", array.values.length);
			// member size
			output ~= format("#%.4x .vsp LDZ2 INC2 INC2 STA2\n", array.type.Size());
			// elements
			output ~= ".vsp LDZ2 #0004 ADD2 STA2\n";

			// push metadata address
			output ~= ".vsp LDZ2\n";
		}
	}

	override void CompileString(StringNode node) {
		auto arrayNode = new ArrayNode(node.error);

		arrayNode.arrayType = new TypeNode(node.error, "core.u8", false);
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
			scopeSize += var.StackSize();

			if (var.type.hasDeinit && !var.type.ptr) {
				output ~= format(".vsp LDZ2 #.2x ADD2", var.offset);
				output ~= format("%s\n", ExtLabel(
					var.type.mod, "type_deinit_", "%s",
					var.type.name.Sanitise()
				));
			}
		}
		output ~= format(".vsp LDZ2 #%.4x ADD2 .vsp STZ2\n", scopeSize);
		output ~= "JMP2r\n";
	}

	override void CompileBreak(WordNode node) {
		if (!inWhile) {
			Error(node.error, "Not in while loop");
		}

		// TODO: fix variables here? forgot to end scope
		output ~= format(";%s JMP2\n", Label("while", "%d_end", currentLoop));
	}

	override void CompileContinue(WordNode node) {
		if (!inWhile) {
			Error(node.error, "Not in while loop");
		}

		// TODO: ditto
		output ~= format(";%s JMP2\n", Label("while", "%d_next", currentLoop));
	}

	override void CompileExtern(ExternNode node) {
		if (node.externType == ExternType.C) {
			Error(node.error, "This backend doesn't support C externs");
		}

		Word word;
		word.raw   = node.externType == ExternType.Raw;
		word.name  = node.func;
		words     ~= word;
	}

	override void CompileCall(WordNode node) {
		output ~= "JSR2\n";
	}

	override void CompileAddr(AddrNode node) {
		if (CountAll(node.func) > 1) {
			Error(node.error, "Multiple matches for identifier '%s'", node.func);
		}

		if (WordExists(node.func)) {
			auto   word   = GetWord(node.func);
			string symbol =
				word.raw? node.func : Label(word);

			output ~= format(";%s\n", symbol);
		}
		else if (VariableExists(node.func)) {
			auto var = GetVariable(node.func);

			if (var.offset == 0) {
				output ~= ".vsp LDZ2\n";
			}
			else {
				output ~= format(".vsp LDZ2 #%.4x ADD2\n", var.offset);
			}
		}
		else if (IsStructMember(node.func)) {
			string name      = node.func[0 .. node.func.countUntil(".")];
			auto   structVar = GetStructVariable(node, node.func);
			size_t offset    = structVar.offset;

			if (GlobalExists(name)) {
				auto var = GetGlobal(name);

				if (var.type.ptr) {
					output ~= format(
						";%s LDA2 #%.4x ADD2\n",
						ExtLabel(var.mod, "global_", "%s", name.Sanitise()), offset
					);
				}
				else {
					output ~= format(
						";%s #%.4x ADD2\n",
						ExtLabel(var.mod, "global_", "%s", name.Sanitise()), offset
					);
				}
			}
			else if (VariableExists(name)) {
				auto var = GetVariable(name);

				if (var.type.ptr) {
					output ~= format(
						".vsp LDZ2 #%.4x ADD2 LDA2 #%.4x ADD2\n", var.offset, offset
					);
				}
				else {
					output ~= format(".vsp LDZ2 #%.4x ADD2\n", var.offset + offset);
				}
			}
			else {
				Error(node.error, "Variable '%s' does not exist", name);
			}
		}
		else if (GlobalExists(node.func)) {
			auto var = GetGlobal(node.func);
			output ~= format(
				";global_%s\n",
				ExtLabel(var.mod, "global_", "%s", node.func.Sanitise())
			);
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
				labelName = Label("type_init_", "%s", Sanitise(node.structure));
				break;
			}
			case "deinit": {
				if (GetType(node.structure).hasDeinit) {
					Error(node.error, "Already implemented in type");
				}

				type.hasDeinit = true;
				labelName = Label("type_deinit_", "%s", Sanitise(node.structure));
				break;
			}
			default: Error(node.error, "Unknown method '%s'", node.method);
		}

		SetType(type.name, type);

		assert(!inScope);
		inScope = true;

		output.StartSection(SectionType.Implement);
		if (output.mode == OutputMode.Module) {
			auto sect   = cast(ImplementSection) output.sect;
			sect.type   = type.name;
			sect.method = node.method;
		}

		output ~= format("@%s\n", labelName);

		foreach (ref inode ; node.nodes) {
			compiler.CompileNode(inode);
		}

		size_t scopeSize;
		foreach (ref var ; variables) {
			scopeSize += var.StackSize();

			if (var.type.hasDeinit && !var.type.ptr) {
				output ~= format(".vsp LDZ2 #.2x ADD2", var.offset);
				output ~= format("%s\n", ExtLabel(
					var.type.mod, "type_deinit_", "%s",
					var.type.name.Sanitise()
				));
			}
		}
		if (scopeSize > 0) {
			output ~= ".vsp LDZ2 ";

			switch (scopeSize) {
				case 1:  output ~= "INC2 "; break;
				case 2:  output ~= "INC2 INC2 "; break;
				case 3:  output ~= "INC2 INC2 INC2 "; break;
				default: output ~= format("#%.4x ADD2 ", scopeSize); break;
			}

			output ~= ".vsp STZ2\n";
		}

		output    ~= "JMP2r\n";
		inScope    = false;
		variables  = [];

		output.FinishSection();
	}

	void SetVariable(
		Node node, Variable var, size_t size = 0, size_t offset = 0, bool member = false,
		bool deref = false
	) {
		if (size == 0) {
			size = var.type.Size();
		}

		string where = ".vsp LDZ2";

		if (var.offset != 0) {
			where ~= format(" #%.4x ADD2", var.offset);
		}
		if (deref) {
			where ~= " LDA2";
		}
		if (offset != 0) {
			where ~= format(" #%.4x ADD2", offset);
		}

		switch (size) {
			case 1: output ~= format("NIP %s STA\n", where); break;
			case 2: output ~= format("%s STA2\n", where); break;
			default: Error(node.error, "Bad variable type size");
		}
	}

	void SetGlobal(
		Node node, Global global, size_t size = 0, size_t offset = 0, bool member = false,
		bool deref = false
	) {
		if (size == 0) {
			size = global.type.Size();
		}

		string symbol = ExtLabel(global.mod, "global_", "%s", global.name.Sanitise());

		if (deref) {
			symbol ~= format(" LDA2");
		}
		if (offset != 0) {
			symbol ~= format(" #%.4x ADD2", offset);
		}

		switch (size) {
			case 1: output ~= format("NIP ;%s STA\n", symbol); break;
			case 2: output ~= format(";%s STA2\n", symbol); break;
			default: Error(node.error, "Bad variable type size");
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
		if (!WordExists(node.func)) {
			Error(node.error, "Function '%s' doesn't exist", node.func);
		}
		if (CountWords(node.func) > 1) {
			Error(node.error, "Multiple matches for function '%s'", node.func);
		}

		auto word = GetWord(node.func);

		if (!word.error) {
			Error(node.error, "Function '%s' doesn't throw", node.func);
		}
		if (word.raw) {
			Error(node.error, "Non-callisto functions can't throw");
		}

		size_t paramSize = word.params.length * 2;

		if (word.params.length > 0) {
			output ~= format(
				"LITr -System/wst DEIr LITr %.2x SUBr\n", word.params.length * 2
			);
		}
		else {
			output ~= "LITr -System/wst DEIr\n";
		}

		if (word.inline) {
			foreach (inode ; word.inlineNodes) {
				compiler.CompileNode(inode);
			}
		}
		else {
			output ~= format("%s\n", Label(word));
		}

		output ~= "LITr -temp STZr\n";

		++ blockCounter;

		output ~= ";exception LDA2 #0000 EQU2\n";
		output ~= format(";%s JCN2\n", Label("catch_", "%d_end", blockCounter));

		// function errored, assume that all it did was consume parameters
		output ~= ".temp LDZ .System/wst DEO\n";

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

			output ~= format(".vsp LDZ2 #.2x ADD2", var.offset);
			output ~= format("%s\n", ExtLabel(
				var.type.mod, "type_deinit_", "%s",
				var.type.name.Sanitise()
			));
		}
		if (GetStackSize() - oldSize > 0) {
			output ~= format(
				".vsp LDZ2 #%.4x ADD2 .vsp STZ2\n", GetStackSize() - oldSize
			);
		}
		variables = oldVars;

		output ~= format("@%s\n", Label("catch", "%d_end", blockCounter));
		output ~= "POPr\n";
	}

	override void CompileThrow(WordNode node) {
		if (!inScope || (!GetWord(thisFunc).error)) {
			Error(node.error, "Not in a function that can throw");
		}
		if (GetWord(thisFunc).inline) {
			Error(node.error, "Can't use throw in an inline function");
		}

		// set exception error
		output ~= "#ffff ;exception STA2\n";

		// copy exception message
		output ~= "#0006 ;memcpy/length STA2\n";
		output ~= "#0000 ;memcpy/srcBank STA2\n";
		output ~= ";memcpy/srcAddr STA2\n";
		output ~= "#0000 ;memcpy/dstBank STA2\n";
		output ~= ";exception INC2 INC2 ;memcpy/dstAddr STA2\n";
		output ~= ";memcpy .System/expansion DEO2\n";

		CompileReturn(node);
	}
}
