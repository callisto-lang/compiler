module callisto.backends.uxn;

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

class BackendUXN : CompilerBackend {
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
	}

	override string[] GetVersions() => [
		// platform
		"UXN", "BigEndian", "16Bit",
		// features
		"IO"
	];

	override string[] FinalCommands() => [
		format("mv %s %s.tal", compiler.outFile, compiler.outFile),
		format("uxnasm %s.tal %s", compiler.outFile, compiler.outFile),
		format("rm %s.tal", compiler.outFile)
	];

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

	override void BeginMain() {
		output ~= "@calmain\n";
	}

	override void Init() {
		output ~= "|0 @vsp $2\n";
		output ~= "|10 @Console &vector $2 &read $1 &pad $5 &write $1 &error $1\n";
		output ~= "|100\n";
		output ~= "@on-reset\n";
		output ~= "    #ffff .vsp STZ2";
		output ~= "    calmain\n";
		output ~= "    BRK\n";
	}

	override void End() {
		output ~= "JMP2r\n";

		foreach (name, var ; globals) {
			output ~= format("@global_%s", name);

			foreach (i ; 0 .. var.Size()) {
				output ~= " #00";
			}

			output ~= "\n";
		}

		// pad for the stack
		output ~= "|e0000\n";
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
					output ~= format("%s\n", node.name);
				}
				else {
					output ~= format("func__%s\n", node.name.Sanitise());
				}
			}
		}
		else if (VariableExists(node.name)) {
			auto var  = GetVariable(node.name);
			output   ~= format(".vsp LDZ2 #%.4X ADD2\n", var.offset);
		}
		else if (node.name in globals) {
			output ~= format(";global_%s\n", node.name.Sanitise());
		}
		else if (node.name in consts) {
			compiler.CompileNode(consts[node.name].value);
		}
		else {
			Error(node.error, "Undefined identifier '%s'", node.name);
		}
	}

	override void CompileInteger(IntegerNode node) {
		output ~= format("#%.4x\n", node.value);
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

			words[node.name] = Word(false, false, []);

			output ~= format("@func__%s\n", node.name.Sanitise());

			foreach (ref inode ; node.nodes) {
				compiler.CompileNode(inode);
			}

			size_t scopeSize;
			foreach (ref var ; variables) {
				scopeSize += var.Size();
			}
			output ~= format(".vsp LDZ2 #%.4x ADD2 .vsp STZ2\n", scopeSize);

			output    ~= "JMP2r\n";
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
			output ~= format("#0000 EQU2 ;if_%d_%d JCN2\n", blockNum, condCounter + 1);

			// create scope
			auto oldVars = variables.dup;
			auto oldSize = GetStackSize();

			foreach (ref inode ; node.doIf[i]) {
				compiler.CompileNode(inode);
			}

			// remove scope
			if (GetStackSize() - oldSize > 0) {
				output ~= format(
					".vsp LDZ2 #%.4x ADD .vsp STZ2\n", GetStackSize() - oldSize
				);
			}
			variables = oldVars;

			output ~= format(";if_%d_end JMP2\n", blockNum);

			++ condCounter;
			output ~= format("@if_%d_%d\n", blockNum, condCounter);
		}

		if (node.hasElse) {
			foreach (ref inode ; node.doElse) {
				compiler.CompileNode(inode);
			}
		}

		output ~= format("@if_%d_end\n", blockNum);
	}

	override void CompileWhile(WhileNode node) {
		++ blockCounter;
		uint blockNum = blockCounter;
		currentLoop   = blockNum;

		output ~= format(";while_%d_condition JMP2\n", blockNum);
		output ~= format("@while_%d\n", blockNum);

		// make scope
		auto oldVars = variables.dup;
		auto oldSize = GetStackSize();

		foreach (ref inode ; node.doWhile) {
			inWhile = true;
			compiler.CompileNode(inode);

			currentLoop = blockNum;
		}

		// remove scope
		if (GetStackSize() - oldSize > 0) {
			output ~= format(
				".vsp LDZ2 #%.4x ADD .vsp STZ2\n", GetStackSize() - oldSize
			);
		}
		variables = oldVars;

		inWhile = false;

		output ~= format("@while_%d_condition\n", blockNum);
		
		foreach (ref inode ; node.condition) {
			compiler.CompileNode(inode);
		}

		output ~= "#0000 NEQ2\n";
		output ~= format(";while_%d JCN2\n", blockNum);
		output ~= format("@while_%d_end\n", blockNum);
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

			output ~= format(";vsp LDA2 #%.4X SUB2 ;vsp STA2\n", var.Size());

			if (var.Size() == 1) {
				output ~= format("#00 ;vsp LDA2 STA\n");
			}
			else if (var.Size() == 2) {
				output ~= format("#0000 ;vsp LDA2 STA2\n");
			}
		}
		else {
			Global global;
			global.type        = types[node.varType];
			globals[node.name] = global;
		}
	}

	override void CompileArray(ArrayNode node) {
		
	}

	override void CompileString(StringNode node) {
		
	}

	override void CompileStruct(StructNode node) {
		
	}

	override void CompileReturn(WordNode node) {
		
	}

	override void CompileConst(ConstNode node) {
		
	}

	override void CompileEnum(EnumNode node) {
		
	}

	override void CompileBreak(WordNode node) {
		
	}

	override void CompileContinue(WordNode node) {
		
	}

	override void CompileUnion(UnionNode node) {
		
	}

	override void CompileAlias(AliasNode node) {
		
	}

	override void CompileExtern(ExternNode node) {
		
	}

}
