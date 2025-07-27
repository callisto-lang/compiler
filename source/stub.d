module callisto.stub;

import callisto.output;
import callisto.parser;
import callisto.mod.mod;
import callisto.compiler;
import callisto.mod.sections;
import callisto.backends.x86_64;

class StubCompiler {
	WriteModule     mod;
	Compiler        compiler;

	this(string path, ModCPU cpu, ModOS os, string source, string dest) {
		mod = new WriteModule(path, cpu, os, source, dest);
	}

	void CompileFuncDef(FuncDefNode node) => mod.Add(FuncDefSection.FromNode(node));

	void CompileLet(LetNode node) {
		auto sect  = new LetSection();
		sect.array = node.array;
		sect.size  = cast(SectionInt) node.arraySize;
		sect.ptr   = node.varType.ptr;
		sect.type  = node.varType.name;
		sect.name  = node.name;
		mod.Add(sect);
	}

	void CompileEnable(EnableNode node) => mod.Add(EnableSection.FromNode(node));

	void CompileStruct(StructNode node) {
		auto sect     = new StructSection();
		sect.name     = node.name;
		sect.inherits = node.inherits? node.inheritsFrom : "";

		foreach (ref member ; node.members) {
			sect.entries ~= ModStructEntry(
				member.type.ptr, member.type.name, member.array,
				cast(SectionInt) member.size, member.name
			);
		}
		mod.Add(sect);
	}

	void CompileConst(ConstNode node) => mod.Add(ConstSection.FromNode(node));

	void CompileEnum(EnumNode node) => mod.Add(EnumSection.FromNode(node));

	void CompileRestrict(RestrictNode node) => mod.Add(RestrictSection.FromNode(node));

	void CompileUnion(UnionNode node) {
		auto sect = new UnionSection();
		sect.name  = node.name;
		sect.types = node.types;
		mod.Add(sect);
	}

	void CompileAlias(AliasNode node) => mod.Add(AliasSection.FromNode(node));

	void Compile(Node[] nodes) {
		foreach (ref node ; nodes) {
			switch (node.type) {
				case NodeType.FuncDef: CompileFuncDef(cast(FuncDefNode) node); break;
				case NodeType.Let: CompileLet(cast(LetNode) node); break;
				case NodeType.Enable: CompileEnable(cast(EnableNode) node); break;
				//case NodeType.Requires: CompileRequires(cast(RequiresNode) node); break;
				case NodeType.Struct: CompileStruct(cast(StructNode) node); break;
				case NodeType.Const: CompileConst(cast(ConstNode) node); break;
				case NodeType.Enum: CompileEnum(cast(EnumNode) node); break;
				case NodeType.Restrict: CompileRestrict(cast(RestrictNode) node); break;
				case NodeType.Union: CompileUnion(cast(UnionNode) node); break;
				case NodeType.Alias: CompileAlias(cast(AliasNode) node); break;
				//case NodeType.Extern: break;
				default: break;
			}
		}

		mod.Finish();
	}
}
