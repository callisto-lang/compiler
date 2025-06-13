module callisto.stub;

import callisto.output;
import callisto.parser;
import callisto.mod.mod;
import callisto.compiler;
import callisto.mod.sections;
import callisto.backends.x86_64;

class StubCompiler {
	WriteModule     mod;
	CompilerBackend backend;
	Compiler        compiler;

	this(string path, ModCPU cpu, ModOS os, string source, string dest) {
		mod = new WriteModule(path, cpu, os, source, dest);

		switch (cpu) {
			case ModCPU.x86_64: backend = new BackendX86_64(); break;
			default: assert(0);
		}

		switch (os) {
			case ModOS.Linux:   backend.os = "linux";   break;
			case ModOS.macOS:   backend.os = "osx";     break;
			case ModOS.DOS:     backend.os = "dos";     break;
			case ModOS.FreeBSD: backend.os = "freebsd"; break;
			default: break;
		}

		compiler         = new Compiler();
		backend.compiler = compiler;
		backend.output   = new Output(path, cpu, os, source, dest);
	}

	void CompileFuncDef(FuncDefNode node) => mod.Add(FuncDefSection.FromNode(node));

	void CompileLet(LetNode node) {
		backend.CompileLet(node);
		mod.Add(LetSection.FromGlobal(backend.GetGlobal(node.name)));
	}

	void CompileEnable(EnableNode node) => mod.Add(EnableSection.FromNode(node));

	void CompileStruct(StructNode node) {
		auto sect = new StructSection();
		sect.name = node.name;

		backend.CompileStruct(node);
		auto type = backend.GetType(node.name);

		assert(type.isStruct);
		foreach (ref member ; type.structure) {
			sect.entries ~= ModStructEntry(
				member.type.ptr, member.type.name, member.array,
				cast(SectionInt) member.size, cast(SectionInt) member.offset,
				member.name
			);
		}

		sect.size = type.size;
		mod.Add(sect);
	}

	void CompileConst(ConstNode node) => mod.Add(ConstSection.FromNode(node));

	void CompileEnum(EnumNode node) => mod.Add(EnumSection.FromNode(node));

	void CompileRestrict(RestrictNode node) => mod.Add(RestrictSection.FromNode(node));

	void CompileUnion(UnionNode node) {
		auto sect = new UnionSection();

		backend.CompileUnion(node);
		auto type = backend.GetType(node.name);

		sect.name = node.name;
		sect.size = type.size;
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
