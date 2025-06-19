module callisto.state;

import callisto.mod.sections;

State state;

struct Function {
	string         mod;
	FuncDefSection sect;
}

class State {
	ModCPU            cpu;
	ModOS             os;
	Function[]        funcs;
	TopLevelSection[] topLevel; // exists here so it can be written in the correct order

	this() {
		
	}

	void Import(Module mod) {
		foreach (ref isect ; mod.sections) {
			switch (isect.GetType()) {
				case SectionType.TopLevel: {
					topLevel ~= cast(TopLevelSection) isect;
					break;
				}
			}
		}
	}
}
