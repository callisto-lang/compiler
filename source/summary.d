module callisto.summary;

import callisto.mod.mod;
import callisto.compiler;
import callisto.mod.sections;

struct Summary {
	ModCPU    cpu;
	ModOS     os;
	Section[] sections;
}
