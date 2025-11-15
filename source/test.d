module callisto.test;

import std.stdio;
import callisto.mod.sections;

int TestProgram() {
	writeln("cac1 test program");

	auto file     = File("test.bin", "wb");
	auto sect     = new TopLevelSection();
	sect.calls    = ["test"];
	sect.assembly = "foobar 2000\n";
	sect.Write(file);

	return 0;
}
