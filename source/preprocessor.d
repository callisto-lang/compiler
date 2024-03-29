module callisto.preprocessor;

import std.file;
import std.path;
import std.stdio;
import std.format;
import std.algorithm;
import callisto.util;
import callisto.error;
import callisto.parser;
import callisto.language;

Node[] Preprocessor(Node[] nodes, string[] includeDirs, ref string[] included) {
	Node[] ret;

	foreach (ref inode ; nodes) {
		if (inode.type == NodeType.Include) {
			auto node = cast(IncludeNode) inode;
			auto path = format("%s/%s", dirName(node.error.file), node.path);

			if (!exists(path)) {
				bool found;
				
				foreach (ref ipath ; includeDirs) {
					path = format("%s/%s", ipath, node.path);

					if (exists(path)) {
						found = true;
						break;
					}
				}

				if (!found) {
					ErrorBegin(node.error);
					stderr.writefln("Can't find file '%s'", node.path);
					exit(1);
				}
			}

			if (included.canFind(path)) {
				continue;
			}

			included ~= path;

			ret ~= Preprocessor(ParseFile(path), includeDirs, included);
		}
		else {
			ret ~= inode;
		}
	}

	return ret;
}
