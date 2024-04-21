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

class PreprocessorError : Exception {
	this() {
		super("", "", 0);
	}
}

class Preprocessor {
	string[] includeDirs;
	string[] included;
	string[] versions;

	final void Error(Char, A...)(ErrorInfo error, in Char[] fmt, A args) {
		ErrorBegin(error);
		stderr.writeln(format(fmt, args));
		throw new PreprocessorError();
	}

	Node[] Run(Node[] nodes) {
		Node[] ret;

		foreach (ref inode ; nodes) {
			switch (inode.type) {
				case NodeType.Include: {
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

					ret ~= Run(ParseFile(path));
					break;
				}
				case NodeType.Version: {
					auto node = cast(VersionNode) inode;

					if (versions.canFind(node.ver)) {
						//ret ~= node.block;
						ret ~= Run(node.block);
					}
					break;
				}
				case NodeType.Enable: {
					auto node = cast(EnableNode) inode;

					if (!versions.canFind(node.ver)) {
						versions ~= node.ver;
					}
					break;
				}
				default: {
					ret ~= inode;
				}
			}
		}

		return ret;
	}
}
