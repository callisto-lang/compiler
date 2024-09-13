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

class Preprocessor {
	string[] includeDirs;
	string[] included;
	string[] versions;
	string[] restricted;
	string[] disabled;
	bool     success = true;

	final void Error(Char, A...)(ErrorInfo error, in Char[] fmt, A args) {
		ErrorBegin(error);
		stderr.writeln(format(fmt, args));
		PrintErrorLine(error);
		success = false;
	}

	Node[] Run(Node[] nodes) {
		Node[] ret;

		foreach (ref inode ; nodes) {
			switch (inode.type) {
				case NodeType.Include: {
					auto node = cast(IncludeNode) inode;
					auto path = format("%s/%s", dirName(node.error.file), node.path);

					if (!exists(path)) {
						auto oldPath = path;
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
							stderr.writefln(
								"Can't find file '%s', tried '%s' and in include paths",
								node.path, oldPath
							);
							exit(1);
						}
					}
					
					path = path.absolutePath().buildNormalizedPath();

					if (included.canFind(path)) {
						continue;
					}

					included ~= path;

					ret ~= Run(ParseFile(path));
					break;
				}
				case NodeType.Version: {
					auto node = cast(VersionNode) inode;
					bool cond = versions.canFind(node.ver);

					if (node.not) cond = !cond;

					if (cond) {
						//ret ~= node.block;
						ret ~= Run(node.block);
					}
					break;
				}
				case NodeType.Enable: {
					auto node = cast(EnableNode) inode;

					if (restricted.canFind(node.ver)) {
						Error(
							node.error, "Attempted to enable restricted version '%s'",
							node.ver
						);
					}

					if (disabled.canFind(node.ver)) break;

					if (!versions.canFind(node.ver)) {
						versions ~= node.ver;
					}
					break;
				}
				case NodeType.Restrict: {
					auto node = cast(RestrictNode) inode;

					restricted ~= node.ver;
					break;
				}
				case NodeType.FuncDef: {
					auto node   = cast(FuncDefNode) inode;
					node.nodes  = Run(node.nodes);
					ret        ~= node;
					break;
				}
				case NodeType.If: {
					auto node = cast(IfNode) inode;

					foreach (ref condition ; node.condition) {
						condition = Run(condition);
					}

					foreach (ref doIf ; node.doIf) {
						doIf = Run(doIf);
					}

					if (node.hasElse) {
						node.doElse = Run(node.doElse);
					}
					ret ~= node;
					break;
				}
				case NodeType.While: {
					auto node = cast(WhileNode) inode;

					node.condition = Run(node.condition);
					node.doWhile   = Run(node.doWhile);

					ret ~= node;
					break;
				}
				case NodeType.Array: {
					auto node = cast(ArrayNode) inode;

					node.elements = Run(node.elements);

					ret ~= node;
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
