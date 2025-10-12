module callisto.profiler;

import std.array;
import std.stdio;
import std.algorithm;
import std.datetime.stopwatch;

struct Result {
	string name;
	double time;
}

class Profiler {
	Result[]  results;
	StopWatch sw;

	void Begin() {
		sw = StopWatch(AutoStart.no);
		sw.start();
	}

	void End(string name) {
		sw.stop();
		results ~= Result(name, sw.peek.total!("msecs"));
	}

	void PrintResults() {
		size_t spaces = results.fold!(
			(a, e) => e.name.length > a.name.length? e : a
		).name.length;

		foreach (result ; results) {
			writefln(
				"%s: %s%sms",
				result.name, replicate([' '], spaces - result.name.length), result.time
			);
		}
	}
}
