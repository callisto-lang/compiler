# Callisto 🚀

Callisto is a reverse Polish notation programming language, inspired by YSL-C3 and Forth. It also includes a subset called CallistoScript, designed for scripting languages like Lua, while still maintaining a relatively low-level approach.

## Supported Targets 🛠️

- **x86 Real Mode, MS-DOS:** Fully supported ✅
- **x86_64 Linux, macOS:** Fully supported ✅
- **ARM64 Linux:** Fully supported ✅
- **Uxn:** Fully supported (note: `implement` is broken, see [issue #6](#6)) ⚠️
- **Lua:** Fully supported (subset: CallistoScript) 📝

## Build Instructions 🧑‍💻

To build the Callisto compiler, you will need a [D compiler](https://dlang.org/download.html). Once installed, build the project using the following command:

```
dub build
```

## Getting Started 🎉

### Running Example Programs

Before running example programs, ensure that the `std` submodule is included in your repository. You can achieve this by cloning the repository recursively or running the following command:

```
git submodule update --init --remote --recursive
```

Example programs are located in the `examples` directory. To compile and run an example, use:

```
cac examples/<exampleName>.cal -i std -o out
./out
```

For more detailed information about Callisto, please refer to the [official documentation](https://callisto.mesyeti.uk/docs).

## Community and Support 💬

For support or discussions, join the `#callisto-lang` channel on [irc.libera.chat](https://libera.chat).
