# The Callisto compiler
Callisto is a stack-based imperative programming language with stack safety enforced
by the compiler (which is optional).

It also has a subset called CallistoScript made to compile to scripting languages like
Lua, while keeping its low level features (like direct access to memory).

## Supported targets
- x86 real mode - MS-DOS, bare metal
- x86_64 - Linux, macOS, FreeBSD (partial)
- ARM64 - Linux, macOS
- Uxn - Varvara
- Lua

## Build
```
dub build
```

The compiler executable will be called `cac`

## Run example programs
Make sure you get the standard library

```
git submodule update --remote
```

Then compile example programs like this

```
cac examples/exampleNameHere.cal -i std
```

The output executable will be called `out`. Any example programs that require extra flags
for compilation will have a compile command in a comment at the top of the source file.

## Community
Join `#callisto-lang` on irc.libera.chat.
