# Callisto
Callisto is a reverse polish notation programming
language inspired by YSL-C3 and Forth

It also has a subset called CallistoScript made for scripting languages like Lua, and
it's still sort of low level

## Supported targets
- x86 real mode, MS-DOS (complete)
- x86_64 Linux, macOS (complete)
- ARM64 Linux (complete)
- Uxn (complete, `implement` broken, see #6)
- Lua (complete, subset CallistoScript)

## Build
You need a [D compiler](https://dlang.org/download.html) to build the Callisto compiler
```
dub build
```

## Try it
Note: to use the example programs, you will need the `std` submodule in this repository,
which you can get by cloning recursively or
doing `git submodule update --init --remote --recursive`

There are some example programs in the `examples` folder, which you can compile
and run like this:
```
cac examples/exampleNameHere.cal -i std -o out
./out
```

To learn more about Callisto, read the [docs](https://callisto.mesyeti.uk/docs)

## Support/talk
`#callisto-lang` on irc.libera.chat
