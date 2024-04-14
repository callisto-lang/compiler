# Callisto
Callisto is a reverse polish notation programming
language inspired by YSL-C3 and Forth

## Supported targets
- x86 real mode (complete)
- YETI-16 Mk2 (in progress)
- x86_64 Linux (complete)

## Build
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
