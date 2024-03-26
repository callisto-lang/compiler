# Callisto
Callisto is a reverse polish notation programming
language inspired by YSL-C3 and Forth

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
cac examples/exampleNameHere.cal --org 100 -i std
nasm -f bin out.asm -o out.com
dosbox out.com
```
