# Callisto
Callisto is a reverse polish notation programming
language inspired by YSL-C3 and Forth

## Build
```
dub build
```

## Try it
Currently whatever is being tested is in
test.cal, compile it with these commands:
```
./cac test.cal --org 100 -i .
nasm -f bin out.asm -o out.com
```
And then run with `dosbox out.com`
