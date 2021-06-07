# jacc

An OpenACC transpiler which enables JIT compilation with dynamic trace.

## Requirements
* Gauche (>= 0.9.8)
* Omni Compiler (>= 1.3.2)
* PGI compiler (>= 19.4)

## Usage
```
% ./tools/c-to-xcodeml input.c
% gosh main.scm < input.xml > output.xml
% ./tools/xcodeml-to-c output.xml
% pgcc -acc output.c
```
