# JACC

Is an OpenACC transpiler which enables JIT compilation with dynamic trace.

Developed by Kazuaki Matsumura from the Accelerators and Communications for High Performance Computing team at BSC

## Requirements
* Gauche (>= 0.9.8)
* XcodeML-tools (See tools/ for the detail)
* PGI compiler (>= 19.4)

## Usage
```
% ./tools/c-to-xcodeml input.c
% gosh main.scm < input.xml > output.xml
% ./tools/xcodeml-to-c output.xml
% pgcc -acc output.c
```
