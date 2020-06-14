# tjni

A static analyzer to detect use-after-free and double-free bugs in JNI programs.

## Personal Information

* Name: Jaemin Hong (홍재민)
* Student ID: 20203698

## Prerequisites

* macOS Catalina
(Otherwise, modification of `test/jni/Makefile` might be required to run tests.) 
* JDK 11.0.6
(Otherwise, modification of `test/jni/Makefile` might be required to run tests.) 
* Scala 2.13.2
* SBT 1.3.10
* OCaml 4.08.0
* Clang 11.0.3
* LLVM 9.0.1

## Build

```bash
make
```

## Test

```bash
make test
```

## Run

### C Analyzer

```bash
cd llvm-analyzer
clang -w -c -emit-llvm -S -fno-discard-value-names -O0 -Xclang -disable-O0-optnone -o [file].tmp.ll [file].c
opt -mem2reg -S -o [file].ll [file].tmp.ll
./analyzer [file].ll
```

### Java Analyzer

```bash
cd bytecode-analyzer
javac [file].java
sbt run --java-analyze [file].class
```

### JNI Analyzer

```bash
cd bytecode-analyzer
javac -h . [file].java
# macOS Catalina with JDK 11.0.6
clang -c \
	-I/Library/Java/JavaVirtualMachines/jdk-11.0.6.jdk/Contents/Home/include \
	-I//Library/Java/JavaVirtualMachines/jdk-11.0.6.jdk/Contents/Home/include/darwin \
	-emit-llvm -S -fno-discard-value-names -Xclang -disable-O0-optnone -g \
	-o [file].tmp.ll \
	[file].c && \
opt -mem2reg -S -o [file].ll [file].tmp.ll && \
../llvm-analyzer/analyzer [file].ll [file].summary
sbt run --jni-analyze [file].class [file].summary
```
