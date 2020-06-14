EXAMPLE_C_FILES = $(shell find . -type f -name '*.c')
EXAMPLE_LL_FILES = $(patsubst %.c, %.ll, $(EXAMPLE_C_FILES))
EXAMPLE_SUMMARY_FILES = $(patsubst %.ll, %.summary, $(EXAMPLE_LL_FILES))

EXAMPLE_JAVA_FILES = $(shell find . -type f -name '*.java')
EXAMPLE_CLASS_FILES = $(patsubst %.java, %.class, $(EXAMPLE_JAVA_FILES))

all:
	make java && make llvm && make summary

java: $(EXAMPLE_CLASS_FILES)

%.class: %.java
	javac -h . $*.java

llvm: $(EXAMPLE_LL_FILES)

%.ll: %.c
	clang -c \
		-I/Library/Java/JavaVirtualMachines/jdk-11.0.6.jdk/Contents/Home/include \
		-I//Library/Java/JavaVirtualMachines/jdk-11.0.6.jdk/Contents/Home/include/darwin \
		-emit-llvm -S -fno-discard-value-names -Xclang -disable-O0-optnone -g \
		-o $*.tmp.ll \
		$*.c && \
	opt -mem2reg -S -o $*.ll $*.tmp.ll && \
	rm $*.tmp.ll

summary: $(EXAMPLE_SUMMARY_FILES)

%.summary: %.ll
	../../llvm-analyzer/analyzer $*.ll $*.summary

clean:
	rm -f *.class *.h *.o *.dylib *.ll *.summary
