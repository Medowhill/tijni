EXAMPLE_C_FILES = $(shell find . -type f -name '*.c')
EXAMPLE_LL_FILES = $(patsubst %.c, %.ll, $(EXAMPLE_C_FILES))

all: $(EXAMPLE_LL_FILES)

%.ll: %.c
	clang -w -c -emit-llvm -S -fno-discard-value-names -O0 -Xclang -disable-O0-optnone -o $*.tmp.ll $< -g
	opt -mem2reg -S -o $*.ll $*.tmp.ll
	rm -f $*.tmp.ll

clean:
	rm -f *.ll
