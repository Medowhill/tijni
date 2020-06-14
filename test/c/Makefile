EXAMPLE_C_FILES = $(shell find . -type f -name '*.c')
EXAMPLE_LL_FILES = $(patsubst %.c, %.ll, $(EXAMPLE_C_FILES))
EXAMPLE_SUMMARY_FILES = $(patsubst %.ll, %.summary, $(EXAMPLE_LL_FILES))

all: $(EXAMPLE_LL_FILES)

%.ll: %.c
	clang -w -c -emit-llvm -S -fno-discard-value-names -O0 -Xclang -disable-O0-optnone -o $*.tmp.ll $< -g
	opt -mem2reg -S -o $*.ll $*.tmp.ll
	rm -f $*.tmp.ll

test: $(EXAMPLE_SUMMARY_FILES)

%.summary: %.ll
	../../llvm-analyzer/analyzer $*.ll $*.summary
	diff $*.summary $*.summary.expected

clean:
	rm -f *.ll *.sumary
