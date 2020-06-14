.PHONY: all test clean

all:
	make -C llvm-analyzer && \
	make -C bytecode-analyzer

clean:
	make -C test clean && \
	make -C llvm-analyzer clean && \
	make -C bytecode-analyzer clean

test:
	make -C test && \
	make -C test test && \
	make -C bytecode-analyzer test
