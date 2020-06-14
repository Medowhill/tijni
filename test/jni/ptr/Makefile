NAME=Ptr

all:
	make java && make c && make run

java: ${NAME}.java
	javac -h . ${NAME}.java

c: ${NAME}.c ${NAME}.h
	gcc -c \
		-I/Library/Java/JavaVirtualMachines/jdk-11.0.6.jdk/Contents/Home/include \
		-I//Library/Java/JavaVirtualMachines/jdk-11.0.6.jdk/Contents/Home/include/darwin \
		${NAME}.c && \
	gcc -dynamiclib -shared -o lib${NAME}.dylib ${NAME}.o

llvm: ${NAME}.c ${NAME}.h
	clang -c \
		-I/Library/Java/JavaVirtualMachines/jdk-11.0.6.jdk/Contents/Home/include \
		-I//Library/Java/JavaVirtualMachines/jdk-11.0.6.jdk/Contents/Home/include/darwin \
		-emit-llvm -S -fno-discard-value-names -Xclang -disable-O0-optnone -g \
		-o ${NAME}.tmp.ll \
		${NAME}.c && \
	opt -mem2reg -S -o ${NAME}.ll ${NAME}.tmp.ll && \
	rm ${NAME}.tmp.ll

run:
	java -Djava.library.path=. ${NAME}

clean:
	rm *.class *.h *.o *.dylib *.ll
