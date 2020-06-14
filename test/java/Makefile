EXAMPLE_JAVA_FILES = $(shell find . -type f -name '*.java')
EXAMPLE_CLASS_FILES = $(patsubst %.java, %.class, $(EXAMPLE_JAVA_FILES))

all: $(EXAMPLE_CLASS_FILES)

%.class: %.java
	javac $*.java

clean:
	rm -f *.class
