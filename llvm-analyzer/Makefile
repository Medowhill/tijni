all:
	dune build src/main.exe
	ln -s -f _build/default/src/main.exe analyzer

clean:
	dune clean
	rm -f analyzer
