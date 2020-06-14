all: proj.tex ref.bib
	pdflatex proj && \
	bibtex proj && \
	pdflatex proj && \
	pdflatex proj

clean:
	rm *.aux *.bbl *.blg *.log *.out
