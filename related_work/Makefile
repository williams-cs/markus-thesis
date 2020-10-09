all: annotated-bibliography.pdf

annotated-bibliography.pdf: annotated-bibliography.tex refs.bib
	pdflatex annotated-bibliography.tex
	bibtex annotated-bibliography
	pdflatex annotated-bibliography.tex
	pdflatex annotated-bibliography.tex

.PHONY: clean
clean:
	@rm -f *.aux *.bbl *.log *.blg *~
	@rm -f annotated-bibliography.pdf
