thesis.pdf: thesis.tex references.bib
	pdflatex thesis.tex
	bibtex thesis
	pdflatex thesis.tex

clean:
	@rm -f *.aux
	@rm -f *.log
	@rm -f *.lot
	@rm -f *.lof
	@rm -f *.bbl
	@rm -f *.toc
	@rm -f thesis.pdf
	@rm -f *~
