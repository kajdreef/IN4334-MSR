PAPER=main

#FIGURES=figures/*

#view: 	${PAPER}.pdf
#	okular ${PAPER}.pdf

${PAPER}.pdf: ${PAPER}.tex $(shell ls *.tex *.bib) $(FIGURES)

	pdflatex ${PAPER}
	pdflatex ${PAPER}
	bibtex ${PAPER}
	pdflatex ${PAPER}
	pdflatex ${PAPER}
	gs -q -dNOPAUSE -dBATCH -dPDFSETTINGS=/prepress \
              -dDownsampleColorImages=false -dAutoFilterColorImages=false  \
              -dColorImageFilter=/FlateEncode -sDEVICE=pdfwrite -sOutputFile=$(PAPER)-embed.pdf \
	      $(PAPER).pdf
	cp $(PAPER)-embed.pdf $(PAPER).pdf

clean: 
	cp ${PAPER}.pdf cleaned.pdf || echo none
	rm ${PAPER}.pdf *.aux *.bbl *.blg *.dvi *.fls *.log *.tex~ *.tex.backup *.bib~ || echo none

