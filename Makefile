FYLE=p
FINAL=spc161

pdf:
	pdflatex $(FYLE)

bbl:	
	pdflatex $(FYLE)
	bibtex $(FYLE)
	pdflatex $(FYLE)
	pdflatex $(FYLE)

final:	pdf bbl
	latexpand --expand-bbl p.bbl p.tex > $(FINAL).tex
	pdflatex $(FINAL)
	pdflatex $(FINAL)
	pdflatex $(FINAL)


clean:
	rm -f *~ *.bak *.log *.blg *.aux
