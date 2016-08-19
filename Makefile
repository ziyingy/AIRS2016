FYLE=p
FINAL=yang-moffat-turpin

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
	/bin/rm -f $(FINAL).zip
	zip $(FINAL).zip \
		Copyright.pdf \
		$(FINAL).{tex,pdf} \
		figs/fig-trec7-ap-scores.pdf \
		figs/fig-score-variation-*.pdf \
		figs/p_value_scatter_sys_pairs.pdf


clean:
	rm -f *~ *.bak *.log *.blg *.aux
