# Major variable definitions

TEX2PDF = texi2pdf --clean
#TEXT2PDF_OPTS = --clean

# Targets and rules 

presentation: beamer

beamer: beamer-progressbar

beamer-progressbar: slides-test2-logo.tex
	$(TEX2PDF) $<

.PHONY: clean

# remove generated files
clean:
	rm -f *.pdf *.aux *.nav *.log *.out *.snm *.toc