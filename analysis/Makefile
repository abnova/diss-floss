# Major variable definitions

RSCRIPT=Rscript
#RSCRIPT=R CMD BATCH
R_OPTS=--no-restore --verbose
R_OPTS_PROD=--no-save --no-restore --silent
#R_OUT=> outputFile.Rout 2>&1

# --no-save --no-restore --verbose myRfile.R > outputFile.Rout 2>&1


# Targets and rules 

PREP_DIR = ../prepare
PREP_DONE = $(PREP_DIR)/transform.done

analysis: prepare \
          eda \
          eda-section \
          eda-report \
          efa \
          efa-report \
          cfa \
          cfa-report \
          sem

prepare: prepare.done
eda: eda.done
eda-section: eda-section-Rnw.done
eda-report: eda-report-Rmd.done
efa: efa.done
efa-report: efa-report-Rmd.done
cfa: cfa.done
cfa-report: cfa-report-Rmd.done
sem: sem.done

prepare.done: $(PREP_DONE)
	@cd $(PREP_DIR) && $(MAKE) transform
	@touch $@

eda.done: eda.R prepare.done
	@$(RSCRIPT) $(R_OPTS) $<
	@touch $@

eda-section-Rnw.done: eda-section.Rnw eda.R prepare
	@$(RSCRIPT) -e 'library(knitr); knit("eda-section.Rnw")'
	@touch $@

eda-report-Rmd.done: eda-report.Rmd eda.R prepare
	@$(RSCRIPT) -e 'library(rmarkdown); render("eda-report.Rmd")'
	@touch $@

eda-report-Rnw.done: eda-report.Rnw eda.R prepare
	@$(RSCRIPT) -e 'library(knitr); knit("eda-report.Rnw")'
	@pdflatex eda-report.tex
	@pdflatex eda-report.tex
	@rm -f eda-report.tex eda-report.log eda-report.aux
	@touch $@

efa.done: efa.R eda.done
	@$(RSCRIPT) $(R_OPTS) $<
	@touch $@

efa-report-Rmd.done: efa-report.Rmd efa.R prepare
	@$(RSCRIPT) -e 'library(rmarkdown); render("efa-report.Rmd", intermediates_dir = NULL)'
	@touch $@

cfa.done: cfa.R efa.done
	@$(RSCRIPT) $(R_OPTS) $<
	@touch $@

cfa-report-Rmd.done: cfa-report.Rmd cfa.R prepare
	@$(RSCRIPT) -e 'library(rmarkdown); render("cfa-report.Rmd", intermediates_dir = NULL)'
	@touch $@

sem.done: sem.R cfa.done
	@$(RSCRIPT) $(R_OPTS) $<
	@touch $@

.PHONY: eda efa cfa sem clean clean-svg clean-pdf

# remove intermediate files
clean:
	rm -f *.svg *.pdf .Rout

clean-latex:
	rm -f *.aux *.out *.log

clean-svg:
	rm -f *.svg

clean-pdf:
	rm -f *.pdf
