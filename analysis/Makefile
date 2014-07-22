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
          efa \
          cfa \
          sem

prepare: prepare.done
eda: eda.done
eda-report: eda-report.done
efa: efa.done
cfa: cfa.done
sem: sem.done

prepare.done: $(PREP_DONE)
	@cd $(PREP_DIR) && $(MAKE) transform
	@touch $@

eda.done: eda.R prepare
	@$(RSCRIPT) $(R_OPTS) $<
	@touch $@

eda-report.done: eda-report.Rmd eda-knitr.R prepare
	@$(RSCRIPT) -e 'library(rmarkdown); render("eda-report.Rmd")'
	@touch $@

efa.done: efa.R eda
	@$(RSCRIPT) $(R_OPTS) $<
	@touch $@

cfa.done: cfa.R efa
	@$(RSCRIPT) $(R_OPTS) $<
	@touch $@

sem.done: sem.R cfa
	@$(RSCRIPT) $(R_OPTS) $<
	@touch $@

.PHONY: eda efa cfa sem clean clean-svg clean-pdf

# remove intermediate files
clean:
	rm -f *.svg *.pdf .Rout

clean-svg:
	rm -f *.svg

clean-pdf:
	rm -f *.pdf
