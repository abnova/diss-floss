# Major variable definitions

RSCRIPT=Rscript
#RSCRIPT=R CMD BATCH
R_OPTS=--no-restore --verbose
R_OPTS_PROD=--no-save --no-restore --silent
#R_OUT=> outputFile.Rout 2>&1

# --no-save --no-restore --verbose myRfile.R > outputFile.Rout 2>&1


# Targets and rules 

PREP_DIR=../prepare

analysis: prepare \
          eda \
          efa \
          cfa \
          sem

prepare: $(PREP_DIR)/transform.done
eda: eda.done
efa: efa.done
cfa: cfa.done
sem: sem.done

eda.done: eda.R prepare
	@$(RSCRIPT) $(R_OPTS) $<
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
