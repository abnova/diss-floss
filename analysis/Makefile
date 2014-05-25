# Major variable definitions

RSCRIPT=Rscript
#RSCRIPT=R CMD BATCH
R_OPTS=--no-restore --verbose
R_OPTS_PROD=--no-save --no-restore --silent
#R_OUT=> outputFile.Rout 2>&1

# --no-save --no-restore --verbose myRfile.R > outputFile.Rout 2>&1


# Targets and rules 

analysis: eda \
          efa \
          cfa \
          sem

eda: eda.R
	@$(RSCRIPT) $(R_OPTS) $<

efa: efa.R
	@$(RSCRIPT) $(R_OPTS) $<

cfa: cfa.R
	@$(RSCRIPT) $(R_OPTS) $<

sem: sem.R
	@$(RSCRIPT) $(R_OPTS) $<

.PHONY: clean clean-svg clean-pdf

# remove intermediate files
clean:
	rm -f *.svg *.pdf .Rout

clean-svg:
	rm -f *.svg

clean-pdf:
	rm -f *.pdf
