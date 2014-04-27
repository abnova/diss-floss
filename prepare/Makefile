# Major variable definitions

RSCRIPT=Rscript
#RSCRIPT=R CMD BATCH
R_OPTS=--no-save --no-restore --verbose
#R_OUT=> outputFile.Rout 2>&1

# --no-save --no-restore --verbose myRfile.R > outputFile.Rout 2>&1


# Targets and rules 

prepare: sample \
         merge \
         cleanup \
         transform

sample: sample.R
	$(RSCRIPT) $(R_OPTS) $<

merge: merge.R
	$(RSCRIPT) $(R_OPTS) $<

cleanup: cleanup.R
	$(RSCRIPT) $(R_OPTS) $<

transform: transform.R
	$(RSCRIPT) $(R_OPTS) $<

.PHONY: clean

# remove intermediate files
clean:
	rm -f tmp*.bz2 *.Rdata .Rout