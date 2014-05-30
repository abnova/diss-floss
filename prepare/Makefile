# Major variable definitions

RSCRIPT=Rscript
#RSCRIPT=R CMD BATCH
R_OPTS=--no-save --no-restore --verbose
#R_OUT=> outputFile.Rout 2>&1

# --no-save --no-restore --verbose myRfile.R > outputFile.Rout 2>&1


# Targets and rules 

IMPORT_DIR=../import

prepare: import \
         transform \
         cleanup \
         merge \
         sample

import: $(IMPORT_DIR)/sf.done
transform: transform.done
cleanup: cleanup.done
merge: merge.done
sample: sample.done

transform.done: transform.R import
	@$(RSCRIPT) $(R_OPTS) $<
	@touch $@

cleanup.done: cleanup.R transform
	@$(RSCRIPT) $(R_OPTS) $<
	@touch $@

merge.done: merge.R cleanup
	@$(RSCRIPT) $(R_OPTS) $<
	@touch $@

sample.done: sample.R merge
	@$(RSCRIPT) $(R_OPTS) $<
	@touch $@

.PHONY: import transform cleanup merge sample clean

# remove intermediate files
clean:
	rm -f tmp*.bz2 *.Rdata .Rout