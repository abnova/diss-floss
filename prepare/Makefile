# Major variable definitions

RSCRIPT=Rscript
#RSCRIPT=R CMD BATCH
R_OPTS=--no-save --no-restore --verbose
#R_OUT=> outputFile.Rout 2>&1

# --no-save --no-restore --verbose myRfile.R > outputFile.Rout 2>&1


# Targets and rules 

IMPORT_DIR = ../import
IMPORT_DONE = $(IMPORT_DIR)/sf.done

prepare: import \
         transform \
         cleanup \
         merge \
         sample

import: import.done
transform: transform.done
cleanup: cleanup.done
merge: merge.done
sample: sample.done

import.done: $(IMPORT_DONE)
	@cd $(IMPORT_DIR) && $(MAKE)
	@touch $@

transform.done: transform.R import.done
	@$(RSCRIPT) $(R_OPTS) $<
	@touch $@

cleanup.done: cleanup.R transform.done
	@$(RSCRIPT) $(R_OPTS) $<
	@touch $@

merge.done: merge.R cleanup.done
	@$(RSCRIPT) $(R_OPTS) $<
	@touch $@

sample.done: sample.R merge.done
	@$(RSCRIPT) $(R_OPTS) $<
	@touch $@

.PHONY: import transform cleanup merge sample clean

# remove intermediate files
clean:
	rm -f tmp*.bz2 *.Rdata .Rout