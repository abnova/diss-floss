# Major variable definitions

RSCRIPT=Rscript
#RSCRIPT=R CMD BATCH
R_OPTS=--no-save --no-restore --verbose
#R_OUT=> outputFile.Rout 2>&1

# --no-save --no-restore --verbose myRfile.R > outputFile.Rout 2>&1


# Targets and rules 

IMPORT_DIR = ../import
IMPORT_DONE = $(IMPORT_DIR)/sf.done

#prepare: import \
#         transform \
#         cleanup \
#         merge \
#         missing \
#         sample

prepare: transform \
         merge \
         missing

import: import.done
transform: transform.done
cleanup: cleanup.done
merge: merge.done
missing: missing.done
sample: sample.done

import.done: $(IMPORT_DONE)
	@cd $(IMPORT_DIR) && $(MAKE)
	@touch $@

# TODO: make work dependency from "import.done"
transform.done: transform.R
	@$(RSCRIPT) $(R_OPTS) $<
	@touch $@

cleanup.done: cleanup.R transform.done
	@$(RSCRIPT) $(R_OPTS) $<
	@touch $@

# TODO: temp dep disabled: cleanup.done
merge.done: merge.R
	@$(RSCRIPT) $(R_OPTS) $<
	@cp ../data/merged/SourceForge/Merged.rds ../data/merged/flossData.rds
	@cp ../data/merged/SourceForge/Merged.rds ../data/ready4eda/flossData.rds
	@touch $@

missing.done: missing.R merge.done
	@$(RSCRIPT) $(R_OPTS) $<
	@cp ../data/imputed/flossDataImputed.rds ../data/ready4efa/flossData.rds
	@cp ../data/imputed/flossDataImputed.rds ../data/ready4cfa/flossData.rds
	@cp ../data/imputed/flossDataImputed.rds ../data/ready4sem/flossData.rds
	@touch $@

sample.done: sample.R missing.done
	@$(RSCRIPT) $(R_OPTS) $<
	@touch $@

.PHONY: import transform missing cleanup merge sample clean

# remove intermediate files
clean:
	rm -f tmp*.bz2 *.Rdata .Rout
