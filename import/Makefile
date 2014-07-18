# Major variable definitions

RSCRIPT=Rscript
#RSCRIPT=R CMD BATCH
R_OPTS=--no-restore --verbose
R_OPTS_PROD=--no-save --no-restore --silent
#R_OUT=> outputFile.Rout 2>&1

# --no-save --no-restore --verbose myRfile.R > outputFile.Rout 2>&1


# Targets and rules 

collection: fm \
            sf \
            al \
            cb

fm: fm.done
sf: sf.done
al: al.done
cb: cb.done

fm.done: getFLOSSmoleDataXML.R
	@$(RSCRIPT) $(R_OPTS) $<
	@touch $@

sf.done: getSourceForgeData.R SourceForge.cfg.tmpl
	@$(RSCRIPT) $(R_OPTS) $<
	@touch $@

al.done: getAngelListData.R
	@$(RSCRIPT) $(R_OPTS) $<
	@touch $@

cb.done: getCrunchBaseDataAPI.R
	@$(RSCRIPT) $(R_OPTS) $<
	@touch $@

.PHONY: fm sf al cb clean clean-fm clean-sf clean-al clean-cb

# remove intermediate files
clean:
	rm -f tmp*.bz2 .Rout

clean-all:
	rm -rf *.done ../cache

clean-fm:
	rm -f fm.done ../cache/FLOSSmole/*.rds

clean-sf:
	rm -f sf.done ../cache/SourceForge/*.rds

clean-al:
	rm -f al.done ../cache/AngelList/*.rds

clean-cb:
	rm -f cb.done ../cache/CrunchBase/*.rds
