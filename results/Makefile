# Major variable definitions

#RSCRIPT=Rscript
RSCRIPT=R CMD BATCH
R_OPTS=--no-save --no-restore --verbose
#R_OUT=> outputFile.Rout 2>&1

# --no-save --no-restore --verbose myRfile.R > outputFile.Rout 2>&1


# Targets and rules 

results:

#importFLOSSmole: getFLOSSmoleDataXML.R
#	$(RSCRIPT) $(R_OPTS) < $<

#importSourceForge: getSourceForgeData.R
#	$(RSCRIPT) $(R_OPTS) < $<

#importAngelList: getAngelListData.R
#	$(RSCRIPT) $(R_OPTS) < $<

#importCrunchBase: getCrunchBaseDataAPI.R
#	$(RSCRIPT) $(R_OPTS) < $<

.PHONY: clean

# remove intermediate files
clean:
	rm -f tmp*.bz2 *.Rdata