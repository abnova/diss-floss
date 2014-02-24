######################################################################
# Top-level Makefile for project DISS-FLOSS
#
# Project DISS-FLOSS
# ==================
#
# R implementation of dissertation research framework
# for data collection, analysis and results reporting
#
# Dissertation Topic: Governance and Organizational Sponsorship as
# Success Factors in Free/Libre and Open Source Software Development: 
# An Empirical Investigation using Structural Equation Modeling
#
# In partial fulfillment of requirements for the degree
# of Doctor of Philosphy in Information Systems
#
# Graduate School of Computer and Information Sciences
# Nova Southeastern University
# (C) 2014 Aleksandr Blekh
######################################################################

# Major variable definitions

PROJECT="diss-floss"
HOME_DIR="~/diss-floss"
REPORT={$(PROJECT)-slides}

COLLECTION_DIR=import
PREPARATION_DIR=prepare
ANALYSIS_DIR=analysis
RESULTS_DIR=results
PRESENTATION_DIR=present

RSCRIPT=Rscript


# Targets and rules 

all: rprofile collection preparation analysis results presentation

rprofile:
	R CMD BATCH ./.Rprofile

collection:
	cd $(COLLECTION_DIR) && $(MAKE)

preparation: collection
	cd $(PREPARATION_DIR) && $(MAKE)

analysis: preparation
	cd $(ANALYSIS_DIR) && $(MAKE)

results: analysis
	cd $(RESULTS_DIR) && $(MAKE)

presentation: results
	cd $(PRESENTATION_DIR) && $(MAKE)


## Phony targets and rules (for commands that do not produce files)

#.html
.PHONY: demo clean

# run demo presentation slides
demo: presentation
	# knitr(Markdown) => HTML page
	# HTML5 presentation via RStudio/RPubs or Slidify
	# OR
	# Shiny app

# remove intermediate files
clean:
	rm -f tmp*.bz2 *.Rdata