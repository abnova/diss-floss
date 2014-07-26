if (!suppressMessages(require(editrules))) install.packages('editrules')
if (!suppressMessages(require(deducorrect))) install.packages('deducorrect')

library(editrules)
library(deducorrect)


### DATA CLEANING IN 7 STATEMENTS

### read data and rules
dat <- read.csv2("mydata.csv", comment.char = "#")
E <- editfile("edits.txt")

### deductive correction
dat1 <- correctTypos(E, dat)
dat2 <- correctSigns(E, dat1$corrected)
dat3 <- correctRounding(E, dat2$corrected)

### localize errors
el <- localizeErrors(E, dat3$corrected)

### deductive imputation
dat4 <- deduImpute(E, dat3$corrected, adapt = el$adapt)
