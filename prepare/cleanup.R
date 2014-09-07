if (!suppressMessages(require(editrules))) install.packages('editrules')
if (!suppressMessages(require(deducorrect))) install.packages('deducorrect')

library(editrules)
library(deducorrect)

PRJ_HOME <- Sys.getenv("DISS_FLOSS_HOME") # getwd()


### DATA CLEANING IN 7 STATEMENTS

### read data and rules

#dat <- read.csv2("test-cleanup-mydata.csv", comment.char = "#")
dat <- data.frame(
  x = c(5, 45, 12),
  y = c(11, 0, 7),
  A = c("c", "a", "c"),
  B = c("g", "f", "f"),
  z = c(16, 46, 19)
  )

E <- editfile("test-cleanup-edits.txt")

### deductive correction
dat1 <- correctTypos(E, dat)
dat2 <- correctSigns(E, dat1$corrected)
dat3 <- correctRounding(E, dat2$corrected)

### localize errors
el <- localizeErrors(E, dat3$corrected)

### deductive imputation
dat4 <- deduImpute(E, dat3$corrected, adapt = el$adapt)
print(dat4$corrected)
print(dat4$status)
print(dat4$corrections)