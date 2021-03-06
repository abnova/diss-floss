#
# This model specification was automatically generated by Onyx
#
library(lavaan);
modelData <- read.table(DATAFILENAME, header = TRUE) ;
 model<-"
! regressions 
   x1=~1.0*x1_
   x1_=~1.0*x1_
! residuals, variances and covariances
   x1 ~~ VAR_x1*x1
   x1 ~~ 0.0*x1_
";
result<-lavaan(model, data=modelData, fixed.x=FALSE, missing="FIML");
summary(result, fit.measures=TRUE);
