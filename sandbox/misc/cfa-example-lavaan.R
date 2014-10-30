library(lavaan)

d <- mtcars[, c("cyl", "qsec", "mpg", "wt")]
d$mpg <- d$mpg/5
cov(d)

d$cyl <- factor(d$cyl, ordered = TRUE)

# cyl is ordered categorical
# mpg, wt, and qsec are continuous

model <- "
# factor structure
f1 =~ 1*wt
f2 =~ 1*mpg + cyl
f3 =~ 1*qsec

# fix some variances/residual variances to zero
# because < 3 indicators per latent variables
wt ~~ 0*wt
qsec ~~ 0*qsec
mpg ~~ 0*mpg

# covariances among the latent variables
f1 ~~ f2
f1 ~~ f3
f2 ~~ f3
"

fit <- cfa(model, data = d, meanstructure=TRUE,
           missing = "pairwise", estimator = "WLSMV")

summary(fit, fit.measures = TRUE, standardize=TRUE)
