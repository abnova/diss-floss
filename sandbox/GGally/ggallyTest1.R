# To rebuild the package:
#
#library(devtools)
#
#build("path/to/the/package")
# build("~/ggally-master")
#
#install.packages("path/to/built/package")
# install_local("~/GGally_0.4.8.tar.gz")
#
# OR, EASIER:
#
# library(devtools)
# install_github("ggally", "tonytonov")
#

library("GGally")
data(iris)

# ggally_cor <- <...>
#assignInNamespace("ggally_cor", ggally_cor, "GGally")

#theme(legend.position = "none", 
#      panel.grid.major = element_blank(), 
#      axis.ticks = element_blank(), 
#      panel.border = element_rect(linetype = "dashed",
#                                  colour = "black", fill = NA))

ggpairs(iris[,1:4],
        lower=list(continuous="smooth", params=c(colour="blue")),
        diag=list(continuous="bar",params=c(colour="blue")), 
        upper=list(params=list(corSize=6)), axisLabels='show')
