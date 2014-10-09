# producing a mosaic plot, using 'vcd' package

library(vcd)

df <- sampleDF(flossData, 9)
factorCols <- vapply(df, is.factor, logical(1))
# a <- table(df[factorCols])

a <- table(df[c("Project.Stage", "License.Restrictiveness")])

# mosaic(a, gp = shading_max, split_vertical = TRUE)
mosaic(a, gp = shading_hcl, split_vertical = TRUE)