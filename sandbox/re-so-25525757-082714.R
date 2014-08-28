rm(list = ls(all.names = TRUE))

# retrieve and prepare data
testData <- "https://github.com/abnova/test/blob/master/flossData4eda.rds?raw=true"

tmpFile <- tempfile()
tmpDir <- tempdir()

download.file(testData, tmpFile, method = 'curl',
              extra = '-L', quiet = TRUE)

df <- readRDS(tmpFile)

df$prjage   <- df[["Project Age"]]
df$teamsize <- df[["Development Team Size"]]
df$license  <- df[["Project License"]]

# produce and display Chart 1
title <- paste("Projects distribution across Project Age range")
xLabel <- "Project Age (months)"

g <- ggplot(data = df, aes(x = prjage)) +
  scale_fill_continuous("Number of\nprojects") + 
  scale_x_continuous(xLabel) +
  scale_y_log10("Number of projects") +
  ggtitle(label=title)

g <- g + geom_histogram(aes(fill = ..count..), binwidth = 1,
                        position = "identity")
print(g)


# produce and display Chart 2
title <- paste("Projects distribution across Development Team Size range")
xLabel <- "Development Team Size"

g <- ggplot(data = df, aes(x = teamsize)) +
  scale_fill_continuous("Number of\nprojects") + 
  scale_x_continuous(xLabel) +
  scale_y_log10("Number of projects") +
  ggtitle(label=title)

g <- g + geom_histogram(aes(fill = ..count..), binwidth = 1,
                        position = "identity")
print(g)


# produce and display Chart 3
MAX_FACTORS <- 10

#df$licenseMod <- c(setdiff(df$license, NA))
##licenseValid <- with(df, license[!is.na(license)])
df$licenseMod <- with(df, reorder(license, license, function(x) -length(x)))
#df$licenseMod <- with(df, ifelse(license > MAX_FACTORS, license, "Other"))
df$licenseMod <- factor(df$licenseMod)
with (df, levels(licenseMod)[rank(-xtabs(~ licenseMod))[levels(licenseMod)] > MAX_FACTORS] <- "Other")

df$licenseFact <-
  with(df, factor(licenseMod,
                  levels=names(sort(table(licenseMod), increasing = FALSE))))

title <- paste("Projects distribution across Project License range")

g <- ggplot(data = df,
            aes(x = licenseFact,
                fill = licenseMod)) +
  geom_bar(stat = "bin", position = "identity") +
  scale_fill_discrete(aes(x=licenseFact)) +
  xlab("Project License") +
  ylab("Number of projects") +
  ggtitle(label=title)
print(g)
