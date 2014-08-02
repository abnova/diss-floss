if (!suppressMessages(require(plspm))) install.packages('plspm')
library(plspm)


dataLoad <- function (dataFile) {
  
  if (file.exists(dataFile)) {
    data <- readRDS(dataFile)
  }
  else {
    error("Data file \'", dataFile, "\' not found! Run 'make' first.")
  }
  
  return (data)
}


##data(flossData)
dataFile <- "~/diss-floss/data/transform/SourceForge/prjLicense.rds"
prjData <- dataLoad(dataFile)

dataFile <- "~/diss-floss/data/transform/SourceForge/userCommunitySize.rds"
successData <- dataLoad(dataFile)

flossData <- merge(prjData, successData)

# convert License Restrictiveness' factor levels to integers
flossData[,3] <- as.integer(flossData[,3])

# convert User Community Size from character to integer
flossData[,4] <- as.integer(flossData[,4])

# remove NAs
#flossData <- flossData[complete.cases(flossData[,3]),]
rowsNA <- apply(flossData, 1, function(x) {any(is.na(x))})
flossData <- flossData[!rowsNA,]

# rows of the path matrix
Governance  <- c(0, 0) # 0, 0, 0
#Sponsorship <- c(0, 0, 0)
Success     <- c(1, 0) # 1, 1, 0

# inner model matrix
successPath <- rbind(Governance, Success) # Sponsorship, 

# add column names
colnames(successPath) <- rownames(successPath)

# blocks of indicators (outer model)
successBlocks <- list(2:3, 4) # 5:8, 9:12

# vector of modes (reflective)
successModes <- rep("A", 2) # 3

# run plspm analysis
successPLS <- plspm(flossData,
                    successPath, successBlocks,
                    modes = successModes)

# 4.2. Handling PLS-PM Results

# what's in foot_pls?
print(successPLS)

# summarized results
print(summary(successPLS))

# 4.3. Measurement Model Assessment: Reflective Indicators

# plotting loadings
gLoadings <- plot(successPLS, what = "loadings")
print(gLoadings)

# outer model results (in a matrix way, unlike tabular in summary())
print(successPLS$outer_model)

# Defense outer model results
print(subset(successPLS$outer_model, block == "Defense"))

# plotting weights
gWeights <- plot(successPLS, what = "weights")
print(gWeights)

# add two more columns NGCH and NGCA
#spainfoot$NGCH = -1 * spainfoot$GCH
#spainfoot$NGCA = -1 * spainfoot$GCA

# check column names
print(names(flossData))

# new list of blocks (with column positions of variables)
newBlocksPos <- list(1:4, c(15,16,7,8), 9:12)

# new list of blocks (with names of variables)
newBlocksStr <- list(
  c("GSH", "GSA", "SSH", "SSA"),
  c("NGCH", "NGCA", "CSH", "CSA"),
  c("WMH", "WMA", "LWR", "LRWL"))

# re-apply plspm
#successPLS <- plspm(flossData,
#                    successPath, newBlocksStr,
#                    modes = successModes)

# plot loadings
gLoadings2 <- plot(successPLS, "loadings")
print(gLoadings2)

# unidimensionality - better results
print(successPLS$unidim)

# loadings and communalities
print(successPLS$outer_model)

# cross-loadings
print(successPLS$crossloadings)

# load ggplot2 and reshape
library(ggplot2)
library(reshape)

# reshape crossloadings data.frame for ggplot
xloads = melt(successPLS$crossloadings, id.vars = c("name", "block"),
              variable_name = "LV")

# bar-charts of crossloadings by block
ggplot(data = xloads,
       aes(x = name, y = value, fill = block)) +
  
  # add horizontal reference lines
  geom_hline(yintercept = 0, color = "gray75") +
  geom_hline(yintercept = 0.5, color = "gray70", linetype = 2) +

  # indicate the use of car-charts
  geom_bar(stat = 'identity', position = 'dodge') +

  # panel display (i.e. faceting)
  facet_wrap(block ~ LV) +

  # tweaking some grahical elements
  theme(axis.text.x = element_text(angle = 90),
        line = element_blank(),
        plot.title = element_text(size = 12)) +

  # add title
  ggtitle("Crossloadings")


# 4.4. Measurement Model Assessment: Formative Indicators


# 4.5. Structural Model Assessment

# inner model
print(successPLS$inner_model)

# inner model summary
print(successPLS$inner_summary)

# select R2
print(successPLS$inner_summary[, "R2", drop = FALSE])

# GoF index
print(successPLS$gof)

# 4.6. Validation

# running bootstrap validation (200 samples)
successVal <- plspm(flossData, successPath, newBlocksStr,
                    modes = successModes,
                    boot.val = TRUE, br = 200)


# bootstrap results
print(successVal$boot)


