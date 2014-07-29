# Adapted example from the Chapter 4 "Interpreting PLS-PM Results"

if (!suppressMessages(require(plspm))) install.packages('plspm')
library(plspm)

data(spainfoot)

# rows of the path matrix
Attack <- c(0, 0, 0)
Defense <- c(0, 0, 0)
Success <- c(1, 1, 0)

# inner model matrix
foot_path <- rbind(Attack, Defense, Success)

# add column names
colnames(foot_path) <- rownames(foot_path)

# blocks of indicators (outer model)
foot_blocks <- list(1:4, 5:8, 9:12)

# vector of modes (reflective)
foot_modes <- rep("A", 3)

# run plspm analysis
foot_pls <- plspm(spainfoot, foot_path, foot_blocks, modes = foot_modes)

# 4.2. Handling PLS-PM Results

# what's in foot_pls?
print(foot_pls)

# summarized results
print(summary(foot_pls))

# 4.3. Measurement Model Assessment: Reflective Indicators

# plotting loadings
gLoadings <- plot(foot_pls, what = "loadings")
print(gLoadings)

# outer model results (in a matrix way, unlike tabular in summary())
print(foot_pls$outer_model)

# Defense outer model results
print(subset(foot_pls$outer_model, block == "Defense"))

# plotting weights
gWeights <- plot(foot_pls, what = "weights")
print(gWeights)

# add two more columns NGCH and NGCA
spainfoot$NGCH = -1 * spainfoot$GCH
spainfoot$NGCA = -1 * spainfoot$GCA

# check column names
print(names(spainfoot))

# new list of blocks (with column positions of variables)
new_blocks_pos <- list(1:4, c(15,16,7,8), 9:12)

# new list of blocks (with names of variables)
new_blocks_str <- list(
  c("GSH", "GSA", "SSH", "SSA"),
  c("NGCH", "NGCA", "CSH", "CSA"),
  c("WMH", "WMA", "LWR", "LRWL"))

# re-apply plspm
foot_pls <- plspm(spainfoot, foot_path, new_blocks_str,
                  modes = foot_modes)

# plot loadings
gLoadings2 <- plot(foot_pls, "loadings")
print(gLoadings2)

# unidimensionality - better results
print(foot_pls$unidim)

# loadings and communalities
print(foot_pls$outer_model)

# cross-loadings
print(foot_pls$crossloadings)

# load ggplot2 and reshape
library(ggplot2)
library(reshape)

# reshape crossloadings data.frame for ggplot
xloads = melt(foot_pls$crossloadings, id.vars = c("name", "block"),
              variable_name = "LV")

# bar-charts of crossloadings by block
ggplot(data = xloads,
       aes(x = name, y = value, fill = block)) +
  # add horizontal reference lines
  geom_hline(yintercept = 0, color = "gray75") +
  geom_hline(yintercept = 0.5, color = "gray70", linetype = 2) +
  # indicate the use of car-charts
  geom_bar(stat = 
             identity
           , position = 
             dodge
  ) +
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
print(foot_pls$inner_model)

# inner model summary
print(foot_pls$inner_summary)

# select R2
print(foot_pls$inner_summary[, "R2", drop = FALSE])

# GoF index
print(foot_pls$gof)

# 4.6. Validation

# running bootstrap validation (200 samples)
foot_val = plspm(spainfoot, foot_path, new_blocks_str,
                 modes = foot_modes,
                 boot.val = TRUE, br = 200)

# bootstrap results
print(foot_val$boot)