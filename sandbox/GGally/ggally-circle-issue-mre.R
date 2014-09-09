---
title: MRE for GGally issue (09/09/2014)
output: html_document
---

Here's the code for an MRE:

```{r}
rm(list = ls(all.names = TRUE))

library("GGally")

# Basketball statistics provided by Nathan Yau at Flowing Data.
nba <- read.csv("http://datasets.flowingdata.com/ppg2008.csv")

# Custom options.
g <- ggcorr(
  nba[,-1],
  geom = "circle",
  max_size = 6,
  size = 3,
  hjust = 0.75,
  #angle = -45,
  palette = "Accent"
) + labs(title = "Basic correlations plot")
```

And the result is not what I expected:

```{r, echo=FALSE}
print(g)
```

Here's my R environment:

```{r}
sessionInfo()
```
