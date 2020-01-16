# R tutorial on Bayesian networks
#
# Copyright (C) Simon Dirmeier (<simon.dirmeier @ bsse.ethz.ch>)
# Date: 17. 01. 2020


library(dplyr)
library(tibble)
library(dagitty)

g <- dagitty('dag {
    RAF [pos="2,2"]
    MEK [pos="2,1"]
    PIP3 [pos="1,0"]
    PIP2 [pos="1,1"]
    PLCG [pos="0,2"]

    RAF -> PIP2 <- PLCG
    PIP2 -> PIP3
    RAF -> MEK
}')

plot(g)

set.seed(42)
signalling.data <- simulateSEM(g, standardized = FALSE, eps=.1)
cat.signalling.data <- lapply(signalling.data, function(col) {
  qs <- quantile(col, c(0.25, 0.75))
  col <- cut(col, breaks = c(-Inf, qs, Inf), labels = c("Low", "Medium", "High"))
  factor(col, levels = c("Low", "Medium", "High"))
}) %>% as.data.frame()

