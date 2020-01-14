library(dplyr)

library(tibble)
library(ggplot2)
library(ggdag)
library(dagitty)
library(bnlearn)


signalling.data <- readRDS("./data/signalling_data.rds")

factor(signalling.data$MEK)

cat.signalling.data <- readRDS("./data/signalling_data-categorical.rds")

cat.signalling.data <- lapply(cat.signalling.data, function(col) {
  factor(col, levels = c("Low", "Medium", "High"))
}) %>% as.data.frame()



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

dag <- empty.graph(colnames(cat.signalling.data))
arcs <- dagitty::edges(g)[,1:2] %>%
  dplyr::mutate(v = as.character(v), w = as.character(w)) %>%
  dplyr::rename(from=v, to=w)
arcs(dag) <- arcs

bn_fit <- bn.fit(dag, cat.signalling.data)



par(mfrow=c(1, 1))
plot(g)
s <- tabu(cat.signalling.data, score = "bde")
plot(s)
s <- pc.stable(cat.signalling.data)
plot(s)

plot(g)
s <- tabu(signalling.data, score = "bge")
plot(s)
s <- pc.stable(signalling.data)
plot(s)
bn.fit.dotplot(bn_fit$MEK, main = NULL, xlab = "P(MEK | RAF)", ylab=NULL)
