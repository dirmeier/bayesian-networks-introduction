library(dplyr)

library(tibble)
library(ggplot2)
library(ggdag)
library(dagitty)
library(bnlearn)


signalling.data <- readRDS("./data/signalling_data.rds")
cat.signalling.data <- readRDS("./data/signalling_data-categorical.rds")

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

score(dag, cat.signalling.data,)


par(mfrow=c(3, 1))
plot(g)
tabu.bic <- tabu(cat.signalling.data, score = "bic")
plot(tabu.bic)

ft <- bn.fit(tabu.bic, cat.signalling.data, method = "mle")
ft$PIP2
ft <- bn.fit(tabu.bic, cat.signalling.data, method = "bayes")
ft$PIP2

tabu.bde <- tabu(cat.signalling.data, score = "bde")
plot(tabu.bde)

pc.bn <- pc.stable(cat.signalling.data, test="x2")
pc.bn
ft <- bn.fit(pc.bn, cat.signalling.data, method = "mle")
ft

plot(g)
s <- tabu(signalling.data, score = "bge")
plot(s)
s <- pc.stable(signalling.data)
plot(s)
bn.fit.dotplot(bn_fit$MEK, main = NULL, xlab = "P(MEK | RAF)", ylab=NULL)
