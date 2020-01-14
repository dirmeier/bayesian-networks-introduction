library(bnlearn)
library(dplyr)

library(tibble)
library(ggdag)
library(dagitty)

signalling.data <- readRDS("./data/signalling_data.rds")
signalling.data <- as.data.frame(signalling.data)
hist(signalling.data[,10])

cat.signalling.data <- apply(signalling.data, 2, function(col) {
  qs <- quantile(col, probs = c(.25, .75))
  cut(col, breaks = c(-Inf, qs, Inf), labels = c("Low", "Medium", "High"))
}) %>% as.data.frame()

cat.signalling.data

par(mfrow=c(2, 1))
s <- tabu(cat.signalling.data[,4:8], score = )
plot(s)
s <- pc.stable(signalling.data[,1:5])
plot(s)

genes <- colnames(signalling.data)[1:5]
genes

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

ggdag(g) + theme_void()


signalling.data <- readRDS("./data/signalling_data.rds")
cat.signalling.data <- readRDS("./data/signalling_data-categorical.rds")


par(mfrow=c(2, 3))
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

