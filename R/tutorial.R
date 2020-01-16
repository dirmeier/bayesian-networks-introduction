# R tutorial on Bayesian networks
#
# Copyright (C) Simon Dirmeier (<simon.dirmeier @ bsse.ethz.ch>)
# Date: 17. 01. 2020

suppressMessages({
  library(dagitty)
  library(bnlearn)
})

par(mfrow=c(2, 1))
source(file.path(here::here(), "R", "_plot_boostrapped_graph.R"))

true.dag <- dagitty('dag {
    RAF [pos="2,2"]
    MEK [pos="2,1"]
    PIP3 [pos="1,0"]
    PIP2 [pos="1,1"]
    PLCG [pos="0,2"]

    RAF -> PIP2 <- PLCG
    PIP2 -> PIP3
    RAF -> MEK
}')

plot(true.dag)


################################################################################
# Part 1
################################################################################

# Read data
cat.signalling.data <- readRDS(
  file.path(here::here(), "data", "signalling_data-categorical.rds"))

# Have a look at the data
head(cat.signalling.data)
head(cat.signalling.data$MEK)

# Look at documentation and learn structure with tabu search and BIC at score
?bnlearn::tabu
bn.tabu <- bnlearn::tabu(cat.signalling.data, score = "bic")
plot(bn.tabu)

# What happens with a different score
bn.tabu.mle <- bnlearn::tabu(cat.signalling.data, score = "loglik")
plot(bn.tabu.mle)

# Look at documentation and learn structure with chi-squared independence tests
?bnlearn::pc.stable
bn.pc <- bnlearn::pc.stable(cat.signalling.data, test = "x2")
plot(bn.pc)

# Different CI test
ci.test("MEK", "RAF", data = cat.signalling.data, test = "x2")
ci.test("MEK", "RAF", data = cat.signalling.data, test = "mi")

# Get CPDAGs
bn.tabu.cpdag <- bnlearn::cpdag(bn.tabu)
plot(bn.tabu.cpdag)
bn.pc.cpdag <- bnlearn::cpdag(bn.pc)
plot(bn.pc.cpdag)

# Estimate local probability tables
bn.tabu.fit <- bn.fit(bn.tabu, cat.signalling.data)

# Make a query
for (i in 1:3) {
  prob <- bnlearn::cpquery(bn.tabu.fit, (MEK == "High"), (RAF == "Low"))
  print(prob)
}

#  LPD of MEK
# The parameters are observed frequencies in the data
bn.tabu.fit$MEK

# High marginal probability
bnlearn::cpquery(bn.tabu.fit, (PIP2 == "Medium"), evidence = TRUE)

# Lower conditional probability
bnlearn::cpquery(bn.tabu.fit,
                 (PIP2 == "Medium"),
                 (PIP3 == "Low" & PLCG == "Low"))

# Even lower conditional probability
bnlearn::cpquery(bn.tabu.fit,
                 (PIP2 == "Medium"),
                 (PIP3 == "Low" & PLCG == "Low" & RAF == "Low"))

# Cannot fit parameters on CPDAGs
bn.fit(bn.pc, cat.signalling.data)
bn.fit(bn.tabu.cpdag, cat.signalling.data)

################################################################################
# Part 2
################################################################################

# Read data
signalling.data <- readRDS(
  file.path(here::here(), "data", "signalling_data.rds"))

# Have a look at the data
head(signalling.data)
head(signalling.data$MEK)
hist(signalling.data$MEK)
hist(signalling.data$PIP2)

# Learn structure with tabu search and BIC at score
bn.tabu.c <- bnlearn::tabu(signalling.data, score = "bic-g")
plot(bn.tabu)
plot(bn.tabu.c)

# What is the score of the DAG if we reverse an edge
bn.tabu.c.t <- set.arc(bn.tabu.c, "MEK", "RAF")
plot(bn.tabu.c)
plot(bn.tabu.c.t)
score(bn.tabu.c, signalling.data, type = "bic-g")
score(bn.tabu.c.t, signalling.data, type = "bic-g")

# Learn structure with chi-squared independence tests
bn.pc.c <- bnlearn::pc.stable(signalling.data, test = "cor")
plot(bn.pc)
plot(bn.pc.c)

# Get CPDAGs
bn.tabu.c.cpdag <- bnlearn::cpdag(bn.tabu.c)
plot(bn.tabu.c.cpdag)
bn.pc.c.cpdag <- bnlearn::cpdag(bn.pc.c)
plot(bn.pc.c.cpdag)

# Estimate local probability tables
bn.tabu.c.fit <- bn.fit(bn.tabu.c, signalling.data)

# Make a query
for (i in 1:3) {
  prob <- bnlearn::cpquery(bn.tabu.c.fit, (MEK > 0), (RAF < 0))
  print(prob)
}

# LPD of MEK
# The parameters are the coefficients of a regression of MEK on RAF
bn.tabu.c.fit$MEK

################################################################################
# Part 23
################################################################################

# Bootstrap the data and estimate a BN on every bootstrap sample
# Then compute edge frequencies and directions
boot.tabu <- boot.strength(cat.signalling.data, algorithm="tabu")
boot.tabu.c <- boot.strength(signalling.data, algorithm="tabu")
head(boot.tabu)

plot.bootstrapped(boot.tabu)
plot.bootstrapped(boot.tabu.c)

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
tabu.bic <- tabu(cat.signalling.data, score = "bic")
plot(tabu.bic)

ft <- bn.fit(tabu.bic, cat.signalling.data, method = "mle")
ft$PIP2
ft <- bn.fit(tabu.bic, cat.signalling.data, method = "bayes")
ft$PIP2

tabu.bde <- tabu(cat.signalling.data, score = "bde")
plot(tabu.bde)

pc.bn <- pc.stable(cat.signalling.data, test="x2")


plot(cpdag(pc.bn))
ft <- bn.fit(pc.bn, cat.signalling.data, method = "mle")
ft

plot(g)
s <- tabu(signalling.data, score = "bge")
plot(s)
ss <- bn.fit(s, signalling.data)
coef(ss$MEK)
mean(signalling.data$MEK)

s <- pc.stable(signalling.data)
bn.fit(s, signalling.data)

plot(s)
bn.fit.dotplot(bn_fit$MEK, main = NULL, xlab = "P(MEK | RAF)", ylab=NULL)


ci.test("PIP3", "PLCG", data=cat.signalling.data, test="x2")
ci.test("PIP3", "PLCG", c("PIP2", "RAF", "MEK"), data=cat.signalling.data, test="x2")


s <- tabu(signalling.data, score = "bge")
plot(s)
score(s, signalling.data)

plot(cpdag(s))

dag1 <- model2network("[MEK][PLCG][RAF|MEK][PIP2|PLCG:RAF][PIP3|PIP2]")
dag2 <- model2network("[RAF][PLCG][MEK|RAF][PIP2|PLCG:RAF][PIP3|PIP2]")
plot(dag1)
plot(dag2)

score(dag1, cat.signalling.data, type = "bde")
score(dag2, cat.signalling.data, type = "bde")

par(mfrow=c(3, 1))
dag1 <- model2network("[MEK][PLCG][RAF|MEK][PIP2|PLCG:RAF][PIP3|PIP2]")
dag2 <- model2network("[RAF][PLCG][MEK|RAF][PIP2|PLCG:RAF][PIP3|PIP2]")
plot(dag1)
plot(dag2)

score(dag1, signalling.data)
score(dag2, signalling.data)
plot(cpdag(dag1))



plot(g)
s <- tabu(signalling.data, score = "bge")
plot(s)
ss <- bn.fit(s, signalling.data)
coef(ss$MEK)
mean(signalling.data$MEK)


