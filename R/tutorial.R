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
# Part 3
################################################################################

# Bootstrap the data and estimate a BN on every bootstrap sample
# Then compute edge frequencies and directions

boot.tabu   <- boot.strength(cat.signalling.data,
                             algorithm="tabu",algorithm.args = list(score="bic"))
boot.tabu.c <- boot.strength(signalling.data,
                             algorithm="tabu", algorithm.args = list(score="bic-g"))
head(boot.tabu)

plot.bootstrapped(boot.tabu)
plot.bootstrapped(boot.tabu.c)


################################################################################
# Other things to try out
################################################################################

# Plot the LPDs of PIP3
bn.fit.dotplot(bn.tabu.fit$PIP3, main = NULL, xlab = NULL, ylab=NULL)

# Are assumptions of normality fulfilled?
bn.fit.qqplot(bn.tabu.c.fit, main = NULL, xlab = NULL, ylab=NULL)

# Compute condutional independencies yourself
ci.test("PIP3", "PLCG", data=cat.signalling.data, test="x2")
ci.test("PIP3", "PLCG", c("PIP2", "RAF", "MEK"), data=cat.signalling.data, test="x2")

# Write down a model, score it, estimate its CPDs, and plot its CPDAG
custom.bn <- model2network("[MEK][PLCG][RAF|MEK][PIP2|PLCG:RAF][PIP3|PIP2]")
plot(custom.bn)
score(custom.bn, signalling.data, type = "bic-g")
bn.fit(custom.bn, signalling.data)
plot(cpdag(custom.bn))

# Blacklist and whitelist some edges and start with an already estimated bn
(blacklist <- data.frame(from=c("RAF", "MEK"), to=c("MEK", "RAF")))
(whitelist <- data.frame(from="PIP3", to="PIP2"))
blackist.bn.fit <- tabu(signalling.data,
                        score = "bic-g",
                        start = bn.tabu.c,
                        whitelist = whitelist,
                        blacklist = blacklist)
plot(blackist.bn.fit)

# Is it a good BN?
score(blackist.bn.fit, signalling.data, type = "bic-g")
score(bn.tabu.c, signalling.data, type = "bic-g")
