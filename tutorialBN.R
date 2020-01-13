############################
#Bayesian networks tutorial#
#Polina Suter              #
#January 2018              #
############################

#install.packages("pcalg")
#install.packages("BiDAG")
library(pcalg)
library(BiDAG)

# Load data set and display the first rows
data(Asia)
head(Asia)
#Asia<-Asia[1:500,] #try later
#store column names, we will need them for plots
varnames<-colnames(Asia)

#store true graph as a binary matrix
trueAsia<-matrix(0,nrow=8,ncol=8)
trueAsia[1,3]<-1
trueAsia[3,6]<-1
trueAsia[4,6]<-1
trueAsia[6,7]<-1
trueAsia[6,8]<-1
trueAsia[5,8]<-1
trueAsia[2,5]<-1
trueAsia[2,4]<-1
#true graph in graphNEL format
trueDAG<-adjacency2dag(trueAsia,nodes=varnames)
#equivalence class of a true DAG
trueCP<-dag2cpdag(trueDAG)

dev.off()
par(mfrow=c(2,2))
plot(trueDAG)
plot(trueCP)

#significance levels for conditional independence tests
alpha<-0.01 #try other values 0.05..0.5
#pc algorithm
pcfit<-pcalg::pc(suffStat = list(dm = as.data.frame(Asia), adaptDF = FALSE),
                     indepTest = binCItest,
                      alpha=alpha,labels=colnames(Asia))
pcCP<-pcfit@graph

#compare PC estimate vs true equivalence class
compareDAGs(pcCP,trueCP)

plot(pcCP)

dev.off()
par(mfrow=c(2,2))
plot(trueDAG)
plot(trueCP)
#MCMC schemes

#first with need object scoreparameters to pass to search functions
scorepar<-scoreparameters(8,"bde",Asia,nodeslabels=colnames(Asia))
#MAP MCMC search
set.seed(100)
#apply iterative MCMC scheme for finding a MAP DAG
myest<-iterativeMCMCsearch(8,scorepar)
mcmcMAP<-adjacency2dag(myest$max$DAG,nodes = varnames)
#convert to equivalence class
mcmcCP<-dag2cpdag(mcmcMAP)
plot(mcmcCP)
#compare against true equivalence class
compareDAGs(mcmcCP,trueCP)
#BDe score of true DAG
DAGscore(8,scorepar,dag2adjacencymatrix(trueDAG))
#BDe score of estimated MAP DAG
DAGscore(8,scorepar,dag2adjacencymatrix(mcmcMAP))

#sampling MCMC
orderest<-orderMCMC(8,scorepar,startspace = myest$max$DAG,chainout=TRUE)
mcmcsample<-adjacency2dag(dag.threshold(8,orderest$chain$incidence,0.5,pdag=TRUE),
                          nodes = varnames)
mcmcCPsample<-dag2cpdag(mcmcsample)

#posterior probabilities of all single edges in a graph
#when avergaed over DAGs
posterior.prob<-edges.posterior(orderest$chain$incidence,pdag=FALSE)
colnames(posterior.prob)<-rownames(posterior.prob)<-varnames
print(round(posterior.prob,2))

#posterior probabilities of all single edges in a graph
##when avergaed over CPDAGs
posterior.prob<-edges.posterior(orderest$chain$incidence,pdag=TRUE)
colnames(posterior.prob)<-rownames(posterior.prob)<-varnames
print(round(posterior.prob,2))

compareDAGs(mcmcCPsample,trueCP)
plot(mcmcCPsample)

#number of of abservations with A=1
A.lines<-which(Asia[,1]==1)
length(A.lines)

#P(T=1|A=1))
length(which(Asia[A.lines,3]==1))/length(A.lines)
length(which(Asia[A.lines,3]==1))
length(A.lines)

#P(T=1)
length(which(Asia[,3]==1))/nrow(Asia)
length(which(Asia[,3]==1))
nrow(Asia)


