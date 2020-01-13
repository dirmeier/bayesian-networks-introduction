library(pcalg)
library(BiDAG)
set.seed(100)
#generate random Bayesian network with n=100 nodes, Gaussian random variables
myDAG<-pcalg::randomDAG(100,prob=3/100,lB=0.4,uB=2)
print(myDAG)
dev.off()
plot(myDAG)
nedges<-sum(dag2adjacencymatrix(myDAG))
#generate 300 observations from this BN
myData<-pcalg::rmvDAG(500,myDAG)


#apply PC algorithm to estimate the equivalence class
pcfit<-pc(suffStat = list(C = cor(myData), n = 500),
          indepTest = gaussCItest,
          alpha=0.01,labels=colnames(myData))
#how good is estimated structure: SHD, TP, FP
compareDAGs(dag2cpdag(pcfit@graph),dag2cpdag(myDAG))
#TPR
TPR<-compareDAGs(dag2cpdag(pcfit@graph),dag2cpdag(myDAG))[2]/nedges
print(TPR)

#apply iterative MCMC scheme to estimate the equivalence class
scorepar<-scoreparameters(100,"bge",myData)
set.seed(100)
myest<-iterativeMCMCsearch(100,scorepar,cpdag=TRUE)
mcmcMAP<-adjacency2dag(myest$max$DAG)
mcmcCP<-dag2cpdag(mcmcMAP)
#compare MAP CPDAG to the true CPDAG
compareDAGs(mcmcCP,dag2cpdag(myDAG))

#obtain a sample of BNs from posterior distribution
mcmcsample<-orderMCMC(100,scorepar,startspace=myest$max$DAG,chainout=TRUE)
#compare CPDAGs obtained via averaging over sample from posterior distribution to the true CPDAG
samp.ch<-sample.check(100,mcmcsample$chain$incidence,myDAG,pdag=TRUE)
print(samp.ch)
