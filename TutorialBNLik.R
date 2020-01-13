######################################
# Example of 3-node Bayesian network #
#                                    #
#Polina Suter                        # 
#January 2018                        #
######################################

#Install the BiDAG package and load it
install.packages("BiDAG", repos="http://lib.stat.cmu.edu/R/CRAN",dependencies=TRUE)
library(BiDAG)
# Load data set and display the first rows
data(Asia)
head(Asia)

train_data<-Asia[158:172,c(4,5,8)]
colnames(train_data)<-c("LungCan","Bronch","Dysp")
train_data

# Calculates the joint probability for the first network and a data point
calc_joint1 = function(LungCan, Bronch, Dysp) {
  # P(LungCan)
  if (LungCan == 1) {
    PLungCan = 1/5
  } else {
    PLungCan = 4/5
  }
  
  # P(Bronch)
  if (Bronch == 1) {
    PBronch = 2/3
  } else {
    PBronch = 1/3
  }
  
  # P(Dysp|LungCan,Bronch)
  if (Dysp == 0){
    if (LungCan == 1) {
      PDysp = 0
    } else {
      if (Bronch == 1) {
        PDysp = 1/9
      } else {
        PDysp = 1
      }
    }
  } else {
    if (LungCan == 1) {
      PDysp = 1
    } else {
      if (Bronch == 1) {
        PDysp = 8/9
      } else {
        PDysp = 0
      }
    }
  }
  
  # joint probability
  return (PBronch*PLungCan*PDysp)
}

# Calculates the joint probability for the second network and a data point
calc_joint2 = function(LungCan, Bronch, Dysp) {
  # P(LungCan)
  if (LungCan == 1) {
    PLungCan = 1/5
  } else {
    PLungCan = 4/5
  }
  
  # P(Bronch)
  if (Bronch == 1) {
    PBronch = 2/3
  } else {
    PBronch = 1/3
  }
  
  
  # P(Dysp|Bronch)
  if(Dysp == 0){
    if (Bronch == 1){
      PDysp = 1/10
    } else {
      PDysp = 3/5
    } 
  } else {
    if (Bronch == 1) {
      PDysp = 9/10
    } else {
      PDysp = 2/5
    }
  }
  
  return (PBronch*PLungCan*PDysp)
}

# Calculate data likelihood for network 1
ll1 = 0
for (i in 1:nrow(train_data)) {
  ll1 = ll1+log(calc_joint1(train_data[i,1], train_data[i,2], train_data[i,3]))
}
print(paste("Log likelihood for network 1:", ll1))

# Calculate data likelihood for network 2
ll2 = 0
for (i in 1:nrow(train_data)) {
  ll2 = ll2+log(calc_joint2(train_data[i,1], train_data[i,2], train_data[i,3]))
}

print(paste("Log likelihood for network 2:", ll2))


