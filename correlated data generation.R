# Covariates and generalization (Round 2)

library(MatchIt)
library(dplyr)
library(overlapping)
library(parallel)
library(generalize)
library(psych)
library(boot)
library(tidyr)
library(faux)

options(mc.cores = parallel::detectCores(logical = FALSE))

#0) Simulation parameters

runs <- 1

num.strata <- 5

ns <- c(4,5) # Vector storing the number of strata for the subclassification estimator

N <- 2000 # Population size

#1) Outputs

# Array containing estimates of PATE

estimators <- array(NA,dim=c(runs,3,length(ns)))

dimnames(estimators) <- list(NULL,c("IPW","subclassification","bart"),c("4strata","5strata"))

# Array containing the calculations of common support

support.array <- array(NA,dim=c(runs,5,length(ns)))

dimnames(support.array) <- list(NULL,c("covars.asmd","prop.asmd","B-index","overlap","overlap.index"),c("4strata","5strata")) # Naming the dimensions of the array

# Array containing the RMSE and bias of the IPW and subclassification estimator

bias.array <- mse.array <- array(NA,dim=c(runs,3,length(ns)))

dimnames(bias.array) <- dimnames(mse.array) <- list(NULL,c("IPW","subclassification","bart"),c("4strata","5strata"))

# Descriptive statistics

ps.sample.mat <- ps.pop.mat <- matrix(NA,runs,7)

colnames(ps.sample.mat) <- colnames(ps.pop.mat) <- c("Mean","sd","Median","Min","Max","skewness","kurtosis")

weights.sample.mat <- matrix(NA,runs,7)

colnames(weights.sample.mat) <- c("Mean","sd","Median","Min","Max","skewness","kurtosis")

# Matrix containing the errors from IPW

ipw.errors <- matrix(NA,runs,1)

############################################################################################

for(k in 1:runs){
  
  set.seed(k+1)
  
  #3) Generate data.
  
  # The following code generates 50 covariates. X1-X5 are the trt effect moderators. X6-X15 are strongly correlated with the moderators
  # X16 - X30 are moderately correlated and X31-X50 are weakly correlated.
  
  covars.set1 <- rnorm_multi(N,mu=c(0,1,0.5,0.5),sd=c(1,1,1,0.5),
                             r=c(0.8,0.5,0.1,0.3,0.2,0.2),
                             varnames = c("X1","X6","X16","X31"))
  
  covars.set2 <- rnorm_multi(N,mu=c(0,1,0.5,0.5),sd=c(1,1,1,0.5),
                             r=c(0.8,0.5,0.1,0.3,0.2,0.2),
                             varnames = c("X2","X7","X17","X32"))
  
  covars.set3 <- rnorm_multi(N,mu=c(0,1,0.5,0.5),sd=c(1,1,1,0.5),
                             r=c(0.8,0.5,0.1,0.3,0.2,0.2),
                             varnames = c("X3","X6b","X18","X33"))
  
  covars.set4 <- rnorm_multi(N,mu=c(0,1,0.5,0.5),sd=c(1,1,1,0.5),
                             r=c(0.8,0.5,0.1,0.3,0.2,0.2),
                             varnames = c("X4","X7b","X18b","X34"))
  
  covars.set5 <- rnorm_multi(N,mu=c(0,1,0.5,0.5),sd=c(1,1,1,0.5),
                             r=c(0.8,0.5,0.1,0.3,0.2,0.2),
                             varnames = c("X5","X6c","X18c","X34b"))
  
  
  
  
  
  
  
  
}






