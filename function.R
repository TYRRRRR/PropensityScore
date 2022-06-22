library(tidyverse)
library(psych)

df <- read_csv("weightSimcalcAcs.csv")

runs <- 100

#Create Matrices
PsSample <- matrix(NA, nrow = 0, ncol = 7)
dimnames(PsSample)<-list(NULL, c("Mean","sd","Median","Min","Max","skewness","kurtosis"))

IpwSample <- matrix(NA, nrow = 0, ncol = 7)
dimnames(IpwSample)<-list(NULL, c("Mean","sd","Median","Min","Max","skewness","kurtosis"))

PsPopulation <- matrix(NA, nrow = 0, ncol = 7)
dimnames(PsPopulation)<-list(NULL, c("Mean","sd","Median","Min","Max","skewness","kurtosis"))

IpwPopulation <- matrix(NA, nrow = 0, ncol = 7)
dimnames(IpwPopulation)<-list(NULL, c("Mean","sd","Median","Min","Max","skewness","kurtosis"))



i <- 1
while (i <=3*runs ){
    dfsample <- df[df[,(i+2)]==1,]
    PsSample <- rbind(PsSample,unname(unlist(describe(unlist(dfsample[,i])))[c(3:5,8,9,11,12)]))
    IpwSample <- rbind(IpwSample,unname(unlist(describe(unlist(dfsample[,i+1])))[c(3:5,8,9,11,12)]))
    
    dfpopulation <- df[df[,(i+2)]==0,]
    PsPopulation <- rbind(PsPopulation,unname(unlist(describe(unlist(dfpopulation[,i])))[c(3:5,8,9,11,12)]))
    IpwPopulation <- rbind(IpwPopulation,unname(unlist(describe(unlist(dfpopulation[,i+1])))[c(3:5,8,9,11,12)]))
    #update i  
    i <- i+3
}




(colMeans(PsSample))

(colMeans(IpwSample))

(colMeans(PsPopulation))

(colMeans(IpwPopulation))