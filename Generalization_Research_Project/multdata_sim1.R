######################################################################
# Combining Multiple Data Sources for Generalization Studies 
######################################################################

#0) Load packages and source functions.

library(pacman)
p_load("dbarts","MASS","MatchIt","Matrix","dplyr","overlapping","parallel","generalize","psych","boot","tidyr","faux","pracma")

source("Bidx.R")
source("functions2.R")

options(mc.cores = parallel::detectCores(logical = FALSE))

#1) Simulation parameters

runs <- 500

num.strata <- 5

N <- 2000 # Population size

#2) Outputs

# Array containing estimates of PATE

est.name <- c("IPW","subclassification","bart","tmle")

estimators <- matrix(NA,runs,length(est.name))

colnames(estimators) <- est.name

# Array containing the calculations of common support

support.mat <- matrix(NA,runs,5)

colnames(support.mat) <- c("covars.asmd","prop.asmd","B-index","overlap","overlap.index") # Naming the dimensions of the array

cor.mat <- matrix(NA,runs,50) # Average correlation between moderators and other variables

colnames(cor.mat) <- paste("X",1:50,sep = "") # Yeran

# Array containing the RMSE and bias of estimators

bias.mat <- mse.mat <- matrix(NA,runs,length(est.name))

colnames(bias.mat) <- colnames(mse.mat) <- est.name

# Descriptive statistics

ps.sample.mat <- ps.pop.mat <- matrix(NA,runs,7)

colnames(ps.sample.mat) <- colnames(ps.pop.mat) <- c("Mean","sd","Median","Min","Max","skewness","kurtosis")

weights.sample.mat <- matrix(NA,runs,7)

colnames(weights.sample.mat) <- c("Mean","sd","Median","Min","Max","skewness","kurtosis")

# Matrix containing the errors from IPW

ipw.errors <- matrix(NA,runs,1)

############################################################################################

for(k in 1:runs){
  print(paste("Iteration",k,"starts."))
  # browser()
  
  set.seed(k+1)
  
  #3) Generate data.
  
  num.var <- 9 # total number of variables that includes moderators and the variables correlated with them
  
  # Generate a positive definite covariance matrix for nine variables: X1,X2,X3 are true moderators
  # X4, X5, X6 are strongly correlated (rho = 0.8) with moderators,  X7, X8, X9 are moderately correlated (rho = 0.4) with moderators
  
  sig.mod <- matrix(1,nrow=num.var,ncol=num.var)
  
  sig.mod[upper.tri(sig.mod)] <- c(0.6,0.6,0.6,0.8,0.8,0.8,0.8,0.8,0.8,0.4,
                                   0.8,0.8,0.8,0.4,0.4,
                                   0.4,0.4,0.4,0.4,0.4,0.4,
                                   0.4,0.4,0.4,0.4,0.4,0.4,0.4,
                                   0.4,0.4,0.4,0.4,0.4,0.4,0.4,0.4)
  sig.mod <- t(sig.mod)
  
  sig.mod[upper.tri(sig.mod)] <- c(0.6,0.6,0.6,0.8,0.8,0.8,0.8,0.8,0.8,0.4,
                                   0.8,0.8,0.8,0.4,0.4,
                                   0.4,0.4,0.4,0.4,0.4,0.4,
                                   0.4,0.4,0.4,0.4,0.4,0.4,0.4,
                                   0.4,0.4,0.4,0.4,0.4,0.4,0.4,0.4)
  
  sig.mod <- nearPD(sig.mod,corr=TRUE)$mat # Approximate a positive definite matrix
  
  # Generate a set of weakly correlated variables.
  
  var.remain <- 41 # We intended to have a total of 50 covariates so this is the difference between the total and num.var 
  
  X <- diag(runif(var.remain,min=0,max=1))
  
  U <- randortho(var.remain,type="orthonormal")
  
  sig.mod2 <- t(U) %*% X %*% U
  
  diag(sig.mod2) <- 1
  
  # Combine the two sets of variables to create the data analytic frame.
  
  d <- as.data.frame(cbind(mvrnorm(N,mu=c(0,0,0,0.1,0.1,0.1,0.5,0.5,0.5),Sigma = sig.mod),mvrnorm(N,mu=c(rep(0.3,var.remain)),Sigma = sig.mod2)))
  
  d$idx <- seq(1:nrow(d))
  
  colnames(d) <- c(paste("X",seq(1:(num.var+var.remain)),sep=""),"idx")
  
  # Compute the correlations among moderators and other variables.
  
  for(l in 1:ncol(cor.mat)){
    cor.mat[k,l] <- mean(cor(d)[l,c(10:(ncol(cor(d))-1))])
  }
  
  covars <- paste("X",1:50,sep = "")
  
  #4) Simulate the outcome model and true propensity scores.
  
  beta <- as.matrix(c(rep(0.8,5),rep(0.3,3)))
  
  alpha <- c(rep(0.5,length(c("X1","X2","X3"))),rep(0.1,length(c("X4","X5","X6","X7","X8","X9"))))
  
  for (j in 1:nrow(d)){
    d$r[j] <- rnorm(1) # random error term
    d$yt[j] <- sum(beta[1:5]*d[j,c("X1","X2","X3","X5","X8")]) + d[j,"r"] + 1 + sum(beta[6:8]*d[j,c("X1","X2","X3")])
    d$yc[j] <- sum(beta[1:5]*d[j,c("X1","X2","X3","X5","X8")]) + d[j,"r"]
    
    d$prob[j] <- inv.logit(sum(alpha*d[j,c("X1","X2","X3","X4","X5","X6","X7","X8","X9")],na.rm = T) - 4.1)
    
  } # Trt effect moderators are X1, X2, X3
  
  true.pate <- mean(d$yt)-mean(d$yc)
  
  ##########################################################################################################################################################
  
  #5) Select sample from pop and randomly assign half to treatment and half to control.
  
  sel.samp <- sample(d$idx,
                     size = round(0.05*nrow(d)),
                     prob = d$prob,
                     replace = FALSE)
  
  d$study<-ifelse((d$idx %in% sel.samp),1,0) # Populate the "study" variable by setting it to 1 if the school was selected and 0 otherwise.
  
  sel.trt <- sample(subset(d,d$study==1)[,"idx"],size=round(0.5*length(sel.samp)),replace=F) # Randomly assign half of sample schools to treatment and half to control
  
  d$trt <- ifelse((d$idx %in% sel.trt),1,ifelse(d$idx %in% setdiff(sel.samp, sel.trt),0,NA)) # Populate the "trt" variable
  
  # Add column for observed outcome.
  
  for(j in 1:nrow(d)){
    d$y[j] <- ifelse(d$trt[j]==1,d$yt[j],ifelse(d$trt[j]==0,d$yc[j],NA))
  }
  
  #6) Estimate the propensity scores from the selected sample.
  
  mod <- glm(as.formula(paste("study",paste(covars,collapse="+"),sep="~")),data=d,family="binomial")
  
  d$ps <- predict(mod,type="response")
  
  ps.sample.mat[k,]<-as.matrix(describe(d$ps[d$study==1])[,c(3:5,8,9,11,12)]) # Summary statistics for propensity scores in sample
  
  ps.pop.mat[k,]<-as.matrix(describe(d$ps[d$study==0])[,c(3:5,8,9,11,12)]) # Summary statistics for propensity scores in population (non-study schools)
  
  mod.weights <- weighting(outcome = "y",treatment = "trt",trial="study",selection_covariates = covars,data=d,selection_method = "lr",trim_weights = FALSE)
  
  weights.sample.mat[k,] <- as.matrix(describe(mod.weights$weights)[,c(3:5,8,9,11,12)]) # Summary statistics for propensity score weights in the sample
  
  #7) Compute measures of common support
  
  covars.asmd <- matrix(NA,length(covars),1)
  
  for(i in 1:length(covars)){
    covars.asmd[i,1] <- abs((mean(d[,covars[i]][d$study==1]) - mean(d[,covars[i]][d$study==0]))/sd(d[,covars[i]]))
  }
  
  support.mat[k,"covars.asmd"] <- apply(data.frame(covars.asmd),2,mean)
  
  # ASMD for propensity scores
  
  support.mat[k,"prop.asmd"] <- (mean(d$ps[d$study==1])-mean(d$ps[d$study==0]))/sd(d$ps)
  
  # B-index based on propensity scores
  
  support.mat[k,"B-index"] <- Bindex(d$ps[d$study==1],d$ps[d$study==0])
  
  # Overlap based on propensity scores
  
  support.mat[k,"overlap"] <- length(which(d$ps[d$study==0] >= min(d$ps[d$study==1]) & (d$ps[d$study==0] <= max(d$ps[d$study==1]))))/nrow(d) # Proportion of population schools whose propensity scores fall in the range of the sample
  
  # Overlap Index
  
  support.mat[k,"overlap.index"] <- overlap(x = list(t = d$ps[d$study==1],c=d$ps[d$study==0]))$OV
  
  #8) Estimate the PATE.
  
  # IPW
  
  estimators[k,"IPW"] <- generalize(outcome="y",treatment="trt",trial="study",selection_covariates = covars,data=d,method = "weighting",selection_method = "lr",
                                    survey_weights = FALSE)$TATE$estimate
  
  # Subclassification with five subclasses
  
  mod.sub <- matchit(as.formula(paste("study",paste(covars,collapse="+"),sep="~")),data=d,method = "subclass",subclass=5,distance="glm")
  
  d.sub <- as.data.frame(cbind(d,match.data(mod.sub)[,c("subclass")]))
  
  colnames(d.sub) <- c(colnames(d),"subclass")
  
  sub_mat <- matrix(NA,5,1)
  rownames(sub_mat) <- c(paste("Stratum",c(1:5),sep=""))
  
  for (l in 1:length(unique(d.sub$subclass))){
    tempDF <- d.sub[as.numeric(d.sub$subclass)== l,]
    n0 <- nrow(d.sub)
    n0prime <- nrow(tempDF[tempDF$study == 0,])
    weight <- n0prime /n0
    
    # If there are no trt/control cases in a stratum, set the stratum estimate to NA  
    if (sum(tempDF$trt,na.rm = T)==0){
      sub_mat[l,1] <- NA
    }else{
    sub_mat[l,1] <- weight*(mean(tempDF$y[tempDF$trt==1],na.rm=TRUE) - mean(tempDF$y[tempDF$trt==0],na.rm=TRUE))
    }
  }
  
  estimators[k,"subclassification"] <- ifelse(any(is.na(sub_mat)),NA,sum(sub_mat))
  
  # BART
  
  estimators[k,"bart"] <- generalize_bart(outcome = "y",treatment = "trt",trial="study",selection_covariates = covars,data=d,is_data_disjoint = TRUE)$TATE$estimate
  
  # TMLE
  
  d.tmle <- data.frame(rbind(d[d$study==1,],d[d$study==0,]))
  
  estimators[k,"tmle"] <- generalize_tmle2(outcome = "y",treatment = "trt",trial="study",selection_covariates = covars,data=d.tmle,is_data_disjoint = TRUE)$TATE$estimate
  
  #9) Estimate the bias and MSE of the PATE estimates.
  
  for(l in 1:length(est.name)){
    bias.mat[k,l] <- true.pate - estimators[k,l]
    mse.mat[k,l] <- (true.pate - estimators[k,l])^2
  }
  print(paste("Iteration",k,"ends."))
}

save.image(file = "res.Rdata")
