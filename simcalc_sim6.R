# Covariates and generalization

library(MatchIt)
library(dplyr)
library(overlapping)
library(parallel)
library(generalize)

options(mc.cores = parallel::detectCores(logical = FALSE))

#0) Import data.
d <- read.csv("simcalc_merged4.csv")

# Source functions.
source("Bidx.R")

#1) Set up simulation parameters

# Covariates from all three data sources

covars <- c("CPSTTENA","CPSTEXPA","CPSTKIDR","CPSTBLFP","CPSTHIFP","CPSTTOFC","CPST00FP","CPST01FP","CPST20FP","CPETDISP","CPERRA7R","CPEMALLP","CPETG07P","CPETG07C","CPETBLAP","CPETHISP","CPETLEPP","CPETECOP","CPETRSKP","CA007TR07R","CA007TM07R","CA311TM07R","CA311TA07R","CA311CM07R","CA311CA07R","RURAL",
            "CAMP_SPER","CAMP_LEPR","CAMP_ECNR","CAMP_ATRR","CAMP_HSR" ,"CAMP_ALLR","C_RATING_A","C_RATING_E","C_RATING_L","C_RATING_R","C_RATING_X",
            "GRDTYPE_B","GRDTYPE_E","GRDTYPE_M","GRDTYPE_S",
            "pct_black_ACS","pct_his_ACS","pct_notcitizen_ACS","pct_englishonly_ACS","pct_spanish_ACS",
            "pct_single_mother_ACS",
            "pct_below10000_ACS",
            "pct_noincome_ACS",
            "medianincome_ACS",
            "pct_lesshigh_ACS",
            "pct_bachelor_ACS",
            "pct_abovebachelor_ACS",
            "pct_foodstamp_ACS",
            "pct_below100_ACS",
            "pct_above150_ACS",
            "pct_bw100149_ACS",
            "pct_unemploy_ACS")

d <- d[,c("CAMPUS",covars,"prob")]

d <- d[complete.cases(d),] # 1390 complete cases with all three data sources and based on 54 variables

num.samp <- round(0.05*nrow(d)) # Specifies that the sample size should be 5% of the population size

runs <- 500 # Number of simulation replications

num.strata <- 5

ns <- c(4,5) # Vector storing the number of strata

# Array containing estimates of PATE
estimators <- array(NA,dim=c(runs,2,length(ns)))

dimnames(estimators) <- list(NULL,c("IPW","subclassification"),c("4strata","5strata"))

# Array containing the calculations of common support

sim.array <- array(NA,dim=c(runs,5,length(ns)))

dimnames(sim.array) <- list(NULL,c("covars.asmd","prop.asmd","B-index","overlap","overlap.index"),c("4strata","5strata")) # Naming the dimensions of the array

# Array containing the ASMD calculations for the covariates

covars.asmd <- array(NA,dim=c(runs,length(covars),length(ns)))

dimnames(covars.asmd) <- list(NULL,covars,c("4strata","5strata"))

# Array containing the RMSE and bias of the IPW and subclassification estimator

bias.array <- mse.array <- array(NA,dim=c(runs,2,length(ns)))

dimnames(bias.array) <- dimnames(mse.array) <- list(NULL,c("IPW","subclassification"),c("4strata","5strata"))

#####################################################################################################################

for(i in 1:runs){
  # browser()
  
  set.seed(i+1)
  
  #3) Select the schools into the sample using the true propensity scores. Note that the sample should be 5% of the population.
  
  sel.samp <- sample(d$CAMPUS,
                     size = num.samp,
                     prob = d$prob,
                     replace = FALSE)
  
  d$study<-ifelse((d$CAMPUS %in% sel.samp),1,0) # Creates the "study" variable by setting it to 1 if the school was selected and 0 otherwise.
  
  #4) Randomly assign half the schools to treatment and the other half to control.
  
  sel.trt <- sample(subset(d,d$study==1)[,"CAMPUS"],size=round(0.5*num.samp),replace=F)
  
  d$trt <- ifelse((d$CAMPUS %in% sel.trt),1,0) # Creates the "trt" variable by setting it to 1 if the school was assigned treatment and 0 otherwise.
  
  # Scale the covariates
  
  d <- data.frame(cbind(d$CAMPUS,data.frame(scale(d[c(covars)])),d$study,d$trt))
  
  colnames(d)[1]<-"CAMPUS"
  
  #5) Simulate the potential outcomes for each school in the population and sample.
  
  beta <- as.matrix(c(rep(0.8,6),rep(0.3,4)))
  
  for (j in 1:nrow(d)){
    d$r[j] <- rnorm(1) # random error term
    d$ytrt[j] <- sum(beta[1:6]*d[j,c("CA007TR07R","CA007TM07R","CA311TM07R","CA311TA07R","CA311CM07R","CA311CA07R")],beta[7:10]*d[j,c("CPSTTENA","CPSTTOFC","CPST20FP","CPSTEXPA")]) + d[j,"r"] + 1 + sum(beta[1:6]*d[j,c("CA007TR07R","CA007TM07R","CA311TM07R","CA311TA07R","CA311CM07R","CA311CA07R")])
    d$yc[j] <- sum(beta[1:6]*d[j,c("CA007TR07R","CA007TM07R","CA311TM07R","CA311TA07R","CA311CM07R","CA311CA07R")],beta[7:10]*d[j,c("CPSTTENA","CPSTTOFC","CPST20FP","CPSTEXPA")]) + d[j,"r"]
  } # Trt effect moderators are CA007TR07R, CA007TM07R, CA311TM07R, CA311TA07R, CA311CM07R, CA311CA07R
  
  #6) Estimate the propensity scores using logistic regression.
  
  covars.sim <- as.matrix(scale(data.frame(d[c(covars)]))) # Matrix of scaled covariate values
  
  for (j in 4:num.strata) {
    # browser()
    
    pos <- match(j,ns) # Match the number of strata with the index in the arrays
    
    mod <- matchit(d.study ~  covars.sim, data = data.frame(d), method = "subclass", subclass = j, estimand = "ATE", distance = "glm")
    
    # Extract the propensity scores
    
    matched<- match.data(mod)
    
    d_new <- data.frame(cbind(d,matched[c("subclass","distance")]))
    
    d_sample <- d_new[d_new$d.study==1,] # Sample data
    
    d_pop <- d_new[d_new$d.study==0,] # Non-sample data
    
    #7) Calculate measures of common support
    
    # ASMD for covariates
    
    for(k in 1:length(covars)){
      
      covars.asmd[i,k,pos] <- abs(mean(d_sample[,covars[k]])-mean(d_pop[,covars[k]]))/sd(d_new[,covars[k]])
    }
    
    sim.array[i,"covars.asmd",pos] <- apply(data.frame(covars.asmd[i,,pos]),2,mean)
    
    # ASMD for propensity scores
    
    sim.array[i,"prop.asmd",pos] <- (mean(d_sample$distance)-(mean(d_pop$distance)))/sd(d_new$distance)
    
    # B-index based on propensity scores
    
    sim.array[i,"B-index",pos] <- Bindex(d_sample$distance,d_pop$distance)
    
    # Overlap based on propensity scores
    
    sim.array[i,"overlap",pos] <- length(which(d_pop$distance >= min(d_sample$distance) & (d_pop$distance <= max(d_sample$distance))))/nrow(d_new) # Proportion of population schools whose propensity scores fall in the range of the sample
    
    # Overlap Index
    
    out <- overlap(x = list(t= d_sample$distance,c=d_pop$distance))
    
    sim.array[i,"overlap.index",pos] <- out$OV
    
    #8) Calculate the PATE using the IPW and subclassification estimator.
    
    truth <- mean(d_new$ytrt) - mean(d_new$yc) # True PATE
    
    # IPW
    
    for (g in 1:nrow(d_sample)){
      d_sample$yobs[g] <-ifelse(d_sample$d.trt[g]==1,d_sample$ytrt[g], d_sample$yc[g])
    } # Observed outcome variable
    d_pop$yobs <- d_pop$d.trt <- NA
    d_stack <- data.frame(rbind(d_sample,d_pop))
    
    print(paste(i,"ipw",pos))
    skip_to_next <- FALSE
    
    tryCatch({mod.ipw <- generalize(outcome="yobs",treatment = "d.trt",trial = "d.study",selection_covariates = covars,data=d_stack,is_data_disjoint = T,method="weighting") # Gets the IPTW estimator using the 'generalize' package
    }, error=function(e){skip_to_next <<- TRUE})
    
    if(skip_to_next){
      
      estimators[i,"IPW",pos] <- NA
      
      next
      
    }else{
      
      mod.ipw <- generalize(outcome="yobs",treatment = "d.trt",trial = "d.study",selection_covariates = covars,data=d_stack,is_data_disjoint = T,method="weighting") # Gets the IPTW estimator using the 'generalize' package
      
      estimators[i,"IPW",pos] <- mod.ipw$TATE$estimate
      }
    
    # Subclassification
    
    sub_mat <- matrix(NA,j,1)
    rownames(sub_mat) <- c(paste("Stratum",c(1:j),sep=""))
    
    for (l in 1:j){
      #set up for classifications estimator formula
      tempDF <- d_new[as.numeric(d_new$subclass)== l,]
      n0 <- nrow(d_pop)
      n0prime <- nrow(tempDF[tempDF$d.study == 0,])
      weight <- n0prime /n0
      #check if there are no treated case 
      if (sum(tempDF$d.trt)==0){
        #if no treated case, set Pate to NA and escape the loop
        sub_mat[l,1] <- NA
      }else{
        #else, calculate weighted ATE for that subclass
        ateTreat <- mean(tempDF$ytrt[tempDF$d.trt==1])
        ateControl <- mean(tempDF$yc[tempDF$d.trt==0])
       
        sub_mat[l,1] <- weight*(ateTreat-ateControl)
      }
    }
    
    estimators[i,"subclassification",pos] <- ifelse(any(is.na(sub_mat)),NA,sum(sub_mat))
    
    #9) Bias and MSE of the estimates
    
    bias.array[i,"IPW",pos] <- (estimators[i,"IPW",pos]) - truth
    bias.array[i,"subclassification",pos] <- (estimators[i,"subclassification",pos]) - truth
    
    mse.array[i,"IPW",pos] <- (estimators[i,"IPW",pos]-truth)^2
    mse.array[i,"subclassification",pos] <- ((estimators[i,"subclassification",pos])-truth)^2
    
  }
}
