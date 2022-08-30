###################################################
### chunk number 1: 
###################################################
install.packages("Synth")
library("Synth")
data("basque")
class(basque) 
str(basque)
dim(basque)
fix(basque)	
names(basque)
###################################################
### chunk number 2: 
###################################################
basque[85:89, 1:4]
###################################################
### chunk number 3: 
###################################################
 dataprep.out <-
 dataprep(foo = basque,
          predictors = c("school.illit" , "school.prim" , "school.med" , #predictors with not
                         "school.high" , "school.post.high" , "invest") ,#missing data 64-69
          predictors.op = "mean" ,
          time.predictors.prior = 1964:1969,
          special.predictors = list(
            list("gdpcap" , 1960:1969 , "mean"),
            list("sec.agriculture" ,      seq(1961,1969,2), "mean"),
            list("sec.energy" ,           seq(1961,1969,2), "mean"),
            list("sec.industry" ,         seq(1961,1969,2), "mean"),
            list("sec.construction" ,     seq(1961,1969,2), "mean"),
            list("sec.services.venta" ,   seq(1961,1969,2), "mean"),
            list("sec.services.nonventa" ,seq(1961,1969,2), "mean"),
            list("popdens", 1969, "mean")
                                   ),
          dependent = "gdpcap",
          unit.variable = "regionno",
          unit.names.variable = "regionname",
          time.variable = "year",
          treatment.identifier = 17, #"Basque Country (Pais Vasco)"
          controls.identifier = c(2:16,18),
          time.optimize.ssr = 1960:1969,
          time.plot = 1955:1997
          )
ls()	
###################################################
### chunk number 4: 
###################################################
class(dataprep.out)
names(dataprep.out)
dataprep.out$X1
dataprep.out$X0
cbind( dataprep.out$X1, dataprep.out$X0)	
###################################################
### chunk number 5: 
###################################################
dataprep.out$Z1
dataprep.out$Z0
	
###################################################
### chunk number 6: Aggregating school high and post-high for X1 and X0 
###################################################
 dataprep.out$X1["school.high",] <- dataprep.out$X1["school.high",] + dataprep.out$X1["school.post.high",]
 dataprep.out$X1 <-   as.matrix(dataprep.out$X1[-which(rownames(dataprep.out$X1)=="school.post.high"),])
 dataprep.out$X0["school.high",] <-   dataprep.out$X0["school.high",] + dataprep.out$X0["school.post.high",]
 dataprep.out$X0 <-  dataprep.out$X0[-which(rownames(dataprep.out$X0)=="school.post.high"),]

lowest  <- which(rownames(dataprep.out$X0)=="school.illit")
highest <- which(rownames(dataprep.out$X0)=="school.high")

 dataprep.out$X1[lowest:highest,] <-  (100*dataprep.out$X1[lowest:highest,]) /  sum(dataprep.out$X1[lowest:highest,])
 dataprep.out$X0[lowest:highest,] <-   100*scale(dataprep.out$X0[lowest:highest,], center=FALSE, scale=colSums(dataprep.out$X0[lowest:highest,]))
	
###################################################
### chunk number 7: 
###################################################
synth.out <- synth(data.prep.obj = dataprep.out,
                    optimxmethod = "BFGS")

class(synth.out)
names(synth.out)
str(synth.out)	
###################################################
### chunk number 8: 
###################################################
 # To compute the annual discrepancies in the GPD trend between the Basque region and its synthetic counterpart we type
 gaps <- dataprep.out$Y1plot - (dataprep.out$Y0plot %*% synth.out$solution.w)
 gaps[1:3, 1]

# Then we can plot gaps over time
plot(c(1955:1997),gaps) 
###################################################
### chunk number 9: 
###################################################
 synth.tables <- synth.tab(dataprep.res = dataprep.out,
                           synth.res = synth.out
                           )
	
###################################################
### chunk number 10: 
###################################################
 names(synth.tables)
	
###################################################
### chunk number 11: 
###################################################
synth.tables$tab.pred[1:5, ]
	
###################################################
### chunk number 12: 
###################################################
synth.tables$tab.w[8:14, ]
	
###################################################
### chunk number 13: 
###################################################
pdf("Example141316.pdf",width=7.5,height=5, bg="lightgoldenrod1")
 path.plot(synth.res = synth.out,
           dataprep.res = dataprep.out,
           Ylab = "real per-capita GDP (1986 USD, thousand)",
           Xlab = "year",
           Ylim = c(0,12),
           Legend = c("Basque country","synthetic Basque country"),
           Legend.position = "bottomright",
		   abline(v=1970,lty="dotted",lwd=2)           
           )
dev.off()	
###################################################
### chunk number 14: 
###################################################
pdf("gap41316.pdf",width=7.5,height=5, bg="lightgoldenrod1")
 gaps.plot(synth.res = synth.out,
           dataprep.res = dataprep.out,
           Ylab = "gap in real per-capita GDP (1986 USD, thousand)",
           Xlab = "year",
           Ylim = c(-1.5,1.5),
           Main = NA,
		   abline(v=1970,lty="dotted",lwd=2)
           )
dev.off()	


###################################################
### chunk number 15:  Placebo Test
###################################################
 dataprep.out <-
              dataprep(foo = basque,
                       predictors = c("school.illit" , "school.prim" , "school.med" ,
                                      "school.high" , "school.post.high" , "invest") ,
                       predictors.op = "mean" ,
                       time.predictors.prior = 1964:1969 ,
                       special.predictors = list(
                         list("gdpcap" , 1960:1969 , "mean"),
                         list("sec.agriculture" ,      seq(1961,1969,2), "mean"),
                         list("sec.energy" ,           seq(1961,1969,2), "mean"),
                         list("sec.industry" ,         seq(1961,1969,2), "mean"),
                         list("sec.construction" ,     seq(1961,1969,2), "mean"),
                         list("sec.services.venta" ,   seq(1961,1969,2), "mean"),
                         list("sec.services.nonventa" ,seq(1961,1969,2), "mean"),
                         list("popdens", 1969, "mean")
                                                ),
                       dependent = "gdpcap",
                       unit.variable = "regionno",
                       unit.names.variable = "regionname",
                       time.variable = "year",
                       treatment.identifier = 10,
                       controls.identifier = c(2:9,11:16,18),
                       time.optimize.ssr = 1960:1969,
                       time.plot = 1955:1997
                       )
	
 dataprep.out$X1["school.high",] <-
   dataprep.out$X1["school.high",] + dataprep.out$X1["school.post.high",]
 dataprep.out$X1 <-
   as.matrix(dataprep.out$X1[-which(rownames(dataprep.out$X1)=="school.post.high"),])
 dataprep.out$X0["school.high",] <-
   dataprep.out$X0["school.high",] + dataprep.out$X0["school.post.high",]
 dataprep.out$X0 <-
   dataprep.out$X0[-which(rownames(dataprep.out$X0)=="school.post.high"),]

 lowest  <- which(rownames(dataprep.out$X0)=="school.illit")
 highest <- which(rownames(dataprep.out$X0)=="school.high")

 dataprep.out$X1[lowest:highest,] <-
  (100*dataprep.out$X1[lowest:highest,]) /
   sum(dataprep.out$X1[lowest:highest,])
 dataprep.out$X0[lowest:highest,] <-
   100*scale(dataprep.out$X0[lowest:highest,],
             center=FALSE,
             scale=colSums(dataprep.out$X0[lowest:highest,])
  )


synth.out <- synth(
                   data.prep.obj = dataprep.out,
                   method = "BFGS"
                   )
				   
pdf("PlaceboCataluna41316.pdf",width=7.5,height=5, bg="lightgoldenrod1")
path.plot(synth.res = synth.out,
          dataprep.res = dataprep.out,
          tr.intake = NA,
          Ylab = "real per-capita GDP (1986 USD, thousand)",
          Xlab = "year",
          Ylim = c(0,12),
          Legend = c("Catalonia country","synthetic Catalonia"),
          Legend.position = "bottomright",
		  abline(v=1970, lty="dotted",lwd=2) 
          )
dev.off()

###################################################
### gap plot Cataluna 
###################################################
pdf("gapplacebo41316.pdf",width=7.5,height=5, bg="lightgoldenrod1")
 gaps.plot(synth.res = synth.out,
           dataprep.res = dataprep.out,
           Ylab = "gap in real per-capita GDP (1986 USD, thousand)",
           Xlab = "year",
           Ylim = c(-1.5,1.5),
           Main = NA,
		   abline(v=1970,lty="dotted",lwd=2)
           )
dev.off()	


###################################################
### chunk number 16:  Gaps Test
###################################################
store <- matrix(NA,length(1955:1997),17)
colnames(store) <- unique(basque$regionname)[-1]

# run placebo test
for(iter in 2:18)
 {
 dataprep.out <-
              dataprep(foo = basque,
                       predictors = c("school.illit" , "school.prim" , "school.med" ,
                                      "school.high" , "school.post.high" , "invest") ,
                       predictors.op = "mean" ,
                       time.predictors.prior = 1964:1969 ,
                       special.predictors = list(
                         list("gdpcap" , 1960:1969 , "mean"),
                         list("sec.agriculture" ,      seq(1961,1969,2), "mean"),
                         list("sec.energy" ,           seq(1961,1969,2), "mean"),
                         list("sec.industry" ,         seq(1961,1969,2), "mean"),
                         list("sec.construction" ,     seq(1961,1969,2), "mean"),
                         list("sec.services.venta" ,   seq(1961,1969,2), "mean"),
                         list("sec.services.nonventa" ,seq(1961,1969,2), "mean"),
                         list("popdens", 1969, "mean")
                                                ),
                       dependent = "gdpcap",
                       unit.variable = "regionno",
                       unit.names.variable = "regionname",
                       time.variable = "year",
                       treatment.identifier = iter,
                       controls.identifier = c(2:18)[-iter+1],
                       time.optimize.ssr = 1960:1969,
                       time.plot = 1955:1997
                       )




 dataprep.out$X1["school.high",] <-
   dataprep.out$X1["school.high",] + dataprep.out$X1["school.post.high",]
 dataprep.out$X1 <-
   as.matrix(dataprep.out$X1[-which(rownames(dataprep.out$X1)=="school.post.high"),])
 dataprep.out$X0["school.high",] <-
   dataprep.out$X0["school.high",] + dataprep.out$X0["school.post.high",]
 dataprep.out$X0 <-
   dataprep.out$X0[-which(rownames(dataprep.out$X0)=="school.post.high"),]

 lowest  <- which(rownames(dataprep.out$X0)=="school.illit")
 highest <- which(rownames(dataprep.out$X0)=="school.high")

 dataprep.out$X1[lowest:highest,] <-
  (100*dataprep.out$X1[lowest:highest,]) /
   sum(dataprep.out$X1[lowest:highest,])
 dataprep.out$X0[lowest:highest,] <-
   100*scale(dataprep.out$X0[lowest:highest,],
             center=FALSE,
             scale=colSums(dataprep.out$X0[lowest:highest,])
  )

# run synth
synth.out <- synth(
                   data.prep.obj = dataprep.out,
                   method = "BFGS"
                   )

# store gaps
store[,iter-1] <- dataprep.out$Y1plot - (dataprep.out$Y0plot %*% synth.out$solution.w)
}

# now do figure
data <- store
rownames(data) <- 1955:1997

# Set bounds in gaps data
gap.start     <- 1
gap.end       <- nrow(data)
years         <- 1955:1997
gap.end.pre  <- which(rownames(data)=="1969")

#  MSPE Pre-Treatment
mse <- apply(data[ gap.start:gap.end.pre,]^2,2,mean)
basque.mse <- as.numeric(mse[16])
# Exclude states with 5 times higher MSPE than basque
data <- data[,mse<5*basque.mse]
Cex.set <- .75

# Plot
pdf("E:/Users/msgc/My Documents/Ph. D/Dropbox/Stats3PHUDCFILY/Tex/placebo1PHUDCFILY.pdf",width=7.5,height=5, bg="lightgoldenrod1")
plot(years,data[gap.start:gap.end,which(colnames(data)=="Basque Country (Pais Vasco)")],
     ylim=c(-2,2),xlab="year",
     xlim=c(1955,1997),ylab="gap in real per-capita GDP (1986 USD, thousand)",
     type="l",lwd=2,col="black",
     xaxs="i",yaxs="i")

# Add lines for control states
for (i in 1:ncol(data)) { lines(years,data[gap.start:gap.end,i],col="gray") }

## Add Basque Line
lines(years,data[gap.start:gap.end,which(colnames(data)=="Basque Country (Pais Vasco)")],lwd=2,col="black")

lines(years,data[gap.start:gap.end,which(colnames(data)=="Cataluna")],lwd=2,col="red")


# Add grid
abline(v=1970,lty="dotted",lwd=2)
abline(h=0,lty="dashed",lwd=2)
legend("bottomright",legend=c("Basque country","control regions"),
lty=c(1,1),col=c("black","gray"),lwd=c(2,1),cex=.8)
arrows(1967,-1.5,1968.5,-1.5,col="black",length=.1)
text(1961.5,-1.5,"Terrorism Onset",cex=Cex.set)
abline(v=1955)
abline(v=1997)
abline(h=-2)
abline(h=2)

dev.off()



# Set bounds in gaps data
gap.start     <- 1
gap.end       <- nrow(data)
years         <- 1970:1997
gap.end.pre  <- which(rownames(data)=="1997")

#  MSPE Post-Treatment
msepost        <-             apply(data[gap.start:gap.end.pre,]^2,2,mean)
basque.msepost <- as.numeric(msepost[16])
# Exclude states with 5 times higher MSPE than basque
basque.msepost/basque.mse
ratios <- as.data.frame(cbind(msepost,mse))
ratios$ratio <- ratios$msepost/ratios$mse
ratios
class(mse)
