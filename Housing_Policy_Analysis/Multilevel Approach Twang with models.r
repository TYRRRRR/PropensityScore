#In multilevel settings, propensity score weights should be calculated within strata so that weights are estimated within nesting units per treatment status, rather than across nesting units. By extension, weights may be nested by relevant attributes, such as age groups.

library(gtools)
library(foreign)
library(twang)
library(survey)
library(dplyr)
a<-read.dta(sprintf("https://docs.google.com/uc?id=%s&export=download", "12Wjp4AiFXC77CcxgCdQpEkOg0u7A_SSt"))

#Will estimate weights within age groups
a$age_group <- quantcut( a$age, 4) 
table(a$age_group)
#Iterates the weight estimation process per nesting unit 
tmp <- with(a, by(a, age_group,
               function(x) ps(treatment ~ educ + educ_sq + married + nodegree + black + hisp + re74 + re75 + re74_sq + re75_sq + u74 + u75 + u74_hisp + u74_black,
			data = x, 
			n.trees=8000,
			interaction.depth=3,
			shrinkage=0.05,
			perm.test.iters=100,
			stop.method=c("es.mean"),
			estimand = "ATT",
			verbose=FALSE)))
			
b<-sapply(tmp, `[`, cbind("w","data"), simplify = FALSE, USE.NAMES = FALSE)

b1<-bind_rows(b)
b1<-cbind(b1$w, b1$data)
dim(b1)
dim(a)

###Extract Table
balance_t <- dx.wts(b1$es.mean.ATT, 
					data = b1[,c("age", "age_sq", "educ", "educ_sq", "married", "nodegree", "black", 
					"hisp", "re74", "re75", "re74_sq", "re75_sq", "u74", "u75", "u74_hisp", "u74_black", 
					"treatment")], treat="treatment", estimand="ATT")
bal.table(balance_t)

library(xtable)
pretty.tab <- balance_t$desc[[2]]$bal.tab$results[,c("tx.mn","tx.sd","ct.mn","ct.sd","p")]
pretty.tab <- cbind(pretty.tab, balance_t$ desc$ unw$ bal.tab$ results[,c("ct.mn","ct.sd","p")])
colnames(pretty.tab)<-c("Mean|Tr=1","S.D.|Tr=1","Mean|Tr=0,w","S.D.|Tr=0,w","p|w","Mean|Tr=0,w","S.D.|Tr=0","p")
xtable(pretty.tab,
caption = "Balance of the treatment and comparison groups",
label = "tab:balance",
digits = c(0, 2, 2, 2, 2,2, 2, 2,2),
align=c("l","r","r","r","r","r","r","r", "r"))

#not clustered
ps.lalonde <- ps(treatment ~ age + age_sq + educ + educ_sq + married + nodegree + black + hisp + re74 + re75 + re74_sq + re75_sq + u74 + u75 + u74_hisp + u74_black,
	data = a, #lalonde[lalonde$hisp==1,]
	n.trees=8000,#maximum number of iterations that gbm will run.
	interaction.depth=3,#controls the level of interactions allowed in the GBM, with default =3
	shrinkage=0.05,#Shrinkage is commonly used in ridge regression where it literally shrinks regression coefficients to zero and, thus, reduces the impact of potentially unstable regression coefficients. In the context of GBMs, shrinkage is used for reducing, or shrinking, the impact of each additional fitted base-learner. It reduces the size of incremental steps and thus penalizes the importance of each consecutive iteration http://www.ncbi.nlm.nih.gov/pmc/articles/PMC3885826/
	perm.test.iters=100,#of Monte Carlo trials are run to establish the reference distribution of KS statistics for each covariate
	stop.method=c("es.mean"),#specifies a set (or sets) of rules and measures for assessing the balance, or equivalence, established on the pretreatment covariates of the weighted treatment and control groups. The ps function selects the optimal number of GBM iterations to minimize the differences between the treatment and control groups as measured by the rules of the given stop.method object.
	estimand = "ATT",
	verbose=FALSE)

lalonde.balance <- bal.table(ps.lalonde)
pretty.tab <- lalonde.balance$es.mean.ATT[,c("tx.mn","ct.mn","p")]
pretty.tab <- cbind(pretty.tab, lalonde.balance$unw[,c("ct.mn","p")])
xtable(pretty.tab,
       caption = "Balance of the treatment and comparison groups",
       label = "tab:balance",
       digits = c(0, 2, 2, 4, 2,2),
       align=c("l","r","r","r","r","r"))

psweight.est <- lm(re78 ~ treatment, data = b1, weights = es.mean.ATT)
summary(psweight.est)
	   
a$w <- get.weights(ps.lalonde, stop.method="es.mean")#original dataset
psweight.est <- lm(re78 ~ treatment, data = a, weights = w)
summary(psweight.est)

psweight.est <- lm(re78 ~ treatment + educ + educ_sq + married + nodegree + black + hisp + re74 + re75 + re74_sq + re75_sq + u74 + u75 + u74_hisp + u74_black, data = b1, weights = es.mean.ATT)
summary(psweight.est)	   

DB.est <- lm(re78 ~ treatment + educ + educ_sq + married + nodegree + black + hisp + re74 + re75 + re74_sq + re75_sq + u74 + u75 + u74_hisp + u74_black, data = a, weights = w)
summary(DB.est)


###multilevel
psw.design <- svydesign(ids=~age_group,#cluster ID
                nest=T,#indicating that data is actually nested
                 weights =~es.mean.ATT,#the weight variable
                 data=b1)#the dataset

#fit outcome model with Taylor series to estimate standard errors
psw.outcome.model <- svyglm(formula = re78~treatment,#formula
                           family=gaussian(),#quasi binomial option includes an additional variance term compared to binomial
                           design=psw.design) #Forces package to use the design described above
summary(psw.outcome.model)
