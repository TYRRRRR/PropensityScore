### Propensity Score Weighting ###
# library(MatchIt)
#install.packages("twang")
library(twang)

library(foreign)#allows use of Stata, SPSS, SAS,..., datasets			   
lalonde<-read.dta("C:\\Users\\msgc\\Dropbox (Penn GSE)\\Stats3PHUDCFILY\\Fourth week\\lalonde.dta")

# Load data
# data("lalonde")
dim(lalonde)
fix(lalonde)
head(lalonde)
summary(lalonde)
table(lalonde$treat)
set.seed(1)
###Last week's 1st stage specification
# treat ~ age + educ + married + nodegree + black + hispan + re74 + re75 + I(age^2) + I(educ*age) + I(educ^2)

# twang does not allow to include transformations, these need to be conducted prior
# lalonde$age2 <- lalonde$age^2
# lalonde$educ2 <- lalonde$educ^2
# lalonde$educage <- lalonde$educ*lalonde$age
### Estimate  propensity score lalonde:
ps.lalonde <- ps(treatment ~ age + age_sq + educ + educ_sq + married + nodegree + black + hisp + re74 + re75 + re74_sq + re75_sq + u74 + u75 + u74_hisp + u74_black,
	data = lalonde, #lalonde[lalonde$hisp==1,]
	n.trees=8000,#maximum number of iterations that gbm will run.
	interaction.depth=3,#controls the level of interactions allowed in the GBM, with default =3
	shrinkage=0.05,#Shrinkage is commonly used in ridge regression where it literally shrinks regression coefficients to zero and, thus, reduces the impact of potentially unstable regression coefficients. In the context of GBMs, shrinkage is used for reducing, or shrinking, the impact of each additional fitted base-learner. It reduces the size of incremental steps and thus penalizes the importance of each consecutive iteration http://www.ncbi.nlm.nih.gov/pmc/articles/PMC3885826/
	perm.test.iters=100,#of Monte Carlo trials are run to establish the reference distribution of KS statistics for each covariate
	stop.method=c("es.mean"),#specifies a set (or sets) of rules and measures for assessing the balance, or equivalence, established on the pretreatment covariates of the weighted treatment and control groups. The ps function selects the optimal number of GBM iterations to minimize the differences between the treatment and control groups as measured by the rules of the given stop.method object.
	estimand = "ATT",
	verbose=FALSE)

summary(ps.lalonde)

trellis.device(color = TRUE)
plot(ps.lalonde, main="Generalized Boosted Regression Models", col=F)
#To get relative influence
tableModel<-summary(ps.lalonde$gbm.obj, n.trees=ps.lalonde$desc$es.mean.ATT$n.trees, plot=F)
rownames(tableModel)<-NULL
library(xtable)
colnames(tableModel) <- c("Variable","Rel Influence")
xtable(tableModel,
caption = "Model fitted for PSM and relative influence of each variable",
label = "MOdelRelInfTYDCFILY",
digits = c(0,0, 2),
align=c("l","r","r"))

# # # Graphical assessment
plot(ps.lalonde, plots=2)#common support
plot(ps.lalonde, plots=3)#stand diff
plot(ps.lalonde, plots=4)


lalonde.balance <- bal.table(ps.lalonde)
lalonde.balance

install.packages("xtable")
library(xtable)
pretty.tab <- lalonde.balance$es.mean.ATT[,c("tx.mn","ct.mn","p")]
pretty.tab <- cbind(pretty.tab, lalonde.balance$unw[,c("ct.mn","p")])
xtable(pretty.tab,
       caption = "Balance of the treatment and comparison groups",
       label = "tab:balance",
       digits = c(0, 2, 2, 4, 2,2),
       align=c("l","r","r","r","r","r"))
# install.packages("survey")

# lalonde$ps<-ps.lalonde$ps
# lalonde$w2<-ps.lalonde$w


library(survey)
lalonde$w <- get.weights(ps.lalonde, stop.method="es.mean")#original dataset
design.ps <- svydesign(ids=~1, weights=~w, data=lalonde) #getting a new dataset with weights
summary(design.ps$variables$w)
names(design.ps)
class(design.ps)
glm1 <- svyglm(re78 ~ treatment, design=design.ps)
summary(glm1)

lm1 <- lm(re78 ~ treatment, data=lalonde)
summary(lm1)

psweight.est <- lm(re78 ~ treatment, data = lalonde, weights = w)
summary(psweight.est)

# Doubly Robust
DB.est <- lm(re78 ~ treatment + age + age_sq + educ + educ_sq + married + nodegree + black + hisp + re74 + re75 + re74_sq + re75_sq + u74 + u75 + u74_hisp + u74_black, data = lalonde)
summary(DB.est)

glm2 <- svyglm(re78 ~ treatment + age + age_sq + educ + educ_sq + married + nodegree + black + hisp + re74 + re75 + re74_sq + re75_sq + u74 + u75 + u74_hisp + u74_black, design=design.ps, , weights = w)
summary(glm2)

library(foreign)
lalonde<-read.dta("path/lalonde.dta")

library(texreg)
table <- texreg(list(lm1, glm1, psweight.est, DB.est, glm2), use.packages=TRUE, label="tab:3", caption="Example", scriptsize=FALSE, custom.model.names=c("(1)","(2)","(3)", "(4)","(5)"), float.pos="b")
table

write.csv(lalonde, "weighteddata.csv", row.names=FALSE)









