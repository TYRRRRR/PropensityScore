library(twang)
library(xtable)
library(stargazer)
library(gtools)
library(foreign)
library(dplyr)
df <- read.csv("C:/Users/tong2/Desktop/EDUC 736/final/finalProject.csv")
colnames(df)
prop.table(table(df$HOUSING_STS))

naivemodel1 <- lm(culGPA ~ HOUSING_STS, data = df)
naivemodel2 <- lm(gpa1 ~ HOUSING_STS, data = df)
naivemodel3 <- lm(gpa2 ~ HOUSING_STS, data = df)
summary(naivemodel1)
summary(naivemodel2)
summary(naivemodel3)



naivemodel11 <- lm(culGPA ~ STDNT_AGE + STDNT_GENDER + STDNT_TEST_ENTRANCE_COMB + HIGH_SCHL_GPA + isSTEM + HOUSING_STS, data = df)
naivemodel22 <- lm(gpa1 ~ STDNT_AGE + STDNT_GENDER + STDNT_TEST_ENTRANCE_COMB + HIGH_SCHL_GPA + isSTEM +HOUSING_STS, data = df)
naivemodel33 <- lm(gpa2 ~ STDNT_AGE + STDNT_GENDER + STDNT_TEST_ENTRANCE_COMB + HIGH_SCHL_GPA + isSTEM +HOUSING_STS, data = df)
summary(naivemodel11)
summary(naivemodel22)
summary(naivemodel33)

ps.final <- ps(HOUSING_STS ~  STDNT_AGE + STDNT_GENDER + STDNT_TEST_ENTRANCE_COMB + HIGH_SCHL_GPA + isSTEM + white + black + hisp + other,
                 data = df,
                 n.trees=8000,#maximum number of iterations that gbm will run.
                 interaction.depth=3,#controls the level of interactions allowed in the GBM, with default =3
                 shrinkage=0.05,#Shrinkage is commonly used in ridge regression where it literally shrinks regression coefficients to zero and, thus, reduces the impact of potentially unstable regression coefficients. In the context of GBMs, shrinkage is used for reducing, or shrinking, the impact of each additional fitted base-learner. It reduces the size of incremental steps and thus penalizes the importance of each consecutive iteration http://www.ncbi.nlm.nih.gov/pmc/articles/PMC3885826/
                 perm.test.iters=100,#of Monte Carlo trials are run to establish the reference distribution of KS statistics for each covariate
                 stop.method=c("es.mean"),#specifies a set (or sets) of rules and measures for assessing the balance, or equivalence, established on the pretreatment covariates of the weighted treatment and control groups. The ps function selects the optimal number of GBM iterations to minimize the differences between the treatment and control groups as measured by the rules of the given stop.method object.
                 estimand = "ATT",
                 verbose=FALSE)





summary(ps.final)
plot(ps.final, main="Generalized Boosted Regression Models", col=F)
plot(ps.final, plots=2)#common support
plot(ps.final, plots=3)#stand diff
plot(ps.final, plots=4)


lalonde.balance <- bal.table(ps.final)
lalonde.balance$unw

pretty.tab <- lalonde.balance$es.mean.ATT[,c("tx.mn","ct.mn","p")]
pretty.tab <- cbind(pretty.tab, lalonde.balance$unw[,c("ct.mn","p")])
xtable(pretty.tab,
       caption = "Balance of the treatment and comparison groups",
       label = "tab:balance",
       digits = c(0, 2, 2, 4, 2,2),
       align=c("l","r","r","r","r","r"))


library(survey)
df$w <- get.weights(ps.final, stop.method="es.mean")#original dataset
design.ps <- svydesign(ids=~1, weights=~w, data=df) #getting a new dataset with weights
summary(design.ps$variables$w)
names(design.ps)


psweight.est <- lm(culGPA ~ HOUSING_STS, data = df, weights = w)
summary(psweight.est)

glm2 <- svyglm(culGPA ~ HOUSING_STS + STDNT_AGE + STDNT_GENDER + STDNT_TEST_ENTRANCE_COMB + HIGH_SCHL_GPA + isSTEM, design=design.ps, weights = w)
summary(glm2)

df$score_group <- quantcut(df$STDNT_TEST_ENTRANCE_COMB, 4) 
table(df$score_group)
df$STDNT_TEST_ENTRANCE_COMB
tmp <- with(df, by(df, score_group,
                  function(x) ps(HOUSING_STS ~  STDNT_AGE + STDNT_GENDER + STDNT_TEST_ENTRANCE_COMB + HIGH_SCHL_GPA + isSTEM + white + black + hisp + other,
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
colnames(b1)
dim(b1)
dim(a)
b1
###Extract Table
balance_t <- dx.wts(b1$es.mean.ATT, 
                    data = b1[,c("STDNT_AGE","STDNT_GENDER","white" , "black" ,"hisp","other","gpa1" ,"gpa2" , "culGPA","isSTEM","HOUSING_STS" ,"FIRST_TERM_EARNED_HRS","SECOND_TERM_EARNED_HRS","STDNT_TEST_ENTRANCE_COMB" ,"HIGH_SCHL_GPA")], treat="HOUSING_STS", estimand="ATT")
bal.table(balance_t)


pretty.tab <- balance_t$desc[[2]]$bal.tab$results[,c("tx.mn","tx.sd","ct.mn","ct.sd","p")]
pretty.tab <- cbind(pretty.tab, balance_t$ desc$ unw$ bal.tab$ results[,c("ct.mn","ct.sd","p")])
colnames(pretty.tab)<-c("Mean|Tr=1","S.D.|Tr=1","Mean|Tr=0,w","S.D.|Tr=0,w","p|w","Mean|Tr=0,w","S.D.|Tr=0","p")
xtable(pretty.tab,
       caption = "Balance of the treatment and comparison groups",
       label = "tab:balance",
       digits = c(0, 2, 2, 2, 2,2, 2, 2,2),
       align=c("l","r","r","r","r","r","r","r", "r"))


###multilevel
psw.design <- svydesign(ids=~score_group,#cluster ID
                        nest=T,#indicating that data is actually nested
                        weights =~es.mean.ATT,#the weight variable
                        data=b1)#the dataset
psw.outcome.model <- svyglm(formula = culGPA ~ HOUSING_STS,#formula
                            family=gaussian(),#quasi binomial option includes an additional variance term compared to binomial
                            design=psw.design) #Forces package to use the design described above
summary(psw.outcome.model)
