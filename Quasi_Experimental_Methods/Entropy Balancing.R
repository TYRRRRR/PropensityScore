install.packages("ebal")
install.packages("twang")#to create the balance table
install.packages("texreg")
library(ebal)

eb <- ebalance(Treatment=treatment,
                   X=X)
library(foreign)
a<-read.dta("G:\\My Drive\\Quasi-experimental Penn 2022\\Session three\\lalonde.dta")

eb <- ebalance(Treatment=a$treatment, X=a[,c("age", "educ", "black", "hisp", "married", "age_sq", "educ_sq", "u74", "u75")], max.iterations = 50000)
str(eb)
tail(eb$ base.weight)  
#don't change the original order, or the order after the implementation of ebalance
a$w[a$treatment==0]<-eb$w
a$w[is.na(a$w)]<-1

library(twang)
lalonde.balance <- dx.wts(a$w, data = a, treat="treatment", estimand="ATT")
bal.table(lalonde.balance)

library(xtable)
pretty.tab <- lalonde.balance$ desc[[2]]$bal.tab$results[,c("tx.mn","tx.sd","ct.mn","ct.sd","p")]
pretty.tab <- cbind(pretty.tab, lalonde.balance$ desc$ unw$ bal.tab$ results[,c("ct.mn","ct.sd","p")])
colnames(pretty.tab)<-c("Mean|Tr=1","S.D.|Tr=1","Mean|Tr=0,w","S.D.|Tr=0,w","p|w","Mean|Tr=0,w","S.D.|Tr=0","p")
xtable(pretty.tab,
caption = "Balance of the treatment and comparison groups",
label = "tab:balance",
digits = c(0, 2, 2, 2, 2,2, 2, 2,2),
align=c("l","r","r","r","r","r","r","r", "r"))


lm1 <- lm(re78 ~ treatment, data=a)
summary(lm1)

psweight.est <- lm(re78 ~ treatment, data = a, weights = w)
summary(psweight.est)

# Doubly Robust
DB.est <- lm(re78 ~ treatment + age + age_sq + educ + educ_sq + married + nodegree + black + hisp + re74 + re75, data = a, weights = w)
summary(DB.est)

DB.est <- lm(re78 ~ treatment + nodegree + re74 + re75, data = a, weights = w)
summary(DB.est)


library(texreg)
table <- texreg(list(lm1, psweight.est, DB.est), use.packages=TRUE, label="tab:3", caption="Example", scriptsize=FALSE, custom.model.names=c("(1)","(2)","(3)"), float.pos="b")
table
