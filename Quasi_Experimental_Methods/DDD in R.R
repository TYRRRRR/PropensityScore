# DDD in R

Treated65Over <- c(rep(1,1000))
outcomeTr65Over <- c(seq(.25, .375, length.out = 500),seq(.476, .65, length.out = 500))

PotentiallyTreatedOtherstate65Over <- c(rep(0,1000))
outcomeTreatedOtherstate65Over <- c(seq(.275, .525, length.out = 1000))

#Controls from my state are still doing better in this setting
ControlInStateLess65 <- c(rep(1,1000))
outcomeCtrlInStateLess65 <- c(seq(.15, .345, length.out = 1000))

ControlOtherStateLess65 <- c(rep(0,1000))
outcomeCtrlOtherStateLess65 <- c(seq(.175, .335, length.out = 1000))

Time <- c(rep(0,500), rep(1,500),rep(0,500), rep(1,500))
Time

plotting <- seq(1,1000, length.out = 1000)
plot(plotting,outcomeTr65Over, ylim = c(0, 1),xlab = "Outcomes", ylab = "Pre-post policy implementation",xaxt='n')
lines(plotting,outcomeTreatedOtherstate65Over, col="blue")
lines(plotting,outcomeCtrlInStateLess65, col="red")
lines(plotting,outcomeCtrlOtherStateLess65, col="pink")
abline(v=500,col=3,lty=1)

legend('topright', col=c("black", "blue", "red", "pink"), fill=c("black", "blue", "red", "pink"), legend=c("Affected", "Potentially Affected", "Non-Affected",  "Potentially non-Affected"), title="Outcomes across groups")

axis(1, at=c(250,750),labels=c("Pre-policy","Post-policy"), col.axis="black", las=0, tck=0)


#These data will be used for the difference of two DDs approach, it does not incorporate all the statistical power of DDD
dataAff <- data.frame(AffPotAFF65Over=c(Treated65Over,PotentiallyTreatedOtherstate65Over), Time=c(Time, Time), Outcome65Over=c(outcomeTr65Over,outcomeTreatedOtherstate65Over)) 
str(dataAff)
dim(dataAff)
fix(dataAff)

fit1 <- lm( Outcome65Over ~ AffPotAFF65Over + Time + Time*AffPotAFF65Over,data=dataAff)
summary(fit1)

dataUnaff <- data.frame(UnAFFPotUnAFFLess65=c(ControlInStateLess65,ControlOtherStateLess65), Time=c(Time, Time), OutcomeLess65=c(outcomeCtrlInStateLess65,outcomeCtrlOtherStateLess65)) 

str(dataUnaff)
dim(dataUnaff)
fix(dataUnaff)


#fit1 <- lm( Outcome65Over ~ AffPotAFF65Over + Time + Time*AffPotAFF65Over,data=data)
#summary(fit1)
fit2 <- lm( OutcomeLess65 ~ UnAFFPotUnAFFLess65 + Time + Time*UnAFFPotUnAFFLess65,data=dataUnaff)
summary(fit2)
a<-fit1$coefficients[4]
b<-fit2$coefficients[4]
#Let's compare how people in each state did
a
b
a-b

a <- colMeans(subset(dataAff, Time == 0 & AffPotAFF65Over == 0, select=Outcome65Over))
b <- colMeans(subset(dataAff, Time == 0 & AffPotAFF65Over == 1, select=Outcome65Over))
c <- colMeans(subset(dataAff, Time == 1 & AffPotAFF65Over == 0, select=Outcome65Over))
d <- colMeans(subset(dataAff, Time == 1 & AffPotAFF65Over == 1, select=Outcome65Over))
e <- colMeans(subset(dataUnaff, Time == 0 & UnAFFPotUnAFFLess65 == 0, select=OutcomeLess65))
f <- colMeans(subset(dataUnaff, Time == 0 & UnAFFPotUnAFFLess65 == 1, select=OutcomeLess65))
g <- colMeans(subset(dataUnaff, Time == 1 & UnAFFPotUnAFFLess65 == 0, select=OutcomeLess65))
h <- colMeans(subset(dataUnaff, Time == 1 & UnAFFPotUnAFFLess65 == 1, select=OutcomeLess65))

a # state j before policy change for potentially treated
b # state i before policy change for treated
c # state j after policy change for potentially treated
d # state i after policy change for treated
e # state j before policy change for control (non-targeted)
f # state i before policy change for control (non-targeted)
g # state j after policy change for control  (non-targeted)
h # state i after policy change for control  (non-targeted)

# Affected - nonAffected
Affected <- (d-b)-(c-a)
NonAffected <- (h-f)-(g-e)
Affected - NonAffected

# State comparison
State <- (d-b)-(h-f)
NonState <- (c-a)-(g-e)
State - NonState

# Regression form
State <- c(rep(1,2000),rep(0,2000))
Outcome <- c(seq(.25, .375, length.out = 500),seq(.476, .65, length.out = 500), seq(.15, .345, length.out = 1000),seq(.275, .525, length.out = 1000),seq(.175, .335, length.out = 1000))
Time <- c(rep(0,500), rep(1,500),rep(0,500), rep(1,500),rep(0,500), rep(1,500),rep(0,500), rep(1,500))
AF <- c(rep(1,1000),rep(0,1000),rep(1,1000),rep(0,1000))

data <- data.frame(Outcome=Outcome, State=State,AF =AF, Time=Time)
str(data)
dim(data)
fix(data)


a <- colMeans(subset(data, Time == 0 & AF == 1 & State == 1, select= Outcome))
b <- colMeans(subset(data, Time == 0 & AF == 1 & State == 0, select= Outcome))
c <- colMeans(subset(data, Time == 1 & AF == 1 & State == 1, select= Outcome))
d <- colMeans(subset(data, Time == 1 & AF == 1 & State == 0, select= Outcome))
e <- colMeans(subset(data, Time == 0 & AF == 0 & State == 1, select= Outcome))
f <- colMeans(subset(data, Time == 0 & AF == 0 & State == 0, select= Outcome))
g <- colMeans(subset(data, Time == 1 & AF == 0 & State == 1, select= Outcome))
h <- colMeans(subset(data, Time == 1 & AF == 0 & State == 0, select= Outcome))

a
b
c
d
e
f
g
h
# Affected - nonAffected
Affected <- (c-a)-(d-b)
NonAffected <- (g-e)-(h-f)
Affected - NonAffected

table(data$State, data$AF)

fit <- lm(Outcome ~ State + AF + State*AF + Time + Time*State + Time*AF + Time*State*AF, data=data)

summary(fit)

write.csv(data,"C:/Users/msgc/Dropbox/Stats3PHUDCFILY/Fourth week/Fifth week/Sixth week/Seventh/synthetic PHUDCFILY/dataforDDD.csv", row.names=F)

#Procedure for Stata
insheet using "C:\Users\msgc\Dropbox\Stats3PHUDCFILY\Fourth week\Fifth week\Sixth week\Seventh\synthetic PHUDCFILY\dataforDDD.csv", clear

*outcome state af time
gen staf = state*af
gen tist = time*state
gen tiaf = time*af
gen ddd = time*af*state

reg outcome state af time staf tist tiaf ddd, robust

