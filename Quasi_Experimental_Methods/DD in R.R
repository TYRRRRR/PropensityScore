# Load the foreign package
library(foreign)
# Import data from web site
# update: first download the file eitc.dta from this link:
# https://docs.google.com/open?id=0B0iAUHM7ljQ1cUZvRWxjUmpfVXM
# Then import from your hard drive:
eitc <- read.dta("C:/Users/msgc/Dropbox/Stats3PHUDCFILY/Fourth week/Fifth week/Sixth week/Seventh/synthetic PHUDCFILY/eitc.dta")
str(eitc)
#######################
state          State of Residence
year           Year [taxyear]
urate          State Unemp Rate
children       Number of Children
nonwhite       Dummy=1 if Hispanic/Black
finc           Annual Family Income 
earn           Annual earnings 
age            Age of woman
ed             Years of education
work           Dummy =1 if Employed last year
unearn         Unearned Income 
#######################
str(eitc)
# Construct a Treatment Variable
# Construct a variable for the treatment called “anykids” = 1 for treated individual (has at least one child); and a variable for after the expansion called “post93? = 1 for 1994 and later.
eitc$post93 <- as.numeric(eitc$year >= 1994)
table(eitc$post93)
eitc$anykids <- as.numeric(eitc$children > 0)
table(eitc$anykids, eitc$children)

#Is the effect of having one kid the same as having 2,3 or 4?
table(eitc$children)
hist(eitc$children)

# How does the interaction looks like?
table(eitc$post93,eitc$anykids)

# Calculate the D-I-D Estimate of the Treatment Effect
# Calculate the unconditional difference-in-difference estimates of the effect of the 1993 EITC expansion on employment of single women.
a <- colMeans(subset(eitc, post93 == 0 & anykids == 0, select=work))
b <- colMeans(subset(eitc, post93 == 0 & anykids == 1, select=work))
c <- colMeans(subset(eitc, post93 == 1 & anykids == 0, select=work))
d <- colMeans(subset(eitc, post93 == 1 & anykids == 1, select=work))
(d-c)-(b-a)
(d-b)-(c-a)

head(eitc[,c("year", "post93", "children","anykids","work")],50)

# Run a simple D-I-D Regression
# Now we will run a regression to estimate the conditional difference-in-difference estimate of the effect of the Earned Income Tax Credit on “work”, using all women with children as the treatment group. The regression equation is as follows:
reg1 <- lm(work ~ post93 + anykids + post93*anykids, data = eitc)
summary(reg1)
# Include Relevant Demographics in Regression
# Adding additional variables is a matter of including them in your coded regression equation, as follows:
reg2 <- lm(work ~ anykids + post93 + post93*anykids + nonwhite
                + age + I(age^2) + ed + I(ed^2) + finc + I(finc-earn), data = eitc)
summary(reg2)

#Effect heterogeneity
reg2.1 <- lm(work ~ anykids + post93 + post93*anykids + age + I(age^2) + ed + I(ed^2) + finc + I(finc-earn), data = eitc[eitc$nonwhite==1,])
summary(reg2.1)
#Effect heterogeneity
reg2.2 <- lm(work ~ anykids + post93 + post93*anykids + age + I(age^2) + ed + I(ed^2) + finc + I(finc-earn), data = eitc[eitc$nonwhite==0,])
summary(reg2.2)
#state fixed effects
eitc$state <- as.factor(eitc$state)
# eitc$year <- as.factor(eitc$year)
reg1 <- lm(work ~ year + anykids + year*anykids, data = eitc)
reg3 <- lm(work ~ anykids + post93 + post93*anykids + nonwhite
                + age + I(age^2) + ed + finc + I(finc-earn)+ state + urate, data = eitc)
summary(reg3)

# Estimate a Placebo Model
# Testing a placebo model is when you arbitrarily choose a treatment time before your actual treatment time, and test to see if you get a significant treatment effect.

# sub set the data, including only years before 1994.
eitc.sub <- eitc[eitc$year <= 1993,]
# Create a new "after treatment" dummy variable
# and interaction term
eitc.sub$post92 <- as.numeric(eitc.sub$year >= 1992)

table(eitc.sub$year)
table(eitc.sub$anykids)
table(eitc.sub$post92)
# Run a placebo regression where placebo treatment = post91*anykids
reg3 <- lm(work ~ anykids + post92 + post92*anykids, data = eitc.sub)
summary(reg3)

reg3 <- lm(work ~ anykids + post92 + post92*anykids + nonwhite
                + age + I(age^2) + ed + I(ed^2) + finc + I(finc-earn)+state + urate, data = eitc.sub)
summary(reg3)

###Graphs
# Take average value of 'work' by year, conditional on anykids
minfo <- aggregate(eitc$work, list(eitc$year,eitc$anykids), mean)

# rename column headings (variables)
colnames(minfo) <- c("YR","Treatment","LFPR")

# Attach a new column with labels
minfo$Group[1:6] <- "Single women, no children"
minfo$Group[7:12] <- "Single women, children"
minfo

install.packages("ggplot2")
library(ggplot2)	#package for creating nice plots
# pdf("DIDPHUDCFILY.pdf",width=7.5,height=5, bg="lightgoldenrod1")
qplot(YR, LFPR, data=minfo, geom=c("point","line"), colour=Group,
        xlab="Year", ylab="Labor Force Participation Rate") +
		geom_vline(xintercept=c(1993), colour="black", size=2)  + theme(plot.background = element_rect(fill='white', colour='white'))
# dev.off()


###Modified
# Take average value of 'work' by year, conditional on anykids
minfo <- aggregate(eitc$work, list(eitc$post93,eitc$anykids), mean)

# rename column headings (variables)
colnames(minfo) <- c("Time","Treatment","LFPR")

# Attach a new column with labels
minfo$Group[1:2] <- "Single women, no children"
minfo$Group[3:4] <- "Single women, children"
minfo

# install.packages("ggplot2")
library(ggplot2)	#package for creating nice plots
# pdf("DIDPHUDCFILY.pdf",width=7.5,height=5, bg="lightgoldenrod1")
qplot(Time, LFPR, data=minfo, geom=c("point","line"), colour=Group,
        xlab="Implementation State (0=Pre, 1=Post)", ylab="Labor Force Participation Rate") +
		geom_vline(xintercept=c(.5), colour="black", size=2)  + theme(plot.background = element_rect(fill='white', colour='white')) + scale_x_continuous(breaks=c(0,1))
# dev.off()



###What about effect heterogeneity
eitc$anykids <- as.numeric(eitc$children >= 3)
eitc$anykids[eitc$children==1|eitc$children==2] <- NA
table(eitc$anykids, eitc$children)
table(eitc$anykids, eitc$year)
table(eitc$anykids[eitc$nonwhite==1], eitc$year[eitc$nonwhite==1])

reg1 <- lm(work ~ post93 + anykids + post93*anykids, data = eitc)
summary(reg1)
# Include Relevant Demographics in Regression
# Adding additional variables is a matter of including them in your coded regression equation, as follows:
reg2 <- lm(work ~ anykids + post93 + post93*anykids + nonwhite
                + age + I(age^2) + ed + I(ed^2) + finc + I(finc-earn), data = eitc)
summary(reg2)

reg2 <- lm(work ~ anykids + post93 + post93*anykids + nonwhite
                + age + I(age^2) + ed + I(ed^2) + finc + I(finc-earn) + as.factor(state), data = eitc)
summary(reg2)

reg2 <- lm(work ~ anykids + as.factor(year) + as.factor(year)*anykids, data = eitc)
summary(reg2)
