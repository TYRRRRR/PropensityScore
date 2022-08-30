minfo <- aggregate(data$Outcome, list(data$Time, data$AF, data$State), mean)
minfo
# rename column headings (variables)
colnames(minfo) <- c("YR","Affected", "State", "LFPR")

# Attach a new column with labels
minfo$Group[1:2] <- "No Affected, State j"
minfo$Group[3:4] <- "Affected, State j"
minfo$Group[5:6] <- "No Affected, State i"
minfo$Group[7:8] <- "Affected, State i"
minfo

install.packages("ggplot2")
library(ggplot2)	#package for creating nice plots
# pdf("DIDPHUDCFILY.pdf",width=7.5,height=5, bg="lightgoldenrod1")
qplot(YR, LFPR, data=minfo, geom=c("point","line"), colour=Group,
        xlab="Year", ylab="Outcome of Interest") +
		geom_vline(xintercept=c(.5), colour="black", size=2)  + theme(plot.background = element_rect(fill='white', colour='white')) + scale_x_continuous(breaks=c(0,1))
# dev.off()
