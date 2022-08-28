load("res.Rdata")
library(tidyverse)
library(psych)
library(openxlsx)
library(data.table)

support.mat
support.mat.df <- as.data.frame(support.mat)
describe(support.mat.df)
support.mat.df <- support.mat.df %>% pivot_longer(cols = covars.asmd:overlap.index, names_to = "Stat Name",values_to = "Value")

p1 <- support.mat.df %>% ggplot(aes(y=Value))+
      geom_boxplot()+
      facet_wrap(~`Stat Name`,scales = "free")+
      theme_classic()+
      theme(axis.line=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())
p1

cor.mat
cor.mat <- as.data.frame(cor.mat)
describe(cor.mat)


bias.mat
describe(bias.mat)
bias.mat <- as.data.frame(bias.mat)
bias.mat <- bias.mat %>% pivot_longer(cols = IPW:tmle, names_to = "Stat Name",values_to = "Value")
p2 <- bias.mat %>% ggplot(aes(y=Value))+
  geom_boxplot()+
  facet_wrap(~`Stat Name`,scales = "free")+
  theme_classic()+
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
p2



mse.mat
describe(mse.mat)
mse.mat <- as.data.frame(mse.mat)
mse.mat <- mse.mat %>% pivot_longer(cols = IPW:tmle, names_to = "Stat Name",values_to = "Value")
p3 <- mse.mat %>% ggplot(aes(y=Value))+
  geom_boxplot()+
  facet_wrap(~`Stat Name`,scales = "free")+
  theme_classic()+
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
p3


ps.sample.mat
ps.sample.mat <- as.data.frame(ps.sample.mat) %>% mutate(group = "sample")



ps.pop.mat
ps.pop.mat <- as.data.frame(ps.pop.mat) %>% mutate(group = "pop")

df_all <- rbind.data.frame(ps.sample.mat,ps.pop.mat)


df_all_T <- df_all %>% pivot_longer(cols = Mean:kurtosis, names_to = "Stat Name",values_to = "Value")


df_all_T %>% ggplot(aes(y=Value,fill = group))+
  geom_boxplot()+
  facet_wrap(~`Stat Name`,scales = "free")+
  theme_classic()+
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())


