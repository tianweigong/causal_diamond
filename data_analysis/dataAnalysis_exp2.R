#+ General settings, echo = FALSE, results = 'hide' -------------------
knitr::opts_chunk$set(warning = FALSE, message = FALSE)

#+ load packages -------------------
#' # load packages
library(tidyr)
library(dplyr)
library(ggplot2)
library(Rmisc)
library(reshape2)
library(plyr)
library(lme4)
library(lmerTest)
# rm(list=ls())

load("exp2.Rda")

#+ demographic  -------------------
#' # demographic
table(df.dmg$gender)
summarySE(df.dmg,measurevar = "age")
table(df.dmg$mycondition)

#+ overall performance   -------------------
#' #overall performance 
df.final$mycondition=factor(df.final$mycondition,levels = c("regular","irregular"))
#persons
df.link.ppl=df.final%>%select("subject","acc_a","A_pro","mycondition")%>%mutate(acc=acc_a,pro=A_pro,acc_a=NULL,A_pro=NULL) %>% 
  rbind(df.final%>%select("subject","acc_b","B_pro","mycondition")%>%mutate(acc=acc_b,pro=B_pro,acc_b=NULL,B_pro=NULL)) %>%
  summarySE(measurevar = "acc",groupvars = c("subject","mycondition"))

summarySE(df.link.ppl,measurevar = "acc",groupvars = "mycondition")
t.test(df.link.ppl$acc[df.link.ppl$mycondition=="regular"],mu=0.33)
cohen.d(df.link.ppl$acc[df.link.ppl$mycondition=="regular"],f=NA,mu=0.33)
t.test(df.link.ppl$acc[df.link.ppl$mycondition=="irregular"],mu=0.33)
cohen.d(df.link.ppl$acc[df.link.ppl$mycondition=="irregular"],f=NA,mu=0.33)


df.device.ppl=df.final%>%select("subject","acc_device","A_pro","B_pro","mycondition")%>%mutate(trial_type=paste(A_pro,B_pro,sep=""))%>%
  summarySE(measurevar = "acc_device",groupvars = c("subject","mycondition"))

summarySE(df.device.ppl,measurevar = "acc_device",groupvars = "mycondition")
t.test(df.device.ppl$acc_device[df.device.ppl$mycondition=="regular"],mu=0.11)
cohen.d(df.device.ppl$acc_device[df.device.ppl$mycondition=="regular"],f=NA,mu=0.11)
t.test(df.device.ppl$acc_device[df.device.ppl$mycondition=="irregular"],mu=0.11)
cohen.d(df.device.ppl$acc_device[df.device.ppl$mycondition=="irregular"],f=NA,mu=0.11)

#links
df.link=df.final%>%select("subject","acc_a","A_pro","mycondition")%>%mutate(acc=acc_a,pro=A_pro,acc_a=NULL,A_pro=NULL) %>% 
  rbind(df.final%>%select("subject","acc_b","B_pro","mycondition")%>%mutate(acc=acc_b,pro=B_pro,acc_b=NULL,B_pro=NULL)) %>%
  summarySE(measurevar = "acc",groupvars = c("subject","pro","mycondition"))

vec.link=unique(df.link$pro)
lapply(as.list(vec.link),function (x){t.test(subset(df.link,mycondition=="regular"&pro==x)$acc,mu=0.33)})
lapply(as.list(vec.link),function (x){t.test(subset(df.link,pro==x)$acc,mu=0.33)})

#structures
df.device=df.final%>%select("subject","acc_device","A_pro","B_pro","mycondition")%>%mutate(trial_type=paste(A_pro,B_pro,sep=""))%>%
  summarySE(measurevar = "acc_device",groupvars = c("subject","trial_type","mycondition"))

vec.device=unique(df.device$trial_type)
lapply(as.list(vec.device),function (x){t.test(subset(df.device,trial_type==x&mycondition=="regular")$acc,mu=0.11)})
lapply(as.list(vec.device),function (x){t.test(subset(df.device,trial_type==x&mycondition=="irregular")$acc,mu=0.11)})

#+ condition comparison -------------------
#' # condition comparison

# df.lme.link=df.final%>%select("subject","acc_a","A_pro","mycondition","seed")%>%mutate(acc=acc_a,pro=A_pro,acc_a=NULL,A_pro=NULL) %>%
#   rbind(df.final%>%select("subject","acc_b","B_pro","mycondition","seed")%>%mutate(acc=acc_b,pro=B_pro,acc_b=NULL,B_pro=NULL))
# 
# #link
# m=glmer(acc ~ mycondition + (1|seed) + (1|pro) + (1|subject), data = df.lme.link, family=binomial)
# m%>% summary()
# confint.merMod(m,method = "Wald")
# 
# #structure
# m=glmer(acc_device ~ mycondition + (1|seed) + (1|trial_type) + (1|subject), data = df.final, family=binomial)
# m%>% summary()
# confint.merMod(m,method = "Wald")
