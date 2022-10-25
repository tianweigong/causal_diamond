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
library(effsize)
# rm(list=ls())

load("exp1.Rda")

#+ demographic  -------------------
#' # demographic
table(df.dmg$gender)
summarySE(df.dmg,measurevar = "age")

#+ overall performance   -------------------
#' #overall performance 

#persons
df.link.ppl=df.final%>%select("subject","acc_a","A_pro")%>%mutate(acc=acc_a,pro=A_pro,acc_a=NULL,A_pro=NULL) %>% 
  rbind(df.final%>%select("subject","acc_b","B_pro")%>%mutate(acc=acc_b,pro=B_pro,acc_b=NULL,B_pro=NULL)) %>%
  summarySE(measurevar = "acc",groupvars = "subject")

summarySE(df.link.ppl,measurevar = "acc")
t.test(df.link.ppl$acc,mu=0.33)
cohen.d(df.link.ppl$acc,f=NA,mu=0.33)

df.device.ppl=df.final%>%select("subject","acc_device","A_pro","B_pro")%>%mutate(trial_type=paste(A_pro,B_pro,sep=""))%>%
  summarySE(measurevar = "acc_device",groupvars = "subject")

summarySE(df.device.ppl,measurevar = "acc_device")
t.test(df.device.ppl$acc_device,mu=0.11)
cohen.d(df.device.ppl$acc_device,f=NA,mu=0.11)

#links
df.link=df.final%>%select("subject","acc_a","A_pro")%>%mutate(acc=acc_a,pro=A_pro,acc_a=NULL,A_pro=NULL) %>% 
  rbind(df.final%>%select("subject","acc_b","B_pro")%>%mutate(acc=acc_b,pro=B_pro,acc_b=NULL,B_pro=NULL)) %>%
  summarySE(measurevar = "acc",groupvars = c("subject","pro"))

vec.link=unique(df.link$pro)
lapply(as.list(vec.link),function (x){t.test(subset(df.link,pro==x)$acc,mu=0.33)})

#structures
df.device=df.final%>%select("subject","acc_device","A_pro","B_pro")%>%mutate(trial_type=paste(A_pro,B_pro,sep=""))%>%
  summarySE(measurevar = "acc_device",groupvars = c("subject","trial_type"))

vec.device=unique(df.device$trial_type)
lapply(as.list(vec.device),function (x){t.test(subset(df.device,trial_type==x)$acc,mu=0.11)})