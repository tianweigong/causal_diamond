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




#subject
n_item=36
acc.sub.a=summarySE(df.final,measurevar = "acc_a",groupvars = c("subject","A_pro","mycondition")) %>% 
  melt(id.vars = c("subject","mycondition","A_pro"), value.name = "acc")%>% 
  subset(variable=="acc_a")
colnames(acc.sub.a)[which(colnames(acc.sub.a)=="A_pro")]="pro"
acc.sub.b=summarySE(df.final,measurevar = "acc_b",groupvars = c("subject","B_pro","mycondition"))%>% 
  melt(id.vars = c("subject","mycondition","B_pro"), value.name = "acc")%>% 
  subset(variable=="acc_b")
colnames(acc.sub.b)[which(colnames(acc.sub.b)=="B_pro")]="pro"
acc.sub=rbind(acc.sub.a,acc.sub.b) %>% 
  summarySE(measurevar = "acc",groupvars = c("subject","mycondition","pro"))

#item (connection)
acc.item.a=summarySE(df.final,measurevar = "acc_a",groupvars = c("trial_type","A_pro","mycondition")) %>% 
  melt(id.vars = c("trial_type","mycondition","A_pro"), value.name = "acc")%>% 
  subset(variable=="acc_a")
colnames(acc.item.a)[which(colnames(acc.item.a)=="A_pro")]="pro"
acc.item.b=summarySE(df.final,measurevar = "acc_b",groupvars = c("trial_type","B_pro","mycondition"))%>% 
  melt(id.vars = c("trial_type","mycondition","B_pro"), value.name = "acc")%>% 
  subset(variable=="acc_b")
colnames(acc.item.b)[which(colnames(acc.item.b)=="B_pro")]="pro"
acc.item=rbind(acc.item.a,acc.item.b)

#clip(device)
acc.clip=summarySE(df.final,measurevar = "acc_device",groupvars = c("trial_type","mycondition")) %>% 
  melt(id.vars = c("trial_type","mycondition"), value.name = "acc")%>% 
  subset(variable=="acc_device")

acc.sub.clip=summarySE(df.final,measurevar = "acc_device",groupvars = c("subject","mycondition")) %>% 
  melt(id.vars = c("subject","mycondition"), value.name = "acc")%>% 
  subset(variable=="acc_device")

#' ## accuracy analysis -- subject
#switch here
# ccon="regular"
ccon="irregular"
acc.sub.bycon=acc.sub %>% subset(mycondition==ccon)
acc.sub.clip.bycon=acc.sub.clip%>%  subset(mycondition==ccon)
acc.item.bycon=acc.item %>% subset(mycondition==ccon)
acc.clip.bycon=acc.clip%>% subset(mycondition==ccon)
n_sub=df.dmg %>% subset(mycondition==ccon) %>%nrow()
#switch end
acc.sub.total=acc.sub.bycon %>%  summarySE(measurevar = "acc",groupvars = c("subject","mycondition"))
mean(acc.sub.total$acc)
sd(acc.sub.total$acc)
t.test(acc.sub.total$acc, mu = .33)
paste("binomial test(participants):",
      qbinom(0.95, n_item, 0.33),
      mean(acc.sub.total$acc*n_item >= qbinom(0.95000001, n_item, 0.33)) %>% round(3)
)

acc.sub.g=acc.sub.bycon %>% subset(pro=="G")
mean(acc.sub.g$acc)
sd(acc.sub.g$acc)
t.test(acc.sub.g$acc, mu =  .33)

acc.sub.n=acc.sub.bycon %>% subset(pro=="N")
mean(acc.sub.n$acc)
sd(acc.sub.n$acc)
t.test(acc.sub.n$acc, mu = .33)

acc.sub.p=acc.sub.bycon %>% subset(pro=="P")
mean(acc.sub.p$acc)
sd(acc.sub.p$acc)
t.test(acc.sub.p$acc, mu =  .33)

#' ## accuracy analysis -- items
paste("binomial test(participants):",
      qbinom(0.95, n_sub, 0.33),
      mean(acc.item.bycon$acc*n_sub >= qbinom(0.95000001, n_sub, 0.33)) %>% round(3)
)

#' ## accuracy analysis -- clips
mean(acc.sub.clip.bycon$acc)
sd(acc.sub.clip.bycon$acc)
t.test(acc.sub.clip.bycon$acc, mu = .11)

paste("binomial test(participants):",
      qbinom(0.95, 18, 0.11),
      mean(acc.sub.clip.bycon$acc*n_item >= qbinom(0.95000001, 18, 0.11)) %>% round(3)
)
paste("binomial test(participants):",
      qbinom(0.95, n_sub, 0.11),
      mean(acc.clip.bycon$acc*n_sub >= qbinom(0.9500001, n_sub, 0.11)) %>% round(3)
)

#+ condition comparison -------------------
#' # condition comparison
acc.sub.total=acc.sub %>%  summarySE(measurevar = "acc",groupvars = c("subject","mycondition"))
t.test(acc ~ mycondition, data=acc.sub.total)

#lme
dt.lme=df.final%>%
  subset()

acc.item.lme.a=df.final%>%subset(select=c("subject","seed","mycondition","A_pro","B_pro","acc_a"))
colnames(acc.item.lme.a)[4]="pro"
colnames(acc.item.lme.a)[5]="partner"
colnames(acc.item.lme.a)[6]="acc"
acc.item.lme.b=df.final%>%subset(select=c("subject","seed","mycondition","B_pro","A_pro","acc_b"))
colnames(acc.item.lme.b)[4]="pro"
colnames(acc.item.lme.b)[5]="partner"
colnames(acc.item.lme.b)[6]="acc"
acc.item.lme=rbind(acc.item.lme.a,acc.item.lme.b)

glmer(acc ~ mycondition + (1|seed) + (1|pro) + (1|subject), data = acc.item.lme, family=binomial) %>% summary()

glmer(acc_device ~ mycondition + (1|seed) + (1|trial_type) + (1|subject), data = dt.lme, family=binomial) %>% summary()