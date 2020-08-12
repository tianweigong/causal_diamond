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

#+ load functions -------------------
#' # load functions

#+ reconstruct data -------------------
#' # reconstruct data
df.raw=read.csv("../../data/exp.csv")

v.dmg=c("date","subject","position","gender","age","engaging","difficult","feedback")
v.exp=c("trial_order","trial_type","trial_id","A_pro","B_pro","A_state","B_state","data_type","clickpattern")
v.id=c("subject","position","mystiset","mycondition")

v.block=c("1tau6bapi","27aen2kpe","7taxs7up7","g6acuntpp","p66g9ghm5","yu6owkvmf","z367bkwmz", "706j5gkma","yq6yj9xpy")#too much click >300!!

mat.seed=matrix(data=c(1,10,2,11,3,12,4,13,5,14,6,15,7,16,8,17,9,18,
                       9,18,1,10,2,11,3,12,4,13,5,14,6,15,7,16,8,17,
                       8,17,9,18,1,10,2,11,3,12,4,13,5,14,6,15,7,16,
                       7,16,8,17,9,18,1,10,2,11,3,12,4,13,5,14,6,15,
                       6,15,7,16,8,17,9,18,1,10,2,11,3,12,4,13,5,14,
                       5,14,6,15,7,16,8,17,9,18,1,10,2,11,3,12,4,13,
                       4,13,5,14,6,15,7,16,8,17,9,18,1,10,2,11,3,12,
                       3,12,4,13,5,14,6,15,7,16,8,17,9,18,1,10,2,11,
                       2,11,3,12,4,13,5,14,6,15,7,16,8,17,9,18,1,10),
                nrow = 18,ncol = 18,byrow = T)

df.raw=df.raw %>% subset(!(subject %in% v.block))

df.dmg=df.raw[,c(v.dmg,v.id)]
df.dmg$gender %>%summary()
df.dmg$mycondition %>%summary()
mean(df.dmg$age)
sd(df.dmg$age)

df.exp=as.data.frame(matrix(NA,ncol=length(v.exp)+length(v.id),nrow=0)) %>%
  setNames(c(v.id,v.exp))
for (i in 1:nrow(df.raw)){
  length.per=df.raw[i,v.exp[1]] %>%as.vector() %>% strsplit(split=",") %>% unlist() %>% length()
  df.per=as.data.frame(matrix(NA,ncol=length(v.exp)+length(v.id),nrow=length.per)) %>%
    setNames(c(v.id,v.exp))
  for (j in v.exp){
    df.per[,j]=df.raw[i,j] %>%as.vector() %>% strsplit(split=",") %>% unlist()
  }
  for (j in v.id){
    df.per[,j]=df.raw[i,j]
  }
  df.exp=rbind(df.exp,df.per)
}

df.exp=df.exp %>%
  mutate(acc_a=as.integer(A_pro==A_state),
         acc_b=as.integer(B_pro==B_state),
         acc_device=as.integer(acc_a & acc_b),
         acc_connect=acc_a+acc_b,
         acc_mean=acc_connect/2)

df.exp$trial_id=as.numeric(df.exp$trial_id)
df.exp$trial_type=paste(df.exp$A_pro,df.exp$B_pro,sep="")
df.exp$trial_order=as.numeric(df.exp$trial_order)
df.final=df.exp %>%subset(data_type=="final" & trial_id!=0)
df.final$choice=paste(df.final$A_state,df.final$B_state,sep="")

#add seed information
df.final$seed=NA
for (i in 1:nrow(df.final)){
  df.final$seed[i]=mat.seed[df.final$mystiset[i],df.final$trial_id[i]]
}

df.final$fit_id=NA
for (i in 1:nrow(df.final)){
  if (df.final$mycondition[i]=="regular"){
    if (df.final$trial_id[i] %%2==1){
      df.final$fit_id[i]=(df.final$seed[i]-1)*18+df.final$trial_id[i]
    }else{
      df.final$fit_id[i]=(df.final$seed[i]-1)*18+df.final$trial_id[i]-1
    }
  }
  
  if (df.final$mycondition[i]=="irregular"){
    if (df.final$trial_id[i] %%2==1){
      df.final$fit_id[i]=(df.final$seed[i]-1)*18+df.final$trial_id[i]+1
    }else{
      df.final$fit_id[i]=(df.final$seed[i]-1)*18+df.final$trial_id[i]
    }
  }
}

save(df.final,df.dmg,file="df.final.Rda")

#+ summary subject data -------------------
#' # summary subject data
# a=summarySE(df.final,measurevar = c("acc_device"),groupvars = "trial_type")
# b=aggregate(acc_connect~ subject+mystiset+mycondition, data=df.final, FUN=sum)
# c=aggregate(acc_device~ subject+mystiset+mycondition, data=df.final, FUN=sum)
# b$bonus=b$acc_connect*0.03
# #write.csv(b,file="bonus.csv")
# mean(b$bonus)
# sd(b$bonus)

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