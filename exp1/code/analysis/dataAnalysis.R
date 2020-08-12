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

# rm(list=ls())

#+ load functions -------------------
#' # load functions

#+ reconstruct data -------------------
#' # reconstruct data
df.raw=read.csv("../../data/exp.csv")

v.dmg=c("date","subject","position","gender","age","engaging","difficult","feedback")
v.exp=c("trial_order","trial_type","trial_id","A_pro","B_pro","A_state","B_state","data_type","clickpattern")
v.id=c("subject","position")

df.dmg=df.raw[,v.dmg]

df.dmg$gender %>%summary()
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
         acc_mean=acc_connect/2
  )

df.exp$trial_id=as.numeric(df.exp$trial_id)
df.final=df.exp %>%subset(data_type=="final" & trial_id!=0)

# save(df.final,file="df.final.Rda")

#+ summary subject data -------------------
#' # summary subject data
# a=summarySE(df.final,measurevar = c("acc_device"),groupvars = "trial_id")
# b=aggregate(acc_connect ~ subject, data=df.final, FUN=sum)
# b$bonus=b$acc_connect*0.03
# # write.csv(b,file="bonus.csv")
# mean(b$bonus)
# sd(b$bonus)

#subject
n_item=36

acc.sub.a=summarySE(df.final,measurevar = "acc_a",groupvars = c("subject","A_pro")) %>% 
  melt(id.vars = c("subject","A_pro"), value.name = "acc")%>% 
  subset(variable=="acc_a")
colnames(acc.sub.a)[which(colnames(acc.sub.a)=="A_pro")]="pro"
acc.sub.b=summarySE(df.final,measurevar = "acc_b",groupvars = c("subject","B_pro"))%>% 
  melt(id.vars = c("subject","B_pro"), value.name = "acc")%>% 
  subset(variable=="acc_b")
colnames(acc.sub.b)[which(colnames(acc.sub.b)=="B_pro")]="pro"
acc.sub=rbind(acc.sub.a,acc.sub.b) %>% 
  summarySE(measurevar = "acc",groupvars = c("subject","pro"))

#item (connection)
acc.item.a=summarySE(df.final,measurevar = "acc_a",groupvars = c("trial_id","A_pro")) %>% 
  melt(id.vars = c("trial_id","A_pro"), value.name = "acc")%>% 
  subset(variable=="acc_a")
colnames(acc.item.a)[which(colnames(acc.item.a)=="A_pro")]="pro"
acc.item.b=summarySE(df.final,measurevar = "acc_b",groupvars = c("trial_id","B_pro"))%>% 
  melt(id.vars = c("trial_id","B_pro"), value.name = "acc")%>% 
  subset(variable=="acc_b")
colnames(acc.item.b)[which(colnames(acc.item.b)=="B_pro")]="pro"
acc.item=rbind(acc.item.a,acc.item.b)

#clip(device)
acc.clip=summarySE(df.final,measurevar = "acc_device",groupvars = c("trial_id")) %>% 
  melt(id.vars = c("trial_id"), value.name = "acc")%>% 
  subset(variable=="acc_device")

acc.sub.clip=summarySE(df.final,measurevar = "acc_device",groupvars = c("subject")) %>% 
  melt(id.vars = c("subject"), value.name = "acc")%>% 
  subset(variable=="acc_device")

#' ## accuracy analysis -- subject

acc.sub.total=acc.sub %>%  summarySE(measurevar = "acc",groupvars = c("subject"))
mean(acc.sub.total$acc)
sd(acc.sub.total$acc)
t.test(acc.sub.total$acc, mu = .33)
t.test(acc.sub.total$acc, mu = .97) #compared to normative model
paste("binomial test(participants):",
      qbinom(0.95, n_item, 0.33),
      mean(acc.sub.total$acc*n_item >= qbinom(0.95000001, n_item, 0.33)) %>% round(3)
)

acc.sub.g=acc.sub %>% subset(pro=="G")
mean(acc.sub.g$acc)
sd(acc.sub.g$acc)
t.test(acc.sub.g$acc, mu =  .33)

acc.sub.n=acc.sub %>% subset(pro=="N")
mean(acc.sub.n$acc)
sd(acc.sub.n$acc)
t.test(acc.sub.n$acc, mu = .33)

acc.sub.p=acc.sub %>% subset(pro=="P")
mean(acc.sub.p$acc)
sd(acc.sub.p$acc)
t.test(acc.sub.p$acc, mu =  .33)

mean(acc.sub.clip$acc)
sd(acc.sub.clip$acc)
t.test(acc.sub.clip$acc, mu = .11)

paste("binomial test(participants):",
      qbinom(0.95, 18, 0.11),
      mean(acc.sub.clip$acc*n_item >= qbinom(0.95000001, 18, 0.11)) %>% round(3)
)

#' ## accuracy analysis -- items
n_sub=nrow(df.dmg)
acc.item.total=acc.item
paste("binomial test(participants):",
      qbinom(0.95, n_sub, 0.33),
      mean(acc.item.total$acc*n_sub > qbinom(0.95000001, n_sub, 0.33)) %>% round(3)
)
#' ## accuracy analysis -- clips
paste("binomial test(participants):",
      qbinom(0.95, n_sub, 0.11),
      mean(acc.clip$acc*n_sub >= qbinom(0.9500001, n_sub, 0.11)) %>% round(3)
)