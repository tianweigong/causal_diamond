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
a=summarySE(df.final,measurevar = c("acc_device"),groupvars = "trial_id")
b=aggregate(acc_connect ~ subject, data=df.final, FUN=sum)
b$bonus=b$acc_connect*0.03
# write.csv(b,file="bonus.csv")
mean(b$bonus)
sd(b$bonus)

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
t.test(acc.sub.g$acc, mu = 1)#compared to normative model

acc.sub.n=acc.sub %>% subset(pro=="N")
mean(acc.sub.n$acc)
sd(acc.sub.n$acc)
t.test(acc.sub.n$acc, mu = .33)
t.test(acc.sub.n$acc, mu = .94)#compared to normative model

acc.sub.p=acc.sub %>% subset(pro=="P")
mean(acc.sub.p$acc)
sd(acc.sub.p$acc)
t.test(acc.sub.p$acc, mu =  .33)
t.test(acc.sub.p$acc, mu = .98)#compared to normative model

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

#+ click pattern  -------------------
#' # click pattern
pat.clc= as.data.frame(matrix(NA,ncol=4,nrow=60)) %>%
  setNames(c("subject","G","P","N"))
for (i in 1:length(unique(df.final$subject))){
  pat.clc$subject[i]=unique(df.final$subject)[i] %>% as.character()
  df.sub=df.final %>% subset(subject==pat.clc$subject[i])
  pat.clc$G[i]=df.sub %>% subset(A_state=="G") %>% nrow()
  pat.clc$G[i]= pat.clc$G[i]+ df.sub %>% subset(B_state=="G") %>% nrow()
  
  pat.clc$P[i]=df.sub %>% subset(A_state=="P") %>% nrow()
  pat.clc$P[i]= pat.clc$P[i]+ df.sub %>% subset(B_state=="P") %>% nrow()
  
  pat.clc$N[i]=df.sub %>% subset(A_state=="N") %>% nrow()
  pat.clc$N[i]= pat.clc$N[i]+ df.sub %>% subset(B_state=="N") %>% nrow()
  
}

n_type_item=12

t.test(pat.clc$G, mu = n_type_item)
t.test(pat.clc$P, mu = n_type_item)
t.test(pat.clc$N, mu = n_type_item)

#+ confound: clickpattern -------------------
#' # confound: clickpattern

clc.sub.a=summarySE(df.final,measurevar = "acc_a",groupvars = c("subject","A_pro","clickpattern")) %>% 
  melt(id.vars = c("subject","A_pro","clickpattern"), value.name = "acc")%>% 
  subset(variable=="acc_a")
colnames(clc.sub.a)[which(colnames(clc.sub.a)=="A_pro")]="pro"
clc.sub.b=summarySE(df.final,measurevar = "acc_b",groupvars = c("subject","B_pro","clickpattern"))%>% 
  melt(id.vars = c("subject","B_pro","clickpattern"), value.name = "acc")%>% 
  subset(variable=="acc_b")
colnames(clc.sub.b)[which(colnames(clc.sub.b)=="B_pro")]="pro"
clc.sub=rbind(clc.sub.a,clc.sub.b) %>% 
  summarySE(measurevar = "acc",groupvars = c("subject","pro","clickpattern"))

clc.sub.g=clc.sub %>% subset(pro=="G")
clc.sub.n=clc.sub %>% subset(pro=="N")
clc.sub.p=clc.sub %>% subset(pro=="P")
clc.sub.total=clc.sub %>%  summarySE(measurevar = "acc",groupvars = c("subject","clickpattern"))

lm(acc ~ clickpattern, data = clc.sub.n) %>%anova()
ggplot(clc.sub.n,aes(x=clickpattern, y=acc))+
  geom_boxplot()

#' ## confound: click strategies

clc.sub.a=plyr::count(df.final,c("subject","A_state","clickpattern"))
colnames(clc.sub.a)[which(colnames(clc.sub.a)=="A_state")]="state"
clc.sub.b=plyr::count(df.final,c("subject","B_state","clickpattern"))
colnames(clc.sub.b)[which(colnames(clc.sub.b)=="B_state")]="state"
clc.sub=rbind(clc.sub.a,clc.sub.b)
clc.sub=aggregate(freq ~ subject + state+clickpattern, data=clc.sub, FUN=sum)

clc.sub.g=clc.sub %>% subset(state=="G")
clc.sub.n=clc.sub %>% subset(state=="N")
clc.sub.p=clc.sub %>% subset(state=="P")

lm(freq ~ clickpattern, data = clc.sub.g) %>%anova()
ggplot(clc.sub.n,aes(x=clickpattern, y=freq))+
  geom_boxplot()+
  ggtitle("N as final answers")

#+ confound: position -------------------
#' # confound: position 
pos.sub.a=summarySE(df.final,measurevar = "acc_a",groupvars = c("subject","A_pro","position")) %>% 
  melt(id.vars = c("subject","A_pro","position"), value.name = "acc")%>% 
  subset(variable=="acc_a")
colnames(pos.sub.a)[which(colnames(pos.sub.a)=="A_pro")]="pro"
pos.sub.b=summarySE(df.final,measurevar = "acc_b",groupvars = c("subject","B_pro","position"))%>% 
  melt(id.vars = c("subject","B_pro","position"), value.name = "acc")%>% 
  subset(variable=="acc_b")
colnames(pos.sub.b)[which(colnames(pos.sub.b)=="B_pro")]="pro"
pos.sub=rbind(pos.sub.a,pos.sub.b) %>% 
  summarySE(measurevar = "acc",groupvars = c("subject","pro","position"))

pos.sub.total=pos.sub %>%  summarySE(measurevar = "acc",groupvars = c("subject","position"))
pos.sub.g=pos.sub %>% subset(pro=="G")
pos.sub.n=acc.sub %>% subset(pro=="N")
pos.sub.p=acc.sub %>% subset(pro=="P")

t.test(pos.sub.g$acc~pos.sub.g$position)


#+ choice analysis -------------------
#' # choice analysis
choi=c(df.final$A_state,df.final$B_state)
sum(choi=="G")/(36*60)
sum(choi=="P")/(36*60)
sum(choi=="N")/(36*60)


choi=data.frame(pro=c(df.final$A_pro,df.final$B_pro),state=c(df.final$A_state,df.final$B_state))
sum(choi$pro=="G" & choi$state=="G") /sum(choi$pro=="G")
sum(choi$pro=="G" & choi$state=="N") /sum(choi$pro=="G")
sum(choi$pro=="G" & choi$state=="P") /sum(choi$pro=="G")

sum(choi$pro=="N" & choi$state=="G") /sum(choi$pro=="N")
sum(choi$pro=="N" & choi$state=="N") /sum(choi$pro=="N")
sum(choi$pro=="N" & choi$state=="P") /sum(choi$pro=="N")

sum(choi$pro=="P" & choi$state=="G") /sum(choi$pro=="P")
sum(choi$pro=="P" & choi$state=="N") /sum(choi$pro=="P")
sum(choi$pro=="P" & choi$state=="P") /sum(choi$pro=="P")

choi$code=0
choi$code[choi$state=="G"]=1
choi$code[choi$state=="N"]=2
choi$code[choi$state=="P"]=3
chisq.test(choi$code)


#+ figure -------------------
#' # figure

MySubFit6 <-function(){
  pic.sub.a=plyr::count(df.final,c("trial_id","A_state"))
  pic.sub.a$cpn="A"
  colnames(pic.sub.a)=c("trial_id","state","freq","cpn")
  
  pic.sub.b=plyr::count(df.final,c("trial_id","B_state"))
  pic.sub.b$cpn="B"
  colnames(pic.sub.b)=c("trial_id","state","freq","cpn")
  
  pic.sub=rbind(pic.sub.a,pic.sub.b)
  
  sub_num=plyr::count(df.final,c("trial_id"))
  sub_num=sub_num$freq[1]
  
  pic.sub$hm=pic.sub$freq/sub_num
  
  for (i in c("A","B")){
    for (j in c("G","P","N")){
      for (m in 1:18){
        if (nrow(subset(pic.sub,(cpn==i&state==j&trial_id==m)))==0){
          pic.sub[nrow(pic.sub) + 1,]=c(m,j,0,i,0)
        }
      }
    }
  }
  pic.sub$trial_id=as.numeric(pic.sub$trial_id)
  pic.sub$hm=as.numeric(pic.sub$hm)
  pic.sub=pic.sub[
    order( pic.sub[,"trial_id"], pic.sub[,"cpn"],pic.sub[,"state"]),
    ]
  
  return(pic.sub)
}
pic.par=MySubFit6()
pic.label=c("1"="GG1","2"="GG2","3"="PG1","4"="PG2","5"="NG1","6"="NG2",
            "7"="GP1","8"="GP2","9"="PP1","10"="PP2","11"="NP1","12"="NP2",
            "13"="GN1","14"="GN2","15"="PN1","16"="PN2","17"="NN1","18"="NN2")
ggplot(pic.par,aes(x=cpn, y=hm,fill=state))+
  geom_bar(stat='identity',position="dodge",colour="black")+
  facet_wrap(~trial_id,nrow=3,labeller = as_labeller(pic.label))+
  xlab("component")