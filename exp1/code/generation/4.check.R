#+ General settings, echo = FALSE, results = 'hide' -------------------
knitr::opts_chunk$set(warning = FALSE, message = FALSE)

#+ load packages -------------------
#' # load packages
library(tidyr)
library(dplyr)
library(ggplot2)
library(e1071)  
# rm(list=ls())
#+ test parameters -------------------
#' # test parameters
data.frame(Time = seq(0, 10, length.out = 1000)) %>%
  mutate(exp = dgamma(Time, shape = 1, rate =1 )) %>%
  gather(fun, Probability, c(exp)) %>%
  mutate(Form = factor(fun, levels = c('exp'),
                       labels = c('demo'))) %>%
  ggplot(aes(x = Time, y = Probability)) +
  # geom_vline(xintercept = c(3.5)) +
  # annotate("rect",xmin = 0,xmax = 3.5,ymin = 0,ymax = 0.8,alpha = 0.2,fill = "blue") +
  geom_point() +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  ggtitle('Examples of causal delays')

#+ set parameters -------------------
#' # set parameters

k_e = 100
r_e = 20 #e->e: m=5,var=0.25
k_a = 1
r_a = 0.2 #a: m=5,var=4
k_b = 1
r_b = 0.2 #b:m=5,var=4

s_e = 6
s_a = 3
s_b = 3

k_pe = 36
r_pe = 12 #preventative p->e: m=3,var=0.25
k_ge = 9
r_ge = 6  #generative g->e: m=1.5,var=0.25

trial_end=20
onfor=0.35

filepath="../../stimulus/exp1/"
filelist=c("1.gg1.Rda","2.gg2.Rda","3.pg1.Rda","4.pg2.Rda","5.ng1.Rda","6.ng2.Rda",
           "7.gp1.Rda","8.gp2.Rda","9.pp1.Rda","10.pp2.Rda","11.np1.Rda","12.np2.Rda",
           "13.gn1.Rda","14.gn2.Rda","15.pn1.Rda","16.pn2.Rda","17.nn1.Rda","18.nn2.Rda")

E_raw=c()

for (i in 1:length(filelist)){
  load(paste(filepath,filelist[i],sep = ""))
  seed=sqc_raw_long$seed[1]
  set.seed(seed)
  E=rgamma(3, shape = k_e, rate = r_e)
  E_raw = c(E_raw,E)
}

hist(E_raw)
qplot(E_raw,bins=6)
sort(E_raw)
mean(E_raw)
sd(E_raw)
skewness(E_raw)

A_raw=c()
for (i in 1:length(filelist)){
  load(paste(filepath,filelist[i],sep = ""))
  A=c(0,sqc_raw_long$time[sqc_raw_long$obj=="A"]) %>%diff()
  A_raw=c(A_raw,A)
}
hist(A_raw)
sort(A_raw)
mean(A_raw)
sd(A_raw)
skewness(A_raw)

B_raw=c()
for (i in 1:length(filelist)){
  load(paste(filepath,filelist[i],sep = ""))
  B=c(0,sqc_raw_long$time[sqc_raw_long$obj=="B"]) %>%diff()
  B_raw=c(B_raw,B)
}
hist(B_raw)
sort(B_raw)
mean(B_raw)
sd(B_raw)
skewness(B_raw)
##
filepath="../../stimulus/exp1/"
filepath="../model/dynamic_analysis/"
filelist=c("gg1.Rda","gg2.Rda","pg1.Rda","pg2.Rda","ng1.Rda","ng2.Rda",
           "gp1.Rda","gp2.Rda","pp1.Rda","pp2.Rda","np1.Rda","np2.Rda",
           "gn1.Rda","gn2.Rda","pn1.Rda","pn2.Rda","nn1.Rda","nn2.Rda")

a_sta=rep(c("G","G","P","P","N","N"),3)
b_sta=c(rep("G",6),rep("P",6),rep("N",6))

pic.model=as.data.frame(matrix(NA, nrow = 0, ncol = 4))%>%
  setNames(c("trial_id","component","state","ratio"))

for (run in 1:18){
  
  pic.sub=as.data.frame(matrix(NA, nrow = 6, ncol = 5))%>%
    setNames(c("trial_id","component","pro","state","ratio"))
  load(paste(filepath,filelist[run],sep = ""))
  online_inder$trial_id=run
  online_inder= online_inder%>% subset(model=="normative")
  online_inder=online_inder %>% subset(stage==5)
  online_inder$ratio=online_inder$ratio/sum(online_inder$ratio) #normalization
  pic.sub$component=c("A","A","A","B","B","B")
  pic.sub$pro=c(rep(a_sta[run],3),rep(b_sta[run],3))
  pic.sub$state=c("G","N","P","G","N","P")
  pic.sub$trial_id=run
  
  pic.sub$ratio[1]=sum(online_inder$ratio[online_inder$A_state=="G"])
  pic.sub$ratio[2]=sum(online_inder$ratio[online_inder$A_state=="N"])
  pic.sub$ratio[3]=sum(online_inder$ratio[online_inder$A_state=="P"])
  pic.sub$ratio[4]=sum(online_inder$ratio[online_inder$B_state=="G"])
  pic.sub$ratio[5]=sum(online_inder$ratio[online_inder$B_state=="N"])
  pic.sub$ratio[6]=sum(online_inder$ratio[online_inder$B_state=="P"])
  
  pic.model=rbind(pic.model,pic.sub)
}


pic.label=c("1"="GG1","2"="GG2","3"="PG1","4"="PG2","5"="NG1","6"="NG2",
            "7"="GP1","8"="GP2","9"="PP1","10"="PP2","11"="NP1","12"="NP2",
            "13"="GN1","14"="GN2","15"="PN1","16"="PN2","17"="NN1","18"="NN2")

ggplot(pic.model,aes(x=component, y=ratio,fill=state))+
  geom_bar(stat='identity',position="dodge",colour="black")+
  facet_wrap(~trial_id,nrow=3,labeller = as_labeller(pic.label))+
  xlab("component")

choi=data.frame(pro=pic.model$pro,state=pic.model$state,ratio=pic.model$ratio)
mean(choi$ratio[choi$pro=="G" & choi$state=="G"]) 
mean(choi$ratio[choi$pro=="G" & choi$state=="N"]) 
mean(choi$ratio[choi$pro=="G" & choi$state=="P"])

mean(choi$ratio[choi$pro=="N" & choi$state=="G"]) 
mean(choi$ratio[choi$pro=="N" & choi$state=="N"])
mean(choi$ratio[choi$pro=="N" & choi$state=="P"])

mean(choi$ratio[choi$pro=="P" & choi$state=="G"]) 
mean(choi$ratio[choi$pro=="P" & choi$state=="N"])
mean(choi$ratio[choi$pro=="P" & choi$state=="P"])


pic.acc.a=as.data.frame(matrix(NA, nrow = 18, ncol = 4))%>%
  setNames(c("trial_id","component","state","ratio"))
pic.acc.b=as.data.frame(matrix(NA, nrow = 18, ncol = 4))%>%
  setNames(c("trial_id","component","state","ratio"))
for (i in 1:18){
  pic.acc.a$trial_id[i]=i
  pic.acc.a$component[i]="A"
  pic.acc.a$state[i]=a_sta[i]
  pic.acc.a$ratio[i]=pic.model$ratio[pic.model$component=="A" & pic.model$state==a_sta[i] & pic.model$trial_id==i]
  
  pic.acc.b$trial_id[i]=i
  pic.acc.b$component[i]="B"
  pic.acc.b$state[i]=b_sta[i]
  pic.acc.b$ratio[i]=pic.model$ratio[pic.model$component=="B" & pic.model$state==b_sta[i] & pic.model$trial_id==i]
  
}

pic.acc=rbind(pic.acc.a,pic.acc.b)
mean(pic.acc$ratio)
pic.acc.g=pic.acc %>% subset(state=="G")
mean(pic.acc.g$ratio)
pic.acc.p=pic.acc %>% subset(state=="P")
mean(pic.acc.p$ratio)
pic.acc.n=pic.acc %>% subset(state=="N")
mean(pic.acc.n$ratio)