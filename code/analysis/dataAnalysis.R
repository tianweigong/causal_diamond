#' ---
#' title: Time & Prevention <br> Data Reconstruction
#' author: Tia Gong
#' output:
#'    html_document:
#'      toc: true
#'      toc_depth: 3
#'      toc_float: true
#'      theme: default
#'      highlight: tango
#' ---

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


#+ load subject data -------------------
#' # load subject data
v.block=c("b8vfi4v7m","w2jgbz0sq","lm7j5xstc","yhnp6ph7i","xhsfpj6kf","nr25kw77x","6zdn5dtje","sljqb7fgn")
v.block=c(v.block,"ad22y5bii")
df.raw=read.csv("../../data/tia_prevent_pilot.csv")

df.raw=df.raw %>%  subset(!(subject %in% v.block))

v.dmg=c("id","date","ip","subject","position","gender","age","engaging","difficult","feedback")
v.exp=setdiff(colnames(df.raw),v.dmg)
v.id=c("subject","position")

df.dmg=df.raw[,v.dmg]

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

# add initial pilot results
df.ipilot=read.csv("../../data/initialpilot.csv")
df.exp=rbind(df.exp,df.ipilot)

df.exp=df.exp %>% 
  mutate(acc_a=as.integer(A_pro==A_state),
         acc_b=as.integer(B_pro==B_state),
         acc_device=as.integer(acc_a & acc_b),
         acc_connect=acc_a+acc_b,
         acc_mean=acc_connect/2
        )

df.exp$trial_id=as.numeric(df.exp$trial_id)
df.final=df.exp %>%subset(data_type=="final" & trial_id!=0 & trial_id!=19)
df.final$source="subject"

#+ summary subject data -------------------
#' # summary subject data
a=summarySE(df.final,measurevar = "acc_device",groupvars = "trial_id")
a$sd=NULL
a$ci=NULL
a$se=NULL
a$trial_name=c("GG","GG","PG","PG",
               "NG","NG","GP","GP",
               "PP","PP","NP","NP",
               "GN","GN","PN","PN",
               "NN","NN")
#+ load model data -------------------
#' # load model data

#simulation result
load("../model/structure_time_sensitive/df.simulate.Rda")
#load("../model/normative_noisy/df.simulate.Rda")
#load("../model/normative_noisy_large/df.simulate.Rda") #no difference from middle-noisy
#load("../model/normative_noisy_small/df.simulate.Rda") #no good as middle-noisy
#load("../model/normative_baseign/df.simulate.Rda") 
#load("../model/normative_baseign_noisy/df.simulate.Rda")

df.simulate=df.simulate %>% 
  mutate(acc_a=as.integer(A_pro==A_state),
         acc_b=as.integer(B_pro==B_state),
         acc_device=as.integer(acc_a & acc_b),
         acc_connect=acc_a+acc_b,
         acc_mean=acc_connect/2
  )

#+ plot acc data -------------------
#' # plot acc data

pic.sim.a=summarySE(df.simulate,measurevar = "acc_a",groupvars = "trial_id")%>% 
  melt(id.vars = c("trial_id","se"), value.name = "acc")%>% 
  subset(variable=="acc_a")
pic.sub.a=summarySE(df.final,measurevar = "acc_a",groupvars = "trial_id") %>% 
  melt(id.vars = c("trial_id","se"), value.name = "acc")%>% 
  subset(variable=="acc_a")

pic.sim.b=summarySE(df.simulate,measurevar = "acc_b",groupvars = "trial_id")%>% 
  melt(id.vars = c("trial_id","se"), value.name = "acc")%>% 
  subset(variable=="acc_b")
pic.sub.b=summarySE(df.final,measurevar = "acc_b",groupvars = "trial_id")%>% 
  melt(id.vars = c("trial_id","se"), value.name = "acc")%>% 
  subset(variable=="acc_b")

pic.sim=rbind(pic.sim.a,pic.sim.b)
pic.sub=rbind(pic.sub.a,pic.sub.b)

ggplot(pic.sub,aes(x=trial_id, y=acc,fill=variable))+
  geom_bar(stat='identity',position="dodge")+
  geom_errorbar(aes(ymin=acc-se, ymax=acc+se),
              width=.2, 
              position=position_dodge(.9))+
  geom_point(pic.sim,mapping =aes(x=trial_id, y=acc,fill=variable),stat="identity",
             position=position_dodge(.9))+
  geom_hline(yintercept=0.33)+
  geom_hline(yintercept=0.67)+
  scale_x_continuous(breaks = c(1:18), 
                     labels = c("GG","GG","PG","PG",
                                "NG","NG","GP","GP",
                                "PP","PP","NP","NP",
                                "GN","GN","PN","PN",
                                "NN","NN"))

hint=c("Generative","Generative","Preventative","Preventative","Non-causal","Non-causal",
       "Generative","Generative","Preventative","Preventative","Non-causal","Non-causal",
       "Generative","Generative","Preventative","Preventative","Non-causal","Non-causal",
       "Generative","Generative","Generative","Generative","Generative","Generative",
       "Preventative","Preventative","Preventative","Preventative","Preventative","Preventative",
       "Non-causal","Non-causal","Non-causal","Non-causal","Non-causal","Non-causal")
pic.total=data.frame(pic.sim$acc,pic.sub$acc,hint)

cor.test(pic.sim$acc,pic.sub$acc)

ggplot(pic.total,aes(x=pic.sim.acc, y=pic.sub.acc,color=hint))+
  geom_point(shape=18,size=3,position =position_dodge(width = 0.05))+
  scale_colour_manual(values = c("#E69F00", "#999999","#56B4E9"))+
  xlab("Local Computations Model Accuracy")+
  ylab("Human Accuracy")+
  labs(color='Correct answer') +
  geom_smooth(aes(x=pic.sim.acc,y=pic.sub.acc),method= "lm",formula=y~x,color="black")+
  theme_bw()+
  theme(
    # Hide panel borders and remove grid lines
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Change axis line
    axis.line = element_line(colour = "black")
  )+
  annotate("text", x = 0.15, y = 1.0, label = "italic('r = , n = ')",parse=TRUE)

#+ plot answer-type data -------------------
#' # plot answer-type data

#reshape subject data
pic.sub.a=count(df.final,c("trial_id","A_state"))
pic.sub.a$cpn="A"
colnames(pic.sub.a)=c("trial_id","state","freq","cpn")

pic.sub.b=count(df.final,c("trial_id","B_state"))
pic.sub.b$cpn="B"
colnames(pic.sub.b)=c("trial_id","state","freq","cpn")

pic.sub=rbind(pic.sub.a,pic.sub.b)

sub_num=count(df.final,c("trial_id"))
sub_num=sub_num$freq[1]

pic.sub$ratio=pic.sub$freq/sub_num

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
pic.sub$ratio=as.numeric(pic.sub$ratio)
pic.sub=pic.sub[
  order( pic.sub[,"trial_id"], pic.sub[,"cpn"],pic.sub[,"state"]),
  ]
#reshape simulation data
pic.sim.a=count(df.simulate,c("trial_id","A_state"))
pic.sim.a$cpn="A"
colnames(pic.sim.a)=c("trial_id","state","freq","cpn")

pic.sim.b=count(df.simulate,c("trial_id","B_state"))
pic.sim.b$cpn="B"
colnames(pic.sim.b)=c("trial_id","state","freq","cpn")

pic.sim=rbind(pic.sim.a,pic.sim.b)

sim_num=count(df.simulate,c("trial_id"))
sim_num=sim_num$freq[1]

pic.sim$ratio=pic.sim$freq/sim_num

for (i in c("A","B")){
  for (j in c("G","P","N")){
    for (m in 1:18){
      if (nrow(subset(pic.sim,(cpn==i&state==j&trial_id==m)))==0){
        pic.sim[nrow(pic.sim) + 1,]=c(m,j,0,i,0)
      }
    }
  }
}
pic.sim$trial_id=as.numeric(pic.sim$trial_id)
pic.sim$ratio=as.numeric(pic.sim$ratio)
pic.sim=pic.sim[
  order( pic.sim[,"trial_id"], pic.sim[,"cpn"],pic.sim[,"state"]),
  ]

#plot the bar 
pic.label=c("1"="GG","2"="GG","3"="PG","4"="PG","5"="NG","6"="NG",
            "7"="GP","8"="GP","9"="PP","10"="PP","11"="NP","12"="NP",
            "13"="GN","14"="GN","15"="PN","16"="PN","17"="NN","18"="NN")

ggplot(pic.sub,aes(x=cpn, y=ratio,fill=state))+
  geom_bar(stat='identity',position="dodge",colour="black")+
  geom_point(pic.sim,mapping =aes(x=cpn, y=ratio,fill=state),stat="identity",
             position=position_dodge(.9))+
  facet_wrap(~trial_id,nrow=3,labeller = as_labeller(pic.label))

#plot the scatter
hint=rep(c("Generative","Non-causal","Preventative"),18*2)

pic.total=data.frame(pic.sim$ratio,pic.sub$ratio,hint)


cor.test(pic.sim$ratio,pic.sub$ratio)

ggplot(pic.total,aes(x=pic.sim.ratio, y=pic.sub.ratio,color=hint))+
  geom_point(shape=18,size=3,position =position_dodge(width = 0.05))+
  scale_colour_manual(values = c("#E69F00", "#999999","#56B4E9"))+
  xlab("Ideal-noisy Model Ratio")+
  ylab("Human Ratio")+
  labs(color='Correct answer') +
  geom_smooth(aes(x=pic.sim.ratio,y=pic.sub.ratio),method= "lm",formula=y~x,color="black")+
  theme_bw()+
  theme(
    # Hide panel borders and remove grid lines
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Change axis line
    axis.line = element_line(colour = "black")
  )+
  annotate("text", x = 0.05, y = 1.0, label = "italic('r = .902')",parse=TRUE)
