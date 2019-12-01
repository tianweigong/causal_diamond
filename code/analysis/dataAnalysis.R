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
rm(list=ls())
v.block=c("b8vfi4v7m","w2jgbz0sq","lm7j5xstc","yhnp6ph7i","xhsfpj6kf","nr25kw77x","6zdn5dtje","sljqb7fgn")
v.block=c(v.block,"ad22y5bii")
df.raw=read.csv("tia_prevent_pilot.csv")

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
df.ipilot=read.csv("initialpilot.csv")
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

a=summarySE(df.final,measurevar = "acc_device",groupvars = "trial_id")
a$sd=NULL
a$ci=NULL
a$se=NULL
a$trial_name=c("GG","GG","PG","PG",
               "NG","NG","GP","GP",
               "PP","PP","NP","NP",
               "GN","GN","PN","PN",
               "NN","NN")

b=df.final %>% subset(trial_id=="15")


#simulation result
df.simulate=df.simulate %>% 
  mutate(acc_a=as.integer(A_pro==A_state),
         acc_b=as.integer(B_pro==B_state),
         acc_device=as.integer(acc_a & acc_b),
         acc_connect=acc_a+acc_b,
         acc_mean=acc_connect/2
  )

df.simulate[which(df.simulate$trial_id=="5"),"B_pro"]="G"

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
  geom_point(shape=18,size=3)+
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
  annotate("text", x = 0.15, y = 1.0, label = "italic('r = .477, n = 14')",parse=TRUE)
