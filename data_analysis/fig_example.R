library(dplyr)
library(tidyr)
library(matrixStats)
library(parallel)
library(spatstat)
library(ggplot2)
source('fun_nor.R')

set.seed(2)

myres<-function(md){
  bl=expand.grid(nod=c("A","B"),pred=c("G","N","P"),prob=NA)
  lis=list("A"= rep(c("G","N","P"),each=3),"B"=rep(c("G","N","P"),3))
  for (i in 1:nrow(bl)){
    bl$prob[i]=sum(md[which(lis[[bl$nod[i]]]==bl$pred[i])])
  }
  bl
}

whole_trial_end=trial_end=7
s_ab=3
s_e=30
k_pe = 36
r_pe = 12 #preventative p->e: m=3,var=0.25
k_ge = 9
r_ge = 6  #generative g->e: m=1.5,var=0.25

baserate=5
baserate_var=0.25
r_e_r = baserate/baserate_var
k_e_r = r_e_r*baserate
k_e_u=1
r_e_u=1/baserate
st_a=c("G","G","G","N","N","N","P","P","P")
st_b=c("G","N","P","G","N","P","G","N","P")

a_pro_list=rep(rep(c("G","P","N"),each=2),3)
b_pro_list=rep(c("G","P","N"),each=6)

sqc=data.frame(obj=c("E","B","A","E","E"),time=c(0,2,3.2,3.8,5.5))



if (1){k_e=k_e_r;r_e=r_e_r}else{k_e=k_e_u;r_e=r_e_u}
md=MyMdNor(sqc,k_e,r_e,k_pe,r_pe,k_ge,r_ge)

md %>% myres() %>%
  mutate(pred=factor(pred,levels=c("G","N","P"),labels=c("Generative","Non-causal","Preventative")))%>%
  ggplot(aes(x=nod,y=prob,fill=pred))+
  geom_bar(stat="identity",position=position_dodge(.9),color="black")+
  ylab("")+
  xlab("")+
  scale_fill_grey(start = 0.5, end = 1)+
  scale_y_continuous(limits = c(0,0.82),breaks = c(0,.2,.4,.6,.8))+
  theme_bw()+  
  theme(text = element_text(size=15),
        panel.grid = element_blank(),
        legend.title = element_blank()
        )

ggsave(file="f_demo1.pdf",width = 5,height = 2.5)


#prepare for feature_based model
load("df.expect.Rda")
df.e=df.expect %>% gather(cue, val, A_delay:B_numw7) %>% na.omit()

#feature  intervention-based
numcue=c("A_numi","B_numi")
varname=c('delay_i',"count_i")
combined_var="fea_i"
modname="feai"
segmod="intervention"
source('fun_fea_fig.R')
s_ab=1
mm=MyMdFea(sqc,"r")

(rowSums(mm)/sum(mm)) %>% 
  myres() %>%
  mutate(pred=factor(pred,levels=c("G","N","P"),labels=c("Generative","Non-causal","Preventative")))%>%
  ggplot(aes(x=nod,y=prob,fill=pred))+
  geom_bar(stat="identity",position=position_dodge(.9),color="black")+
  ylab("")+
  xlab("")+
  scale_fill_grey(start = 0.5, end = 1)+
  scale_y_continuous(limits = c(0,0.82),breaks = c(0,.2,.4,.6,.8))+
  theme_bw()+  
  theme(text = element_text(size=15),
        panel.grid = element_blank(),
        legend.title = element_blank()
  )

ggsave(file="f_demo2.pdf",width = 5,height = 2.5)

#window
segmod="window"

win_len=4
numcue=paste(c("A_numw","B_numw"),win_len,sep="")
varname=paste(c('delay_w',"count_w"),win_len,sep="")
combined_var=paste("fea_w",win_len,sep="")
modname=paste("feaw",win_len,sep = "")
source('fun_fea_fig.R')

mm2=MyMdFea(sqc,"r")

(rowSums(mm2)/sum(mm2)) %>% 
  myres() %>%
  mutate(pred=factor(pred,levels=c("G","N","P"),labels=c("Generative","Non-causal","Preventative")))%>%
  ggplot(aes(x=nod,y=prob,fill=pred))+
  geom_bar(stat="identity",position=position_dodge(.9),color="black")+
  ylab("")+
  xlab("")+
  scale_fill_grey(start = 0.5, end = 1)+
  scale_y_continuous(limits = c(0,0.82),breaks = c(0,.2,.4,.6,.8))+
  theme_bw()+  
  theme(text = element_text(size=15),
        panel.grid = element_blank(),
        legend.title = element_blank()
  )


ggsave(file="f_demo3.pdf",width = 5,height = 2.5)
