library(tidyr)
library(dplyr)
library(ggplot2)
library(Rmisc)
library(reshape2)
library(plyr)
library(RColorBrewer)

load('sti_exp1a/seed3/allstimuli.Rda')

vec.sti=c("gg","gn","gp","ng","nn","np","pg","pn","pp")
dt=data.frame()
for (k in 1:2){
  con=c("r","u")[k]
  for (m in 1:9){
    sqc=get(paste("sqc.",vec.sti[m],"_",con,sep="")) %>% select(obj,time) %>%
      mutate(cond=con,
             sti=vec.sti[m])
    dt=rbind(dt,sqc)
  }
}
dt$sti=factor(dt$sti,levels = rev(vec.sti)) %>% as.numeric()

dt%>%
  mutate(cond=factor(cond,levels = c("r","u"),labels = c("Regular (Seed 3)","Irregular (Seed 3)")))%>%
  ggplot(aes(x=time,y=sti))+
  facet_wrap(~cond)+
  geom_rect(aes(xmin=time-0.07, xmax= time+0.07,ymin=sti-0.3,ymax=sti+0.3,fill=obj))+
  theme_classic()+
  scale_y_continuous(breaks = c(1:9),labels = rev(c("GG","GN","GP","NG","NN","NP","PG","PN","PP")))+
  xlab("Time")+
  scale_fill_manual("Activation",values=c("#b149ff","#00A89D","#FFB34D"),
                    labels=c("Component A","Component B","Effect"))+
  theme(legend.position = "bottom",
        axis.title.y = element_blank(),
        legend.margin=margin(-5,0,0,0))

# ggsave(file="f_exp_sti.pdf",width = 6,height = 3.8)



vec.sti=c("gg","gn","gp","ng","nn","np","pg","pn","pp")
dt=data.frame()
for (k in 1:2){
  con=c("r","u")[k]
  for (m in 1:9){
    
    ff=paste("sti_exp1a/seed",m,"/allstimuli.Rda",sep="")
    load(ff)
    sqc=get(paste("sqc.","gg","_",con,sep="")) %>% select(obj,time) %>%
      mutate(cond=con,
             seed=m)
    dt=rbind(dt,sqc)
  }
}
dt$seed=factor(dt$seed,levels = rev(c(1:9))) %>% as.numeric()

dt%>%
  mutate(cond=factor(cond,levels = c("r","u"),labels = c("Regular (GG)","Irregular (GG)")))%>%
  ggplot(aes(x=time,y=seed))+
  facet_wrap(~cond)+
  geom_rect(aes(xmin=time-0.07, xmax= time+0.07,ymin=seed-0.3,ymax=seed+0.3,fill=obj))+
  theme_classic()+
  scale_y_continuous(breaks = c(1:9),labels = rev(paste("Seed",c(1:9))))+
  xlab("Time")+
  scale_fill_manual("Activation",values=c("#b149ff","#00A89D","#FFB34D"),
                    labels=c("Component A","Component B","Effect"))+
  theme(legend.position = "bottom",
        axis.title.y = element_blank(),
        legend.margin=margin(-5,0,0,0))

# ggsave(file="f_exp_sti_gg.pdf",width = 6.25,height = 3.8)
