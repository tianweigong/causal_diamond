library(tidyr)
library(dplyr)
library(ggplot2)
library(Rmisc)
library(reshape2)
library(plyr)
library(RColorBrewer)

load('sti_exp2/seed3/allstimuli.Rda')

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
  mutate(cond=factor(cond,levels = c("r","u"),labels = c("Regular","Irregular")))%>%
  ggplot(aes(x=time,y=sti))+
  facet_wrap(~cond)+
  geom_rect(aes(xmin=time-0.1, xmax= time+0.1,ymin=sti-0.3,ymax=sti+0.3,fill=obj))+
  theme_classic()+
  scale_y_continuous(breaks = c(1:9),labels = rev(c("GG","GN","GP","NG","NN","NP","PG","PN","PP")))+
  xlab("Time")+
  scale_fill_manual("Activation",values=c("#66A61E","#E6AB02","#666666"),
                    labels=c("A","B","E"))+
  theme(legend.position = "bottom",
        axis.title.y = element_blank(),
        legend.margin=margin(-5,0,0,0))

ggsave(file="f_exp_sti.pdf",width = 6.5,height = 3.8)

