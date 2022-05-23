library(tidyr)
library(dplyr)
library(ggplot2)
library(Rmisc)
library(reshape2)
library(plyr)
library(RColorBrewer)

load("df.expect.Rda")

#delay
df.adelay=df.expect %>% subset(select=c("A_pro","rel","A_delay")) %>%
  mutate(pro=A_pro,delay=A_delay,A_pro=NULL,A_delay=NULL)
df.bdelay=df.expect %>% subset(select=c("B_pro","rel","B_delay")) %>%
  mutate(pro=B_pro,delay=B_delay,B_pro=NULL,B_delay=NULL)
df.delay=rbind(df.adelay,df.bdelay) %>% na.omit()

df.delay %>% 
  mutate(rel=factor(rel,levels=c("r","u"),labels = c("Regular base rate","Irregular base rate")))%>%
  ggplot(aes(delay))+
  # geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
  #                colour="black") +
  geom_density(aes(group = pro,fill=pro))+ 
  theme_bw()+
  facet_wrap(~rel)+
  ylab("Probability Density")+
  xlab("Delay")+
  scale_fill_manual(values=alpha(c("#66A61E","#666666","#E6AB02"),0.7),
                    labels =  c(" Generative"," Non-causal"," Preventative"))+
  scale_x_continuous(limits = c(0,20))+
  scale_y_continuous(limits = c(0,0.66))+
  theme(text = element_text(size=14),
        legend.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position = "bottom", 
        strip.background =element_rect(fill="white",color="white"))

ggsave("f_delay.pdf", width = 5.8, height = 4)

#number (intervention-based)
df.anumi=df.expect %>% subset(select=c("A_pro","rel","A_numi")) %>%
  mutate(pro=A_pro,numi=A_numi,A_pro=NULL,A_numi=NULL)
df.bnumi=df.expect %>% subset(select=c("B_pro","rel","B_numi")) %>%
  mutate(pro=B_pro,numi=B_numi,B_pro=NULL,B_numi=NULL)
df.numi=rbind(df.anumi,df.bnumi) %>% na.omit()

df.numi%>% 
  mutate(rel=factor(rel,levels=c("r","u"),labels = c("Regular base rate","Irregular base rate")))%>%
  ggplot(aes(numi, fill = pro))+
  facet_wrap(~rel)+
  geom_bar(color="black",position="fill")+
  scale_x_continuous(limits = c(-1,7.5),breaks = c(0,1,2,3,4,5,6,7))+
  theme_bw()+
  theme(text = element_text(size=14),
        legend.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position = "bottom",
        strip.background =element_rect(fill="white",color="white")
  )+
  ylab("Probability")+
  xlab("Count (intervention-based)")+
  scale_fill_manual(values=c("#66A61E","#666666","#E6AB02"),
                    labels =  c("Generative ","Non-causal ","Preventative "))

ggsave("f_count.pdf", width = 5.8, height = 4)

df.numi%>% subset(rel=="u") %>% ggplot(aes(val, fill = pro))+
  geom_bar(color="black",position="fill")+
  scale_x_continuous(limits = c(-1,7.5),breaks = c(0,1,2,3,4,5,6,7))+
  theme_bw()+
  theme(text = element_text(size=22),
        legend.title = element_blank(),
        # panel.grid.major = element_blank(),
        # panel.grid.minor = element_blank(),
        panel.background = element_blank()
        # legend.position = "none"
  )+
  ylab("Probability")+
  xlab("Count (intervention-based)")+
  scale_fill_manual(values=c("#66A61E","#666666","#E6AB02"),
                    labels =  c("Generative ","Non-causal ","Preventative "))
ggsave("feature_numi_irregular.pdf", width = 7, height = 4)

#number window
df.anumw=df.e %>% subset(cue=="A_numw")
df.anumw$B_pro=NULL
colnames(df.anumw)=c("simulation","pro","rel","baserate","ori","val","cue")
df.bnumw=df.e %>% subset(cue=="B_numw")
df.bnumw$A_pro=NULL
colnames(df.bnumw)=c("simulation","pro","rel","baserate","ori","val","cue")
df.numw=rbind(df.anumw,df.bnumw)

df.numw%>% subset(rel=="r") %>% ggplot(aes(val, fill = pro))+
  geom_bar(color="black",position="fill")+
  scale_x_continuous(limits = c(-1,7.5),breaks = c(0,1,2,3,4,5,6,7))+
  theme_bw()+
  theme(text = element_text(size=22),
        legend.title = element_blank(),
        # panel.grid.major = element_blank(),
        # panel.grid.minor = element_blank(),
        panel.background = element_blank()
        # legend.position = "none"
  )+
  ylab("Probability")+
  xlab("Count (fixed-window)")+
  scale_fill_manual(values=c("#66A61E","#666666","#E6AB02"),
                    labels =  c("Generative ","Non-causal ","Preventative "))
ggsave("feature_numw_regular.pdf", width = 7, height = 4)

df.numw%>% subset(rel=="u") %>% ggplot(aes(val, fill = pro))+
  geom_bar(color="black",position="fill")+
  scale_x_continuous(limits = c(-1,7.5),breaks = c(0,1,2,3,4,5,6,7))+
  theme_bw()+
  theme(text = element_text(size=22),
        legend.title = element_blank(),
        # panel.grid.major = element_blank(),
        # panel.grid.minor = element_blank(),
        panel.background = element_blank()
        # legend.position = "bottom"
  )+
  ylab("Probability")+
  xlab("Count (fixed-window)")+
  scale_fill_manual(values=c("#66A61E","#666666","#E6AB02"),
                    labels =  c("Generative ","Non-causal ","Preventative "))
# ggsave("feature_numw_forlegend.pdf", width = 7, height = 4)
ggsave("feature_numw_irregular.pdf", width = 7, height = 4)


