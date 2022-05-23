library(tidyr)
library(dplyr)
library(ggplot2)
library(Rmisc)
library(reshape2)
library(plyr)
library(RColorBrewer)

data.frame(t=seq(0,5,length.out=100))%>% 
  mutate(v=dgamma(t,shape=9,rate=6))%>%
  ggplot(aes(x=t,y=v))+
  geom_line()+
  xlab("Time (second)")+
  ylab("")+
  theme_bw()+
  theme(text = element_text(size=14),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggsave("gamma_generate.pdf", width = 3.3, height = 3)

  
data.frame(t=seq(0,5,length.out=100))%>% 
  mutate(v=dgamma(t,shape=36,rate=12))%>%
  ggplot(aes(x=t,y=v))+
  geom_line()+
  xlab("Time (second)")+
  ylab("")+
  theme_bw()+
  theme(text = element_text(size=14),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggsave("gamma_prevent.pdf", width = 3.3, height = 3)

data.frame(t=seq(0,7,length.out=100))%>% 
  mutate(v=dgamma(t,shape=100,rate=20))%>%
  ggplot(aes(x=t,y=v))+
  geom_line()+
  xlab("Time (second)")+
  ylab("")+
  theme_bw()+
  theme(text = element_text(size=14),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggsave("gamma_regular.pdf", width = 4, height = 3)

data.frame(t=seq(0,7,length.out=100))%>% 
  mutate(v=dgamma(t,shape=1,rate=0.2))%>%
  ggplot(aes(x=t,y=v))+
  geom_line()+
  xlab("Time (second)")+
  ylab("")+
  scale_y_continuous(limits = c(0,0.4))+
  theme_bw()+
  theme(text = element_text(size=14),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggsave("gamma_irregular.pdf", width = 4, height = 3)
