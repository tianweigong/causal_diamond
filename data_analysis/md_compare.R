library(dplyr)
library(tidyr)
library(ggplot2)
library(RColorBrewer)
library(data.table)


#exp1
load("modelFit_exp1.Rda")
CV=c()
for (k in c("feaw1","feaw1.5","feaw2","feaw2.5",'feaw3',"feaw3.5",
            "feaw4","feaw4.5","feaw5","feaw5.5",'feaw6',"feaw6.5","feaw7")){
  CV=c(CV,CV.list[[k]][["cv_log"]])
}
df.win.cv=data.frame(win_len=c(1,1.5,2,2.5,3,3.5,4,4.5,5,5.5,6,6.5,7),CV=CV)

df.win.cv%>% ggplot(aes(x=win_len,y=CV))+
  geom_point(color="gray20",size=2)+
  geom_line(color="gray20",size=0.8)+
  theme_bw()+
  scale_x_continuous(breaks = c(1:7))+
  # scale_y_continuous(limits = c(5600,6200))+
  geom_hline(yintercept=CV.list[["feai"]][["cv_log"]], linetype="dashed", color = "gray20",size=0.8)+
  theme_bw()+
  xlab("Window Length (seconds)")+
  ylab("Cross Validation")+
  theme(text = element_text(size=15),
        legend.title = element_blank(),
        # axis.text.x=element_text(angle=45, hjust=1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position = "bottom"
  )
ggsave("win_CV_exp1.pdf", width = 4, height = 3.5)

load("mdall_exp1.Rda")
mdall_acc=mdall%>% subset(choice==trial_type)
ACC_delay=c()
ACC_count=c()
for (k in c("1","1.5","2","2.5",'3',"3.5","4","4.5","5","5.5","6","6.5","7")){
  ACC_delay=c(ACC_delay,mean(mdall_acc[,paste("delay_w",k,sep="")]))
  ACC_count=c(ACC_count,mean(mdall_acc[,paste("count_w",k,sep="")]))
}
df.win.acc=data.frame(win_len=rep(c(1,1.5,2,2.5,3,3.5,4,4.5,5,5.5,6,6.5,7),2),
                      Feature=factor(rep(c("Delay","Count"),each=13),levels = c("Delay","Count")),
                      ACC=c(ACC_delay,
                            ACC_count))
df.win.acc%>% ggplot(aes(x=win_len,y=ACC,color=Feature))+
  geom_point(size=2)+
  geom_line(size=0.8)+
  theme_bw()+
  scale_x_continuous(breaks = c(1:7))+
  # scale_y_continuous(limits = c(0.4,0.8))+
  geom_hline(yintercept=mean(mdall_acc[,"delay_i"]), linetype="dashed", color = "gray20",size=0.8)+
  geom_hline(yintercept=mean(mdall_acc[,"count_i"]), linetype="dashed", color = "gray66",size=0.8)+
  theme_bw()+
  scale_color_manual(values=c( "gray20","gray66"))+
  xlab("Window Length (seconds)")+
  ylab("Accuracy")+
  theme(text = element_text(size=15),
        legend.title = element_blank(),
        # axis.text.x=element_text(angle=45, hjust=1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.text = element_text(size = 13),
        legend.position = c(.85,.18),
        legend.margin =margin(r=0,l=0,t=0,b=0)
        # legend.position = "none"
  )
ggsave("win_ACC_exp1.pdf", width = 4, height = 3.5)
# brewer.pal(n = 8, name = 'Dark2')
# display.brewer.pal(n = 8, name = 'Dark2')


#exp2
load("modelFit_exp2.Rda")
CV=c()
for (k in c("feaw1","feaw1.5","feaw2","feaw2.5",'feaw3',"feaw3.5",
            "feaw4","feaw4.5","feaw5","feaw5.5",'feaw6',"feaw6.5","feaw7")){
  CV=c(CV,CV.list[[k]][["cv_log"]])
}
df.win.cv=data.frame(win_len=c(1,1.5,2,2.5,3,3.5,4,4.5,5,5.5,6,6.5,7),CV=CV)

df.win.cv%>% ggplot(aes(x=win_len,y=CV))+
  geom_point(color="gray20",size=2)+
  geom_line(color="gray20",size=0.8)+
  theme_bw()+
  scale_x_continuous(breaks = c(1:7))+
  # scale_y_continuous(limits = c(5600,6200))+
  geom_hline(yintercept=CV.list[["feai"]][["cv_log"]], linetype="dashed", color = "gray20",size=0.8)+
  theme_bw()+
  xlab("Window Length (seconds)")+
  ylab("Cross Validation")+
  theme(text = element_text(size=15),
        legend.title = element_blank(),
        # axis.text.x=element_text(angle=45, hjust=1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position = "bottom"
  )
ggsave("win_CV_exp2.pdf", width = 4, height = 3.5)

load("mdall_exp2.Rda")
mdall_acc=mdall%>% subset(choice==trial_type)
ACC_delay=c()
ACC_count=c()
for (k in c("1","1.5","2","2.5",'3',"3.5","4","4.5","5","5.5","6","6.5","7")){
  ACC_delay=c(ACC_delay,mean(mdall_acc[,paste("delay_w",k,sep="")]))
  ACC_count=c(ACC_count,mean(mdall_acc[,paste("count_w",k,sep="")]))
}
df.win.acc=data.frame(win_len=rep(c(1,1.5,2,2.5,3,3.5,4,4.5,5,5.5,6,6.5,7),2),
                      Feature=factor(rep(c("Delay","Count"),each=13),levels = c("Delay","Count")),
                      ACC=c(ACC_delay,
                            ACC_count))
df.win.acc%>% ggplot(aes(x=win_len,y=ACC,color=Feature))+
  geom_point(size=2)+
  geom_line(size=0.8)+
  theme_bw()+
  scale_x_continuous(breaks = c(1:7))+
  # scale_y_continuous(limits = c(0.4,0.8))+
  geom_hline(yintercept=mean(mdall_acc[,"delay_i"]), linetype="dashed", color = "gray20",size=0.8)+
  geom_hline(yintercept=mean(mdall_acc[,"count_i"]), linetype="dashed", color = "gray66",size=0.8)+
  theme_bw()+
  scale_color_manual(values=c( "gray20","gray66"))+
  xlab("Window Length (seconds)")+
  ylab("Accuracy")+
  theme(text = element_text(size=15),
        legend.title = element_blank(),
        # axis.text.x=element_text(angle=45, hjust=1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.text = element_text(size = 13),
        legend.position = c(.85,.18),
        legend.margin =margin(r=0,l=0,t=0,b=0)
        # legend.position = "none"
  )
ggsave("win_ACC_exp2.pdf", width = 4, height = 3.5)
# brewer.pal(n = 8, name = 'Dark2')
# display.brewer.pal(n = 8, name = 'Dark2')


#exp3
load("modelFit_exp3.Rda")
CV=c()
for (k in c("feaw1","feaw1.5","feaw2","feaw2.5",'feaw3',"feaw3.5",
            "feaw4","feaw4.5","feaw5","feaw5.5",'feaw6',"feaw6.5","feaw7")){
  CV=c(CV,CV.list[[k]][["cv_log"]])
}
df.win.cv=data.frame(win_len=c(1,1.5,2,2.5,3,3.5,4,4.5,5,5.5,6,6.5,7),CV=CV)

df.win.cv%>% ggplot(aes(x=win_len,y=CV))+
  geom_point(color="gray20",size=2)+
  geom_line(color="gray20",size=0.8)+
  theme_bw()+
  scale_x_continuous(breaks = c(1:7))+
  # scale_y_continuous(limits = c(5600,6200))+
  geom_hline(yintercept=CV.list[["feai"]][["cv_log"]], linetype="dashed", color = "gray20",size=0.8)+
  theme_bw()+
  xlab("Window Length (seconds)")+
  ylab("Cross Validation")+
  theme(text = element_text(size=15),
        legend.title = element_blank(),
        # axis.text.x=element_text(angle=45, hjust=1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position = "bottom"
  )
ggsave("win_CV_exp3.pdf", width = 4, height = 3.5)

load("mdall_exp3.Rda")
mdall_acc=mdall%>% subset(choice==trial_type & seed<10)
ACC_delay=c()
ACC_count=c()
for (k in c("1","1.5","2","2.5",'3',"3.5","4","4.5","5","5.5","6","6.5","7")){
  ACC_delay=c(ACC_delay,mean(mdall_acc[,paste("delay_w",k,sep="")]))
  ACC_count=c(ACC_count,mean(mdall_acc[,paste("count_w",k,sep="")]))
}
df.win.acc=data.frame(win_len=rep(c(1,1.5,2,2.5,3,3.5,4,4.5,5,5.5,6,6.5,7),2),
                      Feature=factor(rep(c("Delay","Count"),each=13),levels = c("Delay","Count")),
                      ACC=c(ACC_delay,
                            ACC_count))
df.win.acc%>% ggplot(aes(x=win_len,y=ACC,color=Feature))+
  geom_point(size=2)+
  geom_line(size=0.8)+
  theme_bw()+
  scale_x_continuous(breaks = c(1:7))+
  # scale_y_continuous(limits = c(0.4,0.8))+
  geom_hline(yintercept=mean(mdall_acc[,"delay_i"]), linetype="dashed", color = "gray20",size=0.8)+
  geom_hline(yintercept=mean(mdall_acc[,"count_i"]), linetype="dashed", color = "gray66",size=0.8)+
  theme_bw()+
  scale_color_manual(values=c( "gray20","gray66"))+
  xlab("Window Length (seconds)")+
  ylab("Accuracy")+
  theme(text = element_text(size=15),
        legend.title = element_blank(),
        # axis.text.x=element_text(angle=45, hjust=1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.text = element_text(size = 13),
        legend.position = c(.85,.18),
        legend.margin =margin(r=0,l=0,t=0,b=0)
        # legend.position = "none"
  )
ggsave("win_ACC_exp3.pdf", width = 4, height = 3.5)
# brewer.pal(n = 8, name = 'Dark2')
# display.brewer.pal(n = 8, name = 'Dark2')








