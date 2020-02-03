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

MySubFit9 <-function(){
  st_a=pro_a=c("G","P","N","G","P","N","G","P","N")
  st_b=pro_b=c("G","G","G","P","P","P","N","N","N")
  
  md.sub=as.data.frame(matrix(NA,ncol=7,nrow=18*9)) %>%
    setNames(c("trial_id","A_pro","B_pro","A_state","B_state","count","hm"))
  
  for (i in 1:18){
    for (j in 1:length(st_a)){
      p=(i-1)*9+j
      md.sub$trial_id[p]=i
      md.sub$A_pro[p]=pro_a[ceiling(i/2)]
      md.sub$B_pro[p]=pro_b[ceiling(i/2)]
      md.sub$A_state[p]=st_a[j]
      md.sub$B_state[p]=st_b[j]
      md.sub$count[p]=df.final %>% subset(trial_id==i & A_state== st_a[j] & B_state==st_b[j]) %>% nrow()
      md.sub$hm[p]=md.sub$count[p]/(df.final %>% subset(trial_id==i) %>% nrow())
    }
  }
  
  return(md.sub)
}


MySubFit6 <-function(){
  md.sub.a=plyr::count(df.final,c("trial_id","A_state"))
  md.sub.a$cpn="A"
  colnames(md.sub.a)=c("trial_id","state","freq","cpn")
  
  md.sub.b=plyr::count(df.final,c("trial_id","B_state"))
  md.sub.b$cpn="B"
  colnames(md.sub.b)=c("trial_id","state","freq","cpn")
  
  md.sub=rbind(md.sub.a,md.sub.b)
  
  sub_num=plyr::count(df.final,c("trial_id"))
  sub_num=sub_num$freq[1]
  
  md.sub$hm=md.sub$freq/sub_num
  
  for (i in c("A","B")){
    for (j in c("G","P","N")){
      for (m in 1:18){
        if (nrow(subset(md.sub,(cpn==i&state==j&trial_id==m)))==0){
          md.sub[nrow(md.sub) + 1,]=c(m,j,0,i,0)
        }
      }
    }
  }
  md.sub$trial_id=as.numeric(md.sub$trial_id)
  md.sub$hm=as.numeric(md.sub$hm)
  md.sub=md.sub[
    order( md.sub[,"trial_id"], md.sub[,"cpn"],md.sub[,"state"]),
    ]
  
  return(md.sub)
}

MyModFit6<- function(md_raw){
  
  md=as.data.frame(matrix(NA, nrow = 18*2*3, ncol = 4))%>%
    setNames(c("trial_id","cpn","state","ratio"))
  
  for (j in 1:18){
    md$cpn[((j-1)*2*3+1):(j*2*3)]=c("A","A","A","B","B","B")
    md$state[((j-1)*2*3+1):(j*2*3)]=c("G","N","P","G","N","P")
    md$trial_id[((j-1)*2*3+1):(j*2*3)]=j
    
    md$ratio[(j-1)*2*3+1]=sum(md_raw$ratio[md_raw$A_state=="G" & md_raw$trial_id==j])
    md$ratio[(j-1)*2*3+2]=sum(md_raw$ratio[md_raw$A_state=="N" & md_raw$trial_id==j])
    md$ratio[(j-1)*2*3+3]=sum(md_raw$ratio[md_raw$A_state=="P" & md_raw$trial_id==j])
    md$ratio[(j-1)*2*3+4]=sum(md_raw$ratio[md_raw$B_state=="G" & md_raw$trial_id==j])
    md$ratio[(j-1)*2*3+5]=sum(md_raw$ratio[md_raw$B_state=="N" & md_raw$trial_id==j])
    md$ratio[(j-1)*2*3+6]=sum(md_raw$ratio[md_raw$B_state=="P" & md_raw$trial_id==j])
  }
  
  return(md)
}

MyModFitSfm6<- function(md_raw){
  
  md_raw$sfm=NA
  for (i in 1:18){
    choice_sub=md_raw %>% subset(trial_id==i)
    for (m in 1:9){
      sfm=exp(choice_sub$ratio[m]*a)/sum(exp(choice_sub$ratio*a))
      md_raw$sfm[which(md_raw$trial==i)[m]]=sfm
    }
  }
  
  ####check
  md.total=merge(md_raw,md9.sub,by=c("trial_id","A_pro","B_pro","A_state","B_state"))
  Sys.sleep(0.01)
  print(sum(md_raw$sfm))
  print(-sum(log(md.total$sfm)*md.total$hm*60)*2+2*log(162))
  flush.console()
  ####check end

  md=as.data.frame(matrix(NA, nrow = 18*2*3, ncol = 4))%>%
    setNames(c("trial_id","cpn","state","ratio"))
  
  for (j in 1:18){
    md$cpn[((j-1)*2*3+1):(j*2*3)]=c("A","A","A","B","B","B")
    md$state[((j-1)*2*3+1):(j*2*3)]=c("G","N","P","G","N","P")
    md$trial_id[((j-1)*2*3+1):(j*2*3)]=j
    
    md$ratio[(j-1)*2*3+1]=sum(md_raw$sfm[md_raw$A_state=="G" & md_raw$trial_id==j])/sum(md_raw$sfm[md_raw$trial_id==j])
    md$ratio[(j-1)*2*3+2]=sum(md_raw$sfm[md_raw$A_state=="N" & md_raw$trial_id==j])/sum(md_raw$sfm[md_raw$trial_id==j])
    md$ratio[(j-1)*2*3+3]=sum(md_raw$sfm[md_raw$A_state=="P" & md_raw$trial_id==j])/sum(md_raw$sfm[md_raw$trial_id==j])
    md$ratio[(j-1)*2*3+4]=sum(md_raw$sfm[md_raw$B_state=="G" & md_raw$trial_id==j])/sum(md_raw$sfm[md_raw$trial_id==j])
    md$ratio[(j-1)*2*3+5]=sum(md_raw$sfm[md_raw$B_state=="N" & md_raw$trial_id==j])/sum(md_raw$sfm[md_raw$trial_id==j])
    md$ratio[(j-1)*2*3+6]=sum(md_raw$sfm[md_raw$B_state=="P" & md_raw$trial_id==j])/sum(md_raw$sfm[md_raw$trial_id==j])
  }
  
  return(md)
}

MyModFitSfm6_2<- function(md_raw1,md_raw2){
  md_raw=merge(md_raw1,md_raw2,by=c("trial_id","A_pro","B_pro","A_state","B_state"))
  md_raw$sfm=NA
  for (i in 1:18){
    choice_sub=md_raw %>% subset(trial_id==i)
    for (m in 1:9){
      sfm=exp(choice_sub$ratio.x[m]*a1+choice_sub$ratio.y[m]*a2)/sum(exp(choice_sub$ratio.x*a1+choice_sub$ratio.y*a2))
      md_raw$sfm[which(md_raw$trial==i)[m]]=sfm
    }
  }
  
  ####check
  md.total=merge(md_raw,md9.sub,by=c("trial_id","A_pro","B_pro","A_state","B_state"))
  Sys.sleep(0.01)
  print(sum(md_raw$sfm))
  print(-sum(log(md.total$sfm)*md.total$hm*60)*2+log(162)*2)
  flush.console()
  ####check end
  
  md=as.data.frame(matrix(NA, nrow = 18*2*3, ncol = 4))%>%
    setNames(c("trial_id","cpn","state","ratio"))
  
  for (j in 1:18){
    md$cpn[((j-1)*2*3+1):(j*2*3)]=c("A","A","A","B","B","B")
    md$state[((j-1)*2*3+1):(j*2*3)]=c("G","N","P","G","N","P")
    md$trial_id[((j-1)*2*3+1):(j*2*3)]=j
    
    md$ratio[(j-1)*2*3+1]=sum(md_raw$sfm[md_raw$A_state=="G" & md_raw$trial_id==j])/sum(md_raw$sfm[md_raw$trial_id==j])
    md$ratio[(j-1)*2*3+2]=sum(md_raw$sfm[md_raw$A_state=="N" & md_raw$trial_id==j])/sum(md_raw$sfm[md_raw$trial_id==j])
    md$ratio[(j-1)*2*3+3]=sum(md_raw$sfm[md_raw$A_state=="P" & md_raw$trial_id==j])/sum(md_raw$sfm[md_raw$trial_id==j])
    md$ratio[(j-1)*2*3+4]=sum(md_raw$sfm[md_raw$B_state=="G" & md_raw$trial_id==j])/sum(md_raw$sfm[md_raw$trial_id==j])
    md$ratio[(j-1)*2*3+5]=sum(md_raw$sfm[md_raw$B_state=="N" & md_raw$trial_id==j])/sum(md_raw$sfm[md_raw$trial_id==j])
    md$ratio[(j-1)*2*3+6]=sum(md_raw$sfm[md_raw$B_state=="P" & md_raw$trial_id==j])/sum(md_raw$sfm[md_raw$trial_id==j])
  }
  
  return(md)
}

load("df.final.Rda")
md6.sub=MySubFit6()
md9.sub=MySubFit9()

load("../model/normative/df.model.q3.Rda")
a=2.67
md6.nor=MyModFitSfm6(df.model)
colnames(md6.nor)[ncol(md6.nor)]="a_normative"

load("../model/feature_based/df.model.delay.Rda")
md.delay=df.model
load("../model/feature_based/df.model.num.Rda")
md.num=df.model
a1=1.92
a2=3.52
md6.fea=MyModFitSfm6_2(md.delay,md.num)
colnames(md6.fea)[ncol(md6.fea)]="b_feature"


load("../model/local/md.large.Rda")
a=2.05
md6.sqc=md.large %>%subset(q1>3.85 & q1<3.87) %>% MyModFitSfm6()
colnames(md6.sqc)[ncol(md6.sqc)]="c_neurath"

pic.cb=merge(md6.nor,md6.fea,by=c("trial_id","cpn","state"))
pic.cb=merge(pic.cb,md6.sqc,by=c("trial_id","cpn","state"))

pic.model=pic.cb %>%  gather(model, ratio, a_normative:c_neurath)

pic.label=c("GG1","GG2","PG1","PG2","NG1","NG2",
            "GP1","GP2","PP1","PP2","NP1","NP2",
            "GN1","GN2","PN1","PN2","NN1","NN2")
md6.sub=md6.sub%>%
  mutate(trial=pic.label[trial_id])
pic.model=pic.model%>%
  mutate(trial=pic.label[trial_id])
  
ggplot()+
  geom_bar(md6.sub,mapping=aes(x=cpn, y=hm,fill=state),stat='identity',position="dodge",color="black")+
  geom_point(pic.model,mapping =aes(x=cpn, y=ratio,color=state,shape=model), stat="identity",
             position=position_dodge(.9),size=1.8)+
  facet_wrap(~trial,nrow=3)+
  theme_bw()+
  theme(text = element_text(size=16),
        legend.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.text = element_text(size = 15),
        legend.position = "bottom",
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-6,0,0,0)
        )+
  labs(fill = "Choice",shape="Model Fitting",y="Ratio",x="Component")+
  scale_fill_manual(values=c("#E4F0EC","#EDB1A4","#6282b1"), #from Ghibli palette :)
                    labels =  c(" Generative  "," Non-causal  "," Preventative  "))+
  scale_color_manual(values=c("black","black","black"))+
  scale_shape_manual(values=c(8,10,2),labels =  c("Normative-noisy  ","Feature-based  ","Expectation-violation  "))+
  guides(fill = guide_legend(order = 1),
         shape = guide_legend(order = 2),color = FALSE)

#ggsave(paste(1,".pdf",sep = ""), width = 16, height = 5.5)


md6.sub$key=rep(c(rep(c("G","G"),2),rep(c("P","G"),2),rep(c("N","G"),2),
                  rep(c("G","P"),2),rep(c("P","P"),2),rep(c("N","P"),2),
                  rep(c("G","N"),2),rep(c("P","N"),2),rep(c("N","N"),2)),each=3)

mean(md6.sub$hm[md6.sub$key=="G" & md6.sub$state=="P"])
mean(md6.sub$hm[md6.sub$key=="N" & md6.sub$state=="P"])
mean(md6.sub$hm[md6.sub$key=="P" & md6.sub$state=="P"])

mean(md6.sub$hm[md6.sub$key=="G" & md6.sub$state=="N"])
mean(md6.sub$hm[md6.sub$key=="N" & md6.sub$state=="N"])
mean(md6.sub$hm[md6.sub$key=="P" & md6.sub$state=="N"])

mean(md6.sub$hm[md6.sub$key=="G" & md6.sub$state=="G"])
mean(md6.sub$hm[md6.sub$key=="N" & md6.sub$state=="G"])
mean(md6.sub$hm[md6.sub$key=="P" & md6.sub$state=="G"])

fig_a=c(rep(c("1.Generative","2.Non-causal","3.Preventative"),3))
fig_b=c(rep("3.Preventative",3),rep("2.Non-causal",3),rep("1.Generative",3))

fig_data=c(0.07,0.23,0.76,0.13,0.62,0.19,0.80,0.15,0.05)
fig_data_pre=c("7%","23%","76%","13%","62%","19%","80%","15%","5%")

dt.fig=data.frame(fig_a,fig_b,fig_data,fig_data_pre)

p1=ggplot(dt.fig,aes(x=fig_a, y=fig_b, fill=fig_data)) + 
  geom_tile()+
  scale_fill_gradientn(colours = c("#191919","white"),trans = "log",limits = c(0.009,1.001),breaks = c(0.009,1.001),na.value = "transparent")+
  ylab("People")+
  xlab("True answer")+
  scale_x_discrete(labels= c("G","N","P"))+
  scale_y_discrete(labels= c("G","N","P"))+
  theme_bw()+
  theme(text = element_text(size=16),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  geom_text(aes(label = fig_data_pre,color = fig_data > 0.5),size=6.5)+
  scale_color_manual(guide = FALSE, values = c("white","black"))


load("../model/normative/df.model.q1.Rda")
pic.nor=MyModFit6(df.model)
pic.nor$key=rep(c(rep(c("G","G"),2),rep(c("P","G"),2),rep(c("N","G"),2),
                  rep(c("G","P"),2),rep(c("P","P"),2),rep(c("N","P"),2),
                  rep(c("G","N"),2),rep(c("P","N"),2),rep(c("N","N"),2)),each=3)

mean(pic.nor$ratio[pic.nor$key=="G" & pic.nor$state=="P"])
mean(pic.nor$ratio[pic.nor$key=="N" & pic.nor$state=="P"])
mean(pic.nor$ratio[pic.nor$key=="P" & pic.nor$state=="P"])

mean(pic.nor$ratio[pic.nor$key=="G" & pic.nor$state=="N"])
mean(pic.nor$ratio[pic.nor$key=="N" & pic.nor$state=="N"])
mean(pic.nor$ratio[pic.nor$key=="P" & pic.nor$state=="N"])

mean(pic.nor$ratio[pic.nor$key=="G" & pic.nor$state=="G"])
mean(pic.nor$ratio[pic.nor$key=="N" & pic.nor$state=="G"])
mean(pic.nor$ratio[pic.nor$key=="P" & pic.nor$state=="G"])

fig_a=c(rep(c("1.Generative","2.Non-causal","3.Preventative"),3))
fig_b=c(rep("3.Generative",3),rep("2.Non-causal",3),rep("1.Preventative",3))

fig_data=c(0.01,0.05,0.98,0.01,0.94,0.02,1,0.01,0.01)
fig_data_pre=c("<1%","5%","98%","<1%","94%","2%","100%","<1%","<1%")

dt.fig=data.frame(fig_a,fig_b,fig_data,fig_data_pre)

p2=ggplot(dt.fig,aes(x=fig_a, y=fig_b, fill=fig_data)) + 
  geom_tile()+
  scale_fill_gradientn(colours = c("#191919","white"),trans = "log",limits = c(0.009,1.001),breaks = c(0.009,1.001),na.value = "transparent")+
  ylab("Normative inference")+
  xlab("True answer")+
  scale_x_discrete(labels= c("G","N","P"))+
  scale_y_discrete(labels= c("G","N","P"))+
  theme_bw()+
  theme(text = element_text(size=16),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  geom_text(aes(label = fig_data_pre,color = fig_data > 0.5),size=6.5)+
  scale_color_manual(guide = FALSE, values = c("white","black"))

multiplot(p1,p2,cols=2)