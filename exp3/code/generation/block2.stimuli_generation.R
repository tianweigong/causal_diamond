#+ General settings, echo = FALSE, results = 'hide' -------------------
knitr::opts_chunk$set(warning = FALSE, message = FALSE)

#+ load packages -------------------
#' # load packages
library(tidyr)
library(dplyr)
library(ggplot2)
# rm(list=ls())

#+ set parameters -------------------
#' # set parameters



s_e = 10
# s_a = 3
# s_b = 3
s_ab=3

k_e_r = 100
r_e_r = 20
k_e_u = 1
r_e_u = 0.2
k_pe_r = 36
r_pe_r = 12
k_ge_r = 9
r_ge_r = 6

# k_bp= 30
# r_bp= 20
t_bp=2

trial_end=20
whole_trial_end=20
onfor=0.35+0.1


MyRemove <- function (v.org,v.prevent,v.range){
  for (k in 1: length(v.prevent)){
    v.org=v.org[!(v.org>v.prevent[k] & v.org<v.range[k])]
  }
  return(v.org)
}


MyReshape <-function(sti_sqc){
  sti_sqc = sti_sqc[order(sti_sqc$time), ] %>% subset(time<whole_trial_end)
  return(sti_sqc)
}

MyIdtCheck <- function(){
  x=c()
  for (k in seq(1,length(sti_list),by=2)){
    mysqc=get(sti_list[k])
    x=c(x,paste(mysqc$time%>%round(3),collapse=""))
  }
  return(length(unique(x))==length(sti_list)/2)
}

MyGen<-function(x){
  while (1){
    A = sort(runif(s_ab,0,whole_trial_end-1))
    B = sort(runif(s_ab,0,whole_trial_end-1))
    E = sort(runif(sample(2:5,1),0,trial_end-1)) %>% c(0) #1:9
    sqc=data.frame(obj = c(rep("A", length(A)), rep("B", length(B)), 
                           rep("E", length(E))), 
                   time =c(A, B, E)) %>% MyReshape()
    
    if (min(diff(sort(c(A,B,E))))<=onfor){
      next
    }
    df.md=myModRe(sqc,2)
    # df.md=df.md %>% subset(sti_id==1)
    # df.md=df.md %>% subset(sti_id==2)
    z=cor(df.md$normative,df.md$delay)
    y=cor(df.md$normative,df.md$number)
    a=which.max(df.md$normative_raw)
    if (max(df.md$normative_raw)>10^-40 && z<0.8 && y<0.8 && (a==6)){
      write.csv(sqc, paste(x,".csv",sep = ""), row.names = F, quote = F)
      savelist=c("sqc","A","B","E","df.md")
      save(list=savelist, file = paste(x,".Rda",sep = ""))
      break
    }
  }
}

wholelist=list()
for (k in 1:100){
  wholelist[[k]]=k
}
library(parallel)
mclapply(wholelist, MyGen,mc.cores = 4)

# 
p=1
p=p+1
file=paste(p,".Rda",sep = "")
file
load(file)
# df.md=myModRe(sqc,2)
cor(df.md$normative,df.md$number)
cor(df.md$normative,df.md$delay)
pic.sti.nor=MyMod6(df.md,df.md$normative)
pic.sti.fea.num=MyMod6(df.md,df.md$number)
pic.sti.fea.delay=MyMod6(df.md,df.md$delay)
pic.sti.nor %>%ggplot()+
  geom_bar(mapping=aes(x=cpn, y=ratio,fill=state),stat='identity',position="dodge",color="black")+
  geom_point(pic.sti.fea.num,mapping =aes(x=cpn, y=ratio,fill=state), stat="identity",
             position=position_dodge(.9),size=1.8,shape=5)+
  geom_point(pic.sti.fea.delay,mapping =aes(x=cpn, y=ratio,fill=state), stat="identity",
             position=position_dodge(.9),size=1.8,shape=16)+
  facet_wrap(~sti_id,nrow=3)+
  theme_bw()+
  scale_fill_manual(values=c("#E4F0EC","#EDB1A4","#6282b1"),
                    labels =  c(" Generative  "," Non-causal  "," Preventative  "))+
  scale_color_manual(values=c("black","black","black"))



myModRe<-function(sqc,section){
  dt.sti.big=as.data.frame(matrix(NA,ncol=9,nrow=0)) %>%
    setNames(c("sti_id","A_state","B_state","normative","number","delay","normative_raw","number_raw","delay_raw"))
  st_a=c("G","G","G","N","N","N","P","P","P")
  st_b=c("G","N","P","G","N","P","G","N","P")
  ru_list=c("r","u")
  # for (section in 1){
    dt.sti=as.data.frame(matrix(NA,ncol=9,nrow=9)) %>%
      setNames(c("sti_id","A_state","B_state","normative","number","delay","normative_raw","number_raw","delay_raw"))
    
    baserate=5
    sqc_raw_long=sqc
    ru_pro=ru_list[section]
    
    
    if (ru_pro=="r"){k_e=k_e_r;r_e=r_e_r;k_pe=k_pe_r;r_pe=r_pe_r;k_ge=k_ge_r;r_ge=r_ge_r}
    if (ru_pro=="u"){k_e=k_e_u;r_e=r_e_u;k_pe=k_pe_r;r_pe=r_pe_r;k_ge=k_ge_r;r_ge=r_ge_r}
    
    nor_sim_prob=MyOnlineInfer_nor(sqc_raw_long,k_e,r_e,k_pe,r_pe,k_ge,r_ge)
    fea_sim_prob=MyOnlineInfer_fea(sqc_raw_long,ru_pro,baserate)
    
    dt.sti$sti_id=section
    for (mo in 1:9){
      dt.sti$A_state[mo]=st_a[mo]
      dt.sti$B_state[mo]=st_b[mo]
      dt.sti$normative[mo]=nor_sim_prob[mo]/sum(nor_sim_prob)
      dt.sti$number[mo]=fea_sim_prob$num[mo]/sum(fea_sim_prob$num)
      dt.sti$delay[mo]=fea_sim_prob$delay[mo]/sum(fea_sim_prob$delay)
      
      dt.sti$normative_raw[mo]=nor_sim_prob[mo]
      dt.sti$number_raw[mo]=fea_sim_prob$num[mo]
      dt.sti$delay_raw[mo]=fea_sim_prob$delay[mo]
    }
    dt.sti.big=rbind(dt.sti.big,dt.sti)
  # }
  return(dt.sti.big)
}



MyMod6<- function(md_raw,md_ratio){
  
  md=as.data.frame(matrix(NA, nrow = 2*2*3, ncol = 4))%>%
    setNames(c("sti_id","cpn","state","ratio"))
  
  for (j in 1:2){
    md$cpn[((j-1)*2*3+1):(j*2*3)]=c("A","A","A","B","B","B")
    md$state[((j-1)*2*3+1):(j*2*3)]=c("G","N","P","G","N","P")
    md$sti_id[((j-1)*2*3+1):(j*2*3)]=j
    
    md$ratio[(j-1)*2*3+1]=sum(md_ratio[md_raw$A_state=="G" & md_raw$sti_id==j])
    md$ratio[(j-1)*2*3+2]=sum(md_ratio[md_raw$A_state=="N" & md_raw$sti_id==j])
    md$ratio[(j-1)*2*3+3]=sum(md_ratio[md_raw$A_state=="P" & md_raw$sti_id==j])
    md$ratio[(j-1)*2*3+4]=sum(md_ratio[md_raw$B_state=="G" & md_raw$sti_id==j])
    md$ratio[(j-1)*2*3+5]=sum(md_ratio[md_raw$B_state=="N" & md_raw$sti_id==j])
    md$ratio[(j-1)*2*3+6]=sum(md_ratio[md_raw$B_state=="P" & md_raw$sti_id==j])
  }
  return(md)
}
