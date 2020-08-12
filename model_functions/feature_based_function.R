library(dplyr)
library(tidyr)
library(spatstat)
load("df.expect.Rda")
df.e=df.expect %>% gather(ori, val, A_delay1:B_numi3) %>% na.omit()
df.e$cue=substr(df.e$ori,1,nchar(df.e$ori)-1)
win_len=4
# win_len=5
df.G.r=df.e %>% subset(rel=="r") %>% subset((A_pro=="G" & cue=="A_delay") | (B_pro=="G" & cue=="B_delay"))
dis.G.r=density(df.G.r$val)
df.N.r=df.e %>% subset(rel=="r") %>% subset((A_pro=="N" & cue=="A_delay") | (B_pro=="N" & cue=="B_delay"))
dis.N.r=density(df.N.r$val)
df.P.r=df.e %>% subset(rel=="r") %>% subset((A_pro=="P" & cue=="A_delay") | (B_pro=="P" & cue=="B_delay"))
dis.P.r=density(df.P.r$val)
df.G.u=df.e %>% subset(rel=="u") %>% subset((A_pro=="G" & cue=="A_delay") | (B_pro=="G" & cue=="B_delay"))
dis.G.u=density(df.G.u$val)
df.N.u=df.e %>% subset(rel=="u") %>% subset((A_pro=="N" & cue=="A_delay") | (B_pro=="N" & cue=="B_delay"))
dis.N.u=density(df.N.u$val)
df.P.u=df.e %>% subset(rel=="u") %>% subset((A_pro=="P" & cue=="A_delay") | (B_pro=="P" & cue=="B_delay"))
dis.P.u=density(df.P.u$val)

masi.G.r=df.e %>% subset(rel=="r") %>% subset((A_pro=="G" & cue=="A_numi") | (B_pro=="G" & cue=="B_numi"))
masi.N.r=df.e %>% subset(rel=="r") %>% subset((A_pro=="N" & cue=="A_numi") | (B_pro=="N" & cue=="B_numi"))
masi.P.r=df.e %>% subset(rel=="r") %>% subset((A_pro=="P" & cue=="A_numi") | (B_pro=="P" & cue=="B_numi"))
masi.G.u=df.e %>% subset(rel=="u") %>% subset((A_pro=="G" & cue=="A_numi") | (B_pro=="G" & cue=="B_numi"))
masi.N.u=df.e %>% subset(rel=="u") %>% subset((A_pro=="N" & cue=="A_numi") | (B_pro=="N" & cue=="B_numi"))
masi.P.u=df.e %>% subset(rel=="u") %>% subset((A_pro=="P" & cue=="A_numi") | (B_pro=="P" & cue=="B_numi"))

masw.G.r=df.e %>% subset(rel=="r") %>% subset((A_pro=="G" & cue=="A_numw") | (B_pro=="G" & cue=="B_numw"))
masw.N.r=df.e %>% subset(rel=="r") %>% subset((A_pro=="N" & cue=="A_numw") | (B_pro=="N" & cue=="B_numw"))
masw.P.r=df.e %>% subset(rel=="r") %>% subset((A_pro=="P" & cue=="A_numw") | (B_pro=="P" & cue=="B_numw"))
masw.G.u=df.e %>% subset(rel=="u") %>% subset((A_pro=="G" & cue=="A_numw") | (B_pro=="G" & cue=="B_numw"))
masw.N.u=df.e %>% subset(rel=="u") %>% subset((A_pro=="N" & cue=="A_numw") | (B_pro=="N" & cue=="B_numw"))
masw.P.u=df.e %>% subset(rel=="u") %>% subset((A_pro=="P" & cue=="A_numw") | (B_pro=="P" & cue=="B_numw"))


###for check
myc=c(approx(dis.G.r$x,dis.G.r$y,xout=2.5,rule=2)$y,
approx(dis.N.r$x,dis.N.r$y,xout=2.5,rule=2)$y,
approx(dis.P.r$x,dis.P.r$y,xout=2.5,rule=2)$y)
myc/sum(myc)

myc=c(approx(dis.G.u$x,dis.G.u$y,xout=2,rule=2)$y,
      approx(dis.N.u$x,dis.N.u$y,xout=2,rule=2)$y,
      approx(dis.P.u$x,dis.P.u$y,xout=2,rule=2)$y)
myc/sum(myc)

myc=c(approx(dis.G.r$x,dis.G.r$y,xout=2,rule=2)$y,
      approx(dis.N.r$x,dis.N.r$y,xout=2,rule=2)$y,
      approx(dis.P.r$x,dis.P.r$y,xout=2,rule=2)$y)
myc/sum(myc)

myc=c(sum(masw.G.r$val==2)/nrow(masw.G.r),
      sum(masw.N.r$val==2)/nrow(masw.N.r),
      sum(masw.P.r$val==2)/nrow(masw.P.r))
myc/sum(myc)
###

s_ab=3
whole_trial_end=20

myStatsSummary <- function(sqc_s,df.sqc=0){
  
  A_delayw=rep(NA,s_ab)
  B_delayw=rep(NA,s_ab)
  A_numw=rep(NA,s_ab)
  B_numw=rep(NA,s_ab)
  
  A_delayi=rep(NA,s_ab)
  B_delayi=rep(NA,s_ab)
  A_numi=rep(NA,s_ab)
  B_numi=rep(NA,s_ab)
  
  A_last=rep(0,s_ab)
  B_last=rep(0,s_ab)
  A_short=rep(0,s_ab)
  B_short=rep(0,s_ab)
  
  sqc_ab=sqc_s %>% subset(obj %in% c("A","B"))
  
  #num/delay intervention
  a_count=0
  b_count=0
  for (i in 1:(nrow(sqc_ab)-1)){
    sqc_sub=sqc_s %>% subset(time>sqc_ab$time[i] & time<sqc_ab$time[i+1] & obj=="E")
    if (sqc_ab$obj[i]=="A"){
      a_count=a_count+1
      A_numi[a_count]=nrow(sqc_sub)
      if (nrow(sqc_sub)>0){
        A_delayi[a_count]=min(sqc_sub$time)-sqc_ab$time[i]
      }else{
        A_delayi[a_count]=sqc_ab$time[i+1]-sqc_ab$time[i]
      }
    }
    if (sqc_ab$obj[i]=="B"){
      b_count=b_count+1
      B_numi[b_count]=nrow(sqc_sub)
      if (nrow(sqc_sub)>0){
        B_delayi[b_count]=min(sqc_sub$time)-sqc_ab$time[i]
      }else{
        B_delayi[b_count]=sqc_ab$time[i+1]-sqc_ab$time[i]
      }
    }
  }
  i=nrow(sqc_ab)
  sqc_sub=sqc_s %>% subset(time>sqc_ab$time[i] & obj=="E")
  if(sqc_ab$obj[nrow(sqc_ab)]=="A"){
    A_last[s_ab]=1
    A_numi[s_ab]=nrow(sqc_sub)
    
    if (nrow(sqc_sub)>0){
      A_delayi[s_ab]=min(sqc_sub$time)-sqc_ab$time[i]
    }else{
      A_delayi[s_ab]=whole_trial_end-sqc_ab$time[i]
    }
  }
  if(sqc_ab$obj[nrow(sqc_ab)]=="B"){
    B_last[s_ab]=1
    B_numi[s_ab]=nrow(sqc_sub)
    
    if (nrow(sqc_sub)>0){
      B_delayi[s_ab]=min(sqc_sub$time)-sqc_ab$time[i]
    }else{
      B_delayi[s_ab]=whole_trial_end-sqc_ab$time[i]
    }
  }
  
  #num/delay window
  a_count=0
  b_count=0
  for (i in 1:(nrow(sqc_ab))){
    win_end=sqc_ab$time[i]+win_len
    sqc_sub=sqc_s %>% subset(time>sqc_ab$time[i] & time<win_end & obj=="E")
    if (sqc_ab$obj[i]=="A"){
      a_count=a_count+1
      A_numw[a_count]=nrow(sqc_sub)
      if(win_end>whole_trial_end){
        A_short[a_count]=1
      }
      if (nrow(sqc_sub)>0){
        A_delayw[a_count]=min(sqc_sub$time)-sqc_ab$time[i]
      }else if (A_short[a_count]){
        A_delayw[a_count]=whole_trial_end-sqc_ab$time[i]
      }else{
        A_delayw[a_count]=win_len
      }
    }
    if (sqc_ab$obj[i]=="B"){
      b_count=b_count+1
      B_numw[b_count]=nrow(sqc_sub)
      if(win_end>whole_trial_end){
        B_short[b_count]=1
      }
      if (nrow(sqc_sub)>0){
        B_delayw[b_count]=min(sqc_sub$time)-sqc_ab$time[i]
      }else if (B_short[b_count]){
        B_delayw[b_count]=whole_trial_end-sqc_ab$time[i]
      }else{
        B_delayw[b_count]=win_len
      }
    }
  }


  df.sqc$A_delayw<-A_delayw
  df.sqc$B_delayw<-B_delayw
  
  df.sqc$A_numw<-A_numw
  df.sqc$B_numw<-B_numw
  
  df.sqc$A_delayi<-A_delayi
  df.sqc$B_delayi<-B_delayi
  
  df.sqc$A_numi<-A_numi
  df.sqc$B_numi<-B_numi
  
  df.sqc$A_short<-A_short
  df.sqc$B_short<-B_short
  
  df.sqc$A_last<-A_last
  df.sqc$B_last<-B_last
  
  return(df.sqc)
}

myFeatureBased <-function(sqc,rl,br){
  
  df.sqc=as.data.frame(matrix(NA,ncol=12,nrow=3)) %>%
    setNames(c("A_delayw","B_delayw","A_numw","B_numw",
               "A_delayi","B_delayi","A_numi","B_numi",
               "A_last","B_last","A_short","B_short"))
  sqc$total_idx=c(1:nrow(sqc))
  df.sqc=myStatsSummary(sqc,df.sqc)
  
  df.compare=as.data.frame(matrix(NA,ncol=6,nrow=9)) %>%
    setNames(c("A_pro","B_pro","delayi","numi","delayw","numw"))
  df.compare$A_pro=c("G","G","G","N","N","N","P","P","P")
  df.compare$B_pro=c("G","N","P","G","N","P","G","N","P")
  
  cue_list=c("delayi","numi","delayw","numw")
  # dens.a=dens.b=coun.a=coun.b=matrix(data=NA,nrow=3,ncol=9)
  
  if (rl=="r"){
    dis.list=c("dis.G.r","dis.N.r","dis.P.r")
    masi.list=c("masi.G.r","masi.N.r","masi.P.r")
    masw.list=c("masw.G.r","masw.N.r","masw.P.r")
  }else{
    dis.list=c("dis.G.u","dis.N.u","dis.P.u")
    masi.list=c("masi.G.u","masi.N.u","masi.P.u")
    masw.list=c("masw.G.u","masw.N.u","masw.P.u")
  }
  
  Acue.delayi=Bcue.delayi=Acue.numi=Bcue.numi=matrix(NA, nrow = 3, ncol = s_ab, dimnames = list(c("G","N","P"), c("I1","I2","I3")))
  Acue.delayw=Bcue.delayw=Acue.numw=Bcue.numw=matrix(NA, nrow = 3, ncol = s_ab, dimnames = list(c("G","N","P"), c("I1","I2","I3")))
  
  #delay G N P
  for (i in 1:3){
    dens=get(dis.list[i])
    cumu=CDF(dens)
    for (j in 1:s_ab){
      Acue.delayi[i,j]=MyDelayApprox(df.sqc$A_delayi[j],df.sqc$A_numi[j],dens,cumu)
      Acue.delayw[i,j]=MyDelayApprox(df.sqc$A_delayw[j],df.sqc$A_numw[j],dens,cumu)
      Bcue.delayi[i,j]=MyDelayApprox(df.sqc$B_delayi[j],df.sqc$B_numi[j],dens,cumu)
      Bcue.delayw[i,j]=MyDelayApprox(df.sqc$B_delayw[j],df.sqc$B_numw[j],dens,cumu)
    }
  }
  
  #num G N P
  for (i in 1:3){
    masi=get(masi.list[i])
    masw=get(masw.list[i])
    for (j in 1:s_ab){
      Acue.numi[i,j]=MyNumApprox(df.sqc$A_numi[j],df.sqc$A_last[j],masi)
      Acue.numw[i,j]=MyNumApprox(df.sqc$A_numw[j],df.sqc$A_short[j],masw)
      Bcue.numi[i,j]=MyNumApprox(df.sqc$B_numi[j],df.sqc$B_last[j],masi)
      Bcue.numw[i,j]=MyNumApprox(df.sqc$B_numw[j],df.sqc$B_short[j],masw)
    }
  }
  
  #normalisation
  for (j in 1:3){
    Acue.delayi[,j]=Acue.delayi[,j]/sum(Acue.delayi[,j])
    Bcue.delayi[,j]=Bcue.delayi[,j]/sum(Bcue.delayi[,j])
    Acue.numi[,j]=Acue.numi[,j]/sum(Acue.numi[,j])
    Bcue.numi[,j]=Bcue.numi[,j]/sum(Bcue.numi[,j])
    
    Acue.delayw[,j]=Acue.delayw[,j]/sum(Acue.delayw[,j])
    Bcue.delayw[,j]=Bcue.delayw[,j]/sum(Bcue.delayw[,j])
    Acue.numw[,j]=Acue.numw[,j]/sum(Acue.numw[,j])
    Bcue.numw[,j]=Bcue.numw[,j]/sum(Bcue.numw[,j])
  }
  
  for (i in 1:3){
    for (j in 1:3){
      df.compare[(i-1)*3+j,"delayi"]=prod(c(Acue.delayi[i,],Bcue.delayi[j,]))
      df.compare[(i-1)*3+j,"delayw"]=prod(c(Acue.delayw[i,],Bcue.delayw[j,]))
      
      df.compare[(i-1)*3+j,"numi"]=prod(c(Acue.numi[i,],Bcue.numi[j,]))
      df.compare[(i-1)*3+j,"numw"]=prod(c(Acue.numw[i,],Bcue.numw[j,]))
    }
  }
  
  for (i in 1:length(cue_list)){
    df.compare[,cue_list[i]]=df.compare[,cue_list[i]]/sum(df.compare[,cue_list[i]],na.rm = T)
  }
  #Simulation
  sim_prob=df.compare %>% subset(select = c(delayi, numi,delayw,numw))
  return(sim_prob)
}

MyDelayApprox<- function(delay,c_num,dens,cumu){
  if (c_num>0){
    res=approx(dens$x,dens$y,xout=delay,rule=2)$y
  }else{
    res=1-cumu(delay)
  }
  return(res)
}

MyNumApprox<- function(num,bound,df){
  if (bound){
    res=sum(df$val>=num)/nrow(df)
  }else{
    res=sum(df$val==num)/nrow(df)
  }
  return(res)
}


MySqc <-function(sqc_raw){
  sqc_raw_sub_a=subset(sqc_raw,obj=="A")
  if (nrow(sqc_raw_sub_a)>0){
    sqc_raw_sub_a$obj_idx=seq(1:nrow(sqc_raw_sub_a))
  }
  sqc_raw_sub_b=subset(sqc_raw,obj=="B")
  if (nrow(sqc_raw_sub_b)>0){
    sqc_raw_sub_b$obj_idx=seq(1:nrow(sqc_raw_sub_b))
  }
  sqc_raw_sub_ab=rbind(sqc_raw_sub_a,sqc_raw_sub_b)
  if (nrow(sqc_raw_sub_ab)>0){
    sqc_raw_sub_ab=sqc_raw_sub_ab[order(sqc_raw_sub_ab$time),]
    sqc_raw_sub_ab$ab_idx=seq(1:nrow(sqc_raw_sub_ab))
  }
  sqc_raw_sub_e=subset(sqc_raw,obj=="E")
  if(nrow(sqc_raw_sub_e)>0){
    sqc_raw_sub_e$obj_idx=seq(1:nrow(sqc_raw_sub_e))
    sqc_raw_sub_e$ab_idx=rep(0,nrow(sqc_raw_sub_e))
  }
  sqc_raw=rbind(sqc_raw_sub_ab,sqc_raw_sub_e)
  sqc_raw=sqc_raw[order(sqc_raw$time),]
  sqc_raw$total_idx=c(1:nrow(sqc_raw))
  return(sqc_raw)
}

MyOnlineInfer_fea <- function(sqc_raw_long,rl,br){
  sqc_raw=sqc_raw_long
  sqc=MySqc(sqc_raw)
  sim_prob=myFeatureBased(sqc,rl,br)
  return(sim_prob)
}