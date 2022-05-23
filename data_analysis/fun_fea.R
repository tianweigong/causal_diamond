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

mas.G.r=df.e %>% subset(rel=="r") %>% subset((A_pro=="G" & cue==numcue[1]) | (B_pro=="G" & cue==numcue[2]))
mas.N.r=df.e %>% subset(rel=="r") %>% subset((A_pro=="N" & cue==numcue[1]) | (B_pro=="N" & cue==numcue[2]))
mas.P.r=df.e %>% subset(rel=="r") %>% subset((A_pro=="P" & cue==numcue[1]) | (B_pro=="P" & cue==numcue[2]))
mas.G.u=df.e %>% subset(rel=="u") %>% subset((A_pro=="G" & cue==numcue[1]) | (B_pro=="G" & cue==numcue[2]))
mas.N.u=df.e %>% subset(rel=="u") %>% subset((A_pro=="N" & cue==numcue[1]) | (B_pro=="N" & cue==numcue[2]))
mas.P.u=df.e %>% subset(rel=="u") %>% subset((A_pro=="P" & cue==numcue[1]) | (B_pro=="P" & cue==numcue[2]))

s_ab=3
whole_trial_end=20

myStatsSummary <- function(sqc_s,df){
  
  sqc_ab=sqc_s %>% subset(obj %in% c("A","B"))
  
  if (segmod=='intervention'){
    #num/delay intervention
    a_count=0;b_count=0
    for (i in 1:(nrow(sqc_ab)-1)){
      sqc_sub=sqc_s %>% subset(time>sqc_ab$time[i] & time<sqc_ab$time[i+1] & obj=="E")
      if (sqc_ab$obj[i]=="A"){
        a_count=a_count+1
        df$A_num[a_count]=nrow(sqc_sub)
        if (nrow(sqc_sub)>0){
          df$A_delay[a_count]=min(sqc_sub$time)-sqc_ab$time[i]
        }else{
          df$A_delay[a_count]=sqc_ab$time[i+1]-sqc_ab$time[i]
        }
      }
      if (sqc_ab$obj[i]=="B"){
        b_count=b_count+1
        df$B_num[b_count]=nrow(sqc_sub)
        if (nrow(sqc_sub)>0){
          df$B_delay[b_count]=min(sqc_sub$time)-sqc_ab$time[i]
        }else{
          df$B_delay[b_count]=sqc_ab$time[i+1]-sqc_ab$time[i]
        }
      }
    }
    i=nrow(sqc_ab)
    sqc_sub=sqc_s %>% subset(time>sqc_ab$time[i] & obj=="E")
    if(sqc_ab$obj[nrow(sqc_ab)]=="A"){
      df$A_last[s_ab]=1
      df$A_num[s_ab]=nrow(sqc_sub)
      if (nrow(sqc_sub)>0){
        df$A_delay[s_ab]=min(sqc_sub$time)-sqc_ab$time[i]
      }else{
        df$A_delay[s_ab]=whole_trial_end-sqc_ab$time[i]
      }
    }
    if(sqc_ab$obj[nrow(sqc_ab)]=="B"){
      df$B_last[s_ab]=1
      df$B_num[s_ab]=nrow(sqc_sub)
      if (nrow(sqc_sub)>0){
        df$B_delay[s_ab]=min(sqc_sub$time)-sqc_ab$time[i]
      }else{
        df$B_delay[s_ab]=whole_trial_end-sqc_ab$time[i]
      }
    }
  }else if (segmod=="window"){
    #num/delay window
    a_count=0;b_count=0
    for (i in 1:(nrow(sqc_ab))){
      win_end=sqc_ab$time[i]+win_len
      sqc_sub=sqc_s %>% subset(time>sqc_ab$time[i] & time<win_end & obj=="E")
      if (sqc_ab$obj[i]=="A"){
        a_count=a_count+1
        df$A_num[a_count]=nrow(sqc_sub)
        if(win_end>whole_trial_end){
          df$A_short[a_count]=1
        }
        if (nrow(sqc_sub)>0){
          df$A_delay[a_count]=min(sqc_sub$time)-sqc_ab$time[i]
        }else if (df$A_short[a_count]){
          df$A_delay[a_count]=whole_trial_end-sqc_ab$time[i]
        }else{
          df$A_delay[a_count]=win_len
        }
      }
      if (sqc_ab$obj[i]=="B"){
        b_count=b_count+1
        df$B_num[b_count]=nrow(sqc_sub)
        if(win_end>whole_trial_end){
          df$B_short[b_count]=1
        }
        if (nrow(sqc_sub)>0){
          df$B_delay[b_count]=min(sqc_sub$time)-sqc_ab$time[i]
        }else if (df$B_short[b_count]){
          df$B_delay[b_count]=whole_trial_end-sqc_ab$time[i]
        }else{
          df$B_delay[b_count]=win_len
        }
      }
    }
  }
  return(df)
}

myFeatureBased <-function(sqc,rl){
  df.sqc=as.data.frame(matrix(NA,ncol=4,nrow=s_ab)) %>%
    setNames(c("A_delay","B_delay","A_num","B_num"))
  df.sqc=df.sqc %>% mutate(A_last=0,B_last=0,A_short=0,B_short=0)
  
  sqc$total_idx=c(1:nrow(sqc))
  df.sqc=myStatsSummary(sqc,df.sqc)
  
  df.compare=as.data.frame(matrix(NA,ncol=6,nrow=9)) %>%
    setNames(c("A_pro","B_pro","delay","num"))
  df.compare$A_pro=c("G","G","G","N","N","N","P","P","P")
  df.compare$B_pro=c("G","N","P","G","N","P","G","N","P")
  
  cue_list=c("delay","num")
  # dens.a=dens.b=coun.a=coun.b=matrix(data=NA,nrow=3,ncol=9)
  
  if (rl=="r"){
    dis.list=c("dis.G.r","dis.N.r","dis.P.r")
    mas.list=c("mas.G.r","mas.N.r","mas.P.r")
  }else{
    dis.list=c("dis.G.u","dis.N.u","dis.P.u")
    mas.list=c("mas.G.u","mas.N.u","mas.P.u")
  }
  
  Acue.delay=Bcue.delay=Acue.num=Bcue.num=matrix(NA, nrow = 3, ncol = s_ab, dimnames = list(c("G","N","P"), c("I1","I2","I3")))
  
  #delay G N P
  for (i in 1:3){
    dens=get(dis.list[i])
    cumu=CDF(dens)
    for (j in 1:s_ab){
      Acue.delay[i,j]=MyDelayApprox(df.sqc$A_delay[j],df.sqc$A_num[j],dens,cumu)
      Bcue.delay[i,j]=MyDelayApprox(df.sqc$B_delay[j],df.sqc$B_num[j],dens,cumu)
    }
  }
  
  #num G N P
  for (i in 1:3){
    mas=get(mas.list[i])
    for (j in 1:s_ab){
      if (segmod=="intervention"){
        Acue.num[i,j]=MyNumApprox(df.sqc$A_num[j],df.sqc$A_last[j],mas)
        Bcue.num[i,j]=MyNumApprox(df.sqc$B_num[j],df.sqc$B_last[j],mas)
      }else if (segmod=="window"){
        Acue.num[i,j]=MyNumApprox(df.sqc$A_num[j],df.sqc$A_short[j],mas)
        Bcue.num[i,j]=MyNumApprox(df.sqc$B_num[j],df.sqc$B_short[j],mas)
      }
    }
  }
  
  #normalisation
  for (j in 1:3){
    Acue.delay[,j]=Acue.delay[,j]/sum(Acue.delay[,j])
    Bcue.delay[,j]=Bcue.delay[,j]/sum(Bcue.delay[,j])
    Acue.num[,j]=Acue.num[,j]/sum(Acue.num[,j])
    Bcue.num[,j]=Bcue.num[,j]/sum(Bcue.num[,j])
  }
  
  for (i in 1:3){
    for (j in 1:3){
      df.compare[(i-1)*3+j,"delay"]=prod(c(Acue.delay[i,],Bcue.delay[j,]))
      df.compare[(i-1)*3+j,"num"]=prod(c(Acue.num[i,],Bcue.num[j,]))
    }
  }
  
  for (i in 1:length(cue_list)){
    df.compare[,cue_list[i]]=df.compare[,cue_list[i]]/sum(df.compare[,cue_list[i]],na.rm = T)
  }
  #Simulation
  sim_prob=df.compare %>% subset(select = c(delay,num))
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

MyMdFea <- function(sqc_raw_long,rl){
  sqc_raw=sqc_raw_long
  sqc=MySqc(sqc_raw)
  sim_prob=myFeatureBased(sqc,rl)
  return(sim_prob)
}
