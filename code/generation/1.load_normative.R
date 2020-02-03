#+ General settings, echo = FALSE, results = 'hide' -------------------
knitr::opts_chunk$set(warning=FALSE, message=FALSE)
#+ load packages -------------------
#' # load packages
library(tidyr)
library(dplyr)
library(ggplot2)
library(matrixStats)
# rm(list=ls())

#+ set parameters -------------------
#' # set parameters
#' please make sure that the parameters are identical to stimuli generation file.
k_e = 100
r_e = 20 #e->e: m=5,var=0.25

k_pe = 36
r_pe = 12 #preventative p->e: m=3,var=0.25
k_ge = 9
r_ge = 6  #generative g->e: m=1.5,var=0.25

large_time=100 # a second far beyond trail_end
sampling_point=200 #for one generative one preventative

#+ load functions-------------------
#' # load functions
MyLeaf <- function(x,pos) {
  pos=pos+1
  if (pos>length(sig_len)){
    p<<-p+1
    all_path_mtx<<-rbind(all_path_mtx, x)
  }else{
    choice=unique(c(setdiff(seq(1:sig_len[pos]),x),1)) 
    for(i in choice){
      x[pos]=i
      MyLeaf(x,pos)
    }
  }
}

MyValidPath<-function(){
  valid_path=as.data.frame(matrix(NA,ncol=4,nrow=0)) %>%
    setNames(c("obj","cause","cause_idx","path"))
  p=1
  for (i in 1:nrow(all_path_mtx)){
    cdd_path=as.data.frame(matrix(NA,ncol=3,nrow=0)) %>%
      setNames(c("obj","cause","cause_idx"))
    for (j in unique(sig_path$obj)){
      cdd_path[nrow(cdd_path)+1,]=sig_path[sig_path$obj==j,][all_path_mtx[i,j-1],]
    }
    cdd_path$path=p
    valid_path=rbind(valid_path,cdd_path)
    p=p+1
  }
  return(valid_path)
}

MyT0 <-function(j){
  if (cdd_path$cause[j]!="E"){
    t0=sqc$time[sqc$obj==cdd_path$cause[j] & sqc$obj_idx==cdd_path$cause_idx[j]]#caution!!!
  }else{
    t0=sqc$time[sqc$obj==cdd_path$cause[j] & sqc$total_idx ==max(e_index[e_index<elist[cdd_path$obj[j]-1]])]#caution!!!
  }
  return(t0)
}

MyPreventIdx <-function(){
  p_index<<-which(sqc$obj %in% plist)
  pe_time<<-c()
  for (q in p_index){
    pe<<-q
    while (1){
      pe<<-pe+1
      if (pe>nrow(sqc)){
        pe_time<<-c(pe_time,large_time)
        break
      }
      if (sqc$obj[pe]=="E"){
        pe_time<<-c(pe_time,sqc$time[pe])
        break
      }
    }
  }
}

MyHidden <- function(){
  #for E->->E
  #no hidden baserate
  prob_sub_e=dgamma(t1-t0, shape=k_e, rate=r_e)
  #hidden baserate
  #using approximation approach
  p_between=sqc %>% subset(time>t0 & time<t1 & obj %in% plist)
  if (nrow(p_between)>0){
    Time=seq(t0,t1, length.out = sampling_point)
    df.sample=data.frame(Time)
    if (length(plist)>1){
      pe_cut_index=p_between$ab_idx
    }else{
      pe_cut_index=p_between$obj_idx
    }
    for (m in 1:nrow(p_between)){
      p_time=p_between$time[m]
      col=paste(p_between$obj[m],p_between$obj_idx[m])
      df.sample[,col]=pgamma(Time-p_time,shape=k_pe, rate=r_pe) #cannot prevent
      df.sample[df.sample$Time<p_time,col]=1 #before P happens, definitely cannot prevent 
      df.sample[df.sample$Time>pe_time[pe_cut_index[m]],col]=1 #After Another E happens, definitely cannot prevent 
    }
    df.sample.dup=df.sample
    df.sample.dup$Time=NULL
    df.sample[,"prevent"]=(1-rowProds(data.matrix(df.sample.dup))) # noisy-or, 1 minus the product all that cannot prevent
    #one-hidden effect
    hidden1=dgamma(Time-t0,shape=k_e, rate=r_e)*df.sample$prevent
    prob_segment=max(summary(ecdf(hidden1)))*dgamma(t1-t0,shape=k_e*2, rate=r_e)
    prob_sub_e=1-(1-prob_sub_e)*(1-prob_segment)
    
    hidden1=dgamma(Time-t0,shape=k_e, rate=r_e)*df.sample$prevent
    hidden2=dgamma(Time-t0,shape=k_e*2, rate=r_e)*df.sample$prevent
    prob_segment=max(summary(ecdf(hidden1)))*max(summary(ecdf(hidden2)))*dgamma(t1-t0,shape=k_e*3, rate=r_e)
    prob_sub_e=1-(1-prob_sub_e)*(1-prob_segment)
    
    hidden1=dgamma(Time-t0,shape=k_e, rate=r_e)*df.sample$prevent
    hidden2=dgamma(Time-t0,shape=k_e*2, rate=r_e)*df.sample$prevent
    hidden3=dgamma(Time-t0,shape=k_e*3, rate=r_e)*df.sample$prevent
    prob_segment=max(summary(ecdf(hidden1)))*max(summary(ecdf(hidden2)))*
      max(summary(ecdf(hidden3)))*dgamma(t1-t0,shape=k_e*4, rate=r_e)
    prob_sub_e=1-(1-prob_sub_e)*(1-prob_segment)
    
    hidden1=dgamma(Time-t0,shape=k_e, rate=r_e)*df.sample$prevent
    hidden2=dgamma(Time-t0,shape=k_e*2, rate=r_e)*df.sample$prevent
    hidden3=dgamma(Time-t0,shape=k_e*3, rate=r_e)*df.sample$prevent
    hidden4=dgamma(Time-t0,shape=k_e*4, rate=r_e)*df.sample$prevent
    prob_segment=max(summary(ecdf(hidden1)))*max(summary(ecdf(hidden2)))*
      max(summary(ecdf(hidden3)))*max(summary(ecdf(hidden4)))*dgamma(t1-t0,shape=k_e*5, rate=r_e)
    prob_sub_e=1-(1-prob_sub_e)*(1-prob_segment)
    
  }
  return(prob_sub_e)
}

MyEndHidden <- function(){
  prob_sub=1
  j=max(e_index)
  e_time=sqc$time[j]
  #no hidden baserate
  prob_sub=1-pgamma(trial_end-e_time, shape=k_e, rate=r_e)
  #hidden baserate
  #using approximation approach
  p_between=sqc %>% subset(time>e_time & time<trial_end & obj %in% plist)
  if (nrow(p_between)>0){
    Time=seq(e_time,trial_end, length.out = sampling_point)
    df.sample=data.frame(Time)
    if (length(plist)>1){
      pe_cut_index=p_between$ab_idx
    }else{
      pe_cut_index=p_between$obj_idx
    }
    for (m in 1:nrow(p_between)){
      p_time=p_between$time[m]
      col=paste(p_between$obj[m],p_between$obj_idx[m])
      df.sample[,col]=pgamma(Time-p_time,shape=k_pe, rate=r_pe) #cannot prevent
      df.sample[df.sample$Time<p_time,col]=1 #before A happens, definitely cannot prevent 
      df.sample[df.sample$Time>pe_time[pe_cut_index[m]],col]=1 #After Another E happens, definitely cannot prevent 
    }
    df.sample.dup=df.sample
    df.sample.dup$Time=NULL
    df.sample[,"prevent"]=(1-rowProds(data.matrix(df.sample.dup))) # noisy-or, 1 minus the product all that cannot prevent
    #one-hidden effect
    hidden1=dgamma(Time-e_time,shape=k_e, rate=r_e)*df.sample$prevent
    prob_segment=max(summary(ecdf(hidden1)))*(1-pgamma(trial_end-e_time,shape=k_e*2, rate=r_e))
    prob_sub=1-(1-prob_sub)*(1-prob_segment)
    
    hidden1=dgamma(Time-e_time,shape=k_e, rate=r_e)*df.sample$prevent
    hidden2=dgamma(Time-e_time,shape=k_e*2, rate=r_e)*df.sample$prevent
    prob_segment=max(summary(ecdf(hidden1)))*max(summary(ecdf(hidden2)))*
      dgamma(trial_end-e_time,shape=k_e*3, rate=r_e)
    prob_sub=1-(1-prob_sub)*(1-prob_segment)
    
    hidden1=dgamma(Time-e_time,shape=k_e, rate=r_e)*df.sample$prevent
    hidden2=dgamma(Time-e_time,shape=k_e*2, rate=r_e)*df.sample$prevent
    hidden3=dgamma(Time-e_time,shape=k_e*3, rate=r_e)*df.sample$prevent
    prob_segment=max(summary(ecdf(hidden1)))*max(summary(ecdf(hidden2)))*
      max(summary(ecdf(hidden3)))*dgamma(trial_end-e_time,shape=k_e*4, rate=r_e)
    prob_sub=1-(1-prob_sub)*(1-prob_segment)
    
    hidden1=dgamma(Time-e_time,shape=k_e, rate=r_e)*df.sample$prevent
    hidden2=dgamma(Time-e_time,shape=k_e*2, rate=r_e)*df.sample$prevent
    hidden3=dgamma(Time-e_time,shape=k_e*3, rate=r_e)*df.sample$prevent
    hidden4=dgamma(Time-e_time,shape=k_e*4, rate=r_e)*df.sample$prevent
    prob_segment=max(summary(ecdf(hidden1)))*max(summary(ecdf(hidden2)))*
      max(summary(ecdf(hidden3)))*max(summary(ecdf(hidden4)))*dgamma(trial_end-e_time,shape=k_e*5, rate=r_e)
    prob_sub=1-(1-prob_sub)*(1-prob_segment)
  }
  return(prob_sub)
}

MyPathIni <- function(){ #glist,plist
  sqc<<-sqc_raw
  #setup path
  elist<<-which(sqc$obj=="E")[-1]
  aflag=1
  bflag=1
  p=2
  sig_len_cum<<-c()
  sig_path<<-as.data.frame(matrix(NA,ncol=3,nrow=0)) %>%
    setNames(c("obj","cause","cause_idx"))
  
  for (i in elist){
    sig_path[nrow(sig_path)+1,]<<-c(p,"E",1)
    
    for (j in 1:i){
      if (sqc$obj[j]=="A" && "A" %in% glist){
        sig_path[nrow(sig_path)+1,]<<-c(p,"A",aflag)
        aflag=aflag+1
      }

      if (sqc$obj[j]=="B" && "B" %in% glist){
        sig_path[nrow(sig_path)+1,]<<-c(p,"B",bflag)
        bflag=bflag+1
      }
    }
    sig_len_cum<<-c(sig_len_cum,nrow(sig_path))
    aflag=1
    bflag=1
    p=p+1
  }
  sig_len<<-c(sig_len_cum[1],diff(sig_len_cum))
  if (nrow(sig_path)){
    sig_path$obj<<-as.numeric(sig_path$obj)
    sig_path$cause_idx<<-as.numeric(sig_path$cause_idx)
    #get valid_path
    all_path_mtx<<-matrix(NA,ncol=length(sig_len),nrow=0)
    p<<-0
    MyLeaf(c(1),0)
    valid_path<<-MyValidPath()
  }else{
    valid_path<<-as.data.frame(matrix(0,ncol=4,nrow=1)) %>%
      setNames(c("obj","cause","cause_idx","path"))
  }

}

MyActual <- function(){ #glist,plist
  MyPreventIdx()
  whole_prob<<-0
  prob_each_path<<-c()
  #begin to calculate the probability for each path
  for (i in unique(valid_path$path)){
    cdd_path<<-valid_path[valid_path$path==i,]
    prob<<-1
    e_index<<-c(1,which(sqc$obj=="E" & sqc$obj_idx %in% cdd_path$obj[cdd_path$cause=="E"]))
    #principle 1: generative causes
    prob_sub<<-1
    if (nrow(cdd_path)){
      for (j in 1:nrow(cdd_path)){
        t1<<-sqc$time[sqc$obj=="E" & sqc$obj_idx==cdd_path$obj[j]]
        t0<<-MyT0(j)
        
        if (cdd_path$cause[j] %in% c("A","B")){
          prob_sub<<-dgamma(t1-t0, shape=k_ge, rate=r_ge)
        }
        if (cdd_path$cause[j]=="E"){
          prob_sub<<-MyHidden()
        }
        prob<<-prob*prob_sub
      }
      #principle 2: preventative causes
      prob_sub<<-1
      for (j in elist){
        for (m in p_index[p_index<j]){
          t_p<<-sqc$time[j]-sqc$time[m]
          if (m== p_index[1]){
            prob_sub<<-pgamma(t_p, shape=k_pe, rate=r_pe)
          }else{
            prob_sub<<-prob_sub*pgamma(t_p, shape=k_pe, rate=r_pe)
          }
        }
        prob<<-prob*prob_sub
      }
    }
    #principle 3: give explanation for inactivation
    g_index<<-which(sqc$obj %in% glist)
    p_index<<-which(sqc$obj %in% plist)
    prob_sub<<-1
    for (m in glist){
      cdd_index<<-which(sqc$obj==m)
      for (j in cdd_index){
        cdd_time<<-sqc$time[j]
        if (sum(cdd_path$cause==m & cdd_path$cause_idx==sqc$obj_idx[j])==0){
          prob_sub_g<<-1-pgamma(trial_end-cdd_time, shape=k_ge, rate=r_ge)
          for (q in p_index){
            if ("E" %in% sqc$obj[q:j] && q<j){ #given principle 1
              next
            }
            Time<<-seq(cdd_time,pe_time[which(p_index==q)], length.out = sampling_point)
            p_time<<-sqc$time[q]
            df.sample<<-data.frame(Time)
            df.sample[,"no_prevent"]<<-pgamma(Time-p_time,shape=k_pe, rate=r_pe) #cannot prevent
            df.sample[df.sample$Time<p_time,"no_prevent"]<<-1 #before A happens, definitely cannot prevent 
            prevent_prob <<- (dgamma(Time-cdd_time, shape=k_ge, rate=r_ge))*(1-df.sample$`no_prevent`)
            prob_sub_g<<-1-(1-prob_sub_g)*(1-max(summary(ecdf(prevent_prob))))
          }
          prob_sub<<-prob_sub*prob_sub_g
        }
      }
      prob<<-prob*prob_sub
    }
    #replicating on E baserate
    prob_sub<<-MyEndHidden()
    prob<<-prob*prob_sub
    #record the prob of the current path
    prob_each_path<<-c(prob_each_path,prob)
    #add up all paths
    whole_prob<<-whole_prob+prob
  }
  #plot(prob_each_path)
}

#+ load models -------------------
#' # load models

#1.GG
MyGG<-function(){
  glist<<-c("A","B")  %>% intersect(sqc_raw$obj)
  plist<<-c()%>% intersect(sqc_raw$obj)
  MyPathIni()
  MyActual()
  return(whole_prob)
}
#2.PG
MyPG<-function(){
  glist<<-c("B")%>% intersect(sqc_raw$obj)
  plist<<-c("A")%>% intersect(sqc_raw$obj)
  MyPathIni()
  MyActual()
  return(whole_prob)
}
#3.NG
MyNG<-function(){
  glist<<-c("B")%>% intersect(sqc_raw$obj)
  plist<<-c()%>% intersect(sqc_raw$obj)
  MyPathIni()
  MyActual()
  return(whole_prob)
}
#4.GP
MyGP<-function(){
  glist<<-c("A")%>% intersect(sqc_raw$obj)
  plist<<-c("B")%>% intersect(sqc_raw$obj)
  MyPathIni()
  MyActual()
  return(whole_prob)
}
#5.PP
MyPP<-function(){
  glist<<-c()%>% intersect(sqc_raw$obj)
  plist<<-c("A","B")%>% intersect(sqc_raw$obj)
  MyPathIni()
  MyActual()
  return(whole_prob)
}
#6.NP
MyNP<-function(){
  glist<<-c()%>% intersect(sqc_raw$obj)
  plist<<-c("B")%>% intersect(sqc_raw$obj)
  MyPathIni()
  MyActual()
  return(whole_prob)
}
#7.GN
MyGN<-function(){
  glist<<-c("A")%>% intersect(sqc_raw$obj)
  plist<<-c()%>% intersect(sqc_raw$obj)
  MyPathIni()
  MyActual()
  return(whole_prob)
}
#8.PN
MyPN<-function(){
  glist<<-c()%>% intersect(sqc_raw$obj)
  plist<<-c("A")%>% intersect(sqc_raw$obj)
  MyPathIni()
  MyActual()
  return(whole_prob)
}
#9.NN
MyNN<-function(){
  glist<<-c()%>% intersect(sqc_raw$obj)
  plist<<-c()%>% intersect(sqc_raw$obj)
  MyPathIni()
  MyActual()
  return(whole_prob)
}

MySummary <-function(){
  
  sim_prob<<-c(MyGG(),MyPG(),MyNG(),
             MyGP(),MyPP(),MyNP(),
             MyGN(),MyPN(),MyNN())
  
  st_a=c("G","P","N","G","P","N","G","P","N")
  st_b=c("G","G","G","P","P","P","N","N","N")
  
  model_result<<-as.data.frame(matrix(NA,ncol=6,nrow=9)) %>%
    setNames(c("trial_id","A_pro","B_pro","A_state","B_state","ratio"))
  model_result$trial_id<<-sti_id
  model_result$A_pro<<-a_pro
  model_result$B_pro<<-b_pro
  model_result$A_state<<-st_a
  model_result$B_state<<-st_b
  model_result$ratio<<-sim_prob # NOT NORMALIZED
}