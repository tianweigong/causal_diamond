#' ---
#' author:  
#' output:
#'    html_document:
#'      toc: true
#'      toc_depth: 3
#'      toc_float: true
#'      theme: default
#'      highlight: tango
#' ---


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

baserate=5
baserate_var=0.25
k_pe = 36
r_pe = 12 #preventative p->e: m=3,var=0.25
k_ge = 9
r_ge = 6  #generative g->e: m=1.5,var=0.25

trial_end=20
large_time=100 # a second far beyond trail_end
sampling_point=200 #for one generative one preventative

#+ load functions-------------------
#' # load functions
MyLeaf <- function(x,pos,all_path_mtx,sig_len,sig_path,takenlist) {
  pos=pos+1
  if (pos>length(sig_len)){
    # p<<-p+1
    all_path_mtx=rbind(all_path_mtx, x)
    return(all_path_mtx)
  }else{
    ruleout=which(sig_path$causename[sig_path$obj==(pos+1)] %in% takenlist) #trim the path
    choice=unique(c(setdiff(seq(1:sig_len[pos]),ruleout),1))#trim the path
    for(i in 1:length(choice)){
      x[pos]=choice[i]
      takenlist[pos]=sig_path$causename[sig_path$obj==(pos+1)][choice[i]]#trim the path
      all_path_mtx=MyLeaf(x,pos,all_path_mtx,sig_len,sig_path,takenlist)
    }
  }
  return(all_path_mtx)
}

MyPathIni <- function(glist,plist,sqc){ #glist,plist
  #setup path
  elist=which(sqc$obj=="E")[-1]
  aflag=1
  bflag=1
  p=2
  sig_len_cum=c()
  sig_path=as.data.frame(matrix(NA,ncol=3,nrow=0)) %>%
    setNames(c("obj","cause","cause_idx"))
  
  for (i in elist){
    sig_path[nrow(sig_path)+1,]=c(p,"E",1)
    
    for (j in 1:i){
      if (sqc$obj[j]=="A" && "A" %in% glist){
        if(sqc$time[i]-sqc$time[j]<5){#trim the path
          sig_path[nrow(sig_path)+1,]=c(p,"A",aflag)
        }#trim the path
        aflag=aflag+1
      }
      
      if (sqc$obj[j]=="B" && "B" %in% glist){
        if (sqc$time[i]-sqc$time[j]<5){#trim the path
          sig_path[nrow(sig_path)+1,]=c(p,"B",bflag)
        }#trim the path
        
        bflag=bflag+1
      }
    }
    sig_len_cum=c(sig_len_cum,nrow(sig_path))
    aflag=1
    bflag=1
    p=p+1
  }
  sig_len=c(sig_len_cum[1],diff(sig_len_cum))
  if (nrow(sig_path)){
    sig_path$obj=as.numeric(sig_path$obj)
    sig_path$cause_idx=as.numeric(sig_path$cause_idx)
    sig_path$causename=paste(sig_path$cause,sig_path$cause_idx,sep="")#trim the path
    #get valid_path
    all_path_mtx=matrix(NA,ncol=length(sig_len),nrow=0)
    # p<<-0
    all_path_mtx=MyLeaf(c(1),0,all_path_mtx,sig_len,sig_path,c("E1"))#trim the path
    sig_path$causename=NULL#trim the path
    valid_path=MyValidPath(all_path_mtx,sig_path)
  }else{
    valid_path=as.data.frame(matrix(0,ncol=4,nrow=1)) %>%
      setNames(c("obj","cause","cause_idx","path"))
  }
}

MyValidPath<-function(all_path_mtx,sig_path){
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

MyT0 <-function(j,cdd_path,sqc,e_index,elist){
  if (cdd_path$cause[j]!="E"){
    t0=sqc$time[sqc$obj==cdd_path$cause[j] & sqc$obj_idx==cdd_path$cause_idx[j]]#caution!!!
  }else{
    t0=sqc$time[sqc$obj==cdd_path$cause[j] & sqc$total_idx ==max(e_index[e_index<elist[cdd_path$obj[j]-1]])]#caution!!!
  }
  return(t0)
}

MyHidden <- function(glist,plist,sqc,k_e,r_e,pe_time,t1,t0,k_pe,r_pe,k_ge,r_ge){
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
    prob_sub_e=prob_sub_e+prob_segment
    
    hidden1=dgamma(Time-t0,shape=k_e, rate=r_e)*df.sample$prevent
    hidden2=dgamma(Time-t0,shape=k_e*2, rate=r_e)*df.sample$prevent
    prob_segment=max(summary(ecdf(hidden1)))*max(summary(ecdf(hidden2)))*dgamma(t1-t0,shape=k_e*3, rate=r_e)
    prob_sub_e=prob_sub_e+prob_segment
    
    hidden1=dgamma(Time-t0,shape=k_e, rate=r_e)*df.sample$prevent
    hidden2=dgamma(Time-t0,shape=k_e*2, rate=r_e)*df.sample$prevent
    hidden3=dgamma(Time-t0,shape=k_e*3, rate=r_e)*df.sample$prevent
    prob_segment=max(summary(ecdf(hidden1)))*max(summary(ecdf(hidden2)))*
      max(summary(ecdf(hidden3)))*dgamma(t1-t0,shape=k_e*4, rate=r_e)
    prob_sub_e=prob_sub_e+prob_segment
    
    hidden1=dgamma(Time-t0,shape=k_e, rate=r_e)*df.sample$prevent
    hidden2=dgamma(Time-t0,shape=k_e*2, rate=r_e)*df.sample$prevent
    hidden3=dgamma(Time-t0,shape=k_e*3, rate=r_e)*df.sample$prevent
    hidden4=dgamma(Time-t0,shape=k_e*4, rate=r_e)*df.sample$prevent
    prob_segment=max(summary(ecdf(hidden1)))*max(summary(ecdf(hidden2)))*
      max(summary(ecdf(hidden3)))*max(summary(ecdf(hidden4)))*dgamma(t1-t0,shape=k_e*5, rate=r_e)
    prob_sub_e=prob_sub_e+prob_segment
    
    hidden1=dgamma(Time-t0,shape=k_e, rate=r_e)*df.sample$prevent
    hidden2=dgamma(Time-t0,shape=k_e*2, rate=r_e)*df.sample$prevent
    hidden3=dgamma(Time-t0,shape=k_e*3, rate=r_e)*df.sample$prevent
    hidden4=dgamma(Time-t0,shape=k_e*4, rate=r_e)*df.sample$prevent
    hidden5=dgamma(Time-t0,shape=k_e*5, rate=r_e)*df.sample$prevent
    prob_segment=max(summary(ecdf(hidden1)))*max(summary(ecdf(hidden2)))*
      max(summary(ecdf(hidden3)))*max(summary(ecdf(hidden4)))*max(summary(ecdf(hidden5)))*dgamma(t1-t0,shape=k_e*6, rate=r_e)
    prob_sub_e=prob_sub_e+prob_segment
  }
  return(prob_sub_e)
}

MyEndHidden <- function(glist,plist,sqc,k_e,r_e,e_index,pe_time,k_pe,r_pe,k_ge,r_ge){
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
    prob_sub=prob_sub+prob_segment
    
    hidden1=dgamma(Time-e_time,shape=k_e, rate=r_e)*df.sample$prevent
    hidden2=dgamma(Time-e_time,shape=k_e*2, rate=r_e)*df.sample$prevent
    prob_segment=max(summary(ecdf(hidden1)))*max(summary(ecdf(hidden2)))*
      (1-pgamma(trial_end-e_time,shape=k_e*3, rate=r_e))
    prob_sub=prob_sub+prob_segment
    
    hidden1=dgamma(Time-e_time,shape=k_e, rate=r_e)*df.sample$prevent
    hidden2=dgamma(Time-e_time,shape=k_e*2, rate=r_e)*df.sample$prevent
    hidden3=dgamma(Time-e_time,shape=k_e*3, rate=r_e)*df.sample$prevent
    prob_segment=max(summary(ecdf(hidden1)))*max(summary(ecdf(hidden2)))*
      max(summary(ecdf(hidden3)))*(1-pgamma(trial_end-e_time,shape=k_e*4, rate=r_e))
    prob_sub=prob_sub+prob_segment
    
    hidden1=dgamma(Time-e_time,shape=k_e, rate=r_e)*df.sample$prevent
    hidden2=dgamma(Time-e_time,shape=k_e*2, rate=r_e)*df.sample$prevent
    hidden3=dgamma(Time-e_time,shape=k_e*3, rate=r_e)*df.sample$prevent
    hidden4=dgamma(Time-e_time,shape=k_e*4, rate=r_e)*df.sample$prevent
    prob_segment=max(summary(ecdf(hidden1)))*max(summary(ecdf(hidden2)))*
      max(summary(ecdf(hidden3)))*max(summary(ecdf(hidden4)))*(1-pgamma(trial_end-e_time,shape=k_e*5, rate=r_e))
    prob_sub=prob_sub+prob_segment
    
    hidden1=dgamma(Time-e_time,shape=k_e, rate=r_e)*df.sample$prevent
    hidden2=dgamma(Time-e_time,shape=k_e*2, rate=r_e)*df.sample$prevent
    hidden3=dgamma(Time-e_time,shape=k_e*3, rate=r_e)*df.sample$prevent
    hidden4=dgamma(Time-e_time,shape=k_e*4, rate=r_e)*df.sample$prevent
    hidden5=dgamma(Time-e_time,shape=k_e*5, rate=r_e)*df.sample$prevent
    prob_segment=max(summary(ecdf(hidden1)))*max(summary(ecdf(hidden2)))*
      max(summary(ecdf(hidden3)))*max(summary(ecdf(hidden4)))*max(summary(ecdf(hidden5)))*(1-pgamma(trial_end-e_time,shape=k_e*6, rate=r_e))
    prob_sub=prob_sub+prob_segment
  }
  return(prob_sub)
}

MyActual <- function(glist,plist,sqc,valid_path,k_e,r_e,k_pe,r_pe,k_ge,r_ge){ #glist,plist
  elist=which(sqc$obj=="E")[-1]
  #MyPreventIdx
  p_index=which(sqc$obj %in% plist)
  pe_time=c()
  for (q in p_index){
    pe=q
    while (1){
      pe=pe+1
      if (pe>nrow(sqc)){
        pe_time=c(pe_time,large_time)
        break
      }
      if (sqc$obj[pe]=="E"){
        pe_time=c(pe_time,sqc$time[pe])
        break
      }
    }
  }
  #MyPreventIdx-end
  whole_prob=0
  prob_each_path=c()
  #begin to calculate the probability for each path
  for (i in unique(valid_path$path)){
    cdd_path=valid_path[valid_path$path==i,]
    prob=1
    e_index=c(1,which(sqc$obj=="E" & sqc$obj_idx %in% cdd_path$obj[cdd_path$cause=="E"]))
    #principle 1: generative causes
    prob_sub=1
    if (nrow(cdd_path)){
      for (j in 1:nrow(cdd_path)){
        t1=sqc$time[sqc$obj=="E" & sqc$obj_idx==cdd_path$obj[j]]
        t0=MyT0(j,cdd_path,sqc,e_index,elist)
        if (cdd_path$cause[j] %in% c("A","B")){
          prob_sub=dgamma(t1-t0, shape=k_ge, rate=r_ge)
        }
        if (cdd_path$cause[j]=="E"){
          prob_sub=MyHidden(glist,plist,sqc,k_e,r_e,pe_time,t1,t0,k_pe,r_pe,k_ge,r_ge)
        }
        prob=prob*prob_sub
      }
      #principle 2: preventative causes
      prob_sub=1
      for (j in elist){
        for (m in p_index[p_index<j]){
          t_p=sqc$time[j]-sqc$time[m]
          if (m== p_index[1]){
            prob_sub=pgamma(t_p, shape=k_pe, rate=r_pe)
          }else{
            prob_sub=prob_sub*pgamma(t_p, shape=k_pe, rate=r_pe)
          }
        }
        prob=prob*prob_sub
      }
    }
    #principle 3: give explanation for inactivation
    g_index=which(sqc$obj %in% glist)
    p_index=which(sqc$obj %in% plist)
    prob_sub=1
    for (m in glist){
      cdd_index=which(sqc$obj==m)
      for (j in cdd_index){
        cdd_time=sqc$time[j]
        if (sum(cdd_path$cause==m & cdd_path$cause_idx==sqc$obj_idx[j])==0){
          prob_sub_g=1-pgamma(trial_end-cdd_time, shape=k_ge, rate=r_ge)
          for (q in p_index){
            if ("E" %in% sqc$obj[q:j] && q<j){ #given principle 1
              next
            }
            Time=seq(cdd_time,pe_time[which(p_index==q)], length.out = sampling_point)
            p_time=sqc$time[q]
            df.sample=data.frame(Time)
            df.sample[,"no_prevent"]=pgamma(Time-p_time,shape=k_pe, rate=r_pe) #cannot prevent
            df.sample[df.sample$Time<p_time,"no_prevent"]=1 #before A happens, definitely cannot prevent 
            prevent_prob = (dgamma(Time-cdd_time, shape=k_ge, rate=r_ge))*(1-df.sample$`no_prevent`)
            prob_sub_g=prob_sub_g+max(summary(ecdf(prevent_prob)))
          }
          prob_sub=prob_sub*prob_sub_g
        }
      }
      prob=prob*prob_sub
    }
    #replicating on E baserate
    prob_sub=MyEndHidden(glist,plist,sqc,k_e,r_e,e_index,pe_time,k_pe,r_pe,k_ge,r_ge)
    prob=prob*prob_sub
    #record the prob of the current path
    prob_each_path=c(prob_each_path,prob)
    #add up all paths
    whole_prob=whole_prob+prob
  }
  return(whole_prob)
  #plot(prob_each_path)
}

#+ load models -------------------
#' # load models

#1.GG
MyGG<-function(sqc,k_e,r_e,k_pe,r_pe,k_ge,r_ge){
  glist=c("A","B")  %>% intersect(sqc$obj)
  plist=c()%>% intersect(sqc$obj)
  valid_path=MyPathIni(glist,plist,sqc)
  whole_prob=MyActual(glist,plist,sqc,valid_path,k_e,r_e,k_pe,r_pe,k_ge,r_ge)
  return(whole_prob)
}
#2.PG
MyPG<-function(sqc,k_e,r_e,k_pe,r_pe,k_ge,r_ge){
  glist=c("B")%>% intersect(sqc$obj)
  plist=c("A")%>% intersect(sqc$obj)
  valid_path=MyPathIni(glist,plist,sqc)
  whole_prob=MyActual(glist,plist,sqc,valid_path,k_e,r_e,k_pe,r_pe,k_ge,r_ge)
  return(whole_prob)
}
#3.NG
MyNG<-function(sqc,k_e,r_e,k_pe,r_pe,k_ge,r_ge){
  glist=c("B")%>% intersect(sqc$obj)
  plist=c()%>% intersect(sqc$obj)
  valid_path=MyPathIni(glist,plist,sqc)
  whole_prob=MyActual(glist,plist,sqc,valid_path,k_e,r_e,k_pe,r_pe,k_ge,r_ge)
  return(whole_prob)
}
#4.GP
MyGP<-function(sqc,k_e,r_e,k_pe,r_pe,k_ge,r_ge){
  glist=c("A")%>% intersect(sqc$obj)
  plist=c("B")%>% intersect(sqc$obj)
  valid_path=MyPathIni(glist,plist,sqc)
  whole_prob=MyActual(glist,plist,sqc,valid_path,k_e,r_e,k_pe,r_pe,k_ge,r_ge)
  return(whole_prob)
}
#5.PP
MyPP<-function(sqc,k_e,r_e,k_pe,r_pe,k_ge,r_ge){
  glist=c()%>% intersect(sqc$obj)
  plist=c("A","B")%>% intersect(sqc$obj)
  valid_path=MyPathIni(glist,plist,sqc)
  whole_prob=MyActual(glist,plist,sqc,valid_path,k_e,r_e,k_pe,r_pe,k_ge,r_ge)
  return(whole_prob)
}
#6.NP
MyNP<-function(sqc,k_e,r_e,k_pe,r_pe,k_ge,r_ge){
  glist=c()%>% intersect(sqc$obj)
  plist=c("B")%>% intersect(sqc$obj)
  valid_path=MyPathIni(glist,plist,sqc)
  whole_prob=MyActual(glist,plist,sqc,valid_path,k_e,r_e,k_pe,r_pe,k_ge,r_ge)
  return(whole_prob)
}
#7.GN
MyGN<-function(sqc,k_e,r_e,k_pe,r_pe,k_ge,r_ge){
  glist=c("A")%>% intersect(sqc$obj)
  plist=c()%>% intersect(sqc$obj)
  valid_path=MyPathIni(glist,plist,sqc)
  whole_prob=MyActual(glist,plist,sqc,valid_path,k_e,r_e,k_pe,r_pe,k_ge,r_ge)
  return(whole_prob)
}
#8.PN
MyPN<-function(sqc,k_e,r_e,k_pe,r_pe,k_ge,r_ge){
  glist=c()%>% intersect(sqc$obj)
  plist=c("A")%>% intersect(sqc$obj)
  valid_path=MyPathIni(glist,plist,sqc)
  whole_prob=MyActual(glist,plist,sqc,valid_path,k_e,r_e,k_pe,r_pe,k_ge,r_ge)
  return(whole_prob)
}
#9.NN
MyNN<-function(sqc,k_e,r_e,k_pe,r_pe,k_ge,r_ge){
  glist=c()%>% intersect(sqc$obj)
  plist=c()%>% intersect(sqc$obj)
  valid_path=MyPathIni(glist,plist,sqc)
  whole_prob=MyActual(glist,plist,sqc,valid_path,k_e,r_e,k_pe,r_pe,k_ge,r_ge)
  return(whole_prob)
}

MySummary <-function(sqc,k_e,r_e,k_pe,r_pe,k_ge,r_ge){
  
  sim_prob=c(MyGG(sqc,k_e,r_e,k_pe,r_pe,k_ge,r_ge),
             MyGN(sqc,k_e,r_e,k_pe,r_pe,k_ge,r_ge),
             MyGP(sqc,k_e,r_e,k_pe,r_pe,k_ge,r_ge),
             MyNG(sqc,k_e,r_e,k_pe,r_pe,k_ge,r_ge),
             MyNN(sqc,k_e,r_e,k_pe,r_pe,k_ge,r_ge),
             MyNP(sqc,k_e,r_e,k_pe,r_pe,k_ge,r_ge),
             MyPG(sqc,k_e,r_e,k_pe,r_pe,k_ge,r_ge),
             MyPN(sqc,k_e,r_e,k_pe,r_pe,k_ge,r_ge),
             MyPP(sqc,k_e,r_e,k_pe,r_pe,k_ge,r_ge))
  return(sim_prob)
}
#+ load stimulus list -------------------
#' # load stimulus list

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

MyOnlineInfer_nor <- function(sqc_raw_long,k_e,r_e,k_pe,r_pe,k_ge,r_ge){
  sqc_raw=sqc_raw_long
  sqc=MySqc(sqc_raw)
  sim_prob=MySummary(sqc,k_e,r_e,k_pe,r_pe,k_ge,r_ge) #normative model
  return(sim_prob)
}

filepath="exp3/"

st_a=c("G","G","G","N","N","N","P","P","P")
st_b=c("G","N","P","G","N","P","G","N","P")
sti_list=c("sqc.gg_r","sqc.gg_u",
           "sqc.gn_r","sqc.gn_u",
           "sqc.gp_r","sqc.gp_u",
           "sqc.ng_r","sqc.ng_u",
           "sqc.nn_r","sqc.nn_u",
           "sqc.np_r","sqc.np_u",
           "sqc.pg_r","sqc.pg_u",
           "sqc.pn_r","sqc.pn_u",
           "sqc.pp_r","sqc.pp_u")
sti_lis2=c("gg_r","gg_u",
           "gn_r","gn_u",
           "gp_r","gp_u",
           "ng_r","ng_u",
           "nn_r","nn_u",
           "np_r","np_u",
           "pg_r","pg_u",
           "pn_r","pn_u",
           "pp_r","pp_u")
a_pro_list=c(rep("G",6),rep("N",6),rep("P",6))
b_pro_list=rep(c(rep("G",2),rep("N",2),rep("P",2)),3)
ru_pro_list=rep(c("r","u"),9)

dt.nor.big=as.data.frame(matrix(NA,ncol=9,nrow=0)) %>%
  setNames(c("seed","sti_id","A_pro","B_pro","ru_pro","A_state","B_state","ratio","raw"))

for (seed in 1: 9){
  filename=paste(filepath,"seed",seed,"/allstimuli.Rda",sep = "")
  load(filename)
  for (sti_num in 1:18){
    r_e_r = baserate/baserate_var
    k_e_r = r_e_r*baserate
    k_e_u=1
    r_e_u=1/baserate
    dt.nor=as.data.frame(matrix(NA,ncol=9,nrow=9)) %>%
      setNames(c("seed","sti_id","A_pro","B_pro","ru_pro","A_state","B_state","ratio","raw"))

    a_pro=a_pro_list[sti_num]
    b_pro=b_pro_list[sti_num]
    sti_name=sti_list[sti_num]
    sqc_raw_long=get(sti_name)
    ru_pro=ru_pro_list[sti_num]

    if (ru_pro=="r"){k_e=k_e_r;r_e=r_e_r}
    if (ru_pro=="u"){k_e=k_e_u;r_e=r_e_u}

    dt.nor$seed=seed
    dt.nor$sti_id=sti_num
    dt.nor$A_pro=a_pro
    dt.nor$B_pro=b_pro
    dt.nor$ru_pro=ru_pro
    nor_sim_prob=MyOnlineInfer_nor(sqc_raw_long,k_e,r_e,k_pe,r_pe,k_ge,r_ge)

    for (mo in 1:9){
      dt.nor$A_state[mo]=st_a[mo]
      dt.nor$B_state[mo]=st_b[mo]
      dt.nor$ratio[mo]=nor_sim_prob[mo]/sum(nor_sim_prob)

      dt.nor$raw[mo]=nor_sim_prob[mo]
    }
    dt.nor.big=rbind(dt.nor.big,dt.nor)
  }
}
#need 7 min or so
for (seed in 10:12){
  for (sti_num in 1:18){
    filename=paste(filepath,"seed",seed,"/",sti_lis2[sti_num],".Rda",sep = "")
    load(filename)
    
    r_e_r = baserate/baserate_var
    k_e_r = r_e_r*baserate
    k_e_u=1
    r_e_u=1/baserate
    dt.nor=as.data.frame(matrix(NA,ncol=9,nrow=9)) %>%
      setNames(c("seed","sti_id","A_pro","B_pro","ru_pro","A_state","B_state","ratio","raw"))
    
    a_pro=a_pro_list[sti_num]
    b_pro=b_pro_list[sti_num]
    sti_name=sti_list[sti_num]
    sqc_raw_long=sqc
    ru_pro=ru_pro_list[sti_num]
    
    if (ru_pro=="r"){k_e=k_e_r;r_e=r_e_r}
    if (ru_pro=="u"){k_e=k_e_u;r_e=r_e_u}
    
    dt.nor$seed=seed
    dt.nor$sti_id=sti_num
    dt.nor$A_pro=a_pro
    dt.nor$B_pro=b_pro
    dt.nor$ru_pro=ru_pro
    nor_sim_prob=MyOnlineInfer_nor(sqc_raw_long,k_e,r_e,k_pe,r_pe,k_ge,r_ge)
    
    for (mo in 1:9){
      dt.nor$A_state[mo]=st_a[mo]
      dt.nor$B_state[mo]=st_b[mo]
      dt.nor$ratio[mo]=nor_sim_prob[mo]/sum(nor_sim_prob)
      
      dt.nor$raw[mo]=nor_sim_prob[mo]
    }
    dt.nor.big=rbind(dt.nor.big,dt.nor)
  }
}


# load("df.model.Rda")
# dt.nor.big=df.model

MyMod6<- function(md_raw,md_ratio){
  
  md=as.data.frame(matrix(NA, nrow = 18*2*3, ncol = 4))%>%
    setNames(c("sti_id","cpn","state","ratio"))
  
  for (j in 1:18){
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

pic=list()
pic.label=c("1"="GG_reliable","2"="GG_unreliable","3"="GN_r","4"="GN_u","5"="GP_r","6"="GP_u",
            "7"="NG_r","8"="NG_u","9"="NN_r","10"="NN_u","11"="NP_r","12"="NP_u",
            "13"="PG_r","14"="PG_u","15"="PN_r","16"="PN_u","17"="PP_r","18"="PP_u")

for (k in 1:18){
  dt.nor=dt.nor.big %>% subset(seed==k)
  pic.sti.nor=MyMod6(dt.nor,dt.nor$ratio)

  pic[[k]]=pic.sti.nor %>%ggplot()+
    geom_bar(mapping=aes(x=cpn, y=ratio,fill=state),stat='identity',position="dodge",color="black")+
    facet_wrap(~sti_id,nrow=3,labeller = as_labeller(pic.label))+
    theme_bw()+
    scale_fill_manual(values=c("#E4F0EC","#EDB1A4","#6282b1"),
                      labels =  c(" Generative  "," Non-causal  "," Preventative  "))+
    scale_color_manual(values=c("black","black","black"))+
    xlab("component")+
    ggtitle(paste("seed",k,sep=""))
}

pic[[1]]
pic[[2]]
pic[[3]]
pic[[4]]
pic[[5]]
pic[[6]]
pic[[7]]
pic[[8]]
pic[[9]]
pic[[10]]
pic[[11]]
pic[[12]]

df.model=dt.nor.big
save(df.model,file="df.model.Rda")

