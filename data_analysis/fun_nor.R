sampling_point=30
large_time=100 # a second far beyond trail_end
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
  for (i in 1:ncol(all_path_mtx)){
    cdd_path=sig_path[sig_path$obj==unique(sig_path$obj)[i],][all_path_mtx[,i],]
    cdd_path$path=seq(1,nrow(cdd_path))
    valid_path=rbind(valid_path,cdd_path)
  }
  return(valid_path)
}

MyCumu<-function(prob,len){ #cumulative probability
  sum(prob*len)
}

MyHidden <- function(glist,plist,sqc,k_e,r_e,pe_time,t1,t0,k_pe,r_pe,k_ge,r_ge){
  #for E->->E
  #no hidden baserate
  prob_sub_e=dgamma(t1-t0, shape=k_e, rate=r_e)
  #hidden baserate
  #using approximation approach
  p_between=sqc %>% subset(time>t0 & time<t1 & obj %in% plist)
  Time=seq(t0,t1, length.out = sampling_point)
  len=(t1-t0)/length(Time)
  
  df.sample=data.frame(Time)
  if (nrow(p_between)>0){
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
    prob_segment=    MyCumu(hidden1,len)*dgamma(t1-t0,shape=k_e*2, rate=r_e)
    prob_sub_e=prob_sub_e+prob_segment
    
    hidden1=dgamma(Time-t0,shape=k_e, rate=r_e)*df.sample$prevent
    hidden2=dgamma(Time-t0,shape=k_e*2, rate=r_e)*df.sample$prevent
    prob_segment= MyCumu(hidden1,len)*MyCumu(hidden2,len)*dgamma(t1-t0,shape=k_e*3, rate=r_e)
    prob_sub_e=prob_sub_e+prob_segment
    
    hidden1=dgamma(Time-t0,shape=k_e, rate=r_e)*df.sample$prevent
    hidden2=dgamma(Time-t0,shape=k_e*2, rate=r_e)*df.sample$prevent
    hidden3=dgamma(Time-t0,shape=k_e*3, rate=r_e)*df.sample$prevent
    prob_segment=MyCumu(hidden1,len)*MyCumu(hidden2,len)*MyCumu(hidden3,len)*dgamma(t1-t0,shape=k_e*4, rate=r_e)
    prob_sub_e=prob_sub_e+prob_segment
    
    hidden1=dgamma(Time-t0,shape=k_e, rate=r_e)*df.sample$prevent
    hidden2=dgamma(Time-t0,shape=k_e*2, rate=r_e)*df.sample$prevent
    hidden3=dgamma(Time-t0,shape=k_e*3, rate=r_e)*df.sample$prevent
    hidden4=dgamma(Time-t0,shape=k_e*4, rate=r_e)*df.sample$prevent
    prob_segment=MyCumu(hidden1,len)*MyCumu(hidden2,len)*MyCumu(hidden3,len)*MyCumu(hidden4,len)*dgamma(t1-t0,shape=k_e*5, rate=r_e)
    prob_sub_e=prob_sub_e+prob_segment
    
    hidden1=dgamma(Time-t0,shape=k_e, rate=r_e)*df.sample$prevent
    hidden2=dgamma(Time-t0,shape=k_e*2, rate=r_e)*df.sample$prevent
    hidden3=dgamma(Time-t0,shape=k_e*3, rate=r_e)*df.sample$prevent
    hidden4=dgamma(Time-t0,shape=k_e*4, rate=r_e)*df.sample$prevent
    hidden5=dgamma(Time-t0,shape=k_e*5, rate=r_e)*df.sample$prevent
    prob_segment=MyCumu(hidden1,len)*MyCumu(hidden2,len)*MyCumu(hidden3,len)*MyCumu(hidden4,len)*MyCumu(hidden5,len)*dgamma(t1-t0,shape=k_e*6, rate=r_e)
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
  Time=seq(e_time,trial_end, length.out = sampling_point)
  len=(trial_end-e_time)/length(Time)
  df.sample=data.frame(Time)
  if (nrow(p_between)>0){
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
    hidden1=dgamma(Time-e_time,shape=k_e, rate=r_e)*df.sample$prevent
    prob_segment=MyCumu(hidden1,len)*(1-pgamma(trial_end-e_time,shape=k_e*2, rate=r_e))
    prob_sub=prob_sub+prob_segment
    
    hidden1=dgamma(Time-e_time,shape=k_e, rate=r_e)*df.sample$prevent
    hidden2=dgamma(Time-e_time,shape=k_e*2, rate=r_e)*df.sample$prevent
    prob_segment=MyCumu(hidden1,len)*MyCumu(hidden2,len)*
      (1-pgamma(trial_end-e_time,shape=k_e*3, rate=r_e))
    prob_sub=prob_sub+prob_segment
    
    hidden1=dgamma(Time-e_time,shape=k_e, rate=r_e)*df.sample$prevent
    hidden2=dgamma(Time-e_time,shape=k_e*2, rate=r_e)*df.sample$prevent
    hidden3=dgamma(Time-e_time,shape=k_e*3, rate=r_e)*df.sample$prevent
    prob_segment=MyCumu(hidden1,len)*MyCumu(hidden2,len)*
      MyCumu(hidden3,len)*(1-pgamma(trial_end-e_time,shape=k_e*4, rate=r_e))
    prob_sub=prob_sub+prob_segment
    
    hidden1=dgamma(Time-e_time,shape=k_e, rate=r_e)*df.sample$prevent
    hidden2=dgamma(Time-e_time,shape=k_e*2, rate=r_e)*df.sample$prevent
    hidden3=dgamma(Time-e_time,shape=k_e*3, rate=r_e)*df.sample$prevent
    hidden4=dgamma(Time-e_time,shape=k_e*4, rate=r_e)*df.sample$prevent
    prob_segment=MyCumu(hidden1,len)*MyCumu(hidden2,len)*
      MyCumu(hidden3,len)*MyCumu(hidden4,len)*(1-pgamma(trial_end-e_time,shape=k_e*5, rate=r_e))
    prob_sub=prob_sub+prob_segment
    
    hidden1=dgamma(Time-e_time,shape=k_e, rate=r_e)*df.sample$prevent
    hidden2=dgamma(Time-e_time,shape=k_e*2, rate=r_e)*df.sample$prevent
    hidden3=dgamma(Time-e_time,shape=k_e*3, rate=r_e)*df.sample$prevent
    hidden4=dgamma(Time-e_time,shape=k_e*4, rate=r_e)*df.sample$prevent
    hidden5=dgamma(Time-e_time,shape=k_e*5, rate=r_e)*df.sample$prevent
    prob_segment=MyCumu(hidden1,len)*MyCumu(hidden2,len)*
      MyCumu(hidden3,len)*MyCumu(hidden4,len)*MyCumu(hidden5,len)*(1-pgamma(trial_end-e_time,shape=k_e*6, rate=r_e))
    prob_sub=prob_sub+prob_segment
  }
  return(prob_sub)
}

MyGenProb<-function(t0,t1,c,glist,plist,sqc,k_e,r_e,pe_time,k_pe,r_pe,k_ge,r_ge){
  if (c %in% c("A","B")){
    prob_sub=dgamma(t1-t0, shape=k_ge, rate=r_ge)
  }
  if (c=="E"){
    prob_sub=MyHidden(glist,plist,sqc,k_e,r_e,pe_time,t1,t0,k_pe,r_pe,k_ge,r_ge)
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
  if (valid_path$obj[1]!=0){
    valid_path1=valid_path[valid_path$cause=="E",]
    valid_path2=valid_path[valid_path$cause!="E",]
    
    valid_path1=valid_path1[with(valid_path1,order(path,obj)),]
    valid_path1$tag=c(1,diff(valid_path1$path))
    valid_path1$cause_idx=c(1,valid_path1$obj[-nrow(valid_path1)])
    valid_path1$cause_idx[valid_path1$tag==1]=1
    valid_path1$tag=NULL
    valid_path=rbind(valid_path1,valid_path2)
    valid_path=valid_path[with(valid_path,order(path,obj)),]
    
    sqc1=sqc
    row.names(sqc1)=paste(sqc1$obj,sqc1$obj_idx)
    valid_path$cau_time=sqc1[paste(valid_path$cause,valid_path$cause_idx),"time"]
    valid_path$eff_time=sqc1[paste("E",valid_path$obj),"time"]
    valid_path$genProb=NA
    valid_path=valid_path%>%mutate(lb=paste(obj,cause,cause_idx,sep=""))
    for (k in unique(valid_path$lb)){
      x=valid_path[which(valid_path$lb==k)[1],]
      valid_path$genProb[which(valid_path$lb==k)]= MyGenProb(x$cau_time[1],x$eff_time[1],x$cause[1],glist,plist,sqc,k_e,r_e,pe_time,k_pe,r_pe,k_ge,r_ge)
    }
  }
  
  for (i in unique(valid_path$path)){
    cdd_path=valid_path[valid_path$path==i,]
    prob=1
    e_index=c(1,which(sqc$obj=="E" & sqc$obj_idx %in% cdd_path$obj[cdd_path$cause=="E"]))
    #principle 1: generative causes
    if (nrow(cdd_path)){
      prob=prod(cdd_path$genProb)
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
          
          Time=seq(0,whole_trial_end, length.out = sampling_point*10)
          len=whole_trial_end/length(Time)
          
          df.sample=data.frame(Time)
          
          for (q in p_index){
            if ("E" %in% sqc$obj[q:j] && q<j){ #given principle 1
              next
            }
            col=paste(sqc$obj[q],sqc$obj_idx[q])
            p_time=sqc$time[q]
            df.sample[,col]=pgamma(Time-p_time,shape=k_pe, rate=r_pe) #cannot prevent
            df.sample[df.sample$Time<p_time,col]=1 #before P happens, definitely cannot prevent 
            df.sample[df.sample$Time>pe_time[which(p_index==q)],col]=1 #After Another E happens, definitely cannot prevent 
          }
          df.sample.dup=df.sample
          df.sample.dup$Time=NULL
          df.sample[,"prevent"]=(1-rowProds(data.matrix(df.sample.dup))) # noisy-or, 1 minus the product all that cannot prevent
          
          prevent_prob = (dgamma(Time-cdd_time, shape=k_ge, rate=r_ge))*df.sample$prevent
          prob_sub_g=prob_sub_g+MyCumu(prevent_prob,len)
          
          prob_sub=prob_sub*prob_sub_g
        }
      }
    }
    prob=prob*prob_sub
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

MyMdNor<- function(sqc_raw,k_e,r_e,k_pe,r_pe,k_ge,r_ge){
  sqc=MySqc(sqc_raw)
  sim_prob=MySummary(sqc,k_e,r_e,k_pe,r_pe,k_ge,r_ge) #normative model
  return(sim_prob/sum(sim_prob))
}