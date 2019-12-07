#' ---
#' title: Time & Prevention <br> core code for normative Bayesian inference <br>
#' author: Tia Gong
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
rm(list=ls())

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

MyT0 <-function(){
  if (cdd_path$cause[j]!="E"){
    t0=sqc$time[sqc$obj==cdd_path$cause[j] & sqc$obj_idx==cdd_path$cause_idx[j]]
  }else{
    t0=sqc$time[sqc$obj==cdd_path$cause[j] & sqc$total_idx ==max(e_index[e_index<elist[cdd_path$obj[j]-1]])]
  }
  return(t0)
}

MyHidden <- function(){
  #principle 2: E->->E
  #no hidden baserate
  prob_sub_e=dgamma(t1-t0, shape=k_e, rate=r_e)
  #hidden baserate
  #using approximation approach
  a_between=sqc %>% subset(time>t0 & time<t1 & obj %in% prelist)
  if (nrow(a_between)>0){
    Time=seq(t0,t1, length.out = sampling_point)
    df.sample=data.frame(Time)
    if (length(prelist)>1){
      ae_cut_index=a_between$ab_idx
    }else{
      ae_cut_index=a_between$obj_idx
    }
    for (m in 1:nrow(a_between)){
      a_time=a_between$time[m]
      col=paste(a_between$obj[m],a_between$obj_idx[m])
      df.sample[,col]=pgamma(Time-a_time,shape=k_ae, rate=r_ae) #cannot prevent
      df.sample[df.sample$Time<a_time,col]=1 #before A happens, definitely cannot prevent 
      df.sample[df.sample$Time>ae_time[ae_cut_index[m]],col]=1 #After Another E happens, definitely cannot prevent 
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
  a_between=sqc %>% subset(time>e_time & time<trial_end & obj %in% prelist)
  if (nrow(a_between)>0){
    Time=seq(e_time,trial_end, length.out = sampling_point)
    df.sample=data.frame(Time)
    if (length(prelist)>1){
      ae_cut_index=a_between$ab_idx
    }else{
      ae_cut_index=a_between$obj_idx
    }
    for (m in 1:nrow(a_between)){
      a_time=a_between$time[m]
      col=paste(a_between$obj[m],a_between$obj_idx[m])
      df.sample[,col]=pgamma(Time-a_time,shape=k_ae, rate=r_ae) #cannot prevent
      df.sample[df.sample$Time<a_time,col]=1 #before A happens, definitely cannot prevent 
      df.sample[df.sample$Time>ae_time[ae_cut_index[m]],col]=1 #After Another E happens, definitely cannot prevent 
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

#+ set parameters -------------------
#' # set parameters
k_e=12.5
r_e=2.5 #e->e: m=5,var=0.25 -> 2 tune the noise here

k_ae=4.5
r_ae=1.5 #preventative a->e: m=3,var=0.25 -> 2 tune the noise here

k_be=1.125
r_be=0.75  #generative b->e: m=1.5,var=0.25 -> 2 tune the noise here

trial_end=20
large_time=100
sampling_point=200
sim_subject=1000

#+ load stimuli -------------------
#' # load stimuli
MySqc <-function(){
  sqc_raw_sub_a=subset(sqc_raw,obj=="A")
  sqc_raw_sub_a$obj_idx=seq(1:nrow(sqc_raw_sub_a))
  sqc_raw_sub_b=subset(sqc_raw,obj=="B")
  sqc_raw_sub_b$obj_idx=seq(1:nrow(sqc_raw_sub_b))
  sqc_raw_sub_ab=rbind(sqc_raw_sub_a,sqc_raw_sub_b)
  sqc_raw_sub_ab=sqc_raw_sub_ab[order(sqc_raw_sub_ab$time),]
  sqc_raw_sub_ab$ab_idx=seq(1:nrow(sqc_raw_sub_ab))
  sqc_raw_sub_e=subset(sqc_raw,obj=="E")
  sqc_raw_sub_e$obj_idx=seq(1:nrow(sqc_raw_sub_e))
  sqc_raw_sub_e$ab_idx=rep(0,nrow(sqc_raw_sub_e))
  sqc_raw<<-rbind(sqc_raw_sub_ab,sqc_raw_sub_e)
  sqc_raw<<-sqc_raw[order(sqc_raw$time),]
  sqc_raw$total_idx<<-c(1:nrow(sqc_raw))
}

filepath="../../../stimulus/pilot/stimulus_sqc/"
#1
sti_id=4# REMEMBER TO CHECK IT
sti_name="pAgB"
sti_no=2
a_pro="P"# REMEMBER TO CHECK IT
b_pro="G" # REMEMBER TO CHECK IT
load(file = paste(filepath,sti_name,sti_no,"/",sti_name,".Rda",sep = ""))
sqc_raw=get(sti_name)
MySqc()

#+ # GG structure -------------------
#' #GG structure
sqc=sqc_raw

elist=which(sqc$obj=="E")[-1]
bflag=1
aflag=1
p=2
sig_len_cum=c()
sig_path=as.data.frame(matrix(NA,ncol=3,nrow=0)) %>%
  setNames(c("obj","cause","cause_idx"))

for (i in elist){
  sig_path[nrow(sig_path)+1,]=c(p,"E",1)
  
  for (j in 1:i){
    if (sqc$obj[j]=="A"){
      sig_path[nrow(sig_path)+1,]=c(p,"A",aflag)
      aflag=aflag+1
    }
    
    if (sqc$obj[j]=="B"){
      sig_path[nrow(sig_path)+1,]=c(p,"B",bflag)
      bflag=bflag+1
    }
  }
  sig_len_cum=c(sig_len_cum,nrow(sig_path))
  
  aflag=1
  bflag=1
  p=p+1
}
sig_len=c(sig_len_cum[1],diff(sig_len_cum))
sig_path$obj=as.numeric(sig_path$obj)
sig_path$cause_idx=as.numeric(sig_path$cause_idx)

all_path_mtx=matrix(NA,ncol=length(sig_len),nrow=0)
p=0
MyLeaf(c(1),0)

# check each path
valid_path=MyValidPath()

# delay model
whole_prob=0
a_index=which(sqc$obj=="A")
b_index=which(sqc$obj=="B")
prelist=c()
prob_each_path=c()

#begin to calculate the probability for each path
for (i in unique(valid_path$path)){
  cdd_path=valid_path[valid_path$path==i,]
  prob=1
  e_index=c(1,which(sqc$obj=="E" & sqc$obj_idx %in% cdd_path$obj[cdd_path$cause=="E"]))
  #principle 2: generative causes
  prob_sub=1
  for (j in 1:nrow(cdd_path)){
    t1=sqc$time[sqc$obj=="E" & sqc$obj_idx==cdd_path$obj[j]]
    t0=MyT0()
    
    if (cdd_path$cause[j]=="A"){
      prob_sub=dgamma(t1-t0, shape=k_be, rate=r_be) #"be" used to reprevent generative causes
    }
    
    if (cdd_path$cause[j]=="B"){
      prob_sub=dgamma(t1-t0, shape=k_be, rate=r_be)
    }
    
    if (cdd_path$cause[j]=="E"){
      prob_sub=dgamma(t1-t0, shape=k_e, rate=r_e)
    }
    prob=prob*prob_sub
  }
  #principle 3: give explanation for inactivation
  prob_sub=1
  for (j in a_index){
    a_time=sqc$time[j]
    if (sum(cdd_path$cause=="A" & cdd_path$cause_idx==sqc$obj_idx[j])==0){
      prob_sub_beyond=1-pgamma(trial_end-a_time, shape=k_be, rate=r_be)
      prob_sub=prob_sub*prob_sub_beyond
    }
  }
  prob=prob*prob_sub
  #replicating on B
  prob_sub=1
  for (j in b_index){
    b_time=sqc$time[j]
    if (sum(cdd_path$cause=="B" & cdd_path$cause_idx==sqc$obj_idx[j])==0){
      prob_sub_beyond=1-pgamma(trial_end-b_time, shape=k_be, rate=r_be)
      prob_sub=prob_sub*prob_sub_beyond
    }
  }
  prob=prob*prob_sub
  
  #replicating on E baserate
  prob_sub=MyEndHidden()
  prob=prob*prob_sub
  #record the prob of the current path
  prob_each_path=c(prob_each_path,prob)
  #add up all paths
  # whole_prob=whole_prob+prob
  #another approch to add all path but usually get 0 when it is not the correct model
  #for R will simplify the product culculation.
  if (i==unique(valid_path$path)[1]){
    whole_prob=prob
  }else{
    whole_prob=1-(1-whole_prob)*(1-prob)
  }
}

if (whole_prob<max(prob_each_path)){
  whole_prob=max(prob_each_path)
}
#plot(prob_each_path)
model_gAgB=whole_prob


#+ GN structure -------------------
#' # GN structure
sqc=sqc_raw
sqc$obj <- as.character(sqc$obj)
sqc$obj[sqc$obj == "A"] <- "M"
sqc$obj[sqc$obj == "B"] <- "A"
sqc$obj[sqc$obj == "M"] <- "B"

elist=which(sqc$obj=="E")[-1]
bflag=1
p=2
sig_len_cum=c()
sig_path=as.data.frame(matrix(NA,ncol=3,nrow=0)) %>%
  setNames(c("obj","cause","cause_idx"))

for (i in elist){
  sig_path[nrow(sig_path)+1,]=c(p,"E",1)
  
  for (j in 1:i){
    if (sqc$obj[j]=="B"){
      sig_path[nrow(sig_path)+1,]=c(p,"B",bflag)
      bflag=bflag+1
    }
  }
  sig_len_cum=c(sig_len_cum,nrow(sig_path))
  
  bflag=1
  p=p+1
}
sig_len=c(sig_len_cum[1],diff(sig_len_cum))
sig_path$obj=as.numeric(sig_path$obj)
sig_path$cause_idx=as.numeric(sig_path$cause_idx)

all_path_mtx=matrix(NA,ncol=length(sig_len),nrow=0)
p=0
MyLeaf(c(1),0)

# check each path
valid_path=MyValidPath()
# delay model
whole_prob=0
b_index=which(sqc$obj=="B")
prelist=c()
prob_each_path=c()

#begin to calculate the probability for each path
for (i in unique(valid_path$path)){
  cdd_path=valid_path[valid_path$path==i,]
  prob=1
  e_index=c(1,which(sqc$obj=="E" & sqc$obj_idx %in% cdd_path$obj[cdd_path$cause=="E"]))
  #principle 2: generative causes
  prob_sub=1
  for (j in 1:nrow(cdd_path)){
    t1=sqc$time[sqc$obj=="E" & sqc$obj_idx==cdd_path$obj[j]]
    t0=MyT0()
    
    if (cdd_path$cause[j]=="B"){
      prob_sub=dgamma(t1-t0, shape=k_be, rate=r_be)
    }
    
    if (cdd_path$cause[j]=="E"){
      #principle 3: E->->E
      #no hidden baserate
      prob_sub=dgamma(t1-t0, shape=k_e, rate=r_e)
    }
    prob=prob*prob_sub
  }
  #principle 3: give explanation for inactivation
  prob_sub=1
  for (j in b_index){
    b_time=sqc$time[j]
    if (sum(cdd_path$cause=="B" & cdd_path$cause_idx==sqc$obj_idx[j])==0){
      prob_sub_beyond=1-pgamma(trial_end-b_time, shape=k_be, rate=r_be)
      #using approximation method to find the probability
      prob_sub=prob_sub*prob_sub_beyond
    }
  }
  prob=prob*prob_sub
  
  #replicating on E baserate
  prob_sub=MyEndHidden()
  prob=prob*prob_sub
  #record the prob of the current path
  prob_each_path=c(prob_each_path,prob)
  #add up all paths
  # whole_prob=whole_prob+prob
  #another approch to add all path but usually get 0 when it is not the correct model
  #for R will simplify the product culculation.
  if (i==unique(valid_path$path)[1]){
    whole_prob=prob
  }else{
    whole_prob=1-(1-whole_prob)*(1-prob)
  }
}

if (whole_prob<max(prob_each_path)){
  whole_prob=max(prob_each_path)
}
#plot(prob_each_path)
model_gAnB=whole_prob

#+ GP structure -------------------
#' # GP structure
sqc=sqc_raw
sqc$obj <- as.character(sqc$obj)
sqc$obj[sqc$obj == "A"] <- "M"
sqc$obj[sqc$obj == "B"] <- "A"
sqc$obj[sqc$obj == "M"] <- "B"

# generate all paths
elist=which(sqc$obj=="E")[-1]
bflag=1
p=2
sig_len_cum=c()
sig_path=as.data.frame(matrix(NA,ncol=3,nrow=0)) %>%
  setNames(c("obj","cause","cause_idx"))

for (i in elist){
  sig_path[nrow(sig_path)+1,]=c(p,"E",1)
  
  for (j in 1:i){
    if (sqc$obj[j]=="B"){
      sig_path[nrow(sig_path)+1,]=c(p,"B",bflag)
      bflag=bflag+1
    }
  }
  sig_len_cum=c(sig_len_cum,nrow(sig_path))
  
  bflag=1
  p=p+1
}
sig_len=c(sig_len_cum[1],diff(sig_len_cum))
sig_path$obj=as.numeric(sig_path$obj)
sig_path$cause_idx=as.numeric(sig_path$cause_idx)

all_path_mtx=matrix(NA,ncol=length(sig_len),nrow=0)
p=0
MyLeaf(c(1),0)

# check each path
valid_path=MyValidPath()


# delay model
whole_prob=0
a_index=which(sqc$obj=="A")
b_index=which(sqc$obj=="B")
prelist=c("A")
ae_time=c()
prob_each_path=c()
for (i in a_index){
  p=i
  while (1){
    p=p+1
    if (p>nrow(sqc)){
      ae_time=c(ae_time,large_time)
      break
    }
    
    if (sqc$obj[p]=="E"){
      ae_time=c(ae_time,sqc$time[p])
      break
    }
  }
}#the max prevention time point of each A

#begin to calculate the probability for each path
for (i in unique(valid_path$path)){
  cdd_path=valid_path[valid_path$path==i,]
  prob=1
  
  e_index=c(1,which(sqc$obj=="E" & sqc$obj_idx %in% cdd_path$obj[cdd_path$cause=="E"]))
  #principle 1: effective prevention
  prob_sub=1
  for (j in elist){
    for (m in a_index[a_index<j]){
      t=sqc$time[j]-sqc$time[m]
      if (m== a_index[1]){
        prob_sub=pgamma(t, shape=k_ae, rate=r_ae)
      }else{
        prob_sub=prob_sub*pgamma(t, shape=k_ae, rate=r_ae)
      }
    }
    prob=prob*prob_sub
  }
  #principle 2: generative causes
  prob_sub=1
  for (j in 1:nrow(cdd_path)){
    t1=sqc$time[sqc$obj=="E" & sqc$obj_idx==cdd_path$obj[j]]
    
    t0=MyT0()
    
    if (cdd_path$cause[j]=="B"){
      prob_sub=dgamma(t1-t0, shape=k_be, rate=r_be)
    }

    if (cdd_path$cause[j]=="E"){
      prob_sub=MyHidden()
    }
    prob=prob*prob_sub
  }

  #principle 3: give explanation for inactivation
  prob_sub=1
  for (j in b_index){
    b_time=sqc$time[j]
    if (sum(cdd_path$cause=="B" & cdd_path$cause_idx==sqc$obj_idx[j])==0){
      prob_sub_b=1-pgamma(trial_end-b_time, shape=k_be, rate=r_be)
      #using approximation method to find the probability
      for (m in a_index){
        if ("E" %in% sqc$obj[m:j] && m<j){ #given principle 1
          next
        }
        Time=seq(b_time,ae_time[which(a_index==m)], length.out = sampling_point)
        a_time=sqc$time[m]
        df.sample=data.frame(Time)
        df.sample[,"no_prevent"]=pgamma(Time-a_time,shape=k_ae, rate=r_ae) #cannot prevent
        df.sample[df.sample$Time<a_time,"no_prevent"]=1 #before A happens, definitely cannot prevent 
        prevent_prob = (dgamma(Time-b_time, shape=k_be, rate=r_be))*(1-df.sample$`no_prevent`)
        prob_sub_b=1-(1-prob_sub_b)*(1-max(summary(ecdf(prevent_prob))))
      }
      prob_sub=prob_sub*prob_sub_b
    }
  }
  prob=prob*prob_sub
  #replicating on E baserate
  prob_sub=MyEndHidden()
  prob=prob*prob_sub
  
  #record the prob of the current path
  prob_each_path=c(prob_each_path,prob)
  #step 4: add up all paths
  #whole_prob=whole_prob+prob
  #another approch to add all path but usually get 0 when it is not the correct model
  #for R will simplify the product culculation.
  if (i==unique(valid_path$path)[1]){
    whole_prob=prob
  }else{
    whole_prob=1-(1-whole_prob)*(1-prob)
  }
}

if (whole_prob<max(prob_each_path)){
  whole_prob=max(prob_each_path)
}
#plot(prob_each_path)
model_gApB=whole_prob

#+ NG structure -------------------
#' # NG structure
sqc=sqc_raw

# generate all paths
elist=which(sqc$obj=="E")[-1]
bflag=1
p=2
sig_len_cum=c()
sig_path=as.data.frame(matrix(NA,ncol=3,nrow=0)) %>%
  setNames(c("obj","cause","cause_idx"))

for (i in elist){
  sig_path[nrow(sig_path)+1,]=c(p,"E",1)
  
  for (j in 1:i){
    if (sqc$obj[j]=="B"){
      sig_path[nrow(sig_path)+1,]=c(p,"B",bflag)
      bflag=bflag+1
    }
  }
  sig_len_cum=c(sig_len_cum,nrow(sig_path))
  
  bflag=1
  p=p+1
}
sig_len=c(sig_len_cum[1],diff(sig_len_cum))
sig_path$obj=as.numeric(sig_path$obj)
sig_path$cause_idx=as.numeric(sig_path$cause_idx)

all_path_mtx=matrix(NA,ncol=length(sig_len),nrow=0)
p=0
MyLeaf(c(1),0)

# check each path
valid_path=MyValidPath()


# delay model
whole_prob=0
b_index=which(sqc$obj=="B")
prelist=c()
prob_each_path=c()

#begin to calculate the probability for each path
for (i in unique(valid_path$path)){
  cdd_path=valid_path[valid_path$path==i,]
  prob=1
  e_index=c(1,which(sqc$obj=="E" & sqc$obj_idx %in% cdd_path$obj[cdd_path$cause=="E"]))
  #principle 2: generative causes
  prob_sub=1
  for (j in 1:nrow(cdd_path)){
    t1=sqc$time[sqc$obj=="E" & sqc$obj_idx==cdd_path$obj[j]]
    t0=MyT0()
    
    if (cdd_path$cause[j]=="B"){
      prob_sub=dgamma(t1-t0, shape=k_be, rate=r_be)
    }
    
    if (cdd_path$cause[j]=="E"){
      #principle 3: E->->E
      #no hidden baserate
      prob_sub=dgamma(t1-t0, shape=k_e, rate=r_e)
    }
    prob=prob*prob_sub
  }
  #principle 3: give explanation for inactivation
  prob_sub=1
  for (j in b_index){
    b_time=sqc$time[j]
    if (sum(cdd_path$cause=="B" & cdd_path$cause_idx==sqc$obj_idx[j])==0){
      prob_sub_beyond=1-pgamma(trial_end-b_time, shape=k_be, rate=r_be)
      #using approximation method to find the probability
      prob_sub=prob_sub*prob_sub_beyond
    }
  }
  prob=prob*prob_sub
  
  #replicating on E baserate
  prob_sub=MyEndHidden()
  prob=prob*prob_sub
  #record the prob of the current path
  prob_each_path=c(prob_each_path,prob)
  #add up all paths
  # whole_prob=whole_prob+prob
  #another approch to add all path but usually get 0 when it is not the correct model
  #for R will simplify the product culculation.
  if (i==unique(valid_path$path)[1]){
    whole_prob=prob
  }else{
    whole_prob=1-(1-whole_prob)*(1-prob)
  }
}

if (whole_prob<max(prob_each_path)){
  whole_prob=max(prob_each_path)
}
#plot(prob_each_path)
model_nAgB=whole_prob

#+ NN structure -------------------
#' # NN structure

sqc=sqc_raw

# generate all paths
elist=which(sqc$obj=="E")[-1]
p=2
sig_len_cum=c()
sig_path=as.data.frame(matrix(NA,ncol=3,nrow=0)) %>%
  setNames(c("obj","cause","cause_idx"))
for (i in elist){
  sig_path[nrow(sig_path)+1,]=c(p,"E",1)
  
  sig_len_cum=c(sig_len_cum,nrow(sig_path))
  p=p+1
  
}
sig_len=c(sig_len_cum[1],diff(sig_len_cum))
sig_path$obj=as.numeric(sig_path$obj)
sig_path$cause_idx=as.numeric(sig_path$cause_idx)

all_path_mtx=matrix(NA,ncol=length(sig_len),nrow=0)
p=0
MyLeaf(c(1),0)

# check each path
valid_path=MyValidPath()

# delay model
whole_prob=0
prelist=c()
prob_each_path=c()
#begin to calculate the probability for each path
for (i in unique(valid_path$path)){
  cdd_path=valid_path[valid_path$path==i,]
  prob=1
  
  e_index=c(1,which(sqc$obj=="E" & sqc$obj_idx %in% cdd_path$obj[cdd_path$cause=="E"]))
  #principle 2: generative causes
  prob_sub=1
  for (j in 1:nrow(cdd_path)){
    t1=sqc$time[sqc$obj=="E" & sqc$obj_idx==cdd_path$obj[j]]
    t0=MyT0()
    if (cdd_path$cause[j]=="E"){
      #principle 3: E->->E
      #using approximation method to find the probability (disjuction thinking here)
      #no hidden baserate
      prob_sub=dgamma(t1-t0, shape=k_e, rate=r_e)
    }
    prob=prob*prob_sub
  }
  
  #principle 3: give explanation for inactivation
  prob_sub=MyEndHidden()
  prob=prob*prob_sub
  #record the prob of the current path
  prob_each_path=c(prob_each_path,prob)
  #add up all paths
  # whole_prob=whole_prob+prob
  #another approch to add all path but usually get 0 when it is not the correct model
  #for R will simplify the product culculation.
  if (i==unique(valid_path$path)[1]){
    whole_prob=prob
  }else{
    whole_prob=1-(1-whole_prob)*(1-prob)
  }
}

if (whole_prob<max(prob_each_path)){
  whole_prob=max(prob_each_path)
}
#plot(prob_each_path)
model_nAnB=whole_prob

#+ NP structure  -------------------
#' # NP structure
sqc=sqc_raw
sqc$obj <- as.character(sqc$obj)
sqc$obj[sqc$obj == "A"] <- "M"
sqc$obj[sqc$obj == "B"] <- "A"
sqc$obj[sqc$obj == "M"] <- "B"

# generate all paths
elist=which(sqc$obj=="E")[-1]
eflag=1
p=2
sig_len_cum=c()
sig_path=as.data.frame(matrix(NA,ncol=3,nrow=0)) %>%
  setNames(c("obj","cause","cause_idx"))

for (i in elist){
  sig_path[nrow(sig_path)+1,]=c(p,"E",1)
  
  sig_len_cum=c(sig_len_cum,nrow(sig_path))
  p=p+1
}
sig_len=c(sig_len_cum[1],diff(sig_len_cum))
sig_path$obj=as.numeric(sig_path$obj)
sig_path$cause_idx=as.numeric(sig_path$cause_idx)

all_path_mtx=matrix(NA,ncol=length(sig_len),nrow=0)
p=0
MyLeaf(c(1),0)

# check each path
valid_path=MyValidPath()

# delay model
whole_prob=0
a_index=which(sqc$obj=="A")
prelist=("A")
ae_time=c()
prob_each_path=c()
for (i in a_index){ 
  p=i
  while (1){
    p=p+1
    if (p>nrow(sqc)){
      ae_time=c(ae_time,large_time)
      break
    }
    
    if (sqc$obj[p]=="E"){
      ae_time=c(ae_time,sqc$time[p])
      break
    }
  }
}#the max prevention time point of each A, principle 3&4

#begin to calculate the probability for each path
for (i in unique(valid_path$path)){
  cdd_path=valid_path[valid_path$path==i,]
  prob=1
  
  e_index=c(1,which(sqc$obj=="E" & sqc$obj_idx %in% cdd_path$obj[cdd_path$cause=="E"]))
  #principle 1: effective prevention
  prob_sub=1
  for (j in elist){
    for (m in a_index[a_index<j]){
      t=sqc$time[j]-sqc$time[m]
      if (m== a_index[1]){
        prob_sub=pgamma(t, shape=k_ae, rate=r_ae)
      }else{
        prob_sub=prob_sub*pgamma(t, shape=k_ae, rate=r_ae)
      }
    }
    prob=prob*prob_sub
  }
  #principle 2: generative causes
  prob_sub=1
  for (j in 1:nrow(cdd_path)){
    
    t1=sqc$time[sqc$obj=="E" & sqc$obj_idx==cdd_path$obj[j]]
    t0=MyT0()
    
    if (cdd_path$cause[j]=="E"){
      prob_sub=MyHidden()
    }
    prob=prob*prob_sub
  }
  
  #principle 3: give explanation for inactivation
  prob_sub=MyEndHidden()
  prob=prob*prob_sub
  #record the prob of the current path
  prob_each_path=c(prob_each_path,prob)
  #add up all paths
  # whole_prob=whole_prob+prob
  #another approch to add all path but usually get 0 when it is not the correct model
  #for R will simplify the product culculation.
  if (i==unique(valid_path$path)[1]){
    whole_prob=prob
  }else{
    whole_prob=1-(1-whole_prob)*(1-prob)
  }
}

if (whole_prob<max(prob_each_path)){
  whole_prob=max(prob_each_path)
}
#plot(prob_each_path)
model_nApB=whole_prob

#+ PG structure -------------------
#' # PG structure
sqc=sqc_raw

# generate all paths
elist=which(sqc$obj=="E")[-1]
bflag=1
p=2
sig_len_cum=c()
sig_path=as.data.frame(matrix(NA,ncol=3,nrow=0)) %>%
  setNames(c("obj","cause","cause_idx"))

for (i in elist){
  sig_path[nrow(sig_path)+1,]=c(p,"E",1)
  
  for (j in 1:i){
    if (sqc$obj[j]=="B"){
      sig_path[nrow(sig_path)+1,]=c(p,"B",bflag)
      bflag=bflag+1
    }
  }
  sig_len_cum=c(sig_len_cum,nrow(sig_path))
  
  bflag=1
  p=p+1
}
sig_len=c(sig_len_cum[1],diff(sig_len_cum))
sig_path$obj=as.numeric(sig_path$obj)
sig_path$cause_idx=as.numeric(sig_path$cause_idx)

all_path_mtx=matrix(NA,ncol=length(sig_len),nrow=0)
p=0
MyLeaf(c(1),0)

# check each path
valid_path=MyValidPath()

# delay model
whole_prob=0
a_index=which(sqc$obj=="A")
b_index=which(sqc$obj=="B")
prelist=c("A")
ae_time=c()
prob_each_path=c()
for (i in a_index){
  p=i
  while (1){
    p=p+1
    if (p>nrow(sqc)){
      ae_time=c(ae_time,large_time)
      break
    }
    
    if (sqc$obj[p]=="E"){
      ae_time=c(ae_time,sqc$time[p])
      break
    }
  }
}#the max prevention time point of each A

#begin to calculate the probability for each path
for (i in unique(valid_path$path)){
  cdd_path=valid_path[valid_path$path==i,]
  prob=1
  
  e_index=c(1,which(sqc$obj=="E" & sqc$obj_idx %in% cdd_path$obj[cdd_path$cause=="E"]))
  #principle 1: effective prevention
  prob_sub=1
  for (j in elist){
    for (m in a_index[a_index<j]){
      t=sqc$time[j]-sqc$time[m]
      if (m== a_index[1]){
        prob_sub=pgamma(t, shape=k_ae, rate=r_ae)
      }else{
        prob_sub=prob_sub*pgamma(t, shape=k_ae, rate=r_ae)
      }
    }
    prob=prob*prob_sub
  }
  #principle 2: generative causes
  prob_sub=1
  for (j in 1:nrow(cdd_path)){
    t1=sqc$time[sqc$obj=="E" & sqc$obj_idx==cdd_path$obj[j]]
    
    t0=MyT0()
    
    if (cdd_path$cause[j]=="B"){
      prob_sub=dgamma(t1-t0, shape=k_be, rate=r_be)
    }

    
    if (cdd_path$cause[j]=="E"){
      prob_sub=MyHidden()
    }
    prob=prob*prob_sub
  }

  #principle 3: give explanation for inactivation
  prob_sub=1
  for (j in b_index){
    b_time=sqc$time[j]
    if (sum(cdd_path$cause=="B" & cdd_path$cause_idx==sqc$obj_idx[j])==0){
      prob_sub_b=1-pgamma(trial_end-b_time, shape=k_be, rate=r_be)
      #using approximation method to find the probability
      for (m in a_index){
        if ("E" %in% sqc$obj[m:j] && m<j){ #given principle 1
          next
        }
        Time=seq(b_time,ae_time[which(a_index==m)], length.out = sampling_point)
        a_time=sqc$time[m]
        df.sample=data.frame(Time)
        df.sample[,"no_prevent"]=pgamma(Time-a_time,shape=k_ae, rate=r_ae) #cannot prevent
        df.sample[df.sample$Time<a_time,"no_prevent"]=1 #before A happens, definitely cannot prevent 
        prevent_prob = (dgamma(Time-b_time, shape=k_be, rate=r_be))*(1-df.sample$`no_prevent`)
        prob_sub_b=1-(1-prob_sub_b)*(1-max(summary(ecdf(prevent_prob))))
      }
      prob_sub=prob_sub*prob_sub_b
    }
  }
  prob=prob*prob_sub
  #replicating on E baserate
  prob_sub=MyEndHidden()
  prob=prob*prob_sub
  
  #record the prob of the current path
  prob_each_path=c(prob_each_path,prob)
  #step 4: add up all paths
  #whole_prob=whole_prob+prob
  #another approch to add all path but usually get 0 when it is not the correct model
  #for R will simplify the product culculation.
  if (i==unique(valid_path$path)[1]){
    whole_prob=prob
  }else{
    whole_prob=1-(1-whole_prob)*(1-prob)
  }
}

if (whole_prob<max(prob_each_path)){
  whole_prob=max(prob_each_path)
}
#plot(prob_each_path)
model_pAgB=whole_prob

#+ PN structure -------------------
#' # PN structure
sqc=sqc_raw

# generate all paths
elist=which(sqc$obj=="E")[-1]
eflag=1
p=2
sig_len_cum=c()
sig_path=as.data.frame(matrix(NA,ncol=3,nrow=0)) %>%
  setNames(c("obj","cause","cause_idx"))

for (i in elist){
  sig_path[nrow(sig_path)+1,]=c(p,"E",1)
  
  sig_len_cum=c(sig_len_cum,nrow(sig_path))
  p=p+1
}
sig_len=c(sig_len_cum[1],diff(sig_len_cum))
sig_path$obj=as.numeric(sig_path$obj)
sig_path$cause_idx=as.numeric(sig_path$cause_idx)

all_path_mtx=matrix(NA,ncol=length(sig_len),nrow=0)
p=0
MyLeaf(c(1),0)

# check each path
valid_path=MyValidPath()


# delay model
whole_prob=0
a_index=which(sqc$obj=="A")
prelist=("A")
ae_time=c()
prob_each_path=c()
for (i in a_index){ 
  p=i
  while (1){
    p=p+1
    if (p>nrow(sqc)){
      ae_time=c(ae_time,large_time)
      break
    }
    
    if (sqc$obj[p]=="E"){
      ae_time=c(ae_time,sqc$time[p])
      break
    }
  }
}#the max prevention time point of each A, principle 3&4

#begin to calculate the probability for each path
for (i in unique(valid_path$path)){
  cdd_path=valid_path[valid_path$path==i,]
  prob=1
  
  e_index=c(1,which(sqc$obj=="E" & sqc$obj_idx %in% cdd_path$obj[cdd_path$cause=="E"]))
  #principle 1: effective prevention
  prob_sub=1
  for (j in elist){
    for (m in a_index[a_index<j]){
      t=sqc$time[j]-sqc$time[m]
      if (m== a_index[1]){
        prob_sub=pgamma(t, shape=k_ae, rate=r_ae)
      }else{
        prob_sub=prob_sub*pgamma(t, shape=k_ae, rate=r_ae)
      }
    }
    prob=prob*prob_sub
  }
  #principle 2: generative causes
  prob_sub=1
  for (j in 1:nrow(cdd_path)){
    
    t1=sqc$time[sqc$obj=="E" & sqc$obj_idx==cdd_path$obj[j]]
    t0=MyT0()
    
    if (cdd_path$cause[j]=="E"){
      prob_sub=MyHidden()
    }
    prob=prob*prob_sub
  }
  
  #principle 3: give explanation for inactivation
  prob_sub=MyEndHidden()
  prob=prob*prob_sub
  #record the prob of the current path
  prob_each_path=c(prob_each_path,prob)
  #add up all paths
  # whole_prob=whole_prob+prob
  #another approch to add all path but usually get 0 when it is not the correct model
  #for R will simplify the product culculation.
  if (i==unique(valid_path$path)[1]){
    whole_prob=prob
  }else{
    whole_prob=1-(1-whole_prob)*(1-prob)
  }
}

if (whole_prob<max(prob_each_path)){
  whole_prob=max(prob_each_path)
}
#plot(prob_each_path)
model_pAnB=whole_prob

#+ PP structure  -------------------
#' # PP structure 
sqc=sqc_raw

# generate all paths
elist=which(sqc$obj=="E")[-1]
p=2
sig_len_cum=c()
sig_path=as.data.frame(matrix(NA,ncol=3,nrow=0)) %>%
  setNames(c("obj","cause","cause_idx"))
for (i in elist){
  sig_path[nrow(sig_path)+1,]=c(p,"E",1)
  
  sig_len_cum=c(sig_len_cum,nrow(sig_path))
  p=p+1
}
sig_len=c(sig_len_cum[1],diff(sig_len_cum))
sig_path$obj=as.numeric(sig_path$obj)
sig_path$cause_idx=as.numeric(sig_path$cause_idx)

all_path_mtx=matrix(NA,ncol=length(sig_len),nrow=0)
p=0
MyLeaf(c(1),0)

# check each path
valid_path=MyValidPath()

# delay model
whole_prob=0
a_index=which(sqc$obj=="A"|sqc$obj=="B") #include both A and B here
prelist=c("A","B")
ae_time=c()
prob_each_path=c()
for (i in a_index){ 
  p=i
  while (1){
    p=p+1
    if (p>nrow(sqc)){
      ae_time=c(ae_time,large_time)
      break
    }
    
    if (sqc$obj[p]=="E"){
      ae_time=c(ae_time,sqc$time[p])
      break
    }
  }
}#the max prevention time point of each A, principle 3&4

#begin to calculate the probability for each path
for (i in unique(valid_path$path)){
  cdd_path=valid_path[valid_path$path==i,]
  prob=1
  e_index=c(1,which(sqc$obj=="E" & sqc$obj_idx %in% cdd_path$obj[cdd_path$cause=="E"]))
  #principle 1: effective prevention
  prob_sub=1
  for (j in elist){
    for (m in a_index[a_index<j]){
      t=sqc$time[j]-sqc$time[m]
      if (m== a_index[1]){
        prob_sub=pgamma(t, shape=k_ae, rate=r_ae)
      }else{
        prob_sub=prob_sub*pgamma(t, shape=k_ae, rate=r_ae)
      }
    }
    prob=prob*prob_sub
  }
  #the end of principle 1
  #principle 2: generative causes
  prob_sub=1
  for (j in 1:nrow(cdd_path)){
    t1=sqc$time[sqc$obj=="E" & sqc$obj_idx==cdd_path$obj[j]]
    t0=MyT0()
    if (cdd_path$cause[j]=="E"){
      prob_sub=MyHidden()
    }
    prob=prob*prob_sub
  }
  #principle 3: give explanation for inactivation: E baserate
  prob_sub=MyEndHidden()
  prob=prob*prob_sub
  
  #record the prob of the current path
  prob_each_path=c(prob_each_path,prob)
  #add up all paths
  # whole_prob=whole_prob+prob
  #another approch to add all path but usually get 0 when it is not the correct model
  #for R will simplify the product culculation.
  if (i==unique(valid_path$path)[1]){
    whole_prob=prob
  }else{
    whole_prob=1-(1-whole_prob)*(1-prob)
  }
}

if (whole_prob<max(prob_each_path)){
  whole_prob=max(prob_each_path)
}
#plot(prob_each_path)
model_pApB=whole_prob