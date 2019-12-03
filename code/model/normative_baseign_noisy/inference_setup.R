#' ---
#' title: Time & Prevention <br> Stimuli Inference <br> preventative A & generative B
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
    t0=sqc$time[sqc$obj==cdd_path$cause[j] & sqc$obj_idx==cdd_path$cause_idx[j]]#caution!!!
  }else{
    t0=sqc$time[sqc$obj==cdd_path$cause[j] & sqc$total_idx ==max(e_index[e_index<elist[cdd_path$obj[j]-1]])]#caution!!!
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
  j=max(e_index) #caution!!!
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
r_e=2.5 #e->e: m=5,var=0.25 -> 2

k_ae=4.5
r_ae=1.5 #preventative a->e: m=3,var=0.25 -> 2

k_be=1.125
r_be=0.75  #generative b->e: m=1.5,var=0.25 -> 2

trial_end=20
large_time=100
sampling_point=200 #for one generative one preventative
sim_subject=1000

#+ path selection principles-------------------
#' # path selection principles
#' #### PRINCIPLES:<br>
#' 1. one effect cannot be caused by two causes.<br>
#' 2. one cause cannot cause two effects.<br>
#' 3. the E activation can be a cause later only when it is caused by the previous E (or it is the initialization).<br>
#' 
#' #### ALGORITHMS:<br>
#' 1. suppose each effect can be caused by its all previous causes.<br>
#' 2. follow 1 to generate all paths and follow "principles" to delete some paths.<br>
#' 
#' #### NOTES:<br>
#' The path number itself can serve as a path/order model,
#' and the paths would be used in the delay model.<br>
#' 
#' #### FYI: Two heuristic principles 
#' 4. if B1 and B2 both sucessfully activate E, then E1 is quicker than E2 (i.e., in "B1, B2, E1, E2" order,
#' we only consider B1->E1 and B2->E2).<br>
#' 5. if there is an index gap between Es' causes (e.g., E2 was caused by B1 while E3 was caused by B3), 
#' there must exist a preventative cause to prevent the hidden index (e.g., B2).<br>

#+ delay model principles -------------------
#' # delay model principles
#' #### PRINCIPLES:<br>
#' 1. as a broad-scope prevention A, all successful E must be outside the functional range of A, i.e., A->E < AE. 
#' to simplify, A->E < AE for the E and its closest previous A (necessary principle 1).<br> 
#' 2. for successful B cause, the cause->effect possibility would be considered (sufficient principle 1).<br> 
#' 3. for successful E cause (baserate), we not only consider E->E(cause->effect), but also consider E->->E,
#' where the hidden E was blocked by one A, and E->->->E where the hidden E was blocked by two A, respectively, and so on
#' (now we just consider at most three A)(sufficient principle 2).<br> 
#' 4. all unsuccessful causes should be explained by "outside the trial" or "be prevented" (necessary principle 2).<br>
#' 
#' #### ALGORITHMS:<br>
#' 1. for each path, calculate the probability of first and second principles firstly.
#' 2. then calculate the probability of last two principles, GIVEN PRINCIPLE1.
#' 3. integrate the probabilities of 4 principles by production.
#' 3. finally, add up the probabilities from all paths.
#' 
#' #### NOTES: <br>
#' 1. if there are multiple ways to realize, use p=1-(1-p_sub1)*(1-p_sub2) rather than p=p_sub1+p_sub2
#' unless we are certain p_subs are independent on each other.<br>
#' 2. someone might wonder whether principle 3,4 are independent on each other given 
#' principle 1, which might be true since we didn't assume an exact A->E in principle 1, only controlling that A->E < AE.
#' Actually after principle 3, the valid A->E range would be smaller than principle 1, but we just simplify the procedure and not consider this.
#' We might later revise to calculate principle 3 and 4 together.
#' 
#' #### IMPORTANTS: <br>
#' 1. the models mentioned above are all based on probability inference. However, we can also use SIMULATION approach, 
#' simulating about 10^6 samples and compare them with original sitmulus(both path and delay models can use this approach).
#' 2. the simulation model can avoid ignorances/simplifications 
#' in normative probabilistic calculation
#' 3. while the normative delay model, can tell us WHICH PATH 
#' is most likely for the underline mechanism of the stimulus.
#' Accordingly, we might ask participants to draw the exact causal activation paths.
#' 
#' #### OTHER THINGS FOR DISCUSSION:
#' The frequencies are different in different conditions: 
#' both generative > one generative one non-causal> 
#' one generative one preventative > both prevenrative
#' no idea about how to control yet 
#' (one potential way might be to select stimuli carefully to decrease the difference)