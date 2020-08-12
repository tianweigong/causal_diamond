library(dplyr)
library(tidyr)
win_len=5
whole_trial_end=20
s_ab=3
s_e=15
k_pe_r = 36
r_pe_r = 12 #preventative p->e: m=3,var=0.25
# k_pe_u = 9
# r_pe_u = 3 #preventative p->e: m=3,var=1
k_ge_r = 9
r_ge_r = 6  #generative g->e: m=1.5,var=0.25
# k_ge_u = 2.25
# r_ge_u = 1.5  #generative g->e: m=1.5,var=1
baserate=5
baserate_var=0.25
r_e_r = baserate/baserate_var
k_e_r = r_e_r*baserate
k_e_u=1
r_e_u=1/baserate
eachgroup=9*2
sti_list=c("sqc.gg_r","sqc.gg_u",
           "sqc.gn_r","sqc.gn_u",
           "sqc.gp_r","sqc.gp_u",
           "sqc.ng_r","sqc.ng_u",
           "sqc.nn_r","sqc.nn_u",
           "sqc.np_r","sqc.np_u",
           "sqc.pg_r","sqc.pg_u",
           "sqc.pn_r","sqc.pn_u",
           "sqc.pp_r","sqc.pp_u")
a_pro_list=c(rep("G",6),rep("N",6),rep("P",6))
b_pro_list=rep(c(rep("G",2),rep("N",2),rep("P",2)),3)
ru_pro_list=rep(c("r","u"),9)

MyRemove <- function (v.org,v.prevent,v.range){
  for (k in 1: length(v.prevent)){
    v.org=v.org[!(v.org>v.prevent[k] & v.org<v.range[k])]
  }
  return(v.org)
}

MyReshape <-function(sti_sqc){
  # sti_sqc = sti_sqc[order(sti_sqc$time), ] %>% subset(time<whole_trial_end)
  sti_sqc = sti_sqc[order(sti_sqc$time), ]
  return(sti_sqc)
}

myStatsSummaryGen <- function(sqc_s,num_model,df.expect=0){
  A_delay=rep(NA,s_ab)
  B_delay=rep(NA,s_ab)
  A_numw=rep(NA,s_ab)
  B_numw=rep(NA,s_ab)
  A_numi=rep(NA,s_ab)
  B_numi=rep(NA,s_ab)
  
  sqc_ab=sqc_s %>% subset(obj %in% c("A","B"))
  
  a_count=0
  b_count=0
  for (i in 1:(nrow(sqc_ab)-1)){
    sqc_sub=sqc_s %>% subset(time>sqc_ab$time[i] & time<sqc_ab$time[i+1])
    if (sqc_ab$obj[i]=="A"){
      a_count=a_count+1
      A_numi[a_count]=sum((sqc_sub$obj=="E"))
    }
    if (sqc_ab$obj[i]=="B"){
      b_count=b_count+1
      B_numi[b_count]=sum((sqc_sub$obj=="E"))
    }
  }
  
  a_count=0
  b_count=0
  for (i in 1:(nrow(sqc_ab))){
    win_end=sqc_ab$time[i]+win_len
    sqc_sub=sqc_s %>% subset(time>sqc_ab$time[i] & time<win_end)
    if (sqc_ab$obj[i]=="A"){
      a_count=a_count+1
      if(win_end<whole_trial_end){
        A_numw[a_count]=sum((sqc_sub$obj=="E"))
      }
    }
    if (sqc_ab$obj[i]=="B"){
      b_count=b_count+1
      if(win_end<whole_trial_end){
        B_numw[b_count]=sum((sqc_sub$obj=="E"))
      }
    }
  }
  
  a_count=0
  b_count=0
  for (i in 1:(nrow(sqc_ab))){
    sqc_sub=sqc_s %>% subset(time>sqc_ab$time[i] & obj=="E")
    delay_e=min(sqc_sub$time)-sqc_ab$time[i]
    if (sqc_ab$obj[i]=="A"){
      a_count=a_count+1
      # if(sqc_ab$time[i]+win_len<whole_trial_end){
        A_delay[a_count]=delay_e
      # }
    }
    if (sqc_ab$obj[i]=="B"){
      b_count=b_count+1
      # if(sqc_ab$time[i]+win_len<whole_trial_end){
        B_delay[b_count]=delay_e
      # }
    }
  }
  
  df.expect$A_delay1[num_model]<-A_delay[1]
  df.expect$A_delay2[num_model]<-A_delay[2]
  df.expect$A_delay3[num_model]<-A_delay[3]
  df.expect$B_delay1[num_model]<-B_delay[1]
  df.expect$B_delay2[num_model]<-B_delay[2]
  df.expect$B_delay3[num_model]<-B_delay[3]
  
  df.expect$A_numw1[num_model]<-A_numw[1]
  df.expect$A_numw2[num_model]<-A_numw[2]
  df.expect$A_numw3[num_model]<-A_numw[3]
  df.expect$B_numw1[num_model]<-B_numw[1]
  df.expect$B_numw2[num_model]<-B_numw[2]
  df.expect$B_numw3[num_model]<-B_numw[3]
  
  df.expect$A_numi1[num_model]<-A_numi[1]
  df.expect$A_numi2[num_model]<-A_numi[2]
  df.expect$A_numi3[num_model]<-A_numi[3]
  df.expect$B_numi1[num_model]<-B_numi[1]
  df.expect$B_numi2[num_model]<-B_numi[2]
  df.expect$B_numi3[num_model]<-B_numi[3]
  
  return(df.expect)
}

MyFeatureGen<-function(snum){
  df.expect=as.data.frame(matrix(NA,ncol=23,nrow=eachgroup)) %>%
    setNames(c("simulation","A_pro","B_pro","rel","baserate",
               "A_delay1","A_delay2","A_delay3",
               "B_delay1","B_delay2","B_delay3",
               "A_numw1","A_numw2","A_numw3",
               "B_numw1","B_numw2","B_numw3",
               "A_numi1","A_numi2","A_numi3",
               "B_numi1","B_numi2","B_numi3"))
  
  A = sort(runif(s_ab,0,whole_trial_end))
  B = sort(runif(s_ab,0,whole_trial_end))
  

  E_raw_r = rgamma(s_e, shape = k_e_r, rate = r_e_r)
  E_r = E_raw_r %>% cumsum() %>% c(0)
  # E_r=E_r[E_r<whole_trial_end]
  
  E_raw_u = rgamma(s_e, shape = k_e_u, rate = r_e_u)
  E_u = E_raw_u %>% cumsum() %>% c(0)
  # E_u=E_u[E_u<whole_trial_end]

  A_g_raw_r=rgamma(s_ab, shape = k_ge_r, rate = r_ge_r)
  B_g_raw_r=rgamma(s_ab, shape = k_ge_r, rate = r_ge_r)
  A_p_raw_r=rgamma(s_ab, shape = k_pe_r, rate = r_pe_r)
  B_p_raw_r=rgamma(s_ab, shape = k_pe_r, rate = r_pe_r)
  
  A_g_r=A_g_raw_r+A
  B_g_r=B_g_raw_r+B
  A_p_r=A_p_raw_r+A
  B_p_r=B_p_raw_r+B
  
  sqc.gg_r=data.frame(obj = c(rep("A", length(A)), rep("B", length(B)), 
                              rep("E", length(E_r)), rep("E",length(A_g_r)),rep("E",length(B_g_r))), 
                      time =c(A, B, E_r, A_g_r, B_g_r),
                      org=c(rep("N", length(A)), rep("N", length(B)), 
                            rep("E", length(E_r)), rep("A",length(A_g_r)),rep("B",length(B_g_r))))  %>% MyReshape()
  
  sqc.gg_u=data.frame(obj = c(rep("A", length(A)), rep("B", length(B)), 
                              rep("E", length(E_u)), rep("E",length(A_g_r)),rep("E",length(B_g_r))), 
                      time =c(A, B, E_u, A_g_r, B_g_r),
                      org=c(rep("N", length(A)), rep("N", length(B)), 
                            rep("E", length(E_u)), rep("A",length(A_g_r)),rep("B",length(B_g_r)))) %>% MyReshape()
  
  #GN
  sqc.gn_r= data.frame(obj = c(rep("A", length(A)), rep("B", length(B)), 
                               rep("E", length(E_r)), rep("E",length(A_g_r))),
                       time =c(A, B, E_r, A_g_r),
                       org=c(rep("N", length(A)), rep("N", length(B)), 
                             rep("E", length(E_r)), rep("A",length(A_g_r)))) %>% MyReshape()
  
  sqc.gn_u= data.frame(obj = c(rep("A", length(A)), rep("B", length(B)), 
                               rep("E", length(E_u)), rep("E",length(A_g_r))),
                       time =c(A, B, E_u, A_g_r),
                       org=c(rep("N", length(A)), rep("N", length(B)), 
                             rep("E", length(E_u)), rep("A",length(A_g_r))))  %>% MyReshape()
  
  
  #GP
  E_r_rm=MyRemove(E_r,B,B_p_r)
  E_u_rm=MyRemove(E_u,B,B_p_r)
  A_g_r_rm=MyRemove(A_g_r,B,B_p_r)
  
  sqc.gp_r=data.frame(obj = c(rep("A", length(A)), rep("B", length(B)), 
                              rep("E", length(E_r_rm)), rep("E",length(A_g_r_rm))),
                      time =c(A, B, E_r_rm, A_g_r_rm),
                      org=c(rep("N", length(A)), rep("N", length(B)), 
                            rep("E", length(E_r_rm)), rep("A",length(A_g_r_rm))))  %>% MyReshape()
  
  sqc.gp_u=data.frame(obj = c(rep("A", length(A)), rep("B", length(B)), 
                              rep("E", length(E_u_rm)), rep("E",length(A_g_r_rm))),
                      time =c(A, B, E_u_rm, A_g_r_rm),
                      org=c(rep("N", length(A)), rep("N", length(B)), 
                            rep("E", length(E_u_rm)), rep("A",length(A_g_r_rm))))  %>% MyReshape()
  
  #NG
  sqc.ng_r=data.frame(obj = c(rep("A", length(A)), rep("B", length(B)), 
                              rep("E", length(E_r)), rep("E",length(B_g_r))), 
                      time =c(A, B, E_r,B_g_r),
                      org=c(rep("N", length(A)), rep("N", length(B)), 
                            rep("E", length(E_r)), rep("B",length(B_g_r))))  %>% MyReshape()
  
  sqc.ng_u=data.frame(obj = c(rep("A", length(A)), rep("B", length(B)), 
                              rep("E", length(E_u)), rep("E",length(B_g_r))), 
                      time =c(A, B, E_u,B_g_r),
                      org=c(rep("N", length(A)), rep("N", length(B)), 
                            rep("E", length(E_u)), rep("B",length(B_g_r))))  %>% MyReshape()
  
  #NN
  sqc.nn_r=data.frame(obj = c(rep("A", length(A)), rep("B", length(B)), 
                              rep("E", length(E_r))), 
                      time =c(A, B, E_r),
                      org=c(rep("N", length(A)), rep("N", length(B)), 
                            rep("E", length(E_r))))  %>% MyReshape()
  
  sqc.nn_u=data.frame(obj = c(rep("A", length(A)), rep("B", length(B)), 
                              rep("E", length(E_u))), 
                      time =c(A, B, E_u),
                      org=c(rep("N", length(A)), rep("N", length(B)), 
                            rep("E", length(E_u))))  %>% MyReshape()
  
  #NP
  E_r_rm=MyRemove(E_r,B,B_p_r)
  E_u_rm=MyRemove(E_u,B,B_p_r)
  
  sqc.np_r=data.frame(obj = c(rep("A", length(A)), rep("B", length(B)), 
                              rep("E", length(E_r_rm))),
                      time =c(A, B, E_r_rm),
                      org=c(rep("N", length(A)), rep("N", length(B)), 
                            rep("E", length(E_r_rm))))  %>% MyReshape()
  
  sqc.np_u=data.frame(obj = c(rep("A", length(A)), rep("B", length(B)), 
                              rep("E", length(E_u_rm))),
                      time =c(A, B, E_u_rm),
                      org=c(rep("N", length(A)), rep("N", length(B)), 
                            rep("E", length(E_u_rm))))  %>% MyReshape()
  #PG
  E_r_rm=MyRemove(E_r,A,A_p_r)
  E_u_rm=MyRemove(E_u,A,A_p_r)
  B_g_r_rm=MyRemove(B_g_r,A,A_p_r)
  
  sqc.pg_r=data.frame(obj = c(rep("A", length(A)), rep("B", length(B)), 
                              rep("E", length(E_r_rm)), rep("E",length(B_g_r_rm))),
                      time =c(A, B, E_r_rm, B_g_r_rm),
                      org=c(rep("N", length(A)), rep("N", length(B)), 
                            rep("E", length(E_r_rm)), rep("B",length(B_g_r_rm))))  %>% MyReshape()
  
  sqc.pg_u=data.frame(obj = c(rep("A", length(A)), rep("B", length(B)), 
                              rep("E", length(E_u_rm)), rep("E",length(B_g_r_rm))),
                      time =c(A, B, E_u_rm, B_g_r_rm),
                      org=c(rep("N", length(A)), rep("N", length(B)), 
                            rep("E", length(E_u_rm)), rep("B",length(B_g_r_rm))))  %>% MyReshape()
  
  #PN
  E_r_rm=MyRemove(E_r,A,A_p_r)
  E_u_rm=MyRemove(E_u,A,A_p_r)
  
  sqc.pn_r=data.frame(obj = c(rep("A", length(A)), rep("B", length(B)), 
                              rep("E", length(E_r_rm))),
                      time =c(A, B, E_r_rm),
                      org=c(rep("N", length(A)), rep("N", length(B)), 
                            rep("E", length(E_r_rm)))) %>% MyReshape()
  
  sqc.pn_u=data.frame(obj = c(rep("A", length(A)), rep("B", length(B)), 
                              rep("E", length(E_u_rm))),
                      time =c(A, B, E_u_rm),
                      org=c(rep("N", length(A)), rep("N", length(B)), 
                            rep("E", length(E_u_rm)))) %>% MyReshape()
  
  #PP
  E_r_rm=MyRemove(E_r,A,A_p_r)
  E_r_rm=MyRemove(E_r_rm,B,B_p_r)
  
  E_u_rm=MyRemove(E_u,A,A_p_r)
  E_u_rm=MyRemove(E_u_rm,B,B_p_r)
  
  sqc.pp_r=data.frame(obj = c(rep("A", length(A)), rep("B", length(B)), 
                              rep("E", length(E_r_rm))), 
                      time =c(A, B, E_r_rm),
                      org=c(rep("N", length(A)), rep("N", length(B)), 
                            rep("E", length(E_r_rm)))) %>% MyReshape()
  
  sqc.pp_u=data.frame(obj = c(rep("A", length(A)), rep("B", length(B)), 
                              rep("E", length(E_u_rm))), 
                      time =c(A, B, E_u_rm),
                      org=c(rep("N", length(A)), rep("N", length(B)), 
                            rep("E", length(E_u_rm)))) %>% MyReshape()
  
  #summary
  for (i in 1:eachgroup){
    df.expect$simulation[i]=snum
    df.expect$A_pro[i]=a_pro_list[i]
    df.expect$B_pro[i]=b_pro_list[i]
    df.expect$rel[i]=ru_pro_list[i]
    df.expect$baserate[i]=baserate
    sqc_s=get(sti_list[i])
    df.expect=myStatsSummaryGen(sqc_s,i,df.expect)
  }
  return(df.expect)
}

MyFeaturePac <- function(pacnum){
  df.expect=data.frame()
  for (i in 1: (sim_sqc/pac)){
    df.expect.sub=MyFeatureGen((pacnum-1)*(sim_sqc/pac)+i)
    df.expect=rbind(df.expect,df.expect.sub)
  }
  save(df.expect,file=paste("simulation/",pacnum,".Rda",sep=""))
}

sim_sqc=10000
pac=50
t0=Sys.time()
wholelist=list()
for (k in 1:pac){
  wholelist[[k]]=k
}
library(parallel)
mclapply(wholelist, MyFeaturePac)
Sys.time()-t0

t0=Sys.time()
df.expect.big=data.frame()
for (k in 1:pac){
  load(paste("simulation/",k,".Rda",sep=""))
  df.expect.big=rbind(df.expect.big,df.expect)
}
df.expect=df.expect.big
# save(df.expect,file="df.expect..Rda")
# save(df.expect,file="df.expect.w3.Rda")
Sys.time()-t0
# save(df.expect,file="df.expect.w3.5.Rda")
# save(df.expect,file="df.expect.w4.5.Rda")
# save(df.expect,file="df.expect.w5.Rda")

