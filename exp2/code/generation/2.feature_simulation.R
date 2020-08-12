library(tidyr)
library(dplyr)
library(ggplot2)
library(Rmisc)

#+ load packages and functions  -------------------
#' # load packages and functions
k_ab = 1
r_ab = 0.2 #control component: m=5,var=25
trial_end=20
whole_trial_end=20

s_e = 10
# s_a = 3
# s_b = 3
s_ab=3

k_pe_r = 36
r_pe_r = 12 #preventative p->e: m=3,var=0.25
k_pe_u = 9
r_pe_u = 3 #preventative p->e: m=3,var=1

k_ge_r = 9
r_ge_r = 6  #generative g->e: m=1.5,var=0.25
k_ge_u = 2.25
r_ge_u = 1.5  #generative g->e: m=1.5,var=1

sim_sqc=20000
eachgroup=18
onfor=0.35+0.1


myStatsSummary <- function(sqc_s,num_model,type=1){
  A_next=rep(NA,s_ab)
  B_next=rep(NA,s_ab)
  A_num=rep(NA,s_ab)
  B_num=rep(NA,s_ab)
  a_count=0
  b_count=0
  for (i in 1:nrow(sqc_s)){
    if (sqc_s$obj[i]=="E"){
      next
    }
    
    if (sqc_s$obj[i]=="A"){
      a_count=a_count+1
      nextflag=1
      numflag=1
      for (j in i:nrow(sqc_s)){
        if (j!=i && sqc_s$obj[j]!="E"){
          numflag=0
          if (is.na(A_num[a_count])){
            A_num[a_count]=0
          }
        }
        if (sqc_s$obj[j]=="E"){
          if (nextflag){
            A_next[a_count]=sqc_s$time[j]-sqc_s$time[i]
            nextflag=0
          }
          if (numflag){
            if (is.na(A_num[a_count])){
              A_num[a_count]=1
            }else{
              A_num[a_count]=A_num[a_count]+1
            }
          }
        }
      }
      if (is.na(A_next[a_count])){
        A_next[a_count]=trial_end-sqc_s$time[i]
      }
    }
    
    if (sqc_s$obj[i]=="B"){
      b_count=b_count+1
      nextflag=1
      numflag=1
      for (j in i:nrow(sqc_s)){
        if (j!=i && sqc_s$obj[j]!="E"){
          numflag=0
          if (is.na(B_num[b_count])){
            B_num[b_count]=0
          }
        }
        if (sqc_s$obj[j]=="E"){
          if (nextflag){
            B_next[b_count]=sqc_s$time[j]-sqc_s$time[i]
            nextflag=0
          }
          if (numflag){
            if (is.na(B_num[b_count])){
              B_num[b_count]=1
            }else{
              B_num[b_count]=B_num[b_count]+1
            }
          }
        }
      }
      if (is.na(B_next[b_count])){
        B_next[b_count]=trial_end-sqc_s$time[i]
      }
    }
  }
  
  if (type==1){
    df.expect$A_delay1[(snum-1)*eachgroup+num_model]<<-A_next[1]
    df.expect$A_delay2[(snum-1)*eachgroup+num_model]<<-A_next[2]
    df.expect$A_delay3[(snum-1)*eachgroup+num_model]<<-A_next[3]
    
    df.expect$B_delay1[(snum-1)*eachgroup+num_model]<<-B_next[1]
    df.expect$B_delay2[(snum-1)*eachgroup+num_model]<<-B_next[2]
    df.expect$B_delay3[(snum-1)*eachgroup+num_model]<<-B_next[3]
    
    df.expect$A_num1[(snum-1)*eachgroup+num_model]<<-A_num[1]
    df.expect$A_num2[(snum-1)*eachgroup+num_model]<<-A_num[2]
    df.expect$A_num3[(snum-1)*eachgroup+num_model]<<-A_num[3]
    
    df.expect$B_num1[(snum-1)*eachgroup+num_model]<<-B_num[1]
    df.expect$B_num2[(snum-1)*eachgroup+num_model]<<-B_num[2]
    df.expect$B_num3[(snum-1)*eachgroup+num_model]<<-B_num[3]
    
  }else{
    df.sqc$A_delay1[1]<<-A_next[1]
    df.sqc$A_delay2[1]<<-A_next[2]
    df.sqc$A_delay3[1]<<-A_next[3]
    
    df.sqc$B_delay1[1]<<-B_next[1]
    df.sqc$B_delay2[1]<<-B_next[2]
    df.sqc$B_delay3[1]<<-B_next[3]
    
    df.sqc$A_num1[1]<<-A_num[1]
    df.sqc$A_num2[1]<<-A_num[2]
    df.sqc$A_num3[1]<<-A_num[3]
    
    df.sqc$B_num1[1]<<-B_num[1]
    df.sqc$B_num2[1]<<-B_num[2]
    df.sqc$B_num3[1]<<-B_num[3]
  }
}

MyRemove <- function (v.org,v.prevent,v.range){
  return(v.org[!((v.org>v.prevent[1] & v.org<v.range[1])|(v.org>v.prevent[2] & v.org<v.range[2])|(v.org>v.prevent[3] & v.org<v.range[3]))])
}

MyReshape <-function(sti_sqc){
  sti_sqc=sti_sqc%>% subset(time<whole_trial_end)
  sti_sqc = sti_sqc[order(sti_sqc$time), ] %>% subset(time<whole_trial_end)
  return(sti_sqc)
}

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


df.expect=as.data.frame(matrix(NA,ncol=17,nrow=sim_sqc*eachgroup)) %>%
  setNames(c("simulation","A_pro","B_pro","rel","baserate",
             "A_delay1","A_delay2","A_delay3",
             "B_delay1","B_delay2","B_delay3",
             "A_num1","A_num2","A_num3",
             "B_num1","B_num2","B_num3"))

snum=15017

while(1){
  baserate=5
  baserate_var=0.25
  
  r_e_r = baserate/baserate_var
  k_e_r = r_e_r*baserate
  
  k_e_u=1
  r_e_u=1/baserate
  
  A = sort(runif(s_ab,0,whole_trial_end))
  B = sort(runif(s_ab,0,whole_trial_end))
  
  #one way for irregular
  # while(1){
  #   E_raw_r = rgamma(s_e, shape = k_e_r, rate = r_e_r)
  #   E_r = E_raw_r %>% cumsum() %>% c(0) 
  #   E_r=E_r[E_r<whole_trial_end]
  #   
  #   E_u = runif(length(E_r)-1,0,whole_trial_end) %>% c(0) 
  #   if(length(E_r)==length(E_u) && min(dgamma(E_raw_r,shape = k_e_r, rate = r_e_r))>=0.05){
  #     break
  #   } #make the number even
  # }
  #anather way for irregular
  while(1){
    E_raw_r = rgamma(s_e, shape = k_e_r, rate = r_e_r)
    E_r = E_raw_r %>% cumsum() %>% c(0)
    E_r=E_r[E_r<whole_trial_end]
    
    E_raw_u = rgamma(s_e, shape = k_e_u, rate = r_e_u)
    E_u = E_raw_u %>% cumsum() %>% c(0)
    E_u=E_u[E_u<whole_trial_end]
    if(min(dgamma(E_raw_r,shape = k_e_r, rate = r_e_r))>=0.05){
      break
    }
  }
  
  while(1){
    A_g_raw_r=rgamma(s_ab, shape = k_ge_r, rate = r_ge_r)
    B_g_raw_r=rgamma(s_ab, shape = k_ge_r, rate = r_ge_r)
    A_p_raw_r=rgamma(s_ab, shape = k_pe_r, rate = r_pe_r)
    B_p_raw_r=rgamma(s_ab, shape = k_pe_r, rate = r_pe_r)
    
    
    A_g_r_check=min(dgamma(A_g_raw_r, shape = k_ge_r, rate = r_ge_r))>=0.05
    B_g_r_check=min(dgamma(B_g_raw_r, shape = k_ge_r, rate = r_ge_r))>=0.05
    A_p_r_check=min(dgamma(A_p_raw_r, shape = k_pe_r, rate = r_pe_r))>=0.05
    B_p_r_check=min(dgamma(B_p_raw_r, shape = k_pe_r, rate = r_pe_r))>=0.05
    
    
    if (A_g_r_check*B_g_r_check*A_p_r_check*B_p_r_check){
      break
    }
    
  }
  
  A_g_r=A_g_raw_r+A
  B_g_r=B_g_raw_r+B
  A_p_r=A_p_raw_r+A
  B_p_r=B_p_raw_r+B
  
  if (min(diff(sort(c(A,B,E_r,A_g_r,B_g_r))))<=onfor | min(diff(sort(c(A,B,E_u,A_g_r,B_g_r))))<=onfor){
    next
  }
  
  #GG
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
  
  snum=snum+1
  #summary
  for (i in 1:eachgroup){
    df.expect$simulation[(snum-1)*eachgroup+i]=snum
    df.expect$A_pro[(snum-1)*eachgroup+i]=a_pro_list[i]
    df.expect$B_pro[(snum-1)*eachgroup+i]=b_pro_list[i]
    df.expect$rel[(snum-1)*eachgroup+i]=ru_pro_list[i]
    df.expect$baserate[(snum-1)*eachgroup+i]=baserate
    sqc_s=get(sti_list[i])
    myStatsSummary(sqc_s,i)
  }

  Sys.sleep(0.01)
  print(snum)
  if (snum>=sim_sqc){
    break
  }
}

df.expect.bk=df.expect
df.expect=df.expect %>% subset(simulation<=15019)
save(df.expect,file="df.expect.Rda")

load("df.expect1.Rda")
df.big=df.expect
load("df.expect4.Rda")
df.big=rbind(df.big,df.expect)
load("df.expect2.Rda")
df.big=rbind(df.big,df.expect)
load("df.expect5.Rda")
df.big=rbind(df.big,df.expect)
load("df.expect3.Rda")
df.big=rbind(df.big,df.expect)
df.expect=df.big
