library(tidyr)
library(dplyr)
library(ggplot2)
library(matrixStats)

sim_sqc=300

k_e_r = 100
r_e_r = 20 #e->e: m=5,var=0.25

k_e_u=1
r_e_u=0.2

s_e = 10
# s_a = 3
# s_b = 3
s_ab=3

k_pe_r = 36
r_pe_r = 12 #preventative p->e: m=3,var=0.25
k_ge_r = 9
r_ge_r = 6  #generative g->e: m=1.5,var=0.25

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
  for (k in 1:length(sti_list)){
    mysqc=get(sti_list[k])
    x=c(x,paste(mysqc$time%>%round(3),collapse=""))
  }
  return(length(unique(x))==length(sti_list))
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

MyStiCheGen<-function(snum){
  
  baserate=5
  baserate_var=0.25
  
  r_e_r = baserate/baserate_var
  k_e_r = r_e_r*baserate
  
  k_e_u=1
  r_e_u=1/baserate
  
  # C= sort(runif(s_ab+s_ab,0,whole_trial_end-1))
  # A = c(C[1],C[5],C[6])
  # B = c(C[2],C[3],C[4])
  
  A = sort(runif(s_ab,0,whole_trial_end))
  B = sort(runif(s_ab,0,whole_trial_end))

  #anather way for irregular
  E_raw_r = rgamma(s_e, shape = k_e_r, rate = r_e_r)
  E_r = E_raw_r %>% cumsum() %>% c(0)
  E_r=E_r[E_r<whole_trial_end]
  
  E_raw_u = rgamma(s_e, shape = k_e_u, rate = r_e_u)
  E_u = E_raw_u %>% cumsum() %>% c(0)
  E_u=E_u[E_u<whole_trial_end]
  
  A_g_raw_r=rgamma(s_ab, shape = k_ge_r, rate = r_ge_r)
  B_g_raw_r=rgamma(s_ab, shape = k_ge_r, rate = r_ge_r)
  A_p_raw_r=rgamma(s_ab, shape = k_pe_r, rate = r_pe_r)
  B_p_raw_r=rgamma(s_ab, shape = k_pe_r, rate = r_pe_r)
  
  A_g_r=A_g_raw_r+A
  B_g_r=B_g_raw_r+B
  A_p_r=A_p_raw_r+A
  B_p_r=B_p_raw_r+B
  
  # if (min(diff(sort(c(A,B,E_r,A_g_r,B_g_r))))<=onfor | min(diff(sort(c(A,B,E_u,A_g_r,B_g_r))))<=onfor){
  #   next
  # }
  
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
  
  myorder=data.frame(obj = c(rep("A", length(A)), rep("B", length(B))
                             ), 
                     time =c(A, B))
  save(list=sti_list %>% c("myorder"),file=paste("simulated_stimuli/",snum,".Rda",sep=""))
}


# wholelist=list()
# for (k in 1:sim_sqc){
#   wholelist[[k]]=k
# }
# library(parallel)
# mclapply(wholelist, MyStiCheGen)

st_a=c("G","G","G","N","N","N","P","P","P")
st_b=c("G","N","P","G","N","P","G","N","P")

myProEst <- function(seed){
  dt.model.big=dt.numi.big=dt.delayw.big=dt.delayi.big=as.data.frame(matrix(NA,ncol=11,nrow=0)) %>%
    setNames(c("seed","sti_id","A_pro","B_pro","ru_pro","A_state","B_state","ratio","ratio2","ratio3","ratio4"))
  
  filename=paste("simulated_stimuli/",seed,".Rda",sep = "")
  load(filename)
  
  for (sti_num in 1:18){
    dt.model=as.data.frame(matrix(NA,ncol=11,nrow=9)) %>%
      setNames(c("seed","sti_id","A_pro","B_pro","ru_pro","A_state","B_state","ratio","ratio2","ratio3","ratio4"))
    

    a_pro=a_pro_list[sti_num]
    b_pro=b_pro_list[sti_num]
    sti_name=sti_list[sti_num]
    sqc_raw_long=get(sti_name)
    ru_pro=ru_pro_list[sti_num]
    
    if (ru_pro=="r"){k_e=k_e_r;r_e=r_e_r;k_pe=k_pe_r;r_pe=r_pe_r;k_ge=k_ge_r;r_ge=r_ge_r}
    if (ru_pro=="u"){k_e=k_e_u;r_e=r_e_u;k_pe=k_pe_r;r_pe=r_pe_r;k_ge=k_ge_r;r_ge=r_ge_r}
    
    dt.model$seed=seed
    dt.model$sti_id=sti_num
    dt.model$A_pro=a_pro
    dt.model$B_pro=b_pro
    dt.model$ru_pro=ru_pro
    fea_sim_prob=MyOnlineInfer_fea(sqc_raw_long,ru_pro,5)
    # fea_sim_prob=MyOnlineInfer_nor(sqc_raw_long,k_e,r_e,k_pe,r_pe,k_ge,r_ge)
    
    for (mo in 1:9){
      dt.model$A_state[mo]=st_a[mo]
      dt.model$B_state[mo]=st_b[mo]
    }
    
    for (mo in 1:9){
      dt.model$ratio[mo]=fea_sim_prob$delayi[mo]
      dt.model$ratio2[mo]=fea_sim_prob$numi[mo]
      dt.model$ratio3[mo]=fea_sim_prob$delayw[mo]
      dt.model$ratio4[mo]=fea_sim_prob$numw[mo]
    }
    # for (mo in 1:9){
    # dt.model$ratio[mo]=fea_sim_prob[mo]/sum(fea_sim_prob)
    # }
    
    dt.model.big=rbind(dt.model.big,dt.model)
  }
  save(dt.model.big,file=paste("simulated_results/",seed,"feature.Rda",sep=""))
}

# wholelist=list()
# x=list.files(pattern = "\\.Rda$")
# y=c(1:300)
# z=c(1:300)
# for (i in 1:300){
#   fn=paste(y[i],"feature.Rda",sep = "")
#   if (fn %in% x){
#     z=setdiff(z,y[i])
#   }
# }
# wholelist=list()
# for (k in 1:sim_sqc){
#   wholelist[[k]]=k
# }
# library(parallel)
# mclapply(wholelist, myProEst,mc.cores=4)

# dt.model=data.frame()
# for (k in 1:sim_sqc){
#   filename=load(paste("simulated_results/",k,"feature.Rda",sep = ""))
#   dt.model=rbind(dt.model,dt.model.big)
# }
# save(dt.model,file="dt.model.fea.Rda")
# 
# dt.model=data.frame()
# for (k in 1:sim_sqc){
#   filename=load(paste("simulated_results/",k,"feature.Rda",sep = ""))
#   dt.model=rbind(dt.model,dt.model.big)
# }
# save(dt.model,file="dt.model.fea.Rda")