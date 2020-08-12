#+ General settings, echo = FALSE, results = 'hide' -------------------
knitr::opts_chunk$set(warning = FALSE, message = FALSE)

#+ load packages -------------------
#' # load packages
library(tidyr)
library(dplyr)
library(ggplot2)
# rm(list=ls())
#+ test parameters -------------------
#' # test parameters
data.frame(Time = seq(0, 10, length.out = 10000)) %>%
  mutate(exp = dgamma(Time, shape = 1, rate =0.2 )) %>%
  gather(fun, Probability, c(exp)) %>%
  mutate(Form = factor(fun, levels = c('exp'),
                       labels = c('demo'))) %>%
  ggplot(aes(x = Time, y = Probability)) +
  # geom_vline(xintercept = c(3.5)) +
  # annotate("rect",xmin = 0,xmax = 3.5,ymin = 0,ymax = 0.8,alpha = 0.2,fill = "blue") +
  geom_point(size=0.01) +
  theme_bw() +
  theme(text = element_text(size=16),
        panel.grid = element_blank(),
        # axis.text=element_blank(),
        # axis.ticks.x=element_blank(),
        axis.title=element_blank())+
  scale_x_continuous(limits = c(0,7),breaks=c(0:7))

#+ set parameters -------------------
#' # set parameters

k_e_r = 100
r_e_r = 20 #e->e: m=5,var=0.25

k_e_u=1
r_e_u=0.2

k_ab = 1
r_ab = 0.2 #control component: m=5,var=25

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

while (1){

  baserate=5
  baserate_var=0.25
  
  r_e_r = baserate/baserate_var
  k_e_r = r_e_r*baserate
  
  k_e_u=1
  r_e_u=1/baserate
  
  C= sort(runif(s_ab+s_ab,0,whole_trial_end-1))
  A = c(C[1],C[5],C[6])
  B = c(C[2],C[3],C[4])
  
  # A = sort(runif(s_ab,0,whole_trial_end))
  # B = sort(runif(s_ab,0,whole_trial_end))
  
  
  #one way for irregular
  # while(1){
  #   E_raw_r = rgamma(s_e, shape = k_e_r, rate = r_e_r)
  #   E_r = E_raw_r %>% cumsum() %>% c(0) 
  #   E_r=E_r[E_r<whole_trial_end]
  #   
  #   E_u = runif(length(E_r)-1,0,whole_trial_end) %>% c(0) 
  #   if(length(E_r)==length(E_u) && min(dgamma(E_raw_r,shape = k_e_r, rate = r_e_r))>=0.05){
  #     break
  #   }
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
  
  if (!MyIdtCheck()){
    next
  }
  
  
  break
}



dt.sti.big=as.data.frame(matrix(NA,ncol=11,nrow=0)) %>%
  setNames(c("sti_id","A_pro","B_pro","A_state","B_state","normative","number","delay","normative_raw","number_raw","delay_raw"))

for (sti_num in 1: 18){
  dt.sti=as.data.frame(matrix(NA,ncol=11,nrow=9)) %>%
    setNames(c("sti_id","A_pro","B_pro","A_state","B_state","normative","number","delay","normative_raw","number_raw","delay_raw"))

  a_pro=a_pro_list[sti_num]
  b_pro=b_pro_list[sti_num]
  sti_name=sti_list[sti_num]
  sqc_raw_long=get(sti_name)
  ru_pro=ru_pro_list[sti_num]
  
  write.csv(sqc_raw_long, paste(sti_name,".csv",sep = ""), row.names = F, quote = F)
  if (ru_pro=="r"){k_e=k_e_r;r_e=r_e_r;k_pe=k_pe_r;r_pe=r_pe_r;k_ge=k_ge_r;r_ge=r_ge_r}
  if (ru_pro=="u"){k_e=k_e_r;r_e=r_e_r;k_pe=k_pe_r;r_pe=r_pe_r;k_ge=k_ge_r;r_ge=r_ge_r}
  # if (ru_pro=="r"){k_e=k_e_u;r_e=r_e_u;k_pe=k_pe_r;r_pe=r_pe_r;k_ge=k_ge_r;r_ge=r_ge_r}
  sti_id=sti_num
  nor_sim_prob=MyOnlineInfer_nor(sqc_raw_long,k_e,r_e,k_pe,r_pe,k_ge,r_ge)
  fea_sim_prob=MyOnlineInfer_fea(sqc_raw_long,ru_pro,baserate)
  
  dt.sti$sti_id=sti_id
  dt.sti$A_pro=a_pro
  dt.sti$B_pro=b_pro
  for (mo in 1:9){
    dt.sti$A_state[mo]=st_a[mo]
    dt.sti$B_state[mo]=st_b[mo]
    dt.sti$normative[mo]=nor_sim_prob[mo]/sum(nor_sim_prob)
    dt.sti$number[mo]=fea_sim_prob$num[mo]/sum(fea_sim_prob$num)
    dt.sti$delay[mo]=fea_sim_prob$delay[mo]/sum(fea_sim_prob$delay)
    
    dt.sti$normative_raw[mo]=nor_sim_prob[mo]
    dt.sti$number_raw[mo]=fea_sim_prob$num[mo]
    dt.sti$delay_raw[mo]=fea_sim_prob$delay[mo]
  }
  
  dt.sti.big=rbind(dt.sti.big,dt.sti)
}

pic.sti.nor=MyMod6(dt.sti.big,dt.sti.big$normative)
pic.sti.fea.num=MyMod6(dt.sti.big,dt.sti.big$number)
pic.sti.fea.delay=MyMod6(dt.sti.big,dt.sti.big$delay)

pic.sti.nor %>%ggplot()+
  geom_bar(mapping=aes(x=cpn, y=ratio,fill=state),stat='identity',position="dodge",color="black")+
  geom_point(pic.sti.fea.num,mapping =aes(x=cpn, y=ratio,fill=state), stat="identity",
             position=position_dodge(.9),size=1.8,shape=5)+
  geom_point(pic.sti.fea.delay,mapping =aes(x=cpn, y=ratio,fill=state), stat="identity",
             position=position_dodge(.9),size=1.8,shape=16)+
  facet_wrap(~sti_id,nrow=3,labeller = as_labeller(pic.label))+
  theme_bw()+
  scale_fill_manual(values=c("#E4F0EC","#EDB1A4","#6282b1"),
                    labels =  c(" Generative  "," Non-causal  "," Preventative  "))+
  scale_color_manual(values=c("black","black","black"))

ggsave("model.pdf")

savelist=c(sti_list,"A","B","E_r","E_u","A_g_r","B_g_r","A_p_r","B_p_r","dt.sti.big")
save(list=savelist, file = "allstimuli.Rda")
cor(dt.sti.big$normative,dt.sti.big$number)
cor(dt.sti.big$normative,dt.sti.big$delay)
E_u

