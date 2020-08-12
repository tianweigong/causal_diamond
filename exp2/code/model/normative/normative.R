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

# baserate=5 #var=0.5
# baserate_var=0.5
# k_pe = 18
# r_pe = 6
# k_ge = 4.5
# r_ge = 3

# baserate=5 #var=0.75
# baserate_var=0.75
# k_pe = 12
# r_pe = 4
# k_ge = 3
# r_ge = 2

# baserate=5 #var=1
# baserate_var=1
# k_pe = 9
# r_pe = 3
# k_ge = 2.25
# r_ge = 1.5

# baserate=5 #var=1.25
# baserate_var=1.25
# k_pe = 7.2
# r_pe = 2.4
# k_ge = 1.8
# r_ge = 1.2

# baserate=5 #var=1.5
# baserate_var=1.5
# k_pe = 6
# r_pe = 2
# k_ge = 1.5
# r_ge = 1

# baserate=5 #var=1.75
# baserate_var=1.75
# k_pe = 5.142857
# r_pe = 1.7142857
# k_ge = 1.285714
# r_ge = 0.85714

# baserate=5 #var=2
# baserate_var=2
# k_pe = 4.5
# r_pe = 1.5
# k_ge = 1.125
# r_ge = 0.75

trial_end=20
whole_trial_end=20
large_time=100 # a second far beyond trail_end
sampling_point=200 #for one generative one preventative

filepath="../../../stimulus/exp2/"

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
a_pro_list=c(rep("G",6),rep("N",6),rep("P",6))
b_pro_list=rep(c(rep("G",2),rep("N",2),rep("P",2)),3)
ru_pro_list=rep(c("r","u"),9)

MyNorMod <-function(seed){
  
  dt.model=as.data.frame(matrix(NA,ncol=8,nrow=0)) %>%
    setNames(c("seed","sti_id","A_pro","B_pro","ru_pro","A_state","B_state","ratio"))
  
  filename=paste(filepath,"seed",seed,"/allstimuli.Rda",sep = "")
  load(filename)
  for (sti_num in 1:18){
    r_e_r = baserate/baserate_var
    k_e_r = r_e_r*baserate
    k_e_u=1
    r_e_u=1/baserate
    dt.nor=as.data.frame(matrix(NA,ncol=8,nrow=9)) %>%
      setNames(c("seed","sti_id","A_pro","B_pro","ru_pro","A_state","B_state","ratio"))

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
    }
    dt.model=rbind(dt.model,dt.nor)
  }
  save(dt.model,file=paste(seed,".Rda",sep=""))
}

seednum=18
wholelist=list()
for (k in 1:seednum){
  wholelist[[k]]=k
}
library(parallel)
mclapply(wholelist, MyNorMod,mc.cores=4)


dt.model.big=data.frame()
for (i in 1:seednum){
  load(paste(i,".Rda",sep=""))
  dt.model.big=rbind(dt.model.big,dt.model)
}
dt.model=dt.model.big
save(dt.model,file="df.nor.Rda")