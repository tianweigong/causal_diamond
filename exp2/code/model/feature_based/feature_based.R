#+ load packages and functions  -------------------
#' # load packages and functions

#see feature_simulation
s_ab=3
trial_end=20
whole_trial_end=20

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

MyFeaMod <- function(seed){
  
  dt.model=as.data.frame(matrix(NA,ncol=11,nrow=0)) %>%
    setNames(c("seed","sti_id","A_pro","B_pro","ru_pro","A_state","B_state","delayw","numw","delayi","numi"))
  
  filename=paste(filepath,"seed",seed,"/allstimuli.Rda",sep = "")
  load(filename)
  
  for (sti_num in 1:18){
    dt.dea=as.data.frame(matrix(NA,ncol=11,nrow=9)) %>%
      setNames(c("seed","sti_id","A_pro","B_pro","ru_pro","A_state","B_state","delayw","numw","delayi","numi"))
    
    a_pro=a_pro_list[sti_num]
    b_pro=b_pro_list[sti_num]
    sti_name=sti_list[sti_num]
    sqc_raw_long=get(sti_name)
    ru_pro=ru_pro_list[sti_num]
    
    dt.dea$seed=seed
    dt.dea$sti_id=sti_num
    dt.dea$A_pro=a_pro
    dt.dea$B_pro=b_pro
    dt.dea$ru_pro=ru_pro
    sim_prob=MyOnlineInfer_fea(sqc_raw_long,ru_pro,5)
    # ru_pro_rev=setdiff(c("u","r"),ru_pro)
    # sim_prob=MyOnlineInfer_fea(sqc_raw_long,ru_pro_rev,5)
    for (mo in 1:9){
      dt.dea$A_state[mo]=st_a[mo]
      dt.dea$B_state[mo]=st_b[mo]
      
      dt.dea$delayi[mo]=sim_prob$delayi[mo]
      dt.dea$numi[mo]=sim_prob$numi[mo]
      dt.dea$delayw[mo]=sim_prob$delayw[mo]
      dt.dea$numw[mo]=sim_prob$numw[mo]
    }
    dt.model=rbind(dt.model,dt.dea)
  }
  save(dt.model,file=paste(seed,".Rda",sep=""))
}

seednum=18
wholelist=list()
for (k in 1:seednum){
  wholelist[[k]]=k
}
library(parallel)
mclapply(wholelist, MyFeaMod,mc.cores=4)

dt.model.big=data.frame()
for (i in 1:seednum){
  load(paste(i,".Rda",sep=""))
  dt.model.big=rbind(dt.model.big,dt.model)
}
dt.model=dt.model.big
# save(dt.model,file="df.fea.Rda")
save(dt.model,file="df.fea.mismatch.Rda")
