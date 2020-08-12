s_ab=3

filepath="../../../stimulus/exp/"
filelist=c("gg1.Rda","gg2.Rda","pg1.Rda","pg2.Rda","ng1.Rda","ng2.Rda",
           "gp1.Rda","gp2.Rda","pp1.Rda","pp2.Rda","np1.Rda","np2.Rda",
           "gn1.Rda","gn2.Rda","pn1.Rda","pn2.Rda","nn1.Rda","nn2.Rda")
st_a=c("G","G","G","N","N","N","P","P","P")
st_b=c("G","N","P","G","N","P","G","N","P")
pro_a=rep(rep(c("G","P","N"),each=2),3)
pro_b=rep(c("G","P","N"),each=6)


for (i in 1:length(filelist)){
  filename=paste(filepath,i,".",filelist[i],sep="")
  load(filename)
  sim_prob=MyOnlineInfer_fea(sqc_raw_long,"r",5)
  # sim_prob=MyOnlineInfer_fea(sqc_raw_long,"u",5)
  dt.model=as.data.frame(matrix(NA,ncol=9,nrow=9)) %>%
    setNames(c("trial_id","A_pro","B_pro","A_state","B_state","delayw","numw","delayi","numi"))
  
  dt.model$trial_id=i
  dt.model$A_pro=pro_a[i]
  dt.model$B_pro=pro_b[i]
  for (mo in 1:9){
    dt.model$A_state[mo]=st_a[mo]
    dt.model$B_state[mo]=st_b[mo]
    
    dt.model$delayi[mo]=sim_prob$delayi[mo]
    dt.model$numi[mo]=sim_prob$numi[mo]
    dt.model$delayw[mo]=sim_prob$delayw[mo]
    dt.model$numw[mo]=sim_prob$numw[mo]
  }
  save(dt.model,file=paste(i,".Rda",sep=""))
}


dt.model.big=data.frame()
for (i in 1:length(filelist)){
  load(paste(i,".Rda",sep=""))
  dt.model.big=rbind(dt.model.big,dt.model)
}
dt.model=dt.model.big
save(dt.model,file="df.fea.Rda")
# save(dt.model,file="df.fea.mismatch.Rda")
