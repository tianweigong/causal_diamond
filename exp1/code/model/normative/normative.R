#+ set parameters -------------------
#' # set parameters
#' please make sure that the parameters are identical to stimuli generation file.
k_e = 100
r_e = 20 #e->e: m=5,var=0.25
k_pe = 36
r_pe = 12 #preventative p->e: m=3,var=0.25
k_ge = 9
r_ge = 6  #generative g->e: m=1.5,var=0.25

# k_e = 50 #var=0.5
# r_e = 10
# k_pe = 18
# r_pe = 6
# k_ge = 4.5
# r_ge = 3

# k_e = 33.33333 #var=0.75
# r_e = 6.666667
# k_pe = 12
# r_pe = 4
# k_ge = 3
# r_ge = 2

# k_e = 25 #var=1
# r_e = 5
# k_pe = 9
# r_pe = 3
# k_ge = 2.25
# r_ge = 1.5

# k_e = 20 #var=1.25
# r_e = 4
# k_pe = 7.2
# r_pe = 2.4
# k_ge = 1.8
# r_ge = 1.2

# k_e = 16.66667 #var=1.5
# r_e = 3.33333
# k_pe = 6
# r_pe = 2
# k_ge = 1.5
# r_ge = 1

# k_e = 14.2857 #var=1.75
# r_e = 2.85714
# k_pe = 5.142857
# r_pe = 1.7142857
# k_ge = 1.285714
# r_ge = 0.85714

# k_e = 12.5 #var=2
# r_e = 2.5
# k_pe = 4.5
# r_pe = 1.5
# k_ge = 1.125
# r_ge = 0.75

trial_end=20
whole_trial_end=20
large_time=100 # a second far beyond trail_end
sampling_point=200 #for one generative one preventative

filepath="../../../stimulus/exp/"
filelist=c("gg1.Rda","gg2.Rda","pg1.Rda","pg2.Rda","ng1.Rda","ng2.Rda",
           "gp1.Rda","gp2.Rda","pp1.Rda","pp2.Rda","np1.Rda","np2.Rda",
           "gn1.Rda","gn2.Rda","pn1.Rda","pn2.Rda","nn1.Rda","nn2.Rda")
st_a=c("G","G","G","N","N","N","P","P","P")
st_b=c("G","N","P","G","N","P","G","N","P")
pro_a=rep(rep(c("G","P","N"),each=2),3)
pro_b=rep(c("G","P","N"),each=6)
df.model=model_result=as.data.frame(matrix(NA,ncol=6,nrow=0)) %>%
  setNames(c("trial_id","A_pro","B_pro","A_state","B_state","ratio"))

for (i in 1:length(filelist)){
  filename=paste(filepath,i,".",filelist[i],sep="")
  load(filename)
  sim_prob=MyOnlineInfer_nor(sqc_raw_long,k_e,r_e,k_pe,r_pe,k_ge,r_ge)
  
  dt.model=as.data.frame(matrix(NA,ncol=6,nrow=9)) %>%
    setNames(c("trial_id","A_pro","B_pro","A_state","B_state","ratio"))
  dt.model$trial_id=i
  dt.model$A_pro=pro_a[i]
  dt.model$B_pro=pro_b[i]
  
  for (mo in 1:9){
    dt.model$A_state[mo]=st_a[mo]
    dt.model$B_state[mo]=st_b[mo]
    dt.model$ratio[mo]=sim_prob[mo]
  }
 
  save(dt.model,file=paste(i,".Rda",sep=""))
}

dt.model.big=data.frame()
for (i in 1:length(filelist)){
  load(paste(i,".Rda",sep=""))
  dt.model.big=rbind(dt.model.big,dt.model)
}
dt.model=dt.model.big
save(dt.model,file="df.nor.Rda")

