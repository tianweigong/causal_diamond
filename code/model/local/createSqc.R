library(stats4)

#DIY grid search

sqc_create<-function(mylist){
  a<-mylist[3]
  likeli<-c()
  tr1<-mylist[1]
  tr2<-mylist[2]
  md<-MySeqFit(tr1,tr2)
  md$q1=tr1
  md$q2=tr2
  md$acc=MyModAcc(md)
  # save(md,file = paste(a,".Rda")) #this code will generate 1001 files
}


MyModAcc <- function(md.acc){
  pro_a=c("G","P","N","G","P","N","G","P","N")
  pro_b=c("G","G","G","P","P","P","N","N","N")
  v.md.acc=c()
  for (i in 1:9){
    for (j in 1:2){
      choice_sub=md.acc %>% subset(trial_id==(i-1)*2+j & A_state==pro_a[i] & B_state==pro_b[i])
      v.md.acc=c(v.md.acc,choice_sub$ratio[1])
    }
  }
  return(mean(v.md.acc))
}



wholelist=list()
p=0
for (x1 in seq(10,0,length.out = 1001)){
  # for (x2 in exp(1-seq(10,0,length.out = 1001))){
    x2=exp(1-x1)
    p=p+1
    wholelist[[p]]=c(x1,x2,p)
  # }
}

ref=matrix(NA,nrow=0,ncol=3)
p=0
for (x1 in seq(10,0,length.out = 1001)){
    x2=exp(1-x1)
  # for (x2 in exp(1-seq(10,0,length.out = 1001))){
    p=p+1
    ref=rbind(ref,c(x1,x2,p))
  # }
}

library(parallel)
mclapply(wholelist, sqc_create)

#sel
load(paste(1,".Rda"))
md_cur=md
md_cur$md_id=1
md.large=md_cur


for (i in 2:nrow(ref)){
  load(paste(i,".Rda"))
  if (paste(md$ratio,collapse="")!=paste(md_cur$ratio,collapse="")){
    md$md_id=i
    md.large=rbind(md.large,md)
    md_cur=md
  }
}

length(unique(md.large$md_id))
unires=c()

#again
load(paste(nrow(ref),".Rda"))
md_cur=md
md_cur$md_id=1
md.large=md_cur
unires=c(paste(md_cur$ratio,collapse=""))

for (i in nrow(ref):1){
  load(paste(i,".Rda"))
  if (paste(md$ratio,collapse="") %in% unires){
  }else{
    md$md_id=i
    md.large=rbind(md.large,md)
    unires=c(unires,paste(md$ratio,collapse=""))
  }
}

length(unique(md.large$md_id))
save(md.large,file="md.large.Rda")
