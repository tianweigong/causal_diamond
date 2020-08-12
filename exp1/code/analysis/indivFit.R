library(stats4)
sublist=as.character(unique(df.final$subject))

#+ individual fit -------------------
#' # individual fit
MySigFit9 <-function(sub_id){
  df.sig=df.final %>% subset(subject==sub_id)
  st_a=pro_a=c("G","P","N","G","P","N","G","P","N")
  st_b=pro_b=c("G","G","G","P","P","P","N","N","N")
  
  md.sub=as.data.frame(matrix(NA,ncol=7,nrow=18*9)) %>%
    setNames(c("fit_id","A_pro","B_pro","A_state","B_state","count","hm"))
  
  for (i in 1:18){
    for (j in 1:length(st_a)){
      p=(i-1)*9+j
      md.sub$fit_id[p]=i
      md.sub$A_pro[p]=pro_a[ceiling(i/2)]
      md.sub$B_pro[p]=pro_b[ceiling(i/2)]
      md.sub$A_state[p]=st_a[j]
      md.sub$B_state[p]=st_b[j]
      md.sub$count[p]=df.sig %>% subset(fit_id==i & A_state== st_a[j] & B_state==st_b[j]) %>% nrow()
      md.sub$hm[p]=md.sub$count[p]/(df.sig %>% subset(fit_id==i) %>% nrow())
    }
  }
  
  return(md.sub)
}


ind_mod1<-function(par,vals){
  a<-par
  likeli=c()
  for (i in 1:18){
    choice_sub=vals %>% subset(fit_id==i)
    for (m in 1:9){
      sfm=exp(choice_sub$ratio[m]/a)/sum(exp(choice_sub$ratio/a))
      likeli=c(likeli, log(sfm)*choice_sub$count[m])
    }
  }
  -sum(likeli)
}

ind_mod2<-function(par,vals){
  likeli=c()
  p1=par[1]
  p2=par[2]
  for (i in 1:18){
    choice_sub=vals %>% subset(fit_id==i)
    for (m in 1:9){
      sfm=exp(choice_sub$ratio1[m]/p1+choice_sub$ratio2[m]/p2)/sum(exp(choice_sub$ratio1/p1+choice_sub$ratio2/p2))
      likeli=c(likeli, log(sfm)*choice_sub$count[m])
    }
  }
  -sum(likeli)
}

ind_mod_rd<-function(vals){
  likeli=c()
  for (i in 1:18){
    choice_sub=vals %>% subset(fit_id==i)
    for (m in 1:9){
      sfm=0.11
      likeli=c(likeli, log(sfm)*choice_sub$count[m])
    }
  }
  -sum(likeli)
}


MyIndivFit<-function(df.sig,subid){
  df.indiv.sub= as.data.frame(matrix(NA,ncol=5,nrow=0)) %>%
    setNames(c("subject","md","par1","par2","BIC"))
  #normative
  vals=merge(md.nor,df.sig,by=c("fit_id","A_pro","B_pro","A_state","B_state"))
  x=optim(0.5,ind_mod1,vals=vals,method = "Brent",lower = -1000,upper=1000)
  df.indiv.sub[nrow(df.indiv.sub)+1,]=c(subid,"normative",as.numeric(x$par),NA,log(18)+2*as.numeric(x$value))
  #one-actual
  vals=merge(md.one,df.sig,by=c("fit_id","A_pro","B_pro","A_state","B_state"))
  x=optim(0.5,ind_mod1,vals=vals,method = "Brent",lower = -1000,upper=1000)
  df.indiv.sub[nrow(df.indiv.sub)+1,]=c(subid,"one_actual",as.numeric(x$par),NA,log(18)+2*as.numeric(x$value))
  
  #feature-based (window-based)
  vals=merge(md.fea.w,df.sig,by=c("fit_id","A_pro","B_pro","A_state","B_state"))
  x=optim(c(0.5,0.5),ind_mod2,vals=vals)
  df.indiv.sub[nrow(df.indiv.sub)+1,]=c(subid,"feature_window",as.numeric(x$par[1]),as.numeric(x$par[2]),log(18)*2+2*as.numeric(x$value))
  #delay(window-based)
  vals=merge(md.fea.w.d,df.sig,by=c("fit_id","A_pro","B_pro","A_state","B_state"))
  x=optim(0.5,ind_mod1,vals=vals,method = "Brent",lower = -1000,upper=1000)
  df.indiv.sub[nrow(df.indiv.sub)+1,]=c(subid,"delay_window",as.numeric(x$par),NA,log(18)+2*as.numeric(x$value))
  #count(window-based)
  vals=merge(md.fea.w.c,df.sig,by=c("fit_id","A_pro","B_pro","A_state","B_state"))
  x=optim(0.5,ind_mod1,vals=vals,method = "Brent",lower = -1000,upper=1000)
  df.indiv.sub[nrow(df.indiv.sub)+1,]=c(subid,"num_window",as.numeric(x$par),NA,log(18)+2*as.numeric(x$value))
  
  #feature-based (intervention-based)
  vals=merge(md.fea.i,df.sig,by=c("fit_id","A_pro","B_pro","A_state","B_state"))
  x=optim(c(0.5,0.5),ind_mod2,vals=vals)
  df.indiv.sub[nrow(df.indiv.sub)+1,]=c(subid,"feature_intervention",as.numeric(x$par[1]),as.numeric(x$par[2]),log(18)*2+2*as.numeric(x$value))
  #delay(intervention-based)
  vals=merge(md.fea.i.d,df.sig,by=c("fit_id","A_pro","B_pro","A_state","B_state"))
  x=optim(0.5,ind_mod1,vals=vals,method = "Brent",lower = -1000,upper=1000)
  df.indiv.sub[nrow(df.indiv.sub)+1,]=c(subid,"delay_intervention",as.numeric(x$par),NA,log(18)+2*as.numeric(x$value))
  #count(intervention-based)
  vals=merge(md.fea.i.c,df.sig,by=c("fit_id","A_pro","B_pro","A_state","B_state"))
  x=optim(0.5,ind_mod1,vals=vals,method = "Brent",lower = -1000,upper=1000)
  df.indiv.sub[nrow(df.indiv.sub)+1,]=c(subid,"num_intervention",as.numeric(x$par),NA,log(18)+2*as.numeric(x$value))
  
  #rd
  vals=df.sig
  df.indiv.sub[nrow(df.indiv.sub)+1,]=c(subid,"random",NA,NA,2*(ind_mod_rd(vals)))
  return(df.indiv.sub)
}

MySubIndivFit<-function(i){
  subid=as.character(sublist[i])
  df.sig=MySigFit9(subid)
  df.indiv.sub=MyIndivFit(df.sig,subid)
  save(df.indiv.sub,file=paste("indivFit/",i,".Rda",sep = ""))
}


# 
# wholelist=list()
# for (k in 1:length(sublist)){
#   wholelist[[k]]=k
# }
# library(parallel)
# mclapply(wholelist, MySubIndivFit,mc.cores=4)

#summary
df.indiv.sum= data.frame()
for (k in 1:length(sublist)){
  load(paste("indivFit/",k,".Rda",sep = ""))
  # df.indiv.sub=subset(df.indiv.sub,!(md %in%c("feature_window","num_window","delay_window")))
  # df.indiv.sub=subset(df.indiv.sub,!(md %in%c("one_actual")))
  df.indiv.sum=rbind(df.indiv.sum,df.indiv.sub[which.min(df.indiv.sub$BIC),])
}
table(df.indiv.sum$md)

#+ model recovery  -------------------
#' # model recovery 
mySimPar1<-function(par,md){
  a=as.numeric(par)
  df.ag=data.frame()
  for (i in 1:18){
    choice_sub=md %>% subset(fit_id==i)
    choice_sub$hm=NA
    choice_sub$count=0
    for (m in 1:9){
      choice_sub$hm[m]=exp(choice_sub$ratio[m]/a)/sum(exp(choice_sub$ratio/a))
    }
    ans=sample(1:9,1,replace=T,prob=choice_sub$hm)
    choice_sub$count[ans]=1
    df.ag=rbind(df.ag,choice_sub)
  }
  df.ag$ratio1=NULL
  df.ag$ratio2=NULL
  df.ag$ratio=NULL
  return(df.ag)
}

mySimPar2<-function(par,md){
  p1=as.numeric(par)[1]
  p2=as.numeric(par)[2]
  df.ag=data.frame()
  for (i in 1:18){
    choice_sub=md %>% subset(fit_id==i)
    choice_sub$hm=NA
    choice_sub$count=0
    for (m in 1:9){
      choice_sub$hm[m]=exp(choice_sub$ratio1[m]/p1+choice_sub$ratio2[m]/p2)/sum(exp(choice_sub$ratio1/p1+choice_sub$ratio2/p2))
    }
    ans=sample(c(1:9),1,replace=T,prob=choice_sub$hm)
    choice_sub$count[ans]=1
    df.ag=rbind(df.ag,choice_sub)
  }
  df.ag$ratio1=NULL
  df.ag$ratio2=NULL
  df.ag$ratio=NULL
  return(df.ag)
}

mySimPar_rd<-function(md){
  df.ag=data.frame()
  for (i in 1:18){
    choice_sub=md %>% subset(fit_id==i)
    choice_sub$hm=NA
    choice_sub$count=0
    for (m in 1:9){
      choice_sub$hm[m]=0.11
    }
    ans=sample(1:9,1,replace=T,prob=choice_sub$hm)
    choice_sub$count[ans]=1
    df.ag=rbind(df.ag,choice_sub)
  }
  df.ag$ratio1=NULL
  df.ag$ratio2=NULL
  df.ag$ratio=NULL
  return(df.ag)
}

myModRec<-function(i){
  df.mdrec.sub=data.frame()
  
  load(paste("indivFit/",i,".Rda",sep = ""))
  df.sub=df.indiv.sub
  #normative
  mdname="normative"
  mod=md.nor
  df.sim=mySimPar1(df.sub$par1[which(df.sub$md==mdname)],mod) %>% MyIndivFit(sublist[i])
  df.sim$agent=mdname
  df.mdrec.sub=rbind(df.mdrec.sub,df.sim)
  #one-actual
  mdname="one_actual"
  mod=md.one
  df.sim=mySimPar1(df.sub$par1[which(df.sub$md==mdname)],mod) %>% MyIndivFit(sublist[i])
  df.sim$agent=mdname
  df.mdrec.sub=rbind(df.mdrec.sub,df.sim)
  #feature-based (window-based)
  mdname="feature_window"
  mod=md.fea.w
  x=c(df.sub$par1[which(df.sub$md==mdname)],df.sub$par2[which(df.sub$md==mdname)]) %>% mySimPar2(mod)
  df.sim=c(df.sub$par1[which(df.sub$md==mdname)],df.sub$par2[which(df.sub$md==mdname)]) %>% mySimPar2(mod) %>% MyIndivFit(sublist[i])
  df.sim$agent=mdname
  df.mdrec.sub=rbind(df.mdrec.sub,df.sim)
  #delay (window-based)
  mdname="delay_window"
  mod=md.fea.w.d
  df.sim=mySimPar1(df.sub$par1[which(df.sub$md==mdname)],mod) %>% MyIndivFit(sublist[i])
  df.sim$agent=mdname
  df.mdrec.sub=rbind(df.mdrec.sub,df.sim)
  #count (window-based)
  mdname="num_window"
  mod=md.fea.w.c
  df.sim=mySimPar1(df.sub$par1[which(df.sub$md==mdname)],mod) %>% MyIndivFit(sublist[i])
  df.sim$agent=mdname
  df.mdrec.sub=rbind(df.mdrec.sub,df.sim)
  
  #feature-based (intervention-based)
  mdname="feature_intervention"
  mod=md.fea.i
  x=c(df.sub$par1[which(df.sub$md==mdname)],df.sub$par2[which(df.sub$md==mdname)]) %>% mySimPar2(mod)
  df.sim=c(df.sub$par1[which(df.sub$md==mdname)],df.sub$par2[which(df.sub$md==mdname)]) %>% mySimPar2(mod) %>% MyIndivFit(sublist[i])
  df.sim$agent=mdname
  df.mdrec.sub=rbind(df.mdrec.sub,df.sim)
  #delay (intervention-based)
  mdname="delay_intervention"
  mod=md.fea.i.d
  df.sim=mySimPar1(df.sub$par1[which(df.sub$md==mdname)],mod) %>% MyIndivFit(sublist[i])
  df.sim$agent=mdname
  df.mdrec.sub=rbind(df.mdrec.sub,df.sim)
  #count (intervention-based)
  mdname="num_intervention"
  mod=md.fea.i.c
  df.sim=mySimPar1(df.sub$par1[which(df.sub$md==mdname)],mod) %>% MyIndivFit(sublist[i])
  df.sim$agent=mdname
  df.mdrec.sub=rbind(df.mdrec.sub,df.sim)
  #random
  mdname="random"
  mod=md.nor
  mod$ratio=0.11
  df.sim=mySimPar_rd(mod) %>% MyIndivFit(sublist[i])
  df.sim$agent=mdname
  df.mdrec.sub=rbind(df.mdrec.sub,df.sim)

  df.mdrec.sub$subbest=as.character(df.indiv.sub$md[which.min(df.indiv.sub$BIC)])
  save(df.mdrec.sub,file=paste("ModelRec/",i,".Rda",sep = ""))
}

# wholelist=list()
# for (k in 1:length(sublist)){
#   wholelist[[k]]=k
# }
# library(parallel)
# t0=Sys.time()
# mclapply(wholelist, myModRec,mc.cores=4)
# Sys.time()-t0
##Time difference of 12.48238 mins

#summary
df.mdrec=data.frame()
aglist=c("normative","one_actual","feature_window","delay_window","num_window",
         "feature_intervention","delay_intervention","num_intervention","random")
for (k in 1:length(sublist)){
  load(paste("ModelRec/",k,".Rda",sep = ""))
  for (i in aglist){#nine different models
    agent_sub=df.mdrec.sub %>% subset(agent==i)
    agent_sub=agent_sub[which.min(agent_sub$BIC),]
    df.mdrec=rbind(df.mdrec,agent_sub)
  }
}

df.mdrec.sum=matrix(NA,ncol=length(aglist),nrow=length(aglist),dimnames=list(aglist,aglist))
for (i in aglist){
  for (j in aglist){
    df.mdrec.sum[i,j]=nrow(subset(df.mdrec,agent==i&md==j))
  }
}
View(df.mdrec.sum)
df.mdrec.best=matrix(NA,ncol=length(aglist),nrow=length(aglist),dimnames=list(aglist,aglist))
for (i in aglist){
  for (j in aglist){
    df.mdrec.best[i,j]=nrow(subset(df.mdrec,subbest==i&agent==i&md==j))
  }
}
View(df.mdrec.best)
