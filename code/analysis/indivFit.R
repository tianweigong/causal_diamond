library(stats4)

ind_mod1<-function(par){
  a<-par
  likeli=c()
  for (i in 1:18){
    choice_sub=vals %>% subset(trial_id==i)
    for (m in 1:9){
      sfm=exp(choice_sub$ratio[m]*a)/sum(exp(choice_sub$ratio*a))
      likeli=c(likeli, log(sfm)*choice_sub$hm[m])
    }
  }
  -sum(likeli)
}

ind_mod2<-function(p1,p2){
  likeli=c()
  for (i in 1:18){
    choice_sub=vals %>% subset(trial_id==i)
    for (m in 1:9){
      sfm=exp(choice_sub$ratio1[m]*p1+choice_sub$ratio2[m]*p2)/sum(exp(choice_sub$ratio1*p1+choice_sub$ratio2*p2))
      likeli=c(likeli, log(sfm)*choice_sub$hm[m])
    }
  }
  -sum(likeli)
}


df.indiv= as.data.frame(matrix(NA,ncol=15,nrow=length(unique(df.final$subject)))) %>%
  setNames(c("subject",
             "norm_par","norm_noise","norm_BIC",
             "delay_par","delay_BIC","number_par","number_BIC",
             "feature_par1","feature_par2","feature_BIC",
             "local_par","local_q","local_BIC","rd_BIC"))

p=0
for (i in unique(df.final$subject)){
  p=p+1
  df.indiv$subject[p]=i
  df.sig=MySigFit9(i)
  
  df.indiv.sub= as.data.frame(matrix(NA,ncol=4,nrow=8)) %>%
    setNames(c("subject","par1","par2","BIC"))
  for (j in 1:8){
    md.sub=md.nor %>% subset(noise==j)
    vals=merge(md.sub,df.sig,by=c("trial_id","A_pro","B_pro","A_state","B_state"))
    x=mle(ind_mod1, start = list(par = 0)) %>% attributes() 
    df.indiv.sub$par1[j]=x$details$par %>% as.numeric()
    df.indiv.sub$par2[j]=j
    df.indiv.sub$BIC[j]=log(162)*2+2*x$details$value
  }
  df.indiv$norm_BIC[p]=min(df.indiv.sub$BIC)
  df.indiv$norm_par[p]=df.indiv.sub$par1[which.min(df.indiv.sub$BIC)]
  df.indiv$norm_noise[p]=df.indiv.sub$par2[which.min(df.indiv.sub$BIC)]
  
  
  vals=merge(df.sig,md.cue1,by=c("trial_id","A_pro","B_pro","A_state","B_state"))
  x=mle(ind_mod1, start = list(par = 0)) %>% attributes() 
  df.indiv$delay_par[p]=x$details$par %>% as.numeric()
  df.indiv$delay_BIC[p]=log(162)*1+2*x$details$value
  
  vals=merge(df.sig,md.cue2,by=c("trial_id","A_pro","B_pro","A_state","B_state"))
  x=mle(ind_mod1, start = list(par = 0)) %>% attributes() 
  df.indiv$number_par[p]=x$details$par %>% as.numeric()
  df.indiv$number_BIC[p]=log(162)*1+2*x$details$value
  
  vals=merge(df.sig,md.fea,by=c("trial_id","A_pro","B_pro","A_state","B_state"))
  x=mle(ind_mod2, start = list(p1 = 0,p2=0)) %>% attributes() 
  df.indiv$feature_par1[p]=x$details$par[1] %>% as.numeric()
  df.indiv$feature_par2[p]=x$details$par[2] %>% as.numeric()
  df.indiv$feature_BIC[p]=log(162)*2+2*x$details$value
  
  df.indiv.sub= as.data.frame(matrix(NA,ncol=4,nrow=length(unique(md.sqc$q1)))) %>%
    setNames(c("subject","par1","par2","BIC"))
  for (j in 1:length(unique(md.sqc$q1))){
    md.sub=md.sqc %>% subset(q1==unique(md.sqc$q1)[j])
    vals=merge(df.sig,md.sub,by=c("trial_id","A_pro","B_pro","A_state","B_state"))
    x=mle(ind_mod1, start = list(par = 0)) %>% attributes() 
    df.indiv.sub$par1[j]=x$details$par[1] %>% as.numeric()
    df.indiv.sub$par2[j]=unique(md.sqc$q1)[j]
    df.indiv.sub$BIC[j]=log(162)*2+2*x$details$value
  }
  df.indiv$local_BIC[p]=min(df.indiv.sub$BIC)
  df.indiv$local_par[p]=df.indiv.sub$par1[which.min(df.indiv.sub$BIC)]
  df.indiv$local_q[p]=df.indiv.sub$par2[which.min(df.indiv.sub$BIC)]
}
# need 5 min to run

##random
p=0
vals=md.ppl
for (i in unique(df.final$subject)){
  p=p+1
  likeli=c()
  for (i in 1:18){
    choice_sub=vals %>% subset(trial_id==i)
    for (m in 1:9){
      sfm=0.11
      likeli=c(likeli,log(sfm)*choice_sub$hm[m])
    }
    df.indiv$rd_BIC[p]=2*(-sum(likeli))
  }
}

#best fit
df.indiv$bestfit=NA
model_list=c("norm","delay","number","feature","local","rd")
for (i in 1: nrow(df.indiv)){
  bestfit=c(df.indiv$norm_BIC[i],
            df.indiv$delay_BIC[i],df.indiv$number_BIC[i],df.indiv$feature_BIC[i],
            df.indiv$local_BIC[i],
            df.indiv$rd_BIC[i])
  df.indiv$bestfit[i]=model_list[which.min(bestfit)]
}

df.indiv %>% subset(bestfit=="norm") %>% nrow()
df.indiv %>% subset(bestfit=="delay") %>% nrow()
df.indiv %>% subset(bestfit=="number") %>% nrow()
df.indiv %>% subset(bestfit=="feature") %>% nrow()
df.indiv %>% subset(bestfit=="local") %>% nrow()
df.indiv %>% subset(bestfit=="rd") %>% nrow()