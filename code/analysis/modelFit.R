#+ load functions -------------------
#' # load functions
MySubFit9 <-function(){
  st_a=pro_a=c("G","P","N","G","P","N","G","P","N")
  st_b=pro_b=c("G","G","G","P","P","P","N","N","N")
  
  md.sub=as.data.frame(matrix(NA,ncol=7,nrow=18*9)) %>%
    setNames(c("trial_id","A_pro","B_pro","A_state","B_state","count","hm"))
  
  for (i in 1:18){
    for (j in 1:length(st_a)){
        p=(i-1)*9+j
        md.sub$trial_id[p]=i
        md.sub$A_pro[p]=pro_a[ceiling(i/2)]
        md.sub$B_pro[p]=pro_b[ceiling(i/2)]
        md.sub$A_state[p]=st_a[j]
        md.sub$B_state[p]=st_b[j]
        md.sub$count[p]=df.final %>% subset(trial_id==i & A_state== st_a[j] & B_state==st_b[j]) %>% nrow()
        md.sub$hm[p]=md.sub$count[p]/(df.final %>% subset(trial_id==i) %>% nrow())
    }
  }
  
  return(md.sub)
}

MySigFit9 <-function(sub_id){
  df.sig=df.final %>% subset(subject==sub_id)
  st_a=pro_a=c("G","P","N","G","P","N","G","P","N")
  st_b=pro_b=c("G","G","G","P","P","P","N","N","N")
  
  md.sub=as.data.frame(matrix(NA,ncol=7,nrow=18*9)) %>%
    setNames(c("trial_id","A_pro","B_pro","A_state","B_state","count","hm"))
  
  for (i in 1:18){
    for (j in 1:length(st_a)){
      p=(i-1)*9+j
      md.sub$trial_id[p]=i
      md.sub$A_pro[p]=pro_a[ceiling(i/2)]
      md.sub$B_pro[p]=pro_b[ceiling(i/2)]
      md.sub$A_state[p]=st_a[j]
      md.sub$B_state[p]=st_b[j]
      md.sub$count[p]=df.sig %>% subset(trial_id==i & A_state== st_a[j] & B_state==st_b[j]) %>% nrow()
      md.sub$hm[p]=md.sub$count[p]/(df.sig %>% subset(trial_id==i) %>% nrow())
    }
  }
  
  return(md.sub)
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

full_mod1<-function(par){
  a<-par
  likeli=c()
  for (i in 1:18){
    choice_sub=vals %>% subset(trial_id==i)
    for (m in 1:9){
      sfm=exp(choice_sub$ratio[m]*a)/sum(exp(choice_sub$ratio*a))
      likeli=c(likeli, log(sfm)*choice_sub$hm[m]*60)
    }
  }
  -sum(likeli)
}

full_mod2<-function(p1,p2){
  likeli=c()
  for (i in 1:18){
    choice_sub=vals %>% subset(trial_id==i)
    for (m in 1:9){
      sfm=exp(choice_sub$ratio1[m]*p1+choice_sub$ratio2[m]*p2)/sum(exp(choice_sub$ratio1*p1+choice_sub$ratio2*p2))
      likeli=c(likeli, log(sfm)*choice_sub$hm[m]*60)
    }
  }
  -sum(likeli)
}

#+ load human data -------------------
#' # load human data
load("df.final.Rda")
df.final$trial_id=as.numeric(df.final$trial_id)
md.ppl=MySubFit9()

#+ load model data -------------------
#' # load model data
for (i in 1:8){
  md_name=paste("../model/normative/df.model.q",i,".Rda",sep="")
  load(md_name)
  df.model$noise=i
  if (i==1){
    md.nor=df.model
  }else{
    md.nor=rbind(md.nor,df.model)
  }
}

load("../model/feature_based/df.model.delay.Rda")
md.cue1=df.model
load("../model/feature_based/df.model.num.Rda")
md.cue2=df.model

md.fea=merge(md.cue1,md.cue2,by=c("trial_id","A_pro","B_pro","A_state","B_state"))
colnames(md.fea)[colnames(md.fea)=="ratio.x"]="ratio1"
colnames(md.fea)[colnames(md.fea)=="ratio.y"]="ratio2"

load("../model/local/md.large.Rda")
md.sqc=md.large

#+ calculate model acc -------------------
#' #  calculate model acc
#normative
acc=c()
for (i in 1:8){
  md.sub=md.nor %>% subset(noise==i)
  acc=c(acc,MyModAcc(md.sub))
}
print(acc)
#feature-based
acc=c()
MyModAcc(md.cue1)
MyModAcc(md.cue2)
for (i in 0:100){
  md.fea$ratio=md.fea$ratio1*(i/100)+md.fea$ratio2*(1-(i/100))
  acc=c(acc,MyModAcc(md.fea))
}
mean(acc)
min(acc)
max(acc)
#expectation-violation
acc=c()
for (i in unique(md.sqc$q2)){
  md.sub=md.sqc %>% subset(q2==i)
  acc=c(acc,MyModAcc(md.sub))
}
print(acc)

#+  model fitting -------------------
#' #  model fitting
df.fitting= as.data.frame(matrix(NA,ncol=4,nrow=0)) %>%
  setNames(c("md","par1","par2","BIC"))
#' ## normative
for (i in 1:8){
  md.sub=md.nor %>% subset(noise==i)
  vals=merge(md.sub,md.ppl,by=c("trial_id","A_pro","B_pro","A_state","B_state"))
  x=mle(full_mod1, start = list(par = 20)) %>% attributes() 
  
  df.fitting[nrow(df.fitting)+1,]=c("normative",
                                   as.numeric(x$details$par),
                                   i,
                                   log(162)*2+2*as.numeric(x$details$value))
}
#' ## feature-based
vals=merge(md.fea,md.ppl,by=c("trial_id","A_pro","B_pro","A_state","B_state"))
x=mle(full_mod2, start = list(p1 = 33,p2=33)) %>% attributes() 
df.fitting[nrow(df.fitting)+1,]=c("feature",
                                  as.numeric(x$details$par[1]),
                                  as.numeric(x$details$par[2]),
                                  log(162)*2+2*as.numeric(x$details$value))
#delay only
vals=merge(md.cue1,md.ppl,by=c("trial_id","A_pro","B_pro","A_state","B_state"))
x=mle(full_mod1, start = list(par = 23)) %>% attributes() 
df.fitting[nrow(df.fitting)+1,]=c("delay",
                                  as.numeric(x$details$par),
                                  NA,
                                  log(162)*1+2*as.numeric(x$details$value))
#number only
vals=merge(md.cue2,md.ppl,by=c("trial_id","A_pro","B_pro","A_state","B_state"))
x=mle(full_mod1, start = list(par = 23)) %>% attributes() 
df.fitting[nrow(df.fitting)+1,]=c("number",
                                  as.numeric(x$details$par),
                                  NA,
                                  log(162)*1+2*as.numeric(x$details$value))


#' ## expectation-violation
for (i in unique(md.sqc$q1)){
  md.sub=md.sqc %>% subset(q1==i)
  vals=merge(md.sub,md.ppl,by=c("trial_id","A_pro","B_pro","A_state","B_state"))
  x=mle(full_mod1, start = list(par = 20)) %>% attributes() 
  
  df.fitting[nrow(df.fitting)+1,]=c("local",
                                    as.numeric(x$details$par),
                                    i,
                                    log(162)*2+2*as.numeric(x$details$value))
}

#' ## random
vals=md.ppl
likeli=c()
for (i in 1:18){
  choice_sub=vals %>% subset(trial_id==i)
  for (m in 1:9){
    sfm=0.11
    likeli=c(likeli, log(sfm)*choice_sub$hm[m]*60)
  }
}

df.fitting[nrow(df.fitting)+1,]=c("random",
                                  NA,
                                  NA,
                                  2*(-sum(likeli)))
View(df.fitting)
