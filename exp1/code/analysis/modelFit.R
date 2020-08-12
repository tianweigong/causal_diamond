library(stats4)
#+ load functions -------------------
#' # load functions
MySubFit9 <-function(df){
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
        md.sub$count[p]=df %>% subset(fit_id==i & A_state== st_a[j] & B_state==st_b[j]) %>% nrow()
        md.sub$hm[p]=md.sub$count[p]/(df %>% subset(fit_id==i) %>% nrow())
    }
  }
  
  return(md.sub)
}

full_mod1<-function(par){
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

full_mod2<-function(p1,p2){
  likeli=c()
  for (i in 1:18){
    choice_sub=vals %>% subset(fit_id==i)
    for (m in 1:9){
      sfm=exp(choice_sub$ratio1[m]/p1+choice_sub$ratio2[m]/p2)/sum(exp(choice_sub$ratio1/p1+choice_sub$ratio2/p2))
      likeli=c(likeli, log(sfm)*choice_sub$count[m])
    }
  }
  -sum(likeli)
}

#+ load human data -------------------
#' # load human data
load("df.final.Rda")
df.final$fit_id=as.numeric(df.final$trial_id)
subnum=60
md.ppl=MySubFit9(df.final)


#+ load model data -------------------
#' # load model data
load("../model/normative/df.nor.Rda")
md.nor=dt.model
md.nor$fit_id=as.numeric(md.nor$trial_id)
load("../model/one_actual/df.one.Rda")
md.one=dt.model
md.one$fit_id=as.numeric(md.one$trial_id)
load("../model/feature_based/df.fea.Rda")
md.fea.w=md.fea.i=dt.model
md.fea.w$fit_id=as.numeric(md.fea.w$trial_id)
md.fea.i$fit_id=as.numeric(md.fea.i$trial_id)
colnames(md.fea.w)[colnames(md.fea.w)=="delayw"]="ratio1"
colnames(md.fea.w)[colnames(md.fea.w)=="numw"]="ratio2"
colnames(md.fea.i)[colnames(md.fea.i)=="delayi"]="ratio1"
colnames(md.fea.i)[colnames(md.fea.i)=="numi"]="ratio2"
md.fea.w.d=md.fea.w
colnames(md.fea.w.d)[colnames(md.fea.w.d)=="ratio1"]="ratio"
md.fea.w.c=md.fea.w
colnames(md.fea.w.c)[colnames(md.fea.w.c)=="ratio2"]="ratio"
md.fea.i.d=md.fea.i
colnames(md.fea.i.d)[colnames(md.fea.i.d)=="ratio1"]="ratio"
md.fea.i.c=md.fea.i
colnames(md.fea.i.c)[colnames(md.fea.i.c)=="ratio2"]="ratio"
#+ calculate model acc -------------------
#' #  calculate model acc
md.nor %>% subset(A_pro==A_state & B_pro==B_state,select="ratio")%>% colMeans()
md.one %>% subset(A_pro==A_state & B_pro==B_state,select="ratio")%>% colMeans()
md.fea.w %>% subset(A_pro==A_state & B_pro==B_state,select=c("ratio1","ratio2"))%>% colMeans()
md.fea.i %>% subset(A_pro==A_state & B_pro==B_state,select=c("ratio1","ratio2"))%>% colMeans()

#+  model fitting -------------------
#' #  model fitting
df.fitting= as.data.frame(matrix(NA,ncol=4,nrow=0)) %>%
  setNames(c("md","par1","par2","BIC"))
#' ## normative
vals=merge(md.nor,md.ppl,by=c("fit_id","A_pro","B_pro","A_state","B_state"))
x=mle(full_mod1, start = list(par = 0.9)) %>% attributes() 
df.fitting[nrow(df.fitting)+1,]=c("normative",
                                 as.numeric(x$details$par),
                                 NA,
                                 log(18*subnum)+2*as.numeric(x$details$value))

#' ## one-actual
vals=merge(md.one,md.ppl,by=c("fit_id","A_pro","B_pro","A_state","B_state"))
x=mle(full_mod1, start = list(par = 0.9)) %>% attributes() 
df.fitting[nrow(df.fitting)+1,]=c("one-actual",
                                  as.numeric(x$details$par),
                                  NA,
                                  log(18*subnum)+2*as.numeric(x$details$value))

#' ## feature-based (window-based)
vals=merge(md.fea.w,md.ppl,by=c("fit_id","A_pro","B_pro","A_state","B_state"))
x=mle(full_mod2, start = list(p1 = 0.9,p2=0.9)) %>% attributes() 
df.fitting[nrow(df.fitting)+1,]=c("feature_window",
                                  as.numeric(x$details$par[1]),
                                  as.numeric(x$details$par[2]),
                                  log(18*subnum)*2+2*as.numeric(x$details$value))
#delay only
vals=merge(md.fea.w.d,md.ppl,by=c("fit_id","A_pro","B_pro","A_state","B_state"))
x=mle(full_mod1, start = list(par = 3)) %>% attributes() 
df.fitting[nrow(df.fitting)+1,]=c("delay_window",
                                  as.numeric(x$details$par),
                                  NA,
                                  log(18*subnum)+2*as.numeric(x$details$value))
#number only
vals=merge(md.fea.w.c,md.ppl,by=c("fit_id","A_pro","B_pro","A_state","B_state"))
x=mle(full_mod1, start = list(par = 3)) %>% attributes() 
df.fitting[nrow(df.fitting)+1,]=c("num_window",
                                  as.numeric(x$details$par),
                                  NA,
                                  log(18*subnum)+2*as.numeric(x$details$value))

#' ## feature-based (intervention-based)
vals=merge(md.fea.i,md.ppl,by=c("fit_id","A_pro","B_pro","A_state","B_state"))
x=mle(full_mod2, start = list(p1 = 0.9,p2=0.9)) %>% attributes() 
df.fitting[nrow(df.fitting)+1,]=c("feature_intervention",
                                  as.numeric(x$details$par[1]),
                                  as.numeric(x$details$par[2]),
                                  log(18*subnum)*2+2*as.numeric(x$details$value))
#delay only
vals=merge(md.fea.i.d,md.ppl,by=c("fit_id","A_pro","B_pro","A_state","B_state"))
x=mle(full_mod1, start = list(par = 3)) %>% attributes() 
df.fitting[nrow(df.fitting)+1,]=c("delay_intervention",
                                  as.numeric(x$details$par),
                                  NA,
                                  log(18*subnum)+2*as.numeric(x$details$value))
#number only
vals=merge(md.fea.i.c,md.ppl,by=c("fit_id","A_pro","B_pro","A_state","B_state"))
x=mle(full_mod1, start = list(par = 3)) %>% attributes() 
df.fitting[nrow(df.fitting)+1,]=c("num_intervention",
                                  as.numeric(x$details$par),
                                  NA,
                                  log(18*subnum)+2*as.numeric(x$details$value))

#' ## random
vals=md.ppl
likeli=c()
for (i in 1:18){
  choice_sub=vals %>% subset(fit_id==i)
  for (m in 1:9){
    sfm=0.11
    likeli=c(likeli, log(sfm)*choice_sub$count[m])
  }
}

df.fitting[nrow(df.fitting)+1,]=c("random",
                                  NA,
                                  NA,
                                  2*(-sum(likeli)))
View(df.fitting)

#+  cross-validation  -------------------
#' #  cross-validation
sublist=unique(df.final$subject)
cvlist=c(0,6,6,6,6,6,6,6,6,6,6) %>% cumsum()
df.cv= as.data.frame(matrix(NA,ncol=6,nrow=0)) %>%
  setNames(c("md","par1","par2","BIC","set","logli"))

myCVcal1 <-function(md,mdname,currentset){
  vals<<-merge(md,md.training,by=c("fit_id","A_pro","B_pro","A_state","B_state"))
  x=mle(full_mod1, start = list(par = 0.9)) %>% attributes()
  vals<<-merge(md,md.testing,by=c("fit_id","A_pro","B_pro","A_state","B_state"))
  
  rel=c(mdname,
        as.numeric(x$details$par),
        NA,
        log(162)+2*as.numeric(x$details$value),
        currentset,
        -full_mod1(as.numeric(x$details$par)))
  return(rel)
}

myCVcal2 <-function(md,mdname,currentset){
  vals<<-merge(md,md.training,by=c("fit_id","A_pro","B_pro","A_state","B_state"))
  x=mle(full_mod2, start = list(p1 = 0.9,p2=0.9)) %>% attributes() 
  vals<<-merge(md,md.testing,by=c("fit_id","A_pro","B_pro","A_state","B_state"))
  
  rel=c(mdname,
        as.numeric(x$details$par[1]),
        as.numeric(x$details$par[2]),
        log(162)*2+2*as.numeric(x$details$value),
        currentset,
        -full_mod2(as.numeric(x$details$par[1]),as.numeric(x$details$par[2])))
  return(rel)
}



for (i in 2:length(cvlist)){
  df.training=df.final %>% subset(!(subject %in% sublist[(cvlist[i-1]+1):cvlist[i]]))
  df.testing=df.final %>% subset(subject %in% sublist[(cvlist[i-1]+1):cvlist[i]])
  md.training=df.training %>% MySubFit9()
  md.testing=df.testing %>%  MySubFit9()
  
  # #normative
  # df.cv[nrow(df.cv)+1,]=myCVcal1(md.nor,"normative",i-1)
  # #one-actual
  # df.cv[nrow(df.cv)+1,]=myCVcal1(md.one,"one_actual",i-1)
  # #feature(window)
  # df.cv[nrow(df.cv)+1,]=myCVcal2(md.fea.w,"feature_window",i-1)
  # #delay only (window)
  # df.cv[nrow(df.cv)+1,]=myCVcal1(md.fea.w.d,"delay_window",i-1)
  # #num only (window)
  # df.cv[nrow(df.cv)+1,]=myCVcal1(md.fea.w.c,"num_window",i-1)
  #feature(intervention)
  df.cv[nrow(df.cv)+1,]=myCVcal2(md.fea.i,"feature_intervention",i-1)
  # #delay only (intervention)
  # df.cv[nrow(df.cv)+1,]=myCVcal1(md.fea.i.d,"delay_intervention",i-1)
  # #num only (intervention)
  # df.cv[nrow(df.cv)+1,]=myCVcal1(md.fea.i.c,"num_intervention",i-1)
  #random
  vals=md.testing
  likeli=c()
  for (k in 1:18){
    choice_sub=vals %>% subset(fit_id==k)
    for (m in 1:9){
      sfm=0.11
      likeli=c(likeli, log(sfm)*choice_sub$count[m])
    }
  }
  df.cv[nrow(df.cv)+1,]=c("random",
                          NA,
                          NA,
                          NA,
                          i-1,
                          sum(likeli))
}
df.cv$logli=as.numeric(df.cv$logli)

df.cv %>% subset(md=="normative",select="logli")%>% colMeans()
df.cv %>% subset(md=="one_actual",select="logli")%>% colMeans()
df.cv %>% subset(md=="feature_window",select="logli")%>% colMeans()
df.cv %>% subset(md=="delay_window",select="logli")%>% colMeans()
df.cv %>% subset(md=="num_window",select="logli")%>% colMeans()
df.cv %>% subset(md=="feature_intervention",select="logli")%>% colMeans()
df.cv %>% subset(md=="delay_intervention",select="logli")%>% colMeans()
df.cv %>% subset(md=="num_intervention",select="logli")%>% colMeans()
df.cv %>% subset(md=="random",select="logli")%>% colMeans()
