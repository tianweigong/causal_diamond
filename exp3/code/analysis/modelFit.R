library(stats4)
library(tidyr)
library(dplyr)
library(ggplot2)
library(Rmisc)
library(reshape2)
library(plyr)
#+ load functions -------------------
#' # load functions
MySubFit9 <-function(df){
  st_a=pro_a=c("G","G","G","N","N","N","P","P","P")
  st_b=pro_b=c("G","N","P","G","N","P","G","N","P")
  
  md.sub=as.data.frame(matrix(NA,ncol=7,nrow=length(unique(df$fit_id))*length(st_a))) %>%
    setNames(c("fit_id","A_pro","B_pro","A_state","B_state","count","hm"))
  p=0
  for (i in unique(df$fit_id)){
    for (j in 1:length(st_a)){
      p=p+1
      md.sub$fit_id[p]=i
      md.sub$A_pro[p]= df.final$A_pro[df.final$fit_id==i][1]
      md.sub$B_pro[p]=df.final$B_pro[df.final$fit_id==i][1]
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
  for (i in unique(vals$fit_id)){
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
  for (i in unique(vals$fit_id)){
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
# df.final=df.final %>% subset(trial_order==1)
# df.final=df.final %>% subset(trial_order %in% c(2,3,4,5))
# df.final=df.final %>% subset(trial_order %in% c(6,7,8,9))
# df.final=df.final %>% subset(trial_order==10)
# df.final=df.final %>% subset(trial_order %in% c(11,12,13,14))
# df.final=df.final %>% subset(trial_order %in% c(15,16,17,18))
md.ppl=MySubFit9(df.final)

#+ load model data -------------------
#' # load model data
load("../model/normative/df.nor.Rda")
md.nor=dt.model
md.nor$fit_id=(md.nor$seed-1)*18+md.nor$sti_id
load("../model/one_actual/df.one.Rda")
md.one=dt.model
md.one$fit_id=(md.one$seed-1)*18+md.one$sti_id
load("../model/feature_based/df.fea.Rda")
md.fea.w=md.fea.i=dt.model
md.fea.w$fit_id=(md.fea.w$seed-1)*18+md.fea.w$sti_id
md.fea.i$fit_id=(md.fea.i$seed-1)*18+md.fea.i$sti_id
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
md.nor %>% subset(A_pro==A_state & B_pro==B_state &ru_pro=="r" &seed<10,select="ratio")%>% colMeans()
md.one %>% subset(A_pro==A_state & B_pro==B_state &ru_pro=="r" &seed<10,select="ratio")%>% colMeans()
md.fea.w %>% subset(A_pro==A_state & B_pro==B_state &ru_pro=="r" &seed<10,select=c("ratio1","ratio2"))%>% colMeans()
md.fea.i %>% subset(A_pro==A_state & B_pro==B_state &ru_pro=="r" &seed<10,select=c("ratio1","ratio2"))%>% colMeans()

md.nor %>% subset(A_pro==A_state & B_pro==B_state &ru_pro=="u" &seed<10,select="ratio")%>% colMeans()
md.one %>% subset(A_pro==A_state & B_pro==B_state &ru_pro=="u" &seed<10,select="ratio")%>% colMeans()
md.fea.w %>% subset(A_pro==A_state & B_pro==B_state &ru_pro=="u" &seed<10,select=c("ratio1","ratio2"))%>% colMeans()
md.fea.i %>% subset(A_pro==A_state & B_pro==B_state &ru_pro=="u" &seed<10,select=c("ratio1","ratio2"))%>% colMeans()
#+  model fitting -regular -------------------
#' #  model fitting -regular
df.fitting= as.data.frame(matrix(NA,ncol=5,nrow=0)) %>%
  setNames(c("md","ru","par1","par2","BIC"))
ccon="r"
# ccon="u"
#' ## normative
vals=merge(subset(md.nor,ru_pro==ccon),md.ppl,by=c("fit_id","A_pro","B_pro","A_state","B_state"))
x=mle(full_mod1, start = list(par = 0.9)) %>% attributes() 
df.fitting[nrow(df.fitting)+1,]=c("normative",
                                  ccon,
                                  as.numeric(x$details$par),
                                  NA,
                                  log(sum(vals$count))+2*as.numeric(x$details$value))


#' ## one-actual
vals=merge(subset(md.one,ru_pro==ccon),md.ppl,by=c("fit_id","A_pro","B_pro","A_state","B_state"))
x=mle(full_mod1, start = list(par = 0.9)) %>% attributes() 
df.fitting[nrow(df.fitting)+1,]=c("one-actual",
                                  ccon,
                                  as.numeric(x$details$par),
                                  NA,
                                  log(sum(vals$count))+2*as.numeric(x$details$value))

#' ## feature-based (window-based)
vals=merge(subset(md.fea.w,ru_pro==ccon),md.ppl,by=c("fit_id","A_pro","B_pro","A_state","B_state"))
x=mle(full_mod2, start = list(p1 = 0.9,p2=0.9)) %>% attributes() 
df.fitting[nrow(df.fitting)+1,]=c("feature_window",
                                  ccon,
                                  as.numeric(x$details$par[1]),
                                  as.numeric(x$details$par[2]),
                                  log(sum(vals$count))*2+2*as.numeric(x$details$value))
#delay only
vals=merge(subset(md.fea.w.d,ru_pro==ccon),md.ppl,by=c("fit_id","A_pro","B_pro","A_state","B_state"))
x=mle(full_mod1, start = list(par = 3)) %>% attributes() 
df.fitting[nrow(df.fitting)+1,]=c("delay_window",
                                  ccon,
                                  as.numeric(x$details$par),
                                  NA,
                                  log(sum(vals$count))+2*as.numeric(x$details$value))
#number only
vals=merge(subset(md.fea.w.c,ru_pro==ccon),md.ppl,by=c("fit_id","A_pro","B_pro","A_state","B_state"))
x=mle(full_mod1, start = list(par = 3)) %>% attributes() 
df.fitting[nrow(df.fitting)+1,]=c("num_window",
                                  ccon,
                                  as.numeric(x$details$par),
                                  NA,
                                  log(sum(vals$count))+2*as.numeric(x$details$value))

#' ## feature-based (intervention-based)
vals=merge(subset(md.fea.i,ru_pro==ccon),md.ppl,by=c("fit_id","A_pro","B_pro","A_state","B_state"))
x=mle(full_mod2, start = list(p1 = 0.9,p2=0.9)) %>% attributes() 
df.fitting[nrow(df.fitting)+1,]=c("feature_intervention",
                                  ccon,
                                  as.numeric(x$details$par[1]),
                                  as.numeric(x$details$par[2]),
                                  log(sum(vals$count))*2+2*as.numeric(x$details$value))
#delay only
vals=merge(subset(md.fea.i.d,ru_pro==ccon),md.ppl,by=c("fit_id","A_pro","B_pro","A_state","B_state"))
x=mle(full_mod1, start = list(par = 3)) %>% attributes() 
df.fitting[nrow(df.fitting)+1,]=c("delay_intervention",
                                  ccon,
                                  as.numeric(x$details$par),
                                  NA,
                                  log(sum(vals$count))+2*as.numeric(x$details$value))
#number only
vals=merge(subset(md.fea.i.c,ru_pro==ccon),md.ppl,by=c("fit_id","A_pro","B_pro","A_state","B_state"))
x=mle(full_mod1, start = list(par = 3)) %>% attributes() 
df.fitting[nrow(df.fitting)+1,]=c("num_intervention",
                                  ccon,
                                  as.numeric(x$details$par),
                                  NA,
                                  log(sum(vals$count))+2*as.numeric(x$details$value))

#' ## random
# vals=md.ppl%>% subset(fit_id%%2==1)
vals=md.ppl%>% subset(fit_id%%2==0)
likeli=c()
for (i in unique(vals$fit_id)){
  choice_sub=vals %>% subset(fit_id==i)
  for (m in 1:9){
    sfm=0.11
    likeli=c(likeli, log(sfm)*choice_sub$count[m])
  }
}

df.fitting[nrow(df.fitting)+1,]=c("random",
                                  ccon,
                                  NA,
                                  NA,
                                  2*(-sum(likeli)))
View(df.fitting)
#+  cross-validation  -------------------
#' #  cross-validation
df.cv= as.data.frame(matrix(NA,ncol=6,nrow=0)) %>%
  setNames(c("md","par1","par2","BIC","set","logli"))
cvlist=c(1:9)
cvlist=c(10:18)
myCVcal1 <-function(md,mdname,currentset){
  vals<<-merge(md,md.training,by=c("fit_id","A_pro","B_pro","A_state","B_state"))
  x=mle(full_mod1, start = list(par = 0.9)) %>% attributes()
  vals<<-merge(md,md.testing,by=c("fit_id","A_pro","B_pro","A_state","B_state"))
  
  rel=c(mdname,
        as.numeric(x$details$par),
        NA,
        log(sum(vals$count))+2*as.numeric(x$details$value),
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
        log(sum(vals$count))*2+2*as.numeric(x$details$value),
        currentset,
        -full_mod2(as.numeric(x$details$par[1]),as.numeric(x$details$par[2])))
  return(rel)
}


for (i in 1:length(cvlist)){
  df.training=df.final %>% subset(mystiset %in%cvlist & mystiset !=cvlist[i])
  df.testing=df.final %>% subset(mystiset==cvlist[i])
  md.training=df.training %>% MySubFit9()
  md.testing=df.testing %>%  MySubFit9()
  
  #normative
  # df.cv[nrow(df.cv)+1,]=myCVcal1(md.nor,"normative",i)
  # #one-actual
  # df.cv[nrow(df.cv)+1,]=myCVcal1(md.one,"one_actual",i)
  # #feature(window)
  # df.cv[nrow(df.cv)+1,]=myCVcal2(md.fea.w,"feature_window",i)
  # #delay only (window)
  # df.cv[nrow(df.cv)+1,]=myCVcal1(md.fea.w.d,"delay_window",i)
  # #num only (window)
  # df.cv[nrow(df.cv)+1,]=myCVcal1(md.fea.w.c,"num_window",i)
  #feature(intervention)
  df.cv[nrow(df.cv)+1,]=myCVcal2(md.fea.i,"feature_intervention",i)
  # #delay only (intervention)
  # df.cv[nrow(df.cv)+1,]=myCVcal1(md.fea.i.d,"delay_intervention",i)
  # #num only (intervention)
  # df.cv[nrow(df.cv)+1,]=myCVcal1(md.fea.i.c,"num_intervention",i)
  #random
  vals=md.testing
  likeli=c()
  for (k in unique(vals$fit_id)){
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
                          i,
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

# df.cv.r=df.cv