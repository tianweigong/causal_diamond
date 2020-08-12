#+ General settings, echo = FALSE, results = 'hide' -------------------
knitr::opts_chunk$set(warning = FALSE, message = FALSE)

#+ load packages -------------------
#' # load packages
library(tidyr)
library(dplyr)
library(ggplot2)
library(Rmisc)
library(reshape2)
library(plyr)
library(lme4)
library(lmerTest)
library(lsr)
# rm(list=ls())

#+ load functions -------------------
#' # load functions
load("df.final.exp2.Rda")
df.final.exp2=df.final
df.final.exp2$exp="a"
df.final.exp2$newseed=paste("A",df.final.exp2$seed,sep = "")
load("df.final.exp3.Rda")
df.final.exp3=df.final
df.final.exp3$exp="b"
df.final.exp3$newseed=paste("B",df.final.exp3$seed,sep = "")
df.final.exp3$choice=paste(df.final.exp3$A_state,df.final.exp3$B_state,sep = "")
df.final.exp3=df.final.exp3%>%subset(trial_order<10)
df.final=rbind(df.final.exp2,df.final.exp3)
#+ F1-score  -------------------
#' # F1-score

mySubFscore<-function(df,struc,ru){
  df=df %>% filter(mycondition==ru)
  df$key=paste(df$A_pro,df$B_pro,sep="")
  df$choice=paste(df$A_state,df$B_state,sep="")
  
  pre=nrow(subset(df,key==struc & choice==struc,select = acc_device))/nrow(subset(df,choice==struc,select = acc_device))
  rec=nrow(subset(df,key==struc & choice==struc,select = acc_device))/nrow(subset(df,key==struc,select = acc_device))
  f1=2*(pre*rec)/(pre+rec)
  
  return(f1)
}

mySubFscore(df.final,"GG","regular")
mySubFscore(df.final,"GG","irregular")
mean(mySubFscore(df.final,"GN","regular"),mySubFscore(df.final,"NG","regular"))
mean(mySubFscore(df.final,"GN","irregular"),mySubFscore(df.final,"NG","irregular"))
mean(mySubFscore(df.final,"GP","regular"),mySubFscore(df.final,"PG","regular"))
mean(mySubFscore(df.final,"GP","irregular"),mySubFscore(df.final,"PG","irregular"))
mySubFscore(df.final,"NN","regular")
mySubFscore(df.final,"NN","irregular")
mean(mySubFscore(df.final,"NP","regular"),mySubFscore(df.final,"PN","regular"))
mean(mySubFscore(df.final,"NP","irregular"),mySubFscore(df.final,"PN","irregular"))
mySubFscore(df.final,"PP","regular")
mySubFscore(df.final,"PP","irregular")

#by seeds
seedlist=unique(df.final$newseed)
df.fscore.seed=as.data.frame(matrix(NA,nrow=0,ncol = 4)) %>%
  setNames(c("subject","structure","f1","mycondition"))
for (i in seedlist){
  df.sub=df.final %>% subset(newseed==i)
  for (j in c("regular","irregular")){
    con=j
    df.fscore.seed[nrow(df.fscore.seed)+1,]=c(i,"GG",mySubFscore(df.sub,"GG",con),con)
    df.fscore.seed[nrow(df.fscore.seed)+1,]=c(i,"GN",mean(c(mySubFscore(df.sub,"GN",con),mySubFscore(df.sub,"NG",con)),na.rm=T),con)
    df.fscore.seed[nrow(df.fscore.seed)+1,]=c(i,"GP",mean(c(mySubFscore(df.sub,"GP",con),mySubFscore(df.sub,"PG",con)),na.rm=T),con)
    df.fscore.seed[nrow(df.fscore.seed)+1,]=c(i,"NN",mySubFscore(df.sub,"NN",con),con)
    df.fscore.seed[nrow(df.fscore.seed)+1,]=c(i,"NP",mean(c(mySubFscore(df.sub,"NP",con),mySubFscore(df.sub,"PN",con)),na.rm=T),con)
    df.fscore.seed[nrow(df.fscore.seed)+1,]=c(i,"PP",mySubFscore(df.sub,"PP",con),con)
  }
}

df.fscore.seed$f1=as.numeric(df.fscore.seed$f1)
df.fscore.seed.sum=summarySE(df.fscore.seed,measurevar = "f1",groupvars = c("mycondition","structure"),na.rm = T)
df.fscore.seed.sum$cilow=df.fscore.seed.sum$f1-df.fscore.seed.sum$ci
df.fscore.seed.sum$ciup=df.fscore.seed.sum$f1+df.fscore.seed.sum$ci

#+ choice analysis -------------------
#' # choice analysis
myChoPattern<-function(df,ru){
  df=df %>% filter(mycondition==ru)
  df$key=paste(df$A_pro,df$B_pro,sep="")
  df$choice=paste(df$A_state,df$B_state,sep="")
  
  sta=c("GG","GN","GP","NG","NN","NP","PG","PN","PP")
  pat=c()
  for (i in 1:9){
    for (j in 1:9){
      df.sub=subset(df,key==sta[j] & choice == sta[i])
      df.total=subset(df,key==sta[j])
      pat=c(pat,nrow(df.sub)/nrow(df.total))
    }
  }
  return(round(pat,2))
}

myOverallPro<-function(df,ru){
  df=df %>% filter(mycondition==ru)
  df$key=paste(df$A_pro,df$B_pro,sep="")
  df$choice=paste(df$A_state,df$B_state,sep="")
  
  sta=c("GG","GN","GP","NG","NN","NP","PG","PN","PP")
  pat=c()
  for (i in 1:9){
    a=c()
    for (j in 1:9){
      df.sub=subset(df,key==sta[j] & choice == sta[i])
      df.total=subset(df,key==sta[j])
      a=c(a,round(mean(nrow(df.sub)/nrow(df.total)),2))
    }
    pat=c(pat,round(sum(a),2))
  }
  return(pat)
}


chopattern=myChoPattern(df.final,"regular")
chopattern=myChoPattern(df.final,"irregular")
overallpro=myOverallPro(df.final,"regular")
overallpro=myOverallPro(df.final,"irregular")


#choice pattern -- link level
df.final.link.a=df.final
colnames(df.final.link.a)[colnames(df.final.link.a)=="A_pro"]="pro"
colnames(df.final.link.a)[colnames(df.final.link.a)=="A_state"]="state"
df.final.link.a$B_pro=NULL
df.final.link.a$B_state=NULL
df.final.link.b=df.final
colnames(df.final.link.b)[colnames(df.final.link.b)=="B_pro"]="pro"
colnames(df.final.link.b)[colnames(df.final.link.b)=="B_state"]="state"
df.final.link.b$A_pro=NULL
df.final.link.b$A_state=NULL
df.final.link=rbind(df.final.link.a,df.final.link.b)

  
myChoPattern_link<-function(df,ru){
  df=df %>% filter(mycondition==ru)
  sta=c("G","N","P")
  pat=c()
  for (i in 1:3){
    for (j in 1:3){
      df.sub=subset(df,pro==sta[j] & state == sta[i])
      df.total=subset(df,pro==sta[j])
      pat=c(pat,nrow(df.sub)/nrow(df.total))
    }
  }
  return(round(pat,2))
}

chopattern=myChoPattern_link(df.final.link,"regular")
chopattern=myChoPattern_link(df.final.link,"irregular")

#statistical analysis
sta=c("GG","GN","GP","NG","NN","NP","PG","PN","PP")
df.choice=as.data.frame(matrix(NA,nrow=0,ncol =11 )) %>%
  setNames(c("subject","mycondition",sta))
for (i in unique(df.final$subject)){
  df=df.final %>% filter(subject==i)
  mycon=as.character(df$mycondition[1])
  df.choice[nrow(df.choice)+1,]=c(i,mycon,myOverallPro(df,mycon))
}

for (i in unique(df.final$newseed)){
  mycon="regular"
  df=df.final %>% filter(newseed==i & mycondition==mycon)
  df.choice[nrow(df.choice)+1,]=c(i,mycon,myOverallPro(df,mycon))
  
  mycon="irregular"
  df=df.final %>% filter(newseed==i & mycondition==mycon)
  df.choice[nrow(df.choice)+1,]=c(i,mycon,myOverallPro(df,mycon))
  
}


df.choice.r=df.choice %>% subset(mycondition=="regular")
t.test(as.numeric(df.choice.r$GG),mu=1)### /###
t.test(as.numeric(df.choice.r$GN),mu=1)
t.test(as.numeric(df.choice.r$GP),mu=1)
t.test(as.numeric(df.choice.r$NG),mu=1)
t.test(as.numeric(df.choice.r$NN),mu=1)#/#
t.test(as.numeric(df.choice.r$NP),mu=1)### /###
t.test(as.numeric(df.choice.r$PG),mu=1)
t.test(as.numeric(df.choice.r$PN),mu=1)### /###
t.test(as.numeric(df.choice.r$PP),mu=1)### /###


df.choice.u=df.choice %>% subset(mycondition=="irregular")
t.test(as.numeric(df.choice.u$GG),mu=1)###
t.test(as.numeric(df.choice.u$GN),mu=1)
t.test(as.numeric(df.choice.u$GP),mu=1)
t.test(as.numeric(df.choice.u$NG),mu=1)
t.test(as.numeric(df.choice.u$NN),mu=1) #/#
t.test(as.numeric(df.choice.u$NP),mu=1)###
t.test(as.numeric(df.choice.u$PG),mu=1)
t.test(as.numeric(df.choice.u$PN),mu=1)###
t.test(as.numeric(df.choice.u$PP),mu=1)



#+ intervention analysis -------------------
#' # intervention analysis
ordlist=c("AAABBB","ABABAB","AABBAB",
          "ABBABA","AABABB","ABABBA",
          "ABBAAB","ABAABB","ABBBAA",
          "BAAABB","BAABAB","BABAAB",
          "BBBAAA","BBBAAA","BBAAAB",
          "BBABAA","BABBAA","BABABA")
ordlist2=c("ABABAB","BBAABA","BBBAAA",
           "AABBAB","BABABA","BAABAB",
           "ABAABB","AAABBB","BABBAA")
ord_mix=c("ABABAB","BABABA")
ord_one=c("ABABBA","BABAAB","ABBABA","BAABAB")
ord_two=c("ABAABB","BABBAA","AABBAB","BBAABA",
          "AABABB","BBABAA","ABBAAB","BAABBA")
ord_thr=c("ABBBAA","AABBBA","BAAABB","BBAAAB")
ord_sep=c("AAABBB","BBBAAA")

matdf=data.frame(seed=c(1:18),interven=rep(NA,18))
for (i in 1:18){
  if (ordlist[i] %in% ord_mix){matdf$interven[i]=0}
  if (ordlist[i] %in% ord_one){matdf$interven[i]=1}
  if (ordlist[i] %in% ord_two){matdf$interven[i]=2}
  if (ordlist[i] %in% ord_thr){matdf$interven[i]=3}
  if (ordlist[i] %in% ord_sep){matdf$interven[i]=4}
}
table(matdf$interven)

matdf2=data.frame(seed=c(1:9),interven=rep(NA,9))
for (i in 1:9){
  if (ordlist2[i] %in% ord_mix){matdf2$interven[i]=0}
  if (ordlist2[i] %in% ord_one){matdf2$interven[i]=1}
  if (ordlist2[i] %in% ord_two){matdf2$interven[i]=2}
  if (ordlist2[i] %in% ord_thr){matdf2$interven[i]=3}
  if (ordlist2[i] %in% ord_sep){matdf2$interven[i]=4}
}
table(matdf2$interven)

df.final$interven=NA
for (i in 1:nrow(df.final)){
  if (df.final$exp[i]=="a"){
    df.final$interven[i]=matdf$interven[which(matdf$seed==df.final$seed[i])]
  }
  if (df.final$exp[i]=="b"){
    df.final$interven[i]=matdf2$interven[which(matdf2$seed==df.final$seed[i])]
  }
  
}


df.interven=summarySE(df.final,measurevar = "acc_device",groupvars = c("mycondition","interven"))
df.interven$interven=as.factor(df.interven$interven)
df.interven$mycondition=factor(df.interven$mycondition,
                               levels=levels(df.interven$mycondition)[order(levels(df.interven$mycondition), decreasing = TRUE)])

glmer(acc_device ~ interven +(1|newseed)+ (1|trial_type)+ (1|subject), data = subset(df.final,mycondition=="regular") , family=binomial) %>% summary()
glmer(acc_device ~ interven + (1|newseed)+ (1|trial_type)+ (1|subject), data = subset(df.final,mycondition=="irregular") , family=binomial) %>% summary()