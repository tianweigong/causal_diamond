library(tidyr)
library(dplyr)
library(ggplot2)
library(matrixStats)
library(Rmisc)
library(lsr)
library(reshape2)
library(RColorBrewer)
sim_sqc=300

#likelihood
load("dt.model.nor.Rda")
df.nor=dt.model %>% filter(A_pro==A_state & B_pro==B_state)
df.nor$mod="nor"
load("dt.model.one.Rda")
df.one=dt.model %>% filter(A_pro==A_state & B_pro==B_state)
df.one$mod="oneactual"
load("dt.model.fea.Rda")
colnames(dt.model)[which(colnames(dt.model)%in%c("ratio","ratio2","ratio3","ratio4"))]=c("delayi","numi","delayw","numw")
df.fea=dt.model %>% filter(A_pro==A_state & B_pro==B_state) %>% 
  melt(measure.vars = c("delayi","numi","delayw","numw"),value.name="ratio",variable.name="mod")

summarySE(df.nor,measurevar = "ratio",groupvars = c("mod","ru_pro"))
summarySE(df.one,measurevar = "ratio",groupvars =  c("mod","ru_pro"))
summarySE(df.fea,measurevar = "ratio",groupvars = c("mod","ru_pro"))

cohensD(ratio~ru_pro,df.nor)
cohensD(ratio~ru_pro,df.one)
cohensD(ratio~ru_pro,filter(df.fea,mod=="delayw"))
cohensD(ratio~ru_pro,filter(df.fea,mod=="numw"))
cohensD(ratio~ru_pro,filter(df.fea,mod=="delayi"))
cohensD(ratio~ru_pro,filter(df.fea,mod=="numi"))

dt.mod.sum=rbind(summarySE(df.nor,measurevar = "ratio",groupvars = c("mod","ru_pro")),
                 summarySE(df.one,measurevar = "ratio",groupvars =  c("mod","ru_pro")))
dt.mod.sum= rbind(dt.mod.sum,
                  summarySE(df.fea,measurevar = "ratio",groupvars =  c("mod","ru_pro")))
dt.mod.sum$mod= factor(dt.mod.sum$mod, levels = c("nor","oneactual","delayw","numw","delayi","numi"))
# save(dt.mod.sum,file = "dt.mod.sum.Rda")

t.test(filter(df.nor,ru_pro=="r")[,"ratio"],filter(df.one,ru_pro=="r")[,"ratio"])
t.test(filter(df.one,ru_pro=="r")[,"ratio"],filter(df.fea,ru_pro=="r"&mod=="delayw")[,"ratio"])
t.test(filter(df.fea,ru_pro=="r"&mod=="delayw")[,"ratio"],filter(df.fea,ru_pro=="r"&mod=="delayi")[,"ratio"])
t.test(filter(df.fea,ru_pro=="r"&mod=="delayi")[,"ratio"],filter(df.fea,ru_pro=="r"&mod=="numw")[,"ratio"])
t.test(filter(df.fea,ru_pro=="r"&mod=="numw")[,"ratio"],filter(df.fea,ru_pro=="r"&mod=="numi")[,"ratio"])

t.test(filter(df.nor,ru_pro=="u")[,"ratio"],filter(df.one,ru_pro=="u")[,"ratio"])
t.test(filter(df.one,ru_pro=="u")[,"ratio"],filter(df.fea,ru_pro=="u"&mod=="delayw")[,"ratio"])
t.test(filter(df.fea,ru_pro=="u"&mod=="delayw")[,"ratio"],filter(df.fea,ru_pro=="u"&mod=="delayi")[,"ratio"])
t.test(filter(df.fea,ru_pro=="u"&mod=="delayi")[,"ratio"],filter(df.fea,ru_pro=="u"&mod=="numw")[,"ratio"])
t.test(filter(df.fea,ru_pro=="u"&mod=="numw")[,"ratio"],filter(df.fea,ru_pro=="u"&mod=="numi")[,"ratio"])

#F1-score
myModFscore1<-function(df,struc,ru){
  # sub=md.nor %>% subset(noise==1)
  df=df %>% filter(ru_pro==ru)
  df$key=paste(df$A_pro,df$B_pro,sep="")
  df$choice=paste(df$A_state,df$B_state,sep="")
  
  pre=sum(subset(df,key==struc & choice==struc,select = ratio))/sum(subset(df,choice==struc,select = ratio))
  rec=sum(subset(df,key==struc & choice==struc,select = ratio))/sum(subset(df,key==struc,select = ratio))
  f1=2*(pre*rec)/(pre+rec)
  
  return(f1)
}

myChoPattern<-function(df,ru){
  df=df %>% filter(ru_pro==ru)
  df$key=paste(df$A_pro,df$B_pro,sep="")
  df$choice=paste(df$A_state,df$B_state,sep="")
  
  sta=c("GG","GN","GP","NG","NN","NP","PG","PN","PP")
  pat=c()
  for (i in 1:9){
    for (j in 1:9){
      df.sub=subset(df,key==sta[j] & choice == sta[i])
      pat=c(pat,mean(df.sub$ratio))
    }
  }
  return(round(pat,2))
}

myOverallPro<-function(df,ru){
  df=df %>% filter(ru_pro==ru)
  df$key=paste(df$A_pro,df$B_pro,sep="")
  df$choice=paste(df$A_state,df$B_state,sep="")
  
  sta=c("GG","GN","GP","NG","NN","NP","PG","PN","PP")
  pat=c()
  for (i in 1:9){
    a=c()
    for (j in 1:9){
      df.sub=subset(df,key==sta[j] & choice == sta[i])
      a=c(a,round(mean(df.sub$ratio),2))
    }
    pat=c(pat,round(sum(a),2))
  }
  return(pat)
}


load("dt.model.nor.Rda")
load("dt.model.one.Rda")
load("dt.model.fea.Rda")
colnames(dt.model)[which(colnames(dt.model)%in%c("ratio","ratio2","ratio3","ratio4"))]=c("delayi","numi","delayw","numw")
dt.model=dt.model %>% 
  melt(measure.vars = c("delayi","numi","delayw","numw"),value.name="ratio",variable.name="mod") %>% 
  filter(mod=="numw")

myModFscore1(dt.model,"GG","r")
myModFscore1(dt.model,"GG","u")
mean(myModFscore1(dt.model,"GN","r"),myModFscore1(dt.model,"NG","r"))
mean(myModFscore1(dt.model,"GN","u"),myModFscore1(dt.model,"NG","u"))
mean(myModFscore1(dt.model,"GP","r"),myModFscore1(dt.model,"PG","r"))
mean(myModFscore1(dt.model,"GP","u"),myModFscore1(dt.model,"PG","u"))
myModFscore1(dt.model,"NN","r")
myModFscore1(dt.model,"NN","u")
mean(myModFscore1(dt.model,"NP","r"),myModFscore1(dt.model,"PN","r"))
mean(myModFscore1(dt.model,"NP","u"),myModFscore1(dt.model,"PN","u"))
myModFscore1(dt.model,"PP","r")
myModFscore1(dt.model,"PP","u")

load("dt.model.nor.Rda")
load("dt.model.one.Rda")
load("dt.model.fea.Rda")
dt.model$choice=paste(dt.model$A_state,dt.model$B_state,sep="")

colnames(dt.model)[which(colnames(dt.model)%in%c("ratio","ratio2","ratio3","ratio4"))]=c("delayi","numi","delayw","numw")
dt.model=dt.model %>% 
  melt(measure.vars = c("delayi","numi","delayw","numw"),value.name="ratio",variable.name="mod") %>% 
  filter(mod=="numw")

chopattern=myChoPattern(dt.model,"r")
chopattern=myChoPattern(dt.model,"u")

myOverallPro(dt.model,"r")
myOverallPro(dt.model,"u")

chisq.test(c(215,100))

#F1-score comparison
load("dt.model.nor.Rda")
load("dt.model.one.Rda")
load("dt.model.fea.Rda")
colnames(dt.model)[colnames(dt.model)=="ratio"]="delayi"
colnames(dt.model)[colnames(dt.model)=="ratio2"]="numi"
colnames(dt.model)[colnames(dt.model)=="ratio3"]="delayw"
colnames(dt.model)[colnames(dt.model)=="ratio4"]="numw"
colnames(dt.model)[colnames(dt.model)=="numi"]="ratio"

df.fscore.seed=as.data.frame(matrix(NA,nrow=0,ncol = 4)) %>%
  setNames(c("subject","structure","f1","mycondition"))
for (i in 1:30){
  df.sub=dt.model %>% subset(seed<=(i*10) & seed>(i-1)*10)
  for (j in c("r","u")){
    con=j
    df.fscore.seed[nrow(df.fscore.seed)+1,]=c(i,"GG",myModFscore1(df.sub,"GG",con),con)
    df.fscore.seed[nrow(df.fscore.seed)+1,]=c(i,"GN",mean(c(myModFscore1(df.sub,"GN",con),myModFscore1(df.sub,"NG",con)),na.rm=T),con)
    df.fscore.seed[nrow(df.fscore.seed)+1,]=c(i,"GP",mean(c(myModFscore1(df.sub,"GP",con),myModFscore1(df.sub,"PG",con)),na.rm=T),con)
    df.fscore.seed[nrow(df.fscore.seed)+1,]=c(i,"NN",myModFscore1(df.sub,"NN",con),con)
    df.fscore.seed[nrow(df.fscore.seed)+1,]=c(i,"NP",mean(c(myModFscore1(df.sub,"NP",con),myModFscore1(df.sub,"PN",con)),na.rm=T),con)
    df.fscore.seed[nrow(df.fscore.seed)+1,]=c(i,"PP",myModFscore1(df.sub,"PP",con),con)
  }
}

# df.fscore.nor=df.fscore.seed
df.fscore.nor$model="1.nor"

# df.fscore.one=df.fscore.seed
df.fscore.one$model="2.one"

# df.fscore.fea.w.d=df.fscore.seed
df.fscore.fea.w.d$model="3.delayw"

# df.fscore.fea.w.n=df.fscore.seed
df.fscore.fea.w.n$model="4.numw"

# df.fscore.fea.i.d=df.fscore.seed
df.fscore.fea.i.d$model="5.delayi"

# df.fscore.fea.i.n=df.fscore.seed
df.fscore.fea.i.n$model="6.numi"

df.fscore.mod=df.fscore.nor %>% rbind(df.fscore.one)%>% rbind(df.fscore.fea.w.d)%>% 
  rbind(df.fscore.fea.w.n)%>% rbind(df.fscore.fea.i.d)%>% 
  rbind(df.fscore.fea.i.n)

save(df.fscore.mod,df.fscore.mod.sum,file = "df.fscore.mod.Rda")



load("dt.model.nor.Rda")
load("dt.model.one.Rda")
load("dt.model.fea.Rda")
colnames(dt.model)[colnames(dt.model)=="ratio"]="delayi"
colnames(dt.model)[colnames(dt.model)=="ratio2"]="numi"
colnames(dt.model)[colnames(dt.model)=="ratio3"]="delayw"
colnames(dt.model)[colnames(dt.model)=="ratio4"]="numw"
colnames(dt.model)[colnames(dt.model)=="numi"]="ratio"

df.over.seed.r=as.data.frame(matrix(NA,nrow=0,ncol = 10)) %>%
  setNames(c("con","GG","GN","GP","NG","NN","NP","PG","PN","PP"))
df.over.seed.u=as.data.frame(matrix(NA,nrow=0,ncol = 10)) %>%
  setNames(c("con","GG","GN","GP","NG","NN","NP","PG","PN","PP"))
for (i in 1:30){
  df.sub=dt.model %>% subset(seed<=(i*10) & seed>(i-1)*10)
  df.over.seed.r[nrow(df.over.seed.r)+1,]=c("r",myOverallPro(df.sub,"r"))
  df.over.seed.u[nrow(df.over.seed.u)+1,]=c("u",myOverallPro(df.sub,"u"))
}
t.test(as.numeric(df.over.seed.r$GG),mu=1)
t.test(as.numeric(df.over.seed.r$GN),mu=1)
t.test(as.numeric(df.over.seed.r$GP),mu=1)

# 
# df.fscore.seed$f1=as.numeric(df.fscore.seed$f1)
# t.test(f1~mycondition,subset(df.fscore.seed,structure=="GG"),paired=T)
# t.test(f1~mycondition,subset(df.fscore.seed,structure=="GN"),paired=T)
# t.test(f1~mycondition,subset(df.fscore.seed,structure=="GP"),paired=T)
# t.test(f1~mycondition,subset(df.fscore.seed,structure=="NN"),paired=T)
# t.test(f1~mycondition,subset(df.fscore.seed,structure=="NP"),paired=T)
# t.test(f1~mycondition,subset(df.fscore.seed,structure=="PP"),paired=T)
# library(agricolae)
# aov(f1~structure,subset(df.fscore.seed,mycondition=="r")) %>% LSD.test("structure") %>% plot()
# aov(f1~structure,subset(df.fscore.seed,mycondition=="u")) %>% LSD.test("structure") %>% plot()




#intervention

# ordlist=c()
# for (i in 1:300){
#   file=paste("simulated_stimuli/",i,".Rda",sep="")
#   load(file)
#   x=as.vector(sqc.gg_r$obj)
#   ordlist=c(ordlist,paste(x[x!="E"],collapse = ''))
# }
ord_mix=c("ABABAB","BABABA")
ord_one=c("ABABBA","BABAAB","ABBABA","BAABAB")
ord_two=c("ABAABB","BABBAA","AABBAB","BBAABA",
          "AABABB","BBABAA","ABBAAB","BAABBA")
ord_thr=c("ABBBAA","AABBBA","BAAABB","BBAAAB")
ord_sep=c("AAABBB","BBBAAA")

matdf=data.frame(seed=c(1:300),interven=rep(NA,300))
for (i in 1:300){
  file=paste("simulated_stimuli/",i,".Rda",sep="")
  load(file)
  x=as.vector(sqc.gg_r$obj)
  x=paste(x[x!="E"],collapse = '')
  if (x %in% ord_mix){matdf$interven[i]=0}
  if (x %in% ord_one){matdf$interven[i]=1}
  if (x %in% ord_two){matdf$interven[i]=2}
  if (x %in% ord_thr){matdf$interven[i]=3}
  if (x %in% ord_sep){matdf$interven[i]=4}
}
table(matdf$interven)

load("dt.model.nor.Rda")
df.nor=dt.model %>% filter(A_pro==A_state & B_pro==B_state)
df.nor$mod="1.nor"
df.nor=df.nor[,-c(9:11)]
load("dt.model.one.Rda")
df.one=dt.model %>% filter(A_pro==A_state & B_pro==B_state)
df.one$mod="2.oneactual"
df.one=df.one[,-c(9:11)]
load("dt.model.fea.Rda")
colnames(dt.model)[which(colnames(dt.model)%in%c("ratio","ratio2","ratio3","ratio4"))]=c("5.delayi","6.numi","3.delayw","4.numw")
df.fea=dt.model %>% filter(A_pro==A_state & B_pro==B_state) %>% 
  melt(measure.vars = c("5.delayi","6.numi","3.delayw","4.numw"),value.name="ratio",variable.name="mod")
df.fea=df.fea[,-c(8:9)]
df.mod=rbind(df.nor,df.one)
df.mod=rbind(df.mod,df.fea)
df.mod$interven=NA
for (i in 1:nrow(df.mod)){
  df.mod$interven[i]=matdf$interven[which(matdf$seed==df.mod$seed[i])]
}
df.mod.interven=summarySE(df.mod,measurevar = "ratio",groupvars = c("mod","ru_pro","interven"))
df.mod.interven$interven=as.factor(df.mod.interven$interven)

#
df.mod$key=paste(df.mod$A_pro,df.mod$B_pro,sep = "")
df.sub=df.mod %>% filter(mod=="1.nor")
df.sub=df.mod %>% filter(mod=="2.oneactual")
df.sub=df.mod %>% filter(mod=="4.numw")
aov(ratio ~ interven+ru_pro+interven*ru_pro,data=df.sub) %>% summary()

df.sub=df.mod %>% filter(mod=="1.nor" & ru_pro=="r")
df.sub=df.mod %>% filter(mod=="2.oneactual" & ru_pro=="r")
df.sub=df.mod %>% filter(mod=="3.delayw" & ru_pro=="r")
df.sub=df.mod %>% filter(mod=="4.numw" & ru_pro=="r")
df.sub=df.mod %>% filter(mod=="5.delayi" & ru_pro=="r")
df.sub=df.mod %>% filter(mod=="6.numi" & ru_pro=="r")
aov(ratio ~ interven,data=df.sub) %>% summary()

df.sub=df.mod %>% filter(mod=="1.nor" & ru_pro=="u")
df.sub=df.mod %>% filter(mod=="2.oneactual" & ru_pro=="u")
df.sub=df.mod %>% filter(mod=="3.delayw" & ru_pro=="u")
df.sub=df.mod %>% filter(mod=="4.numw" & ru_pro=="u")
df.sub=df.mod %>% filter(mod=="5.delayi" & ru_pro=="u")
df.sub=df.mod %>% filter(mod=="6.numi" & ru_pro=="u")
aov(ratio ~ interven,data=df.sub) %>% summary()

df.sub=df.mod %>% filter(mod=="1.nor" & ru_pro=="r" & interven %in% c(3,1))
aov(ratio ~ interven,data=df.sub) %>% summary()

