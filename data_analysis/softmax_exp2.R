library(dplyr)
library(tidyr)
library(matrixStats)
library(parallel)
library(Rmisc)
library(spatstat)
library(data.table)
source('fun_softmax.R')

set.seed(2)


mdall=expand.grid(choice=c("GG","GN","GP","NG","NN","NP","PG","PN","PP"),
                  id=c(1:8))%>%
  mutate(stiID=paste("S",id,sep=""),nor=0,fea_i=0,fea_w=0,people=0)

load("exp2.Rda")
df.off$choice=paste(df.off$A_state,df.off$B_state,sep="")
load('sti_exp2/PG.Rda')
for (k in 1:4){
  a_idx=which(mdall$stiID==paste("S",k,sep="") )
  mdall$nor[a_idx]=b_nor[[paste("S",k,sep="")]]
  mdall$fea_i[a_idx]=b_ssi[[paste("S",k,sep="")]]
  mdall$fea_w[a_idx]=b_ssw[[paste("S",k,sep="")]]
  
  for (m in a_idx){
    sub=df.off %>% subset(trID==mdall$id[m]+6 & choice==mdall$choice[m])
    mdall$people[m]=nrow(sub)
  }
}
load('sti_exp2/GN.Rda')
for (k in 5:8){
  a_idx=which(mdall$stiID==paste("S",k,sep=""))
  mdall$nor[a_idx]=b_nor[[paste("S",k,sep="")]]
  mdall$fea_i[a_idx]=b_ssi[[paste("S",k,sep="")]]
  mdall$fea_w[a_idx]=b_ssw[[paste("S",k,sep="")]]
  
  for (m in a_idx){
    sub=df.off %>% subset(trID==mdall$id[m]+6 & choice==mdall$choice[m])
    mdall$people[m]=nrow(sub)
  }
}

# save(mdall,file="mdall_exp2.Rda")

#random
-round(2*sum(mdall$people)*log(0.11))
round(sum(mdall$people)*log(0.11))


#model fitting
var_list=list("feai"=c("fea_i"),"nor"=c("nor"),"feaw"=c("fea_w"))


vals=mdall %>% mutate(uni_id=stiID)
loglen=sum(vals$people)
BIC.list=list()
for (k in 1:length(var_list)){
  varsname=var_list[[k]]
  vals=as.data.frame(vals)
  if (length(varsname)==1){
    vals$prob=vals[,varsname[1]]
    vals=as.data.table(vals)
    BIC.list[[names(var_list)[k]]]=optim(0.1,MyMdFit,vals=vals,method = "Brent",lower = -30,upper = 30)
  }else{
    vals$prob1=vals[,varsname[1]]
    vals$prob2=vals[,varsname[2]]
    vals=as.data.table(vals)
    BIC.list[[names(var_list)[k]]]=optim(c(0.1,0.1),MyMdFit2,vals=vals,method = "Nelder-Mead")
  }
  BIC.list[[names(var_list)[k]]][["tau"]]=round(exp(BIC.list[[names(var_list)[k]]]$par),2)
  BIC.list[[names(var_list)[k]]][["BIC"]]=2*BIC.list[[names(var_list)[k]]]$value+length(varsname)*log(loglen)
}

CV.list=list()
vals =vals %>% mutate(trType=ifelse(id %in% c(1:4),"PG","GN"))
for (k in 1:length(var_list)){
  varsname=var_list[[k]]
  vals=as.data.frame(vals)
  cv_log=c()
  if (length(varsname)==1){
    vals$prob=vals[,varsname[1]]
    vals=as.data.table(vals)
  }else{
    vals$prob1=vals[,varsname[1]]
    vals$prob2=vals[,varsname[2]]
    vals=as.data.table(vals)
  }
  
  for (m in unique(vals$trType)){
    vals1=vals %>% subset(trType!=m)
    vals2=vals %>% subset(trType==m)
    if (length(varsname)==1){
      CV.list[[names(var_list)[k]]][[m]]=optim(0.1,MyMdFit,vals=vals1,method = "Brent",lower = -30,upper = 30)
      cv_log=c(cv_log,MyMdFit(CV.list[[names(var_list)[k]]][[m]]$par,vals2))
    }else{
      CV.list[[names(var_list)[k]]][[m]]=optim(c(0.1,0.1),MyMdFit2,vals=vals1,method = "Nelder-Mead")
      cv_log=c(cv_log,MyMdFit2(CV.list[[names(var_list)[k]]][[m]]$par,vals2))
    }
  }
  CV.list[[names(var_list)[k]]][["cv_log"]]=sum(cv_log)
}

#individual fit
df.off=df.off %>% mutate(choice=paste(A_state,B_state,sep=""))
subvec=as.character(unique(df.dmg$subject))
var_list_idd=list("nor"=c("nor"),"feai"=c("fea_i"),"feaw"=c("fea_w"))

MyIndFit<-function(subk){
  mdsub=mdall %>% mutate(uni_id=id)
  mdsub$people=0
  for (k in 1:nrow(mdsub)){
    sub=df.off %>% subset(subID==subk & trID== mdsub$id[k]+6 &
                              choice==mdsub$choice[k])
    mdsub$people[k]=nrow(sub)
  }
  trlist=mdsub$uni_id[which(mdsub$people>0)]
  
  BIC.idd=list()
  CV.idd=list()
  for (k in 1:length(var_list_idd)){
    vals=mdsub[which(mdsub$uni_id %in% trlist),] %>% as.data.frame()
    varsname=var_list_idd[[k]]
    if (length(varsname)==1){
      vals$prob=vals[,varsname[1]]
      vals=as.data.table(vals)
      BIC.idd[[names(var_list_idd)[k]]]=optim(0.1,MyMdFit,vals=vals,method = "Brent",lower = -30,upper = 30)
    }else{
      vals$prob1=vals[,varsname[1]]
      vals$prob2=vals[,varsname[2]]
      vals=as.data.table(vals)
      BIC.idd[[names(var_list_idd)[k]]]=optim(c(0.1,0.1),MyMdFit2,vals=vals,method = "Nelder-Mead")
    }
    BIC.idd[[names(var_list_idd)[k]]][["tau"]]=round(exp(BIC.idd[[names(var_list_idd)[k]]]$par),2)
    BIC.idd[[names(var_list_idd)[k]]][["BIC"]]=2*BIC.idd[[names(var_list_idd)[k]]]$value+length(varsname)*log(loglen)
    
    CV.idd[[names(var_list_idd)[k]]]=list()
    cv_log=c()
    for (m in trlist){
      vals1=vals %>% subset(uni_id!=m)
      vals2=vals %>% subset(uni_id==m)
      if (length(varsname)==1){
        CV.idd[[names(var_list_idd)[k]]][[m]]=optim(0.1,MyMdFit,vals=vals1,method = "Brent",lower = -30,upper = 30)
        cv_log=c(cv_log,MyMdFit(CV.idd[[names(var_list_idd)[k]]][[m]]$par,vals2))
      }else{
        CV.idd[[names(var_list_idd)[k]]][[m]]=optim(c(0.1,0.1),MyMdFit2,vals=vals1,method = "Nelder-Mead")
        cv_log=c(cv_log,MyMdFit2(CV.idd[[names(var_list_idd)[k]]][[m]]$par,vals2))
      }
    }
    CV.idd[[names(var_list_idd)[k]]][["cv_log"]]=sum(cv_log)
  }
  save(BIC.idd,CV.idd,file=paste(fldnew,"/",subk,".Rda",sep=""))
}

loglen=8
fldnew="softmax_exp2"
wholelist=as.list(as.character(unique(df.off$subID)))
mclapply(wholelist,MyIndFit,mc.cores=4)
