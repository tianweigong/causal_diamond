library(dplyr)
library(tidyr)
library(Rmisc)
library(matrixStats)
library(parallel)
library(spatstat)
library(data.table)
source('fun_softmax.R')

set.seed(2)

load("exp3.Rda")
df.final$choice=paste(df.final$A_state,df.final$B_state,sep="")
load('mdall_exp3.Rda')

#randomBIC
round(2*nrow(df.final)*log(0.11))
round(nrow(df.final)*log(0.11))

mdall$people=0
df.final$choice=paste(df.final$A_state,df.final$B_state,sep = "")
for (k in 1:nrow(mdall)){
  sub=df.final %>% subset(seed==mdall$seed[k] & trial_type== mdall$trial_type[k] &
                          choice==mdall$choice[k] & mycondition==mdall$mycondition[k])
  mdall$people[k]=nrow(sub)
}

var_list=list("feai"=c("fea_i"),"nor"=c("nor"),#"feaANDnor"=c("fea_i","nor"),
              "delayi"=c("delay_i"),"counti"=c("count_i"))
for (k in c(1,1.5,2,2.5,3,3.5,4,4.5,5,5.5,6,6.5,7)){
  var_list[[paste("feaw",k,sep="")]]=paste("fea_w",k,sep="")
  if (k ==4){
    var_list[[paste("delayw",k,sep="")]]=paste("delay_w",k,sep="")
    var_list[[paste("countw",k,sep="")]]=paste("count_w",k,sep="")
  }
}

vals=mdall
loglen=sum(mdall$people)
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
  BIC.list[[names(var_list)[k]]][["BIC"]]=round(2*BIC.list[[names(var_list)[k]]]$value+length(varsname)*log(loglen))
}

CV.list=list()
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
  
  for (m in unique(vals$seed)){
    vals1=vals %>% subset(seed!=m)
    vals2=vals %>% subset(seed==m)
    if (length(varsname)==1){
      CV.list[[names(var_list)[k]]][[m]]=optim(0.1,MyMdFit,vals=vals1,method = "Brent",lower = -30,upper = 30)
      cv_log=c(cv_log,MyMdFit(CV.list[[names(var_list)[k]]][[m]]$par,vals2))
    }else{
      CV.list[[names(var_list)[k]]][[m]]=optim(c(0.1,0.1),MyMdFit2,vals=vals1,method = "Nelder-Mead")
      cv_log=c(cv_log,MyMdFit2(CV.list[[names(var_list)[k]]][[m]]$par,vals2))
    }
  }
  CV.list[[names(var_list)[k]]][["cv_log"]]=round(sum(cv_log))
}
save(CV.list,BIC.list,file = "modelFit_exp3.Rda")

#individual fit
subvec=as.character(unique(df.dmg$subject))
var_list_idd=list("nor"=c("nor"),"feai"=c("fea_i"),"feaw"=c("fea_w4"))


MyIndFit<-function(subk){
  mdsub=mdall
  mdsub$people=0
  for (k in 1:nrow(mdsub)){
    sub=df.final %>% subset(subject==subk &seed==mdall$seed[k] & trial_type== mdall$trial_type[k] &
                              choice==mdall$choice[k] & mycondition==mdall$mycondition[k])
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

loglen=18
fldnew="softmax_exp3"
wholelist=as.list(as.character(unique(df.dmg$subject)))
mclapply(wholelist,MyIndFit,mc.cores=20)
