library(dplyr)
library(tidyr)
library(matrixStats)
library(parallel)
library(Rmisc)
library(spatstat)
library(data.table)
source('fun_softmax.R')

set.seed(2)

load("exp1a.Rda")
load('mdall_exp1a.Rda')
mdall$people=0
for (k in 1:nrow(mdall)){
  sub=df.final %>% subset(seed==mdall$seed[k] & trial_type== mdall$trial_type[k] &
                            choice==mdall$choice[k] & mycondition==mdall$mycondition[k])
  mdall$people[k]=nrow(sub)
}
mdall1=mdall %>% mutate(uni_id=paste("1a",uni_id,sep="_"))


load("exp1b.Rda")
load('mdall_exp1b.Rda')
mdall$people=0
df.final$choice=paste(df.final$A_state,df.final$B_state,sep = "")
for (k in 1:nrow(mdall)){
  sub=df.final %>% subset(seed==mdall$seed[k] & trial_type== mdall$trial_type[k] &
                            choice==mdall$choice[k] & mycondition==mdall$mycondition[k])
  mdall$people[k]=nrow(sub)
}
mdall2=mdall %>% subset(seed<10) %>% mutate(uni_id=paste("1b",uni_id,sep="_"))


mdall=rbind(mdall1,mdall2)

#structure fit
var_list_idd=list("nor"=c("nor"),"feai"=c("fea_i"),"feaw"=c("fea_w4"))

MyStrFit<-function(subk){

  trlist=mdall$uni_id[intersect(which(mdall$trial_type==subk[1]),which(mdall$ru_pro==subk[2]))] %>% unique()
  
  BIC.idd=list()
  CV.idd=list()
  for (k in 1:length(var_list_idd)){
    vals=mdall[which(mdall$uni_id %in% trlist),] %>% as.data.frame()
    varsname=var_list_idd[[k]]
    
    loglen=sum(vals$people)
    
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
  list("BIC"=BIC.idd,"CV"=CV.idd)
}


wholelist=list(c("GG","r"),c("GN","r"),c("GP","r"),
              c("NG","r"),c("NN","r"),c("NP","r"),
              c("PG","r"),c("PN","r"),c("PP","r"),
              c("GG","u"),c("GN","u"),c("GP","u"),
              c("NG","u"),c("NN","u"),c("NP","u"),
              c("PG","u"),c("PN","u"),c("PP","u"))
x=mclapply(wholelist,MyStrFit,mc.cores=4)

b=c()
for (k in 1:9){ #regular structures
  bic=x[[k]][[1]]
  b=c(b,which.min(sapply(bic, "[[","BIC")[1:2]))
}
table(b)

b=c()
for (k in 10:18){ #irregular structures
  bic=x[[k]][[1]]
  b=c(b,which.min(sapply(bic, "[[","BIC")[1:2]))
}
table(b)

v=c()
for (k in 1:9){ #regular structures
  cv=x[[k]][[2]]
  v=c(v,which.min(sapply(cv, "[[","cv_log")[1:2]))
}
table(v)

v=c()
for (k in 10:18){ #irregular structures
  cv=x[[k]][[2]]
  v=c(v,which.min(sapply(cv, "[[","cv_log")[1:2]))
}
table(v)
