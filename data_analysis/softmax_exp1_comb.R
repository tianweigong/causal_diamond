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


vals=rbind(mdall1,mdall2)
vals$prob=vals[,"nor"]
vals=as.data.table(vals)
BIC=optim(0.1,MyMdFit,vals=vals,method = "Brent",lower = -30,upper = 30)
#0.45

vals$prob=vals[,"fea_i"]
vals=as.data.table(vals)
BIC=optim(0.1,MyMdFit,vals=vals,method = "Brent",lower = -30,upper = 30)
exp(-1.473522)
#0.23