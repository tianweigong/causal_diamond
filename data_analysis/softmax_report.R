library(dplyr)
library(tidyr)
library(matrixStats)
library(Rmisc)
library(data.table)

set.seed(2)

#exp1a
fld="softmax_exp1a"
load("exp1a.Rda")
f=list.files(path = fld,pattern = "\\.Rda$")

idd_regular=c()
idd_irregular=c()
for (k in 1:length(f)){
  load(paste(fld,"/",f[k],sep=""))
  CV.idd[["random"]]=list("cv_log"=-log(0.11)*18) 
  r=c()
  for (m in 1:length(CV.idd)){r=c(r,CV.idd[[m]][["cv_log"]])}
  y=which(r==min(r))
  if (length(y)>1){y=sample(y,1)}
  
  if ( substr(f[k],1,9) %in% df.dmg$subject[df.dmg$mycondition=="regular"]){
    idd_regular=c(idd_regular,names(CV.idd)[y])
  }else if( substr(f[k],1,9) %in% df.dmg$subject[df.dmg$mycondition=="irregular"]){
    idd_irregular=c(idd_irregular,names(CV.idd)[y])
  }
}
table(idd_regular)/sum(table(idd_regular))
table(idd_irregular)/sum(table(idd_irregular))

idd_regular=c()
idd_irregular=c()
for (k in 1:length(f)){
  load(paste(fld,"/",f[k],sep=""))
  BIC.idd[["random"]]=list("BIC"=-2*log(0.11)*18) 
  r=c()
  for (m in 1:length(BIC.idd)){r=c(r,BIC.idd[[m]][["BIC"]])}
  y=which(r==min(r))
  if (length(y)>1){y=sample(y,1)}
  
  if ( substr(f[k],1,9) %in% df.dmg$subject[df.dmg$mycondition=="regular"]){
    idd_regular=c(idd_regular,names(BIC.idd)[y])
  }else if( substr(f[k],1,9) %in% df.dmg$subject[df.dmg$mycondition=="irregular"]){
    idd_irregular=c(idd_irregular,names(BIC.idd)[y])
  }

}
table(idd_regular)/sum(table(idd_regular))
table(idd_irregular)/sum(table(idd_irregular))


#exp1b
fld="softmax_exp1b"
load("exp1b.Rda")
f=list.files(path = fld,pattern = "\\.Rda$")

idd_regular=c()
idd_irregular=c()
for (k in 1:length(f)){
  load(paste(fld,"/",f[k],sep=""))
  CV.idd[["random"]]=list("cv_log"=-log(0.11)*18) 
  r=c()
  for (m in 1:length(CV.idd)){r=c(r,CV.idd[[m]][["cv_log"]])}
  y=which(r==min(r))
  if (length(y)>1){y=sample(y,1)}
  
  if ( substr(f[k],1,9) %in% df.dmg$subject[df.dmg$mycondition=="regular"]){
    idd_regular=c(idd_regular,names(CV.idd)[y])
  }else if( substr(f[k],1,9) %in% df.dmg$subject[df.dmg$mycondition=="irregular"]){
    idd_irregular=c(idd_irregular,names(CV.idd)[y])
  }
}
table(idd_regular)/sum(table(idd_regular))
table(idd_irregular)/sum(table(idd_irregular))


idd_regular=c()
idd_irregular=c()
for (k in 1:length(f)){
  load(paste(fld,"/",f[k],sep=""))
  BIC.idd[["random"]]=list("BIC"=-2*log(0.11)*18) 
  r=c()
  for (m in 1:length(BIC.idd)){r=c(r,BIC.idd[[m]][["BIC"]])}
  y=which(r==min(r))
  if (length(y)>1){y=sample(y,1)}
  
  if ( substr(f[k],1,9) %in% df.dmg$subject[df.dmg$mycondition=="regular"]){
    idd_regular=c(idd_regular,names(BIC.idd)[y])
  }else if( substr(f[k],1,9) %in% df.dmg$subject[df.dmg$mycondition=="irregular"]){
    idd_irregular=c(idd_irregular,names(BIC.idd)[y])
  }
}
table(idd_regular)/sum(table(idd_regular))
table(idd_irregular)/sum(table(idd_irregular))



#exp1b no ground truth
fld="softmax_exp1b_ngt"
load("exp1b.Rda")
f=list.files(path = fld,pattern = "\\.Rda$")

idd_regular=c()
idd_irregular=c()
for (k in 1:length(f)){
  load(paste(fld,"/",f[k],sep=""))
  CV.idd[["random"]]=list("cv_log"=-log(0.11)*9) 
  r=c()
  for (m in 1:length(CV.idd)){r=c(r,CV.idd[[m]][["cv_log"]])}
  y=which(r==min(r))
  if (length(y)>1){y=sample(y,1)}
  
  if ( substr(f[k],1,9) %in% df.dmg$subject[df.dmg$mycondition=="regular"]){
    idd_regular=c(idd_regular,names(CV.idd)[y])
  }else if( substr(f[k],1,9) %in% df.dmg$subject[df.dmg$mycondition=="irregular"]){
    idd_irregular=c(idd_irregular,names(CV.idd)[y])
  }
}
table(idd_regular)/sum(table(idd_regular))
table(idd_irregular)/sum(table(idd_irregular))

idd_regular=c()
idd_irregular=c()
for (k in 1:length(f)){
  load(paste(fld,"/",f[k],sep=""))
  BIC.idd[["random"]]=list("BIC"=-2*log(0.11)*9) 
  r=c()
  for (m in 1:length(BIC.idd)){r=c(r,BIC.idd[[m]][["BIC"]])}
  y=which(r==min(r))
  if (length(y)>1){y=sample(y,1)}
  
  if ( substr(f[k],1,9) %in% df.dmg$subject[df.dmg$mycondition=="regular"]){
    idd_regular=c(idd_regular,names(BIC.idd)[y])
  }else if( substr(f[k],1,9) %in% df.dmg$subject[df.dmg$mycondition=="irregular"]){
    idd_irregular=c(idd_irregular,names(BIC.idd)[y])
  }
}
table(idd_regular)/sum(table(idd_regular))
table(idd_irregular)/sum(table(idd_irregular))



#exp1b no ground truth
fld="softmax_exp1b_ngt"
load("exp1b.Rda")
f=list.files(path = fld,pattern = "\\.Rda$")

idd_regular=c()
idd_irregular=c()
for (k in 1:length(f)){
  load(paste(fld,"/",f[k],sep=""))
  CV.idd[["random"]]=list("cv_log"=-log(0.11)*9) 
  r=c()
  for (m in 1:length(CV.idd)){r=c(r,CV.idd[[m]][["cv_log"]])}
  y=which(r==min(r))
  if (length(y)>1){y=sample(y,1)}
  
  if ( substr(f[k],1,9) %in% df.dmg$subject[df.dmg$mycondition=="regular"]){
    idd_regular=c(idd_regular,names(CV.idd)[y])
  }else if( substr(f[k],1,9) %in% df.dmg$subject[df.dmg$mycondition=="irregular"]){
    idd_irregular=c(idd_irregular,names(CV.idd)[y])
  }
}
round(table(idd_regular)/sum(table(idd_regular)),2)
round(table(idd_irregular)/sum(table(idd_irregular)),2)

idd_regular=c()
idd_irregular=c()
for (k in 1:length(f)){
  load(paste(fld,"/",f[k],sep=""))
  BIC.idd[["random"]]=list("BIC"=-2*log(0.11)*9) 
  r=c()
  for (m in 1:length(BIC.idd)){r=c(r,BIC.idd[[m]][["BIC"]])}
  y=which(r==min(r))
  if (length(y)>1){y=sample(y,1)}
  
  if ( substr(f[k],1,9) %in% df.dmg$subject[df.dmg$mycondition=="regular"]){
    idd_regular=c(idd_regular,names(BIC.idd)[y])
  }else if( substr(f[k],1,9) %in% df.dmg$subject[df.dmg$mycondition=="irregular"]){
    idd_irregular=c(idd_irregular,names(BIC.idd)[y])
  }
}
round(table(idd_regular)/sum(table(idd_regular)),2)
round(table(idd_irregular)/sum(table(idd_irregular)),2)


#exp 2
fld="softmax_exp2"
load("exp2.Rda")
f=list.files(path = fld,pattern = "\\.Rda$")

idd_regular=c()
for (k in 1:length(f)){
  load(paste(fld,"/",f[k],sep=""))
  CV.idd[["random"]]=list("cv_log"=-log(0.11)*8) 
  r=c()
  for (m in 1:length(CV.idd)){r=c(r,CV.idd[[m]][["cv_log"]])}
  y=which(r==min(r))
  if (length(y)>1){y=sample(y,1)}
  
  idd_regular=c(idd_regular,names(CV.idd)[y])
}
table(idd_regular)/sum(table(idd_regular))

idd_regular=c()
for (k in 1:length(f)){
  load(paste(fld,"/",f[k],sep=""))
  BIC.idd[["random"]]=list("BIC"=-2*log(0.11)*8) 
  r=c()
  for (m in 1:length(BIC.idd)){r=c(r,BIC.idd[[m]][["BIC"]])}
  y=which(r==min(r))
  if (length(y)>1){y=sample(y,1)}
  
  idd_regular=c(idd_regular,names(BIC.idd)[y])
  
}
table(idd_regular)/sum(table(idd_regular))






