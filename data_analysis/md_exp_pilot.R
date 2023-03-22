library(dplyr)
library(tidyr)
library(matrixStats)
library(parallel)
library(spatstat)
source('fun_nor.R')

set.seed(2)

whole_trial_end=trial_end=20
s_ab=3
s_e=30
k_pe = 36
r_pe = 12 #preventative p->e: m=3,var=0.25
k_ge = 9
r_ge = 6  #generative g->e: m=1.5,var=0.25

baserate=5
baserate_var=0.25
r_e_r = baserate/baserate_var
k_e_r = r_e_r*baserate
k_e_u=1
r_e_u=1/baserate
st_a=c("G","G","G","N","N","N","P","P","P")
st_b=c("G","N","P","G","N","P","G","N","P")
sti_list=c("1.gg1.Rda","2.gg2.Rda",
           "3.pg1.Rda","4.pg2.Rda",
           "5.ng1.Rda","6.ng2.Rda",
           "7.gp1.Rda","8.gp2.Rda",
           "9.pp1.Rda","10.pp2.Rda",
           "11.np1.Rda","12.np2.Rda",
           "13.gn1.Rda","14.gn2.Rda",
           "15.pn1.Rda","16.pn2.Rda",
           "17.nn1.Rda","18.nn2.Rda")
a_pro_list=rep(rep(c("G","P","N"),each=2),3)
b_pro_list=rep(c("G","P","N"),each=6)
ru_pro_list=rep("r",18)

MyNorMdRun<-function(){
  md=data.frame()
  for (k in 1:length(sti_list)){
    load(paste(filepath,"/",sti_list[k],sep=""))
    df=data.frame(seed=k,sti_id=sti_list[k],ru_pro=ru_pro_list[k],
                  a_pro=a_pro_list[k],b_pro=b_pro_list[k],A_state=st_a,B_state=st_b)
    if (ru_pro_list[k]=="r"){k_e=k_e_r;r_e=r_e_r}else if (ru_pro_list[k]=="u"){k_e=k_e_u;r_e=r_e_u}
    df$nor=MyMdNor(sqc_raw_long,k_e,r_e,k_pe,r_pe,k_ge,r_ge)
    #mismatch delay
    if (ru_pro_list[k]=="u"){k_e=k_e_r;r_e=r_e_r}else if (ru_pro_list[k]=="r"){k_e=k_e_u;r_e=r_e_u}
    df$nor_mis=MyMdNor(sqc_raw_long,k_e,r_e,k_pe,r_pe,k_ge,r_ge)
    
    md=rbind(md,df)
  }
  
  save(md,file=paste(new_filepath,"/","nor_.Rda",sep = ""))
}

MyFeaMdRun<-function(){
  md=data.frame()
  for (k in 1:length(sti_list)){
    load(paste(filepath,"/",sti_list[k],sep=""))
    df=data.frame(seed=k,sti_id=sti_list[k],ru_pro=ru_pro_list[k],
                  a_pro=a_pro_list[k],b_pro=b_pro_list[k],A_state=st_a,B_state=st_b)
    df[,varname]=MyMdFea(sqc_raw_long,ru_pro_list[k])
    df[,combined_var]=rowSums(df[,varname])/sum(df[,varname])
    
    #mismatch
    df[,paste(varname,"mis",sep="_")]=MyMdFea(sqc_raw_long,setdiff(c("r","u"),ru_pro_list[k]))
    df[,paste(combined_var,"mis",sep="_")]=rowSums(df[,paste(varname,"mis",sep="_")])/sum(df[,paste(varname,"mis",sep="_")])
    
    md=rbind(md,df)
  }
  
  save(md,file=paste(new_filepath,"/",modname,"_.Rda",sep = ""))
}


filepath="sti_exp1"
new_filepath="model_exp1"

#normative
MyNorMdRun()

#prepare for feature_based model
load("df.expect.Rda")
df.e=df.expect %>% gather(cue, val, A_delay:B_numw7) %>% na.omit()

#feature  intervention-based
numcue=c("A_numi","B_numi")
varname=c('delay_i',"count_i")
combined_var="fea_i"
modname="feai"
segmod="intervention"

source('fun_fea.R')
MyFeaMdRun()

#window
segmod="window"
for (k in c(1,1.5,2,2.5,3,3.5,4,4.5,5,5.5,6,6.5,7)){
  win_len=k
  numcue=paste(c("A_numw","B_numw"),k,sep="")
  varname=paste(c('delay_w',"count_w"),k,sep="")
  combined_var=paste("fea_w",k,sep="")
  modname=paste("feaw",k,sep = "")
  source('fun_fea.R')
  MyFeaMdRun()
}

#combined
fld="model_exp1"
f=list.files(path = fld,pattern = "\\.Rda$")
ff=strsplit(f,"_")
mdname=c()
for (k in 1:length(ff)){mdname=c(mdname,ff[[k]][1])}
mdname=unique(mdname)
md.list=list()
for (k in 1:length(mdname)){md.list[[mdname[k]]]=data.frame()}
for (k in 1:length(f)){
  load(paste(fld,"/",f[k],sep=""))
  md.list[[ff[[k]][1]]]=rbind(md.list[[ff[[k]][1]]],md)
}

mdall=md.list[[mdname[[1]]]]
for (k in 2:length(mdname)){mdall=merge(mdall,md.list[[mdname[[k]]]])}
mdall=mdall %>% 
  mutate(choice=paste(A_state,B_state,sep = ""),
         trial_type=paste(a_pro,b_pro,sep=""),
         uni_id=paste(seed,ru_pro,trial_type,sep="_"),
         A_state=NULL,B_state=NULL,a_pro=NULL,b_pro=NULL,sti_id=NULL)
mdall$mycondition=NA
mdall$mycondition[which(mdall$ru_pro=="r")]="regular"
mdall$mycondition[which(mdall$ru_pro=="u")]="irregular"

save(mdall,file="mdall_exp1.Rda")
