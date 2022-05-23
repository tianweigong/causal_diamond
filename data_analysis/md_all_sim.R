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
eachgroup=9*2
st_a=c("G","G","G","N","N","N","P","P","P")
st_b=c("G","N","P","G","N","P","G","N","P")
sti_list=c("sqc.gg_r","sqc.gg_u",
           "sqc.gn_r","sqc.gn_u",
           "sqc.gp_r","sqc.gp_u",
           "sqc.ng_r","sqc.ng_u",
           "sqc.nn_r","sqc.nn_u",
           "sqc.np_r","sqc.np_u",
           "sqc.pg_r","sqc.pg_u",
           "sqc.pn_r","sqc.pn_u",
           "sqc.pp_r","sqc.pp_u")
a_pro_list=c(rep("G",6),rep("N",6),rep("P",6))
b_pro_list=rep(c(rep("G",2),rep("N",2),rep("P",2)),3)
ru_pro_list=rep(c("r","u"),9)

MyNorMdRun<-function(seed_fld){
  load(paste(filepath,"/",'seed',seed_fld,".Rda",sep = ""))
  md=data.frame()
  for (k in 1:length(sti_list)){
    df=data.frame(seed=seed_fld,sti_id=sti_list[k],ru_pro=ru_pro_list[k],
                  a_pro=a_pro_list[k],b_pro=b_pro_list[k],A_state=st_a,B_state=st_b)
    if (ru_pro_list[k]=="r"){k_e=k_e_r;r_e=r_e_r}else if (ru_pro_list[k]=="u"){k_e=k_e_u;r_e=r_e_u}
    sqc_raw_long=get(sti_list[k])
    df$nor=MyMdNor(sqc_raw_long,k_e,r_e,k_pe,r_pe,k_ge,r_ge)
    #mismatch delay
    # if (ru_pro_list[k]=="u"){k_e=k_e_r;r_e=r_e_r}else if (ru_pro_list[k]=="r"){k_e=k_e_u;r_e=r_e_u}
    # sqc_raw_long=get(sti_list[k])
    # df$nor_mis=MyMdNor(sqc_raw_long,k_e,r_e,k_pe,r_pe,k_ge,r_ge)
    
    md=rbind(md,df)
  }
  save(md,file=paste(new_filepath,"/","nor_",seed_fld,".Rda",sep = ""))
}

MyFeaMdRun<-function(seed_fld){
  load(paste(filepath,"/",'seed',seed_fld,".Rda",sep = ""))
  md=data.frame()
  for (k in 1:length(sti_list)){
    df=data.frame(seed=seed_fld,sti_id=sti_list[k],ru_pro=ru_pro_list[k],
                  a_pro=a_pro_list[k],b_pro=b_pro_list[k],A_state=st_a,B_state=st_b)
    sqc_raw_long=get(sti_list[k])
    df[,varname]=MyMdFea(sqc_raw_long,ru_pro_list[k])
    df[,combined_var]=rowSums(df[,varname])/sum(df[,varname])
    
    #mismatch
    # df[,paste(varname,"mis",sep="_")]=MyMdFea(sqc_raw_long,setdiff(c("r","u"),ru_pro_list[k]))
    # df[,paste(combined_var,"mis",sep="_")]=rowSums(df[,paste(varname,"mis",sep="_")])/sum(df[,paste(varname,"mis",sep="_")])
    # 
    md=rbind(md,df)
  }
  save(md,file=paste(new_filepath,"/",modname,"_",seed_fld,".Rda",sep = ""))
}


filepath="sti_simulation"
new_filepath="model_simulation"

sim_num=300

#normative
wholelist=as.list(c(1:sim_num))
mclapply(wholelist,MyNorMdRun,mc.cores=20)

#prepare for feature_based model
load("df.expect.Rda")
df.e=df.expect %>% gather(cue, val, A_delay:B_numw5) %>% na.omit()

#feature  intervention-based
numcue=c("A_numi","B_numi")
varname=c('delay_i',"count_i")
combined_var="fea_i"
modname="feai"
segmod="intervention"

source('fun_fea.R')
wholelist=as.list(c(1:sim_num))
mclapply(wholelist,MyFeaMdRun,mc.cores=20)

#window
segmod="window"
for (k in c(4)){#c(2,2.5,3,3.5,4,4.5,5)
  win_len=k
  numcue=paste(c("A_numw","B_numw"),k,sep="")
  varname=paste(c('delay_w',"count_w"),k,sep="")
  combined_var=paste("fea_w",k,sep="")
  modname=paste("feaw",k,sep = "")
  source('fun_fea.R')
  wholelist=as.list(c(1:sim_num))
  mclapply(wholelist,MyFeaMdRun,mc.cores=20)
}

#combined
fld=new_filepath
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

mdall$order_a=mdall$order_b=NA
for (k in unique(mdall$seed)){
  idx=which(mdall$seed==k)
  load(paste(filepath,"/",'seed',k,".Rda",sep = ""))
  o=subset(sqc.gg_r,obj!="E")$obj
  mdall$order_a[idx]=sum(diff(which(o=="A"))==1)
  mdall$order_b[idx]=sum(diff(which(o=="B"))==1)
}

mdall=mdall%>%mutate(order_all=order_a+order_b,
               uni_label=paste(seed,trial_type,mycondition,sep="_"))

save(mdall,file="mdall_sim.Rda")

# #simulate
# mySimLarge<-function(pr,amp=10){
#   md=data.frame()
#   for (k in unique(mdall$uni_label)){
#     d=mdall%>% subset(uni_label==k)
#     df=subset(d,select = c("uni_label","trial_type","mycondition","order_all","order_a","order_b"))%>%
#       mutate(A_pro=substr(trial_type,1,1),B_pro=substr(trial_type,2,2))
#     df=df[rep(1,amp),]
#     df$choice=sample(d$choice,size=amp,prob=d[,pr],replace = T)
#     df=df%>%mutate(A_state=substr(choice,1,1),B_state=substr(choice,2,2))
#     md=rbind(md,df)
#   }
#   return(md)
# }
# 
# # load("mdall_sim.Rda")
# # 
# md.nor_large=mySimLarge("nor")
# md.feai_large=mySimLarge("fea_i")
# md.feaw_large=mySimLarge("fea_w4")
# # 
# md.simLarge=list("nor_large"=md.nor_large,"feai_large"=md.feai_large,"feaw_large"=md.feaw_large)
# save(md.simLarge,md.nor_large,md.feai_large,md.feaw_large,file="md.simLarge.Rda")