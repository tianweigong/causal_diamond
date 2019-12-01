sqc=sqc_raw
#+ path model -------------------
#' # path model
#' ## generate all paths
elist=which(sqc$obj=="E")[-1]
bflag=1
aflag=1
p=2
sig_len_cum=c()
sig_path=as.data.frame(matrix(NA,ncol=3,nrow=0)) %>%
  setNames(c("obj","cause","cause_idx"))

for (i in elist){
  sig_path[nrow(sig_path)+1,]=c(p,"E",1)
  
  for (j in 1:i){
    if (sqc$obj[j]=="A"){
      sig_path[nrow(sig_path)+1,]=c(p,"A",aflag)
      aflag=aflag+1
    }
    
    if (sqc$obj[j]=="B"){
      sig_path[nrow(sig_path)+1,]=c(p,"B",bflag)
      bflag=bflag+1
    }
  }
  sig_len_cum=c(sig_len_cum,nrow(sig_path))
  
  aflag=1
  bflag=1
  p=p+1
}
sig_len=c(sig_len_cum[1],diff(sig_len_cum))
sig_path$obj=as.numeric(sig_path$obj)
sig_path$cause_idx=as.numeric(sig_path$cause_idx)

all_path_mtx=matrix(NA,ncol=length(sig_len),nrow=0)
p=0
MyLeaf(c(1),0)
# all_path_mtx=all_path_mtx[!duplicated(all_path_mtx),]

#' ## check each path
valid_path=MyValidPath()

#+ delay model -------------------
#' # delay model
whole_prob=0
a_index=which(sqc$obj=="A")
b_index=which(sqc$obj=="B")
prelist=c()
prob_each_path=c()

#begin to calculate the probability for each path
for (i in unique(valid_path$path)){
  cdd_path=valid_path[valid_path$path==i,]
  prob=1
  e_index=c(1,which(sqc$obj=="E" & sqc$obj_idx %in% cdd_path$obj[cdd_path$cause=="E"]))
  #principle 2: generative causes
  prob_sub=1
  for (j in 1:nrow(cdd_path)){
    t1=sqc$time[sqc$obj=="E" & sqc$obj_idx==cdd_path$obj[j]]
    t0=MyT0()
    
    if (cdd_path$cause[j]=="A"){
      prob_sub=dgamma(t1-t0, shape=k_be, rate=r_be) #"be" used to reprevent generative causes
    }
    
    if (cdd_path$cause[j]=="B"){
      prob_sub=dgamma(t1-t0, shape=k_be, rate=r_be)
    }
    
    if (cdd_path$cause[j]=="E"){
      prob_sub=dgamma(t1-t0, shape=k_e, rate=r_e)
    }
    prob=prob*prob_sub
  }
  #principle 3: give explanation for inactivation
  prob_sub=1
  for (j in a_index){
    a_time=sqc$time[j]
    if (sum(cdd_path$cause=="A" & cdd_path$cause_idx==sqc$obj_idx[j])==0){
      prob_sub_beyond=1-pgamma(trial_end-a_time, shape=k_be, rate=r_be)
      prob_sub=prob_sub*prob_sub_beyond
    }
  }
  prob=prob*prob_sub
  #replicating on B
  prob_sub=1
  for (j in b_index){
    b_time=sqc$time[j]
    if (sum(cdd_path$cause=="B" & cdd_path$cause_idx==sqc$obj_idx[j])==0){
      prob_sub_beyond=1-pgamma(trial_end-b_time, shape=k_be, rate=r_be)
      prob_sub=prob_sub*prob_sub_beyond
    }
  }
  prob=prob*prob_sub
  
  #replicating on E baserate
  prob_sub=MyEndHidden()
  prob=prob*prob_sub
  #record the prob of the current path
  prob_each_path=c(prob_each_path,prob)
  #add up all paths
  # whole_prob=whole_prob+prob
  #another approch to add all path but usually get 0 when it is not the correct model
  #for R will simplify the product culculation.
  if (i==unique(valid_path$path)[1]){
    whole_prob=prob
  }else{
    whole_prob=1-(1-whole_prob)*(1-prob)
  }
}

if (whole_prob<max(prob_each_path)){
  whole_prob=max(prob_each_path)
}
plot(prob_each_path)
model_gAgB=whole_prob


