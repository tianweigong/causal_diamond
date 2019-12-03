sqc=sqc_raw
#+ path model -------------------
#' # path model
#' ## generate all paths
elist=which(sqc$obj=="E")[-1]
p=2
sig_len_cum=c()
sig_path=as.data.frame(matrix(NA,ncol=3,nrow=0)) %>%
  setNames(c("obj","cause","cause_idx"))
for (i in elist){
  sig_path[nrow(sig_path)+1,]=c(p,"E",1)
  
  sig_len_cum=c(sig_len_cum,nrow(sig_path))
  p=p+1
  
}
sig_len=c(sig_len_cum[1],diff(sig_len_cum))
sig_path$obj=as.numeric(sig_path$obj)
sig_path$cause_idx=as.numeric(sig_path$cause_idx)

all_path_mtx=matrix(NA,ncol=length(sig_len),nrow=0)
p=0
MyLeaf(c(1),0)

#' ## check each path
valid_path=MyValidPath()

#+ delay model -------------------
#' # delay model
whole_prob=0
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
    if (cdd_path$cause[j]=="E"){
      #principle 3: E->->E
      #using approximation method to find the probability (disjuction thinking here)
      #no hidden baserate
      prob_sub=dgamma(t1-t0, shape=k_e, rate=r_e)
    }
    prob=prob*prob_sub
  }
 
  #principle 3: give explanation for inactivation
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
model_nAnB=whole_prob
