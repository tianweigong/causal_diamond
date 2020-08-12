sep_stage=1
st_a=c("G","G","G","N","N","N","P","P","P")
st_b=c("G","N","P","G","N","P","G","N","P")
MySqc <-function(sqc_raw){
  sqc_raw_sub_a=subset(sqc_raw,obj=="A")
  if (nrow(sqc_raw_sub_a)>0){
    sqc_raw_sub_a$obj_idx=seq(1:nrow(sqc_raw_sub_a))
  }
  sqc_raw_sub_b=subset(sqc_raw,obj=="B")
  if (nrow(sqc_raw_sub_b)>0){
    sqc_raw_sub_b$obj_idx=seq(1:nrow(sqc_raw_sub_b))
  }
  sqc_raw_sub_ab=rbind(sqc_raw_sub_a,sqc_raw_sub_b)
  if (nrow(sqc_raw_sub_ab)>0){
    sqc_raw_sub_ab=sqc_raw_sub_ab[order(sqc_raw_sub_ab$time),]
    sqc_raw_sub_ab$ab_idx=seq(1:nrow(sqc_raw_sub_ab))
  }
  sqc_raw_sub_e=subset(sqc_raw,obj=="E")
  if(nrow(sqc_raw_sub_e)>0){
    sqc_raw_sub_e$obj_idx=seq(1:nrow(sqc_raw_sub_e))
    sqc_raw_sub_e$ab_idx=rep(0,nrow(sqc_raw_sub_e))
  }
  sqc_raw=rbind(sqc_raw_sub_ab,sqc_raw_sub_e)
  sqc_raw=sqc_raw[order(sqc_raw$time),]
  sqc_raw$total_idx=c(1:nrow(sqc_raw))
  return(sqc_raw)
}

MyOnlineInfer_nor <- function(sqc_raw_long,k_e,r_e,k_pe,r_pe,k_ge,r_ge){
  sqc_raw=sqc_raw_long
  sqc=MySqc(sqc_raw)
  sim_prob=MySummary(sqc,k_e,r_e,k_pe,r_pe,k_ge,r_ge) #normative model
  return(sim_prob)
}

MyOnlineInfer_fea <- function(sqc_raw_long,rl,br){
  sqc_raw=sqc_raw_long
  sqc=MySqc(sqc_raw)
  sim_prob=myFeatureBased(sqc,rl,br)
  return(sim_prob)
}


MyMod6<- function(md_raw,md_ratio){
  
  md=as.data.frame(matrix(NA, nrow = 18*2*3, ncol = 4))%>%
    setNames(c("sti_id","cpn","state","ratio"))
  
  for (j in 1:18){
    md$cpn[((j-1)*2*3+1):(j*2*3)]=c("A","A","A","B","B","B")
    md$state[((j-1)*2*3+1):(j*2*3)]=c("G","N","P","G","N","P")
    md$sti_id[((j-1)*2*3+1):(j*2*3)]=j
    
    md$ratio[(j-1)*2*3+1]=sum(md_ratio[md_raw$A_state=="G" & md_raw$sti_id==j])
    md$ratio[(j-1)*2*3+2]=sum(md_ratio[md_raw$A_state=="N" & md_raw$sti_id==j])
    md$ratio[(j-1)*2*3+3]=sum(md_ratio[md_raw$A_state=="P" & md_raw$sti_id==j])
    md$ratio[(j-1)*2*3+4]=sum(md_ratio[md_raw$B_state=="G" & md_raw$sti_id==j])
    md$ratio[(j-1)*2*3+5]=sum(md_ratio[md_raw$B_state=="N" & md_raw$sti_id==j])
    md$ratio[(j-1)*2*3+6]=sum(md_ratio[md_raw$B_state=="P" & md_raw$sti_id==j])
  }
  return(md)
}



pic.label=c("1"="GG_r","2"="GG_u","3"="GN_r","4"="GN_u","5"="GP_r","6"="GP_u",
            "7"="NG_r","8"="NG_u","9"="NN_r","10"="NN_u","11"="NP_r","12"="NP_u",
            "13"="PG_r","14"="PG_u","15"="PN_r","16"="PN_u","17"="PP_r","18"="PP_u")