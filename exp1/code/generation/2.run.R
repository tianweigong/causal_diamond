sep_stage=1
st_a=c("G","P","N","G","P","N","G","P","N")
st_b=c("G","G","G","P","P","P","N","N","N")
MySqc <-function(){
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
  sqc_raw<<-rbind(sqc_raw_sub_ab,sqc_raw_sub_e)
  sqc_raw<<-sqc_raw[order(sqc_raw$time),]
  sqc_raw$total_idx<<-c(1:nrow(sqc_raw))
}

MyOnlineInfer <- function(){
  online_inder<<-as.data.frame(matrix(NA,ncol=8,nrow=0)) %>%
    setNames(c("trial_id","A_pro","B_pro","A_state","B_state","ratio","stage","model"))

  for (i in 1:sep_stage){
    sqc_raw <<-sqc_raw_long %>% subset(time<i*(whole_trial_end/sep_stage))
    MySqc()
    trial_end<<- i*(whole_trial_end/sep_stage)
    
    MySummary() #normative model
    model_result$stage<<-i
    model_result$model<<-"normative"
    online_inder<<-rbind(model_result,online_inder)
    
  }
  save(online_inder,file=paste("norm_",sti_name,".Rda",sep = ""))
}
