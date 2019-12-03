#' ---
#' title: Time & Prevention <br> Stimuli Inference <br> preventative A & generative B
#' author: Tia Gong
#' output:
#'    html_document:
#'      toc: true
#'      toc_depth: 3
#'      toc_float: true
#'      theme: default
#'      highlight: tango
#' ---

#+ General settings, echo = FALSE, results = 'hide' -------------------
knitr::opts_chunk$set(warning=FALSE, message=FALSE)

#+ load packages -------------------
#' # load packages
library(tidyr)
library(dplyr)
library(ggplot2)
# rm(list=ls())

sim_prob=c(model_gAgB,model_pAgB,model_nAgB,
          model_gApB,model_pApB,model_nApB,
          model_gAnB,model_pAnB,model_nAnB)

st_a=c("G","P","N","G","P","N","G","P","N")
st_b=c("G","G","G","P","P","P","N","N","N")
  
simulate_result=as.data.frame(matrix(NA,ncol=6,nrow=sim_subject)) %>%
  setNames(c("id","trial_id","A_pro","B_pro","A_state","B_state"))
simulate_result$trial_id=sti_id
simulate_result$A_pro=a_pro
simulate_result$B_pro=b_pro

for (i in 1:sim_subject){
  m=sample(c(1,2,3,4,5,6,7,8,9),size=1,prob=sim_prob,replace = T)
  simulate_result$id[i]=i
  simulate_result$A_state[i]= st_a[m]
  simulate_result$B_state[i]= st_b[m]
}

save(simulate_result,file=paste(sti_name,sti_no,".Rda",sep = ""))