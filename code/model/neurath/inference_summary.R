#' ---
#' title: Time & Prevention <br> Stimuli Inference <br> delay Model
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

df.simulate=simulate_result=as.data.frame(matrix(NA,ncol=6,nrow=0)) %>%
  setNames(c("id","trial_id","A_pro","B_pro","A_state","B_state"))

filelist=list.files(path = ".",pattern = ".Rda$") #caution!!!
for (i in 1:length(filelist)){
  load(filelist[i])
  df.simulate=rbind(df.simulate,simulate_result)
}

save(df.simulate,file="df.simulate.Rda")
