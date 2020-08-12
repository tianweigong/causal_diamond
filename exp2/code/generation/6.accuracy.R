#+ General settings, echo = FALSE, results = 'hide' -------------------
knitr::opts_chunk$set(warning = FALSE, message = FALSE)

#+ load packages -------------------
#' # load packages
library(tidyr)
library(dplyr)
library(ggplot2)
# rm(list=ls())
#+ test parameters -------------------
#' # test parameters
filepath="../../stimulus/exp2/"
a_pro_list=c(rep("G",6),rep("N",6),rep("P",6))
b_pro_list=rep(c(rep("G",2),rep("N",2),rep("P",2)),3)
ru_pro_list=rep(c("r","u"),9)

dt.regular=data.frame()
dt.irregular=data.frame()
for (k in 1:18){
  
  load(paste(filepath,"seed",k,"/allstimuli.Rda",sep=""))
  
  regular=dt.sti.big %>% subset(sti_id %% 2==1 & A_pro==A_state & B_pro==B_state)
  dt.regular=rbind(dt.regular,regular)
  
  irregular=dt.sti.big %>% subset(sti_id %% 2==0 & A_pro==A_state & B_pro==B_state)
  dt.irregular=rbind(dt.irregular,irregular)
}

mean(dt.regular$normative)
mean(dt.irregular$normative)
t.test(dt.regular$normative,dt.irregular$normative,paired = T)

mean(dt.regular$delay)
mean(dt.irregular$delay)
t.test(dt.regular$delay,dt.irregular$delay,paired = T)

mean(dt.regular$number)
mean(dt.irregular$number)
t.test(dt.regular$number,dt.irregular$number,paired = T)
