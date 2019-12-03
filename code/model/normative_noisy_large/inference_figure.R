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
# library(tidyr)
# library(dplyr)
# library(ggplot2)
# rm(list=ls())
options(scipen = 0) 
options(digits = 2)

fig_a=c(rep(c("1.Generative","2.Preventative","3.Non-causal"),3))
fig_b=c(rep("3.Generative",3),rep("2.Preventative",3),rep("1.Non-causal",3))

#fig_data=c(10^(-300),0.006,2*10^(-260),3*10^(-18),10^(-300),10^(-300),2*10^(-238),10^(-300),10^(-270))
fig_data=c(model_gAgB,model_pAgB,model_nAgB,model_gApB,model_pApB,model_nApB,model_gAnB,model_pAnB,model_nAnB)
fig_data_pre= format(fig_data, scientific = TRUE, trim = TRUE)
fig_data_pre[fig_data_pre=="0"]="trivial"

dt.fig=data.frame(fig_a,fig_b,fig_data,fig_data_pre)

name_summary="practice_panb_modirate"

ggplot(dt.fig,aes(x=fig_a, y=fig_b, fill=fig_data)) + 
  geom_tile()+
  scale_fill_gradientn(colours = c("white","steelblue"),trans = "log")+
  geom_text(aes(label = fig_data_pre))+
  ylab("Component B")+
  xlab("Component A")+
  theme(legend.position = "none")+
  ggtitle(name_summary)
# 
ggsave(paste(name_summary,".pdf",sep = ""), width = 5, height = 4)
save(dt.fig,file=paste(name_summary,".Rda",sep = ""))
