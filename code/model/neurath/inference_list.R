#+ load packages -------------------
#' # load packages
library(tidyr)
library(dplyr)
library(ggplot2)
library(matrixStats)
#rm(list=ls())
#+ set parameters -------------------
#' # set parameters
#' please make sure that the parameters are identical to stimuli generation file.

k_e=25
r_e=5 #e->e: m=5,var=0.25 -> 1

k_ae=9
r_ae=3 #preventative a->e: m=3,var=0.25 -> 1
k_be=2.25
r_be=1.5  #generative b->e: m=1.5,var=0.25 -> 1

trial_end=20
sampling_point=300 #for one generative one preventative



#+ load stimuli -------------------
#' # load stimuli

#1
sti_id=1# REMEMBER TO CHECK IT
sti_name="gAgB"
sti_no=1
a_pro="G"# REMEMBER TO CHECK IT
b_pro="G" # REMEMBER TO CHECK IT
load(file = paste("../myStimuli/",sti_name,sti_no,"/",sti_name,".Rda",sep = ""))
sqc=get(sti_name)
sqc$total_idx=c(1:nrow(sqc))

#2
sti_id=2# REMEMBER TO CHECK IT
sti_name="gAgB"
sti_no=2
a_pro="G"# REMEMBER TO CHECK IT
b_pro="G" # REMEMBER TO CHECK IT
load(file = paste("../myStimuli/",sti_name,sti_no,"/",sti_name,".Rda",sep = ""))
sqc=get(sti_name)
sqc$total_idx=c(1:nrow(sqc))

#3
sti_id=3# REMEMBER TO CHECK IT
sti_name="pAgB"
sti_no=1
a_pro="P"# REMEMBER TO CHECK IT
b_pro="G" # REMEMBER TO CHECK IT
load(file = paste("../myStimuli/",sti_name,sti_no,"/",sti_name,".Rda",sep = ""))
sqc=get(sti_name)
sqc$total_idx=c(1:nrow(sqc))

#4
sti_id=4# REMEMBER TO CHECK IT
sti_name="pAgB"
sti_no=2
a_pro="P"# REMEMBER TO CHECK IT
b_pro="G" # REMEMBER TO CHECK IT
load(file = paste("../myStimuli/",sti_name,sti_no,"/",sti_name,".Rda",sep = ""))
sqc=get(sti_name)
sqc$total_idx=c(1:nrow(sqc))

#5
sti_id=5# REMEMBER TO CHECK IT
sti_name="nAgB"
sti_no=1
a_pro="N"# REMEMBER TO CHECK IT
b_pro="G" # REMEMBER TO CHECK IT
load(file = paste("../myStimuli/",sti_name,sti_no,"/",sti_name,".Rda",sep = ""))
sqc=get(sti_name)
sqc$total_idx=c(1:nrow(sqc))

#6
sti_id=6# REMEMBER TO CHECK IT
sti_name="nAgB"
sti_no=2
a_pro="N"# REMEMBER TO CHECK IT
b_pro="G" # REMEMBER TO CHECK IT
load(file = paste("../myStimuli/",sti_name,sti_no,"/",sti_name,".Rda",sep = ""))
sqc=get(sti_name)
sqc$total_idx=c(1:nrow(sqc))

#7
sti_id=7# REMEMBER TO CHECK IT
sti_name="gApB"
sti_no=1
a_pro="G"# REMEMBER TO CHECK IT
b_pro="P" # REMEMBER TO CHECK IT
load(file = paste("../myStimuli/",sti_name,sti_no,"/",sti_name,".Rda",sep = ""))
sqc=get(sti_name)
sqc$total_idx=c(1:nrow(sqc))

#8
sti_id=8# REMEMBER TO CHECK IT
sti_name="gApB"
sti_no=2
a_pro="G"# REMEMBER TO CHECK IT
b_pro="P" # REMEMBER TO CHECK IT
load(file = paste("../myStimuli/",sti_name,sti_no,"/",sti_name,".Rda",sep = ""))
sqc=get(sti_name)
sqc$total_idx=c(1:nrow(sqc))

#9
sti_id=9# REMEMBER TO CHECK IT
sti_name="pApB"
sti_no=1
a_pro="P"# REMEMBER TO CHECK IT
b_pro="P" # REMEMBER TO CHECK IT
load(file = paste("../myStimuli/",sti_name,sti_no,"/",sti_name,".Rda",sep = ""))
sqc=get(sti_name)
sqc$total_idx=c(1:nrow(sqc))

#10
sti_id=10# REMEMBER TO CHECK IT
sti_name="pApB"
sti_no=2
a_pro="P"# REMEMBER TO CHECK IT
b_pro="P" # REMEMBER TO CHECK IT
load(file = paste("../myStimuli/",sti_name,sti_no,"/",sti_name,".Rda",sep = ""))
sqc=get(sti_name)
sqc$total_idx=c(1:nrow(sqc))

#11
sti_id=11# REMEMBER TO CHECK IT
sti_name="nApB"
sti_no=1
a_pro="N"# REMEMBER TO CHECK IT
b_pro="P" # REMEMBER TO CHECK IT
load(file = paste("../myStimuli/",sti_name,sti_no,"/",sti_name,".Rda",sep = ""))
sqc=get(sti_name)
sqc$total_idx=c(1:nrow(sqc))

#12
sti_id=12# REMEMBER TO CHECK IT
sti_name="nApB"
sti_no=2
a_pro="N"# REMEMBER TO CHECK IT
b_pro="P" # REMEMBER TO CHECK IT
load(file = paste("../myStimuli/",sti_name,sti_no,"/",sti_name,".Rda",sep = ""))
sqc=get(sti_name)
sqc$total_idx=c(1:nrow(sqc))

#13
sti_id=13# REMEMBER TO CHECK IT
sti_name="gAnB"
sti_no=1
a_pro="G"# REMEMBER TO CHECK IT
b_pro="N" # REMEMBER TO CHECK IT
load(file = paste("../myStimuli/",sti_name,sti_no,"/",sti_name,".Rda",sep = ""))
sqc=get(sti_name)
sqc$total_idx=c(1:nrow(sqc))

#14
sti_id=14# REMEMBER TO CHECK IT
sti_name="gAnB"
sti_no=2
a_pro="G"# REMEMBER TO CHECK IT
b_pro="N" # REMEMBER TO CHECK IT
load(file = paste("../myStimuli/",sti_name,sti_no,"/",sti_name,".Rda",sep = ""))
sqc=get(sti_name)
sqc$total_idx=c(1:nrow(sqc))

#15
sti_id=15# REMEMBER TO CHECK IT
sti_name="pAnB"
sti_no=1
a_pro="P"# REMEMBER TO CHECK IT
b_pro="N" # REMEMBER TO CHECK IT
load(file = paste("../myStimuli/",sti_name,sti_no,"/",sti_name,".Rda",sep = ""))
sqc=get(sti_name)
sqc$total_idx=c(1:nrow(sqc))

#16
sti_id=16# REMEMBER TO CHECK IT
sti_name="pAnB"
sti_no=2
a_pro="P"# REMEMBER TO CHECK IT
b_pro="N" # REMEMBER TO CHECK IT
load(file = paste("../myStimuli/",sti_name,sti_no,"/",sti_name,".Rda",sep = ""))
sqc=get(sti_name)
sqc$total_idx=c(1:nrow(sqc))

#17
sti_id=17# REMEMBER TO CHECK IT
sti_name="nAnB"
sti_no=1
a_pro="N"# REMEMBER TO CHECK IT
b_pro="N" # REMEMBER TO CHECK IT
load(file = paste("../myStimuli/",sti_name,sti_no,"/",sti_name,".Rda",sep = ""))
sqc=get(sti_name)
sqc$total_idx=c(1:nrow(sqc))

#18
sti_id=18# REMEMBER TO CHECK IT
sti_name="nAnB"
sti_no=2
a_pro="N"# REMEMBER TO CHECK IT
b_pro="N" # REMEMBER TO CHECK IT
load(file = paste("../myStimuli/",sti_name,sti_no,"/",sti_name,".Rda",sep = ""))
sqc=get(sti_name)
sqc$total_idx=c(1:nrow(sqc))
