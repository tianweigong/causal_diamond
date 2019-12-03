#+ load stimuli -------------------
#' # load stimuli
MySqc <-function(){
  sqc_raw_sub_a=subset(sqc_raw,obj=="A")
  sqc_raw_sub_a$obj_idx=seq(1:nrow(sqc_raw_sub_a))
  sqc_raw_sub_b=subset(sqc_raw,obj=="B")
  sqc_raw_sub_b$obj_idx=seq(1:nrow(sqc_raw_sub_b))
  sqc_raw_sub_ab=rbind(sqc_raw_sub_a,sqc_raw_sub_b)
  sqc_raw_sub_ab=sqc_raw_sub_ab[order(sqc_raw_sub_ab$time),]
  sqc_raw_sub_ab$ab_idx=seq(1:nrow(sqc_raw_sub_ab))
  sqc_raw_sub_e=subset(sqc_raw,obj=="E")
  sqc_raw_sub_e$obj_idx=seq(1:nrow(sqc_raw_sub_e))
  sqc_raw_sub_e$ab_idx=rep(0,nrow(sqc_raw_sub_e))
  sqc_raw<<-rbind(sqc_raw_sub_ab,sqc_raw_sub_e)
  sqc_raw<<-sqc_raw[order(sqc_raw$time),]
  sqc_raw$total_idx<<-c(1:nrow(sqc_raw))
}

filepath="../../../stimulus/pilot/stimulus_sqc/"

#1
sti_id=1# REMEMBER TO CHECK IT
sti_name="gAgB"
sti_no=1
a_pro="G"# REMEMBER TO CHECK IT
b_pro="G" # REMEMBER TO CHECK IT
load(file = paste(filepath,sti_name,sti_no,"/",sti_name,".Rda",sep = ""))
sqc_raw=get(sti_name)
MySqc()

#2
sti_id=2# REMEMBER TO CHECK IT
sti_name="gAgB"
sti_no=2
a_pro="G"# REMEMBER TO CHECK IT
b_pro="G" # REMEMBER TO CHECK IT
load(file = paste(filepath,sti_name,sti_no,"/",sti_name,".Rda",sep = ""))
sqc_raw=get(sti_name)
MySqc()

#3
sti_id=3# REMEMBER TO CHECK IT
sti_name="pAgB"
sti_no=1
a_pro="P"# REMEMBER TO CHECK IT
b_pro="G" # REMEMBER TO CHECK IT
load(file = paste(filepath,sti_name,sti_no,"/",sti_name,".Rda",sep = ""))
sqc_raw=get(sti_name)
MySqc()

#4
sti_id=4# REMEMBER TO CHECK IT
sti_name="pAgB"
sti_no=2
a_pro="P"# REMEMBER TO CHECK IT
b_pro="G" # REMEMBER TO CHECK IT
load(file = paste(filepath,sti_name,sti_no,"/",sti_name,".Rda",sep = ""))
sqc_raw=get(sti_name)
MySqc()

#5
sti_id=5# REMEMBER TO CHECK IT
sti_name="nAgB"
sti_no=1
a_pro="N"# REMEMBER TO CHECK IT
b_pro="G" # REMEMBER TO CHECK IT
load(file = paste(filepath,sti_name,sti_no,"/",sti_name,".Rda",sep = ""))
sqc_raw=get(sti_name)
MySqc()

#6
sti_id=6# REMEMBER TO CHECK IT
sti_name="nAgB"
sti_no=2
a_pro="N"# REMEMBER TO CHECK IT
b_pro="G" # REMEMBER TO CHECK IT
load(file = paste(filepath,sti_name,sti_no,"/",sti_name,".Rda",sep = ""))
sqc_raw=get(sti_name)
MySqc()

#7
sti_id=7# REMEMBER TO CHECK IT
sti_name="gApB"
sti_no=1
a_pro="G"# REMEMBER TO CHECK IT
b_pro="P" # REMEMBER TO CHECK IT
load(file = paste(filepath,sti_name,sti_no,"/",sti_name,".Rda",sep = ""))
sqc_raw=get(sti_name)
MySqc()

#8
sti_id=8# REMEMBER TO CHECK IT
sti_name="gApB"
sti_no=2
a_pro="G"# REMEMBER TO CHECK IT
b_pro="P" # REMEMBER TO CHECK IT
load(file = paste(filepath,sti_name,sti_no,"/",sti_name,".Rda",sep = ""))
sqc_raw=get(sti_name)
MySqc()

#9
sti_id=9# REMEMBER TO CHECK IT
sti_name="pApB"
sti_no=1
a_pro="P"# REMEMBER TO CHECK IT
b_pro="P" # REMEMBER TO CHECK IT
load(file = paste(filepath,sti_name,sti_no,"/",sti_name,".Rda",sep = ""))
sqc_raw=get(sti_name)
MySqc()

#10
sti_id=10# REMEMBER TO CHECK IT
sti_name="pApB"
sti_no=2
a_pro="P"# REMEMBER TO CHECK IT
b_pro="P" # REMEMBER TO CHECK IT
load(file = paste(filepath,sti_name,sti_no,"/",sti_name,".Rda",sep = ""))
sqc_raw=get(sti_name)
MySqc()

#11
sti_id=11# REMEMBER TO CHECK IT
sti_name="nApB"
sti_no=1
a_pro="N"# REMEMBER TO CHECK IT
b_pro="P" # REMEMBER TO CHECK IT
load(file = paste(filepath,sti_name,sti_no,"/",sti_name,".Rda",sep = ""))
sqc_raw=get(sti_name)
MySqc()

#12
sti_id=12# REMEMBER TO CHECK IT
sti_name="nApB"
sti_no=2
a_pro="N"# REMEMBER TO CHECK IT
b_pro="P" # REMEMBER TO CHECK IT
load(file = paste(filepath,sti_name,sti_no,"/",sti_name,".Rda",sep = ""))
sqc_raw=get(sti_name)
MySqc()

#13
sti_id=13# REMEMBER TO CHECK IT
sti_name="gAnB"
sti_no=1
a_pro="G"# REMEMBER TO CHECK IT
b_pro="N" # REMEMBER TO CHECK IT
load(file = paste(filepath,sti_name,sti_no,"/",sti_name,".Rda",sep = ""))
sqc_raw=get(sti_name)
MySqc()

#14
sti_id=14# REMEMBER TO CHECK IT
sti_name="gAnB"
sti_no=2
a_pro="G"# REMEMBER TO CHECK IT
b_pro="N" # REMEMBER TO CHECK IT
load(file = paste(filepath,sti_name,sti_no,"/",sti_name,".Rda",sep = ""))
sqc_raw=get(sti_name)
MySqc()

#15
sti_id=15# REMEMBER TO CHECK IT
sti_name="pAnB"
sti_no=1
a_pro="P"# REMEMBER TO CHECK IT
b_pro="N" # REMEMBER TO CHECK IT
load(file = paste(filepath,sti_name,sti_no,"/",sti_name,".Rda",sep = ""))
sqc_raw=get(sti_name)
MySqc()

#16
sti_id=16# REMEMBER TO CHECK IT
sti_name="pAnB"
sti_no=2
a_pro="P"# REMEMBER TO CHECK IT
b_pro="N" # REMEMBER TO CHECK IT
load(file = paste(filepath,sti_name,sti_no,"/",sti_name,".Rda",sep = ""))
sqc_raw=get(sti_name)
MySqc()

#17
sti_id=17# REMEMBER TO CHECK IT
sti_name="nAnB"
sti_no=1
a_pro="N"# REMEMBER TO CHECK IT
b_pro="N" # REMEMBER TO CHECK IT
load(file = paste(filepath,sti_name,sti_no,"/",sti_name,".Rda",sep = ""))
sqc_raw=get(sti_name)
MySqc()

#18
sti_id=18# REMEMBER TO CHECK IT
sti_name="nAnB"
sti_no=2
a_pro="N"# REMEMBER TO CHECK IT
b_pro="N" # REMEMBER TO CHECK IT
load(file = paste(filepath,sti_name,sti_no,"/",sti_name,".Rda",sep = ""))
sqc_raw=get(sti_name)
MySqc()
