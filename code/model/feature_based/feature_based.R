#+ load packages and functions  -------------------
#' # load packages and functions

#see feature_simulation
load("df.expect.Rda")
#+ infer the structure  -------------------
#' # infer the structure
#mean between C & nearest E, min between C & nearest E, the last one is set at 20s
myFeatureBased <-function(){

df.sqc<<-as.data.frame(matrix(NA,ncol=12,nrow=1)) %>%
  setNames(c("A_delay1","A_delay2","A_delay3",
           "B_delay1","B_delay2","B_delay3",
           "A_num1","A_num2","A_num3",
           "B_num1","B_num2","B_num3"))
myStatsSummary(sqc,0,0)

df.compare=as.data.frame(matrix(NA,ncol=4,nrow=9)) %>%
  setNames(c("A_pro","B_pro","mean","num"))
df.compare$A_pro=c("G","P","N","G","P","N","G","P","N")
df.compare$B_pro=c("G","G","G","P","P","P","N","N","N")
# cue_list_a=c("A_mean","A_num")
# cue_list_b=c("B_mean","B_num")
cue_list=c("delay","num")
for (i in 1:9){
      
      df.expect.sub=df.expect %>% subset(A_pro==df.compare$A_pro[i])
      
      df.dens=density(df.expect.sub[,"A_delay1"])
      dens.a1=approx(df.dens$x,df.dens$y,xout=df.sqc[1,"A_delay1"],rule=2)$y
      df.dens=density(df.expect.sub[,"A_delay2"])
      dens.a2=approx(df.dens$x,df.dens$y,xout=df.sqc[1,"A_delay2"],rule=2)$y
      df.dens=density(df.expect.sub[,"A_delay3"])
      dens.a3=approx(df.dens$x,df.dens$y,xout=df.sqc[1,"A_delay3"],rule=2)$y
      
      if(is.na(df.sqc$A_num1[1])){
        coun.a1=sum(is.na(df.expect.sub$A_num1))/nrow(df.expect.sub)
      }else{
        coun.a1=nrow(subset(df.expect.sub,A_num1==df.sqc$A_num1[1]))/nrow(df.expect.sub)
      }
      if(is.na(df.sqc$A_num2[1])){
        coun.a2=sum(is.na(df.expect.sub$A_num2))/nrow(df.expect.sub)
      }else{
        coun.a2=nrow(subset(df.expect.sub,A_num2==df.sqc$A_num2[1]))/nrow(df.expect.sub)
      }
      if(is.na(df.sqc$A_num3[1])){
        coun.a3=sum(is.na(df.expect.sub$A_num3))/nrow(df.expect.sub)
      }else{
        coun.a3=nrow(subset(df.expect.sub,A_num3==df.sqc$A_num3[1]))/nrow(df.expect.sub)
      }
      
      
      df.expect.sub=df.expect %>% subset(B_pro==df.compare$B_pro[i])
      
      df.dens=density(df.expect.sub[,"B_delay1"])
      dens.b1=approx(df.dens$x,df.dens$y,xout=df.sqc[1,"B_delay1"],rule=2)$y
      df.dens=density(df.expect.sub[,"B_delay2"])
      dens.b2=approx(df.dens$x,df.dens$y,xout=df.sqc[1,"B_delay2"],rule=2)$y
      df.dens=density(df.expect.sub[,"B_delay3"])
      dens.b3=approx(df.dens$x,df.dens$y,xout=df.sqc[1,"B_delay3"],rule=2)$y
      
      if(is.na(df.sqc$B_num1[1])){
        coun.b1=sum(is.na(df.expect.sub$B_num1))/nrow(df.expect.sub)
      }else{
        coun.b1=nrow(subset(df.expect.sub,B_num1==df.sqc$B_num1[1]))/nrow(df.expect.sub)
      }
      if(is.na(df.sqc$B_num2[1])){
        coun.b2=sum(is.na(df.expect.sub$B_num2))/nrow(df.expect.sub)
      }else{
        coun.b2=nrow(subset(df.expect.sub,B_num2==df.sqc$B_num2[1]))/nrow(df.expect.sub)
      }
      if(is.na(df.sqc$B_num3[1])){
        coun.b3=sum(is.na(df.expect.sub$B_num3))/nrow(df.expect.sub)
      }else{
        coun.b3=nrow(subset(df.expect.sub,B_num3==df.sqc$B_num3[1]))/nrow(df.expect.sub)
      }
      df.compare[i,"delay"]=dens.a1*dens.a2*dens.a3*dens.b1*dens.b2*dens.b3
      df.compare[i,"num"]=coun.a1*coun.a2*coun.a3*coun.b1*coun.b2*coun.b3
}

for (i in 1:2){
  df.compare[,cue_list[i]]=df.compare[,cue_list[i]]/sum(df.compare[,cue_list[i]],na.rm = T)
}

#Simulation
sim_prob=df.compare$num #change here to get different feature

st_a=c("G","P","N","G","P","N","G","P","N")
st_b=c("G","G","G","P","P","P","N","N","N")

model_result=as.data.frame(matrix(NA,ncol=6,nrow=9)) %>%
  setNames(c("trial_id","A_pro","B_pro","A_state","B_state","ratio"))
model_result$trial_id=sti_id
model_result$A_pro=a_pro
model_result$B_pro=b_pro
model_result$A_state=st_a
model_result$B_state=st_b
model_result$ratio=sim_prob/sum(sim_prob)
save(model_result,file=paste(sti_name,sti_no,".Rda",sep = ""))
}

filepath="../../../stimulus/exp1/"

#1
sti_id=1# REMEMBER TO CHECK IT
sti_name="gg"
sti_no=1
a_pro="G"# REMEMBER TO CHECK IT
b_pro="G" # REMEMBER TO CHECK IT
load(file = paste(filepath,sti_id,".",sti_name,sti_no,".Rda",sep = ""))
sqc=sqc_raw_long
sqc$total_idx=c(1:nrow(sqc))
myFeatureBased()
#2
sti_id=2# REMEMBER TO CHECK IT
sti_name="gg"
sti_no=2
a_pro="G"# REMEMBER TO CHECK IT
b_pro="G" # REMEMBER TO CHECK IT
load(file = paste(filepath,sti_id,".",sti_name,sti_no,".Rda",sep = ""))
sqc=sqc_raw_long
sqc$total_idx=c(1:nrow(sqc))
myFeatureBased()
#3
sti_id=3# REMEMBER TO CHECK IT
sti_name="pg"
sti_no=1
a_pro="P"# REMEMBER TO CHECK IT
b_pro="G" # REMEMBER TO CHECK IT
load(file = paste(filepath,sti_id,".",sti_name,sti_no,".Rda",sep = ""))
sqc=sqc_raw_long
sqc$total_idx=c(1:nrow(sqc))
myFeatureBased()
#4
sti_id=4# REMEMBER TO CHECK IT
sti_name="pg"
sti_no=2
a_pro="P"# REMEMBER TO CHECK IT
b_pro="G" # REMEMBER TO CHECK IT
load(file = paste(filepath,sti_id,".",sti_name,sti_no,".Rda",sep = ""))
sqc=sqc_raw_long
sqc$total_idx=c(1:nrow(sqc))
myFeatureBased()
#5
sti_id=5# REMEMBER TO CHECK IT
sti_name="ng"
sti_no=1
a_pro="N"# REMEMBER TO CHECK IT
b_pro="G" # REMEMBER TO CHECK IT
load(file = paste(filepath,sti_id,".",sti_name,sti_no,".Rda",sep = ""))
sqc=sqc_raw_long
sqc$total_idx=c(1:nrow(sqc))
myFeatureBased()
#6
sti_id=6# REMEMBER TO CHECK IT
sti_name="ng"
sti_no=2
a_pro="N"# REMEMBER TO CHECK IT
b_pro="G" # REMEMBER TO CHECK IT
load(file = paste(filepath,sti_id,".",sti_name,sti_no,".Rda",sep = ""))
sqc=sqc_raw_long
sqc$total_idx=c(1:nrow(sqc))
myFeatureBased()
#7
sti_id=7# REMEMBER TO CHECK IT
sti_name="gp"
sti_no=1
a_pro="G"# REMEMBER TO CHECK IT
b_pro="P" # REMEMBER TO CHECK IT
load(file = paste(filepath,sti_id,".",sti_name,sti_no,".Rda",sep = ""))
sqc=sqc_raw_long
sqc$total_idx=c(1:nrow(sqc))
myFeatureBased()
#8
sti_id=8# REMEMBER TO CHECK IT
sti_name="gp"
sti_no=2
a_pro="G"# REMEMBER TO CHECK IT
b_pro="P" # REMEMBER TO CHECK IT
load(file = paste(filepath,sti_id,".",sti_name,sti_no,".Rda",sep = ""))
sqc=sqc_raw_long
sqc$total_idx=c(1:nrow(sqc))
myFeatureBased()
#9
sti_id=9# REMEMBER TO CHECK IT
sti_name="pp"
sti_no=1
a_pro="P"# REMEMBER TO CHECK IT
b_pro="P" # REMEMBER TO CHECK IT
load(file = paste(filepath,sti_id,".",sti_name,sti_no,".Rda",sep = ""))
sqc=sqc_raw_long
sqc$total_idx=c(1:nrow(sqc))
myFeatureBased()
#10
sti_id=10# REMEMBER TO CHECK IT
sti_name="pp"
sti_no=2
a_pro="P"# REMEMBER TO CHECK IT
b_pro="P" # REMEMBER TO CHECK IT
load(file = paste(filepath,sti_id,".",sti_name,sti_no,".Rda",sep = ""))
sqc=sqc_raw_long
sqc$total_idx=c(1:nrow(sqc))
myFeatureBased()
#11
sti_id=11# REMEMBER TO CHECK IT
sti_name="np"
sti_no=1
a_pro="N"# REMEMBER TO CHECK IT
b_pro="P" # REMEMBER TO CHECK IT
load(file = paste(filepath,sti_id,".",sti_name,sti_no,".Rda",sep = ""))
sqc=sqc_raw_long
sqc$total_idx=c(1:nrow(sqc))
myFeatureBased()
#12
sti_id=12# REMEMBER TO CHECK IT
sti_name="np"
sti_no=2
a_pro="N"# REMEMBER TO CHECK IT
b_pro="P" # REMEMBER TO CHECK IT
load(file = paste(filepath,sti_id,".",sti_name,sti_no,".Rda",sep = ""))
sqc=sqc_raw_long
sqc$total_idx=c(1:nrow(sqc))
myFeatureBased()
#13
sti_id=13# REMEMBER TO CHECK IT
sti_name="gn"
sti_no=1
a_pro="G"# REMEMBER TO CHECK IT
b_pro="N" # REMEMBER TO CHECK IT
load(file = paste(filepath,sti_id,".",sti_name,sti_no,".Rda",sep = ""))
sqc=sqc_raw_long
sqc$total_idx=c(1:nrow(sqc))
myFeatureBased()
#14
sti_id=14# REMEMBER TO CHECK IT
sti_name="gn"
sti_no=2
a_pro="G"# REMEMBER TO CHECK IT
b_pro="N" # REMEMBER TO CHECK IT
load(file = paste(filepath,sti_id,".",sti_name,sti_no,".Rda",sep = ""))
sqc=sqc_raw_long
sqc$total_idx=c(1:nrow(sqc))
myFeatureBased()
#15
sti_id=15# REMEMBER TO CHECK IT
sti_name="pn"
sti_no=1
a_pro="P"# REMEMBER TO CHECK IT
b_pro="N" # REMEMBER TO CHECK IT
load(file = paste(filepath,sti_id,".",sti_name,sti_no,".Rda",sep = ""))
sqc=sqc_raw_long
sqc$total_idx=c(1:nrow(sqc))
myFeatureBased()
#16
sti_id=16# REMEMBER TO CHECK IT
sti_name="pn"
sti_no=2
a_pro="P"# REMEMBER TO CHECK IT
b_pro="N" # REMEMBER TO CHECK IT
load(file = paste(filepath,sti_id,".",sti_name,sti_no,".Rda",sep = ""))
sqc=sqc_raw_long
sqc$total_idx=c(1:nrow(sqc))
myFeatureBased()
#17
sti_id=17# REMEMBER TO CHECK IT
sti_name="nn"
sti_no=1
a_pro="N"# REMEMBER TO CHECK IT
b_pro="N" # REMEMBER TO CHECK IT
load(file = paste(filepath,sti_id,".",sti_name,sti_no,".Rda",sep = ""))
sqc=sqc_raw_long
sqc$total_idx=c(1:nrow(sqc))
myFeatureBased()
#18
sti_id=18# REMEMBER TO CHECK IT
sti_name="nn"
sti_no=2
a_pro="N"# REMEMBER TO CHECK IT
b_pro="N" # REMEMBER TO CHECK IT
load(file = paste(filepath,sti_id,".",sti_name,sti_no,".Rda",sep = ""))
sqc=sqc_raw_long
sqc$total_idx=c(1:nrow(sqc))
myFeatureBased()

df.model=simulate_result=as.data.frame(matrix(NA,ncol=6,nrow=0)) %>%
  setNames(c("trial_id","A_pro","B_pro","A_state","B_state","ratio"))

filelist=c("gg1.Rda","gg2.Rda","pg1.Rda","pg2.Rda","ng1.Rda","ng2.Rda",
           "gp1.Rda","gp2.Rda","pp1.Rda","pp2.Rda","np1.Rda","np2.Rda",
           "gn1.Rda","gn2.Rda","pn1.Rda","pn2.Rda","nn1.Rda","nn2.Rda")

#filelist=list.files(path = ".",pattern = ".Rda$")
for (i in 1:length(filelist)){
  load(filelist[i])
  df.model=rbind(df.model,model_result)
}

save(df.model,file="df.model.Rda")
