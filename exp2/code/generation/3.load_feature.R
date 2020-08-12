#+ load packages and functions  -------------------
#' # load packages and functions

#see feature_simulation
load("df.expect.Rda")
#+ infer the structure  -------------------
#' # infer the structure
#mean between C & nearest E, min between C & nearest E, the last one is set at 20s
myFeatureBased <-function(sqc,rl,br){
  
df.sqc<<-as.data.frame(matrix(NA,ncol=12,nrow=1)) %>%
  setNames(c("A_delay1","A_delay2","A_delay3",
           "B_delay1","B_delay2","B_delay3",
           "A_num1","A_num2","A_num3",
           "B_num1","B_num2","B_num3"))
sqc$total_idx=c(1:nrow(sqc))
myStatsSummary(sqc,0,0)

df.compare=as.data.frame(matrix(NA,ncol=4,nrow=9)) %>%
  setNames(c("A_pro","B_pro","delay","num"))
df.compare$A_pro=c("G","G","G","N","N","N","P","P","P")
df.compare$B_pro=c("G","N","P","G","N","P","G","N","P")
# cue_list_a=c("A_mean","A_num")
# cue_list_b=c("B_mean","B_num")
cue_list=c("delay","num")
for (i in 1:9){
  
      df.expect.sub=df.expect %>% subset(rel==rl & baserate==br) %>% 
        subset(A_pro==df.compare$A_pro[i])
      
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
      
      
      df.expect.sub=df.expect %>% subset(rel==rl & baserate==br) %>%
        subset(B_pro==df.compare$B_pro[i])
      # Sys.sleep(0.01)
      # print(nrow(df.expect.sub))
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
sim_prob=df.compare %>% subset(select = c(delay, num)) #change here to get different feature
return(sim_prob)
}
