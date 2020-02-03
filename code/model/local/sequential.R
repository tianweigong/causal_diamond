#+ set parameters -------------------
#' # set parameters
library(dplyr)
k_e = 100
r_e = 20 #e->e: m=5,var=0.25

k_pe = 36
r_pe = 12 #preventative p->e: m=3,var=0.25

k_ge = 9
r_ge = 6  #generative g->e: m=1.5,var=0.25

trial_end=20
large_time=30 # a second far beyond trial_end
simulating_point=1

filepath="../../../stimulus/exp/"
#load dataset
sti_id="1"# REMEMBER TO CHECK IT
sti_name="gg"
sti_no="1"
a_pro="G"# REMEMBER TO CHECK IT
b_pro="G" # REMEMBER TO CHECK IT
load(file = paste(filepath,sti_id,".",sti_name,sti_no,".Rda",sep = ""))
gg1=sqc_raw_long
#2
sti_id=2# REMEMBER TO CHECK IT
sti_name="gg"
sti_no=2
a_pro="G"# REMEMBER TO CHECK IT
b_pro="G" # REMEMBER TO CHECK IT
load(file = paste(filepath,sti_id,".",sti_name,sti_no,".Rda",sep = ""))
gg2=sqc_raw_long
#3
sti_id=3# REMEMBER TO CHECK IT
sti_name="pg"
sti_no=1
a_pro="P"# REMEMBER TO CHECK IT
b_pro="G" # REMEMBER TO CHECK IT
load(file = paste(filepath,sti_id,".",sti_name,sti_no,".Rda",sep = ""))
pg1=sqc_raw_long
#4
sti_id=4# REMEMBER TO CHECK IT
sti_name="pg"
sti_no=2
a_pro="P"# REMEMBER TO CHECK IT
b_pro="G" # REMEMBER TO CHECK IT
load(file = paste(filepath,sti_id,".",sti_name,sti_no,".Rda",sep = ""))
pg2=sqc_raw_long
#5
sti_id=5# REMEMBER TO CHECK IT
sti_name="ng"
sti_no=1
a_pro="N"# REMEMBER TO CHECK IT
b_pro="G" # REMEMBER TO CHECK IT
load(file = paste(filepath,sti_id,".",sti_name,sti_no,".Rda",sep = ""))
ng1=sqc_raw_long
#6
sti_id=6# REMEMBER TO CHECK IT
sti_name="ng"
sti_no=2
a_pro="N"# REMEMBER TO CHECK IT
b_pro="G" # REMEMBER TO CHECK IT
load(file = paste(filepath,sti_id,".",sti_name,sti_no,".Rda",sep = ""))
ng2=sqc_raw_long
#7
sti_id=7# REMEMBER TO CHECK IT
sti_name="gp"
sti_no=1
a_pro="G"# REMEMBER TO CHECK IT
b_pro="P" # REMEMBER TO CHECK IT
load(file = paste(filepath,sti_id,".",sti_name,sti_no,".Rda",sep = ""))
gp1=sqc_raw_long
#8
sti_id=8# REMEMBER TO CHECK IT
sti_name="gp"
sti_no=2
a_pro="G"# REMEMBER TO CHECK IT
b_pro="P" # REMEMBER TO CHECK IT
load(file = paste(filepath,sti_id,".",sti_name,sti_no,".Rda",sep = ""))
gp2=sqc_raw_long
#9
sti_id=9# REMEMBER TO CHECK IT
sti_name="pp"
sti_no=1
a_pro="P"# REMEMBER TO CHECK IT
b_pro="P" # REMEMBER TO CHECK IT
load(file = paste(filepath,sti_id,".",sti_name,sti_no,".Rda",sep = ""))
pp1=sqc_raw_long
#10
sti_id=10# REMEMBER TO CHECK IT
sti_name="pp"
sti_no=2
a_pro="P"# REMEMBER TO CHECK IT
b_pro="P" # REMEMBER TO CHECK IT
load(file = paste(filepath,sti_id,".",sti_name,sti_no,".Rda",sep = ""))
pp2=sqc_raw_long
#11
sti_id=11# REMEMBER TO CHECK IT
sti_name="np"
sti_no=1
a_pro="N"# REMEMBER TO CHECK IT
b_pro="P" # REMEMBER TO CHECK IT
load(file = paste(filepath,sti_id,".",sti_name,sti_no,".Rda",sep = ""))
np1=sqc_raw_long
#12
sti_id=12# REMEMBER TO CHECK IT
sti_name="np"
sti_no=2
a_pro="N"# REMEMBER TO CHECK IT
b_pro="P" # REMEMBER TO CHECK IT
load(file = paste(filepath,sti_id,".",sti_name,sti_no,".Rda",sep = ""))
np2=sqc_raw_long
#13
sti_id=13# REMEMBER TO CHECK IT
sti_name="gn"
sti_no=1
a_pro="G"# REMEMBER TO CHECK IT
b_pro="N" # REMEMBER TO CHECK IT
load(file = paste(filepath,sti_id,".",sti_name,sti_no,".Rda",sep = ""))
gn1=sqc_raw_long
#14
sti_id=14# REMEMBER TO CHECK IT
sti_name="gn"
sti_no=2
a_pro="G"# REMEMBER TO CHECK IT
b_pro="N" # REMEMBER TO CHECK IT
load(file = paste(filepath,sti_id,".",sti_name,sti_no,".Rda",sep = ""))
gn2=sqc_raw_long
#15
sti_id=15# REMEMBER TO CHECK IT
sti_name="pn"
sti_no=1
a_pro="P"# REMEMBER TO CHECK IT
b_pro="N" # REMEMBER TO CHECK IT
load(file = paste(filepath,sti_id,".",sti_name,sti_no,".Rda",sep = ""))
pn1=sqc_raw_long
#16
sti_id=16# REMEMBER TO CHECK IT
sti_name="pn"
sti_no=2
a_pro="P"# REMEMBER TO CHECK IT
b_pro="N" # REMEMBER TO CHECK IT
load(file = paste(filepath,sti_id,".",sti_name,sti_no,".Rda",sep = ""))
pn2=sqc_raw_long
#17
sti_id=17# REMEMBER TO CHECK IT
sti_name="nn"
sti_no=1
a_pro="N"# REMEMBER TO CHECK IT
b_pro="N" # REMEMBER TO CHECK IT
load(file = paste(filepath,sti_id,".",sti_name,sti_no,".Rda",sep = ""))
nn1=sqc_raw_long
#18
sti_id=18# REMEMBER TO CHECK IT
sti_name="nn"
sti_no=2
a_pro="N"# REMEMBER TO CHECK IT
b_pro="N" # REMEMBER TO CHECK IT
load(file = paste(filepath,sti_id,".",sti_name,sti_no,".Rda",sep = ""))
nn2=sqc_raw_long

#parameter tuning
grain=0.4
Time=seq(0, large_time, length.out = large_time/grain+1)
# thres_up=6 # accumulative
# thres_low=0.0001 # probabilistic
#+ load functions-------------------
#' # load functions
Mylog<-function(prob){
  return(-log(1-prob))
}

MySeq <-function(thres_up,thres_low,sqc_raw){
  sqc=sqc_raw
  sqc$actual=NA
  
  dis.e.exp=matrix(0,ncol=1,nrow=length(Time))
  dis.a.exp=matrix(0,ncol=3,nrow=length(Time))
  dis.b.exp=matrix(0,ncol=3,nrow=length(Time))
  c_list=c("A","B") #control component list
  a_state=NA
  b_state=NA
  a_count=0
  b_count=0
  glist=c()
  plist=c()
  dis.prev=matrix(0,ncol=1,nrow=length(Time))
  
  for (g in 2:(trial_end/grain)){
    t=Time[g]
    sqc_sub=sqc %>% subset(time>t-grain & time<t)
    #initialization
    if (t==grain){
      dis.e.exp[g:length(Time),1]=pgamma(Time[Time>0],k_e,r_e)
      next
    }
    if (("A" %in% sqc_sub$obj)){
      a_count=a_count+1
      if ("A" %in% glist){
        dis.a.exp[g:length(Time),a_count]=pgamma(Time[1:(length(Time)-g+1)],k_ge,r_ge)
        next
      }
      
      if ("A" %in% plist){
        dis.prev[g:length(Time),1]=(pgamma(Time[1:(length(Time)-g+1)],k_pe,r_pe))*dis.prev[g:length(Time),1]
        next
      }
      
    }
    if (("B" %in% sqc_sub$obj)){
      b_count=b_count+1
      if ("B" %in% glist){
        dis.b.exp[g:length(Time),b_count]=pgamma(Time[1:(length(Time)-g+1)],k_ge,r_ge)
        next
      }
      
      if ("B" %in% glist){
        dis.prev[g:length(Time),1]=pgamma(Time[1:(length(Time)-g+1)],k_pe,r_pe)*dis.prev[g:length(Time),1]
        next
      }
      
    }

    #generative attribution
    if ("E" %in% sqc_sub$obj){
      x_obj=which.max(c(dis.e.exp[g,1],max(dis.a.exp[g,]),max(dis.b.exp[g,])))
      x_exp_add_prev=sum(Mylog(c(dis.e.exp[g,1]*dis.prev[g,1],
                          dis.a.exp[g,1]*dis.prev[g,1],dis.a.exp[g,2]*dis.prev[g,1],dis.a.exp[g,3]*dis.prev[g,1],
                          dis.b.exp[g,1]*dis.prev[g,1],dis.b.exp[g,2]*dis.prev[g,1],dis.b.exp[g,3]*dis.prev[g,1])))
      x_exp=sum(Mylog(c(dis.e.exp[g,1],dis.a.exp[g,1],dis.a.exp[g,2],dis.a.exp[g,3],
                        dis.b.exp[g,1],dis.b.exp[g,2],dis.b.exp[g,3])))
      
      
      if (x_exp_add_prev<thres_low & x_exp>=thres_low){
        
        cdd_n=sqc %>% subset((obj %in% plist) & time<t)
        cause=cdd_n$obj[nrow(cdd_n)] %>%as.character()
        plist=setdiff(plist,cause)
        dis.prev[g:length(Time),1]=0
        
        if (x_obj==1){
          dis.e.exp[g:length(Time),1]=pgamma(Time[1:(length(Time)-g+1)],k_e,r_e)
        }
        
        if (x_obj==2){
          a_obj=which.max(dis.a.exp[g,])
          dis.a.exp[g:length(Time),a_obj]=0
        }
        
        if (x_obj==3){
          b_obj=which.max(dis.b.exp[g,])
          dis.b.exp[g:length(Time),b_obj]=0
        }
        next
      }
      
      
      if (x_exp>=thres_low){ #within expectation
        if (x_obj==1){
          dis.e.exp[g:length(Time),1]=pgamma(Time[1:(length(Time)-g+1)],k_e,r_e)
        }
        
        if (x_obj==2){
          a_obj=which.max(dis.a.exp[g,])
          dis.a.exp[g:length(Time),a_obj]=0
        }
        
        if (x_obj==3){
          b_obj=which.max(dis.b.exp[g,])
          dis.b.exp[g:length(Time),b_obj]=0
        }
        next
      }
        
      if (x_exp<thres_low){ 
        cdd_g=sqc %>% subset((obj %in% c_list) & time<t)
        if (nrow(cdd_g)>0){
          cdd_g$p_g=dgamma(t-cdd_g$time,k_ge,r_ge)
          if (Mylog(max(cdd_g$p_g))>thres_low){
          cause=cdd_g$obj[which.max(cdd_g$p_g)] %>%as.character()
          glist= cause%>% c(glist)
          plist= setdiff(plist,glist)
          next
          }
        }

        if (x_obj==1){
          dis.e.exp[g:length(Time),1]=pgamma(Time[1:(length(Time)-g+1)],k_e,r_e)
        }
        
        if (x_obj==2){
          a_obj=which.max(dis.a.exp[g,])
          dis.a.exp[g:length(Time),a_obj]=0
        }
        
        if (x_obj==3){
          b_obj=which.max(dis.b.exp[g,])
          dis.b.exp[g:length(Time),b_obj]=0
        }
        next
      }
    }
    
    #preventative attribution
    exp_e=dis.e.exp[g,1]
    exp_a=dis.a.exp[g,]
    exp_b=dis.b.exp[g,]
    if (sum(Mylog(c(exp_e,exp_a,exp_b)))>thres_up){
      cdd_p=sqc %>% subset(!(obj %in% glist) & time<t)
      re=which.max(c(exp_e,exp_a,exp_b))
      if (nrow(cdd_p)>0){
        cause=cdd_p$obj[nrow(cdd_p)] %>%as.character()
        plist= cause%>% c(plist)
        if (re==1){
          dis.e.exp[g:length(Time),1]=pgamma(Time[1:(length(Time)-g+1)],k_e,r_e)
        }
        
        if (re==2){
          a_obj=which.max(dis.a.exp[g,])
          dis.a.exp[g:length(Time),a_obj]=0
        }
        
        if (re==3){
          b_obj=which.max(dis.b.exp[g,])
          dis.b.exp[g:length(Time),b_obj]=0
        }
      }else{
        if (re==2){
          glist=setdiff(glist,"A")
          dis.a.exp[g:length(Time),a_obj]=0
        }
        if (re==3){
          glist=setdiff(glist,"B")
          dis.b.exp[g:length(Time),b_obj]=0
        }
      }
      
    }
  }
  
  result=c("N","N")
  if ("A" %in% glist){
    result[1]="G"
  }
  if ("A" %in% plist){
    result[1]="P"
  }
  
  if ("B" %in% glist){
    result[2]="G"
  }
  if ("B" %in% plist){
    result[2]="P"
  }
  
  return(result)
}

MySummary <-function(thres_up,thres_low,sqc_raw,sti_id,a_pro,b_pro){
  decision=as.data.frame(matrix(NA,ncol=2,nrow=simulating_point)) %>%
    setNames(c("A_state","B_state"))
  
  for (s in 1: simulating_point){
    decision[s,]=MySeq(thres_up,thres_low,sqc_raw)
  }
  
  
  st_a=c("G","P","N","G","P","N","G","P","N")
  st_b=c("G","G","G","P","P","P","N","N","N")
  
  model_result=as.data.frame(matrix(NA,ncol=6,nrow=9)) %>%
    setNames(c("trial_id","A_pro","B_pro","A_state","B_state","ratio"))
  model_result$trial_id=sti_id
  model_result$A_pro=a_pro
  model_result$B_pro=b_pro
  model_result$A_state=st_a
  model_result$B_state=st_b
  
  for (i in 1:9){
    model_result$ratio[i]= nrow(subset(decision,A_state==model_result$A_state[i] & B_state==model_result$B_state[i]))/simulating_point
  }
  
  return(model_result)
  
}
#+ load stimulus list -------------------
#' # load stimulus list

MySeqFit<-function(thres_up=0,thres_low=0){
  #1
  sti_id="1"# REMEMBER TO CHECK IT
  sti_name="gg"
  sti_no="1"
  a_pro="G"# REMEMBER TO CHECK IT
  b_pro="G" # REMEMBER TO CHECK IT
  sqc_raw=gg1
  m_gg1=MySummary(thres_up,thres_low,sqc_raw,sti_id,a_pro,b_pro)
  #2
  sti_id=2# REMEMBER TO CHECK IT
  sti_name="gg"
  sti_no=2
  a_pro="G"# REMEMBER TO CHECK IT
  b_pro="G" # REMEMBER TO CHECK IT
  sqc_raw=gg2
  m_gg2=MySummary(thres_up,thres_low,sqc_raw,sti_id,a_pro,b_pro)
  #3
  sti_id=3# REMEMBER TO CHECK IT
  sti_name="pg"
  sti_no=1
  a_pro="P"# REMEMBER TO CHECK IT
  b_pro="G" # REMEMBER TO CHECK IT
  sqc_raw=pg1
  m_pg1=MySummary(thres_up,thres_low,sqc_raw,sti_id,a_pro,b_pro)
  #4
  sti_id=4# REMEMBER TO CHECK IT
  sti_name="pg"
  sti_no=2
  a_pro="P"# REMEMBER TO CHECK IT
  b_pro="G" # REMEMBER TO CHECK IT
  sqc_raw=pg2
  m_pg2=MySummary(thres_up,thres_low,sqc_raw,sti_id,a_pro,b_pro)
  #5
  sti_id=5# REMEMBER TO CHECK IT
  sti_name="ng"
  sti_no=1
  a_pro="N"# REMEMBER TO CHECK IT
  b_pro="G" # REMEMBER TO CHECK IT
  sqc_raw=ng1
  m_ng1=MySummary(thres_up,thres_low,sqc_raw,sti_id,a_pro,b_pro)
  #6
  sti_id=6# REMEMBER TO CHECK IT
  sti_name="ng"
  sti_no=2
  a_pro="N"# REMEMBER TO CHECK IT
  b_pro="G" # REMEMBER TO CHECK IT
  sqc_raw=ng2
  m_ng2=MySummary(thres_up,thres_low,sqc_raw,sti_id,a_pro,b_pro)
  #7
  sti_id=7# REMEMBER TO CHECK IT
  sti_name="gp"
  sti_no=1
  a_pro="G"# REMEMBER TO CHECK IT
  b_pro="P" # REMEMBER TO CHECK IT
  sqc_raw=gp1
  m_gp1=MySummary(thres_up,thres_low,sqc_raw,sti_id,a_pro,b_pro)
  #8
  sti_id=8# REMEMBER TO CHECK IT
  sti_name="gp"
  sti_no=2
  a_pro="G"# REMEMBER TO CHECK IT
  b_pro="P" # REMEMBER TO CHECK IT
  sqc_raw=gp2
  m_gp2=MySummary(thres_up,thres_low,sqc_raw,sti_id,a_pro,b_pro)
  #9
  sti_id=9# REMEMBER TO CHECK IT
  sti_name="pp"
  sti_no=1
  a_pro="P"# REMEMBER TO CHECK IT
  b_pro="P" # REMEMBER TO CHECK IT
  sqc_raw=pp1
  m_pp1=MySummary(thres_up,thres_low,sqc_raw,sti_id,a_pro,b_pro)
  #10
  sti_id=10# REMEMBER TO CHECK IT
  sti_name="pp"
  sti_no=2
  a_pro="P"# REMEMBER TO CHECK IT
  b_pro="P" # REMEMBER TO CHECK IT
  sqc_raw=pp2
  m_pp2=MySummary(thres_up,thres_low,sqc_raw,sti_id,a_pro,b_pro)
  #11
  sti_id=11# REMEMBER TO CHECK IT
  sti_name="np"
  sti_no=1
  a_pro="N"# REMEMBER TO CHECK IT
  b_pro="P" # REMEMBER TO CHECK IT
  sqc_raw=np1
  m_np1=MySummary(thres_up,thres_low,sqc_raw,sti_id,a_pro,b_pro)
  #12
  sti_id=12# REMEMBER TO CHECK IT
  sti_name="np"
  sti_no=2
  a_pro="N"# REMEMBER TO CHECK IT
  b_pro="P" # REMEMBER TO CHECK IT
  sqc_raw=np2
  m_np2=MySummary(thres_up,thres_low,sqc_raw,sti_id,a_pro,b_pro)
  #13
  sti_id=13# REMEMBER TO CHECK IT
  sti_name="gn"
  sti_no=1
  a_pro="G"# REMEMBER TO CHECK IT
  b_pro="N" # REMEMBER TO CHECK IT
  sqc_raw=gn1
  m_gn1=MySummary(thres_up,thres_low,sqc_raw,sti_id,a_pro,b_pro)
  #14
  sti_id=14# REMEMBER TO CHECK IT
  sti_name="gn"
  sti_no=2
  a_pro="G"# REMEMBER TO CHECK IT
  b_pro="N" # REMEMBER TO CHECK IT
  sqc_raw=gn2
  m_gn2=MySummary(thres_up,thres_low,sqc_raw,sti_id,a_pro,b_pro)
  #15
  sti_id=15# REMEMBER TO CHECK IT
  sti_name="pn"
  sti_no=1
  a_pro="P"# REMEMBER TO CHECK IT
  b_pro="N" # REMEMBER TO CHECK IT
  sqc_raw=pn1
  m_pn1=MySummary(thres_up,thres_low,sqc_raw,sti_id,a_pro,b_pro)
  #16
  sti_id=16# REMEMBER TO CHECK IT
  sti_name="pn"
  sti_no=2
  a_pro="P"# REMEMBER TO CHECK IT
  b_pro="N" # REMEMBER TO CHECK IT
  sqc_raw=pn2
  m_pn2=MySummary(thres_up,thres_low,sqc_raw,sti_id,a_pro,b_pro)
  #17
  sti_id=17# REMEMBER TO CHECK IT
  sti_name="nn"
  sti_no=1
  a_pro="N"# REMEMBER TO CHECK IT
  b_pro="N" # REMEMBER TO CHECK IT
  sqc_raw=nn1
  m_nn1=MySummary(thres_up,thres_low,sqc_raw,sti_id,a_pro,b_pro)
  #18
  sti_id=18# REMEMBER TO CHECK IT
  sti_name="nn"
  sti_no=2
  a_pro="N"# REMEMBER TO CHECK IT
  b_pro="N" # REMEMBER TO CHECK IT
  sqc_raw=nn2
  m_nn2=MySummary(thres_up,thres_low,sqc_raw,sti_id,a_pro,b_pro)
  
  df.model=do.call("rbind", list(m_gg1,m_gg2,m_pg1,m_pg2,m_ng1,m_ng2,
                                 m_gp1,m_gp2,m_pp1,m_pp2,m_np1,m_np2,
                                 m_gn1,m_gn2,m_pn1,m_pn2,m_nn1,m_nn2))
  return(df.model)
}