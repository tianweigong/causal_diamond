library(dplyr)
library(tidyr)
library(matrixStats)

whole_trial_end=20 #ending of the trial
mini_gap=0.3 #minimal gap between two events
s_ab=3
s_e=100
k_pe = 36
r_pe = 12
k_ge = 9
r_ge = 6

baserate=5
baserate_var=0.25
r_e_r = baserate/baserate_var
k_e_r = r_e_r*baserate
k_e_u=1
r_e_u=1/baserate

eachgroup=9*2
stifea=expand.grid(A=c("G","N","P"),B=c("G","N","P")) %>% arrange_all()

MyRemove <- function (v.org,v.prevent,v.range){
  for (k in 1: length(v.prevent)){
    v.org=v.org[!(v.org>v.prevent[k] & v.org<v.range[k])]
  }
  return(v.org)
}

MyReshape <-function(sti_sqc){
  sti_sqc[order(sti_sqc$time), ]
}

MyStiGen<-function(or){

  while(1){
    
    c_all=sort(runif(s_ab*2,0,whole_trial_end))
    
    A = c_all[which(or=="A")]
    B = c_all[which(or=="B")]
    
    E_raw = rgamma(s_e, shape = k_e_r, rate = r_e_r) %>% cumsum() %>% c(0) 
    E_r = E_raw[E_raw<whole_trial_end]
    
    E_raw = rgamma(s_e, shape = k_e_u, rate = r_e_u) %>% cumsum() %>% c(0) 
    E_u = E_raw[E_raw<whole_trial_end]
    
    A_g_raw=rgamma(s_ab, shape = k_ge, rate = r_ge)
    B_g_raw=rgamma(s_ab, shape = k_ge, rate = r_ge)
    
    A_p_raw=rgamma(s_ab, shape = k_pe, rate = r_pe)
    B_p_raw=rgamma(s_ab, shape = k_pe, rate = r_pe)
    
    A_g=A_g_raw+A
    B_g=B_g_raw+B
    
    A_p=A_p_raw+A
    B_p=B_p_raw+B
    
    if (min(diff(sort(c(E_r,A,B,A_g,B_g))))>mini_gap && 
        min(diff(sort(c(E_u,A,B,A_g,B_g))))>mini_gap){break}
  }
  
  sti.lis=list()
  for (j in c("r","u")){
    if(j=="r"){E=E_r};if(j=="u"){E=E_u}
    
    for (k in 1:nrow(stifea)){
      s=c(stifea$A[k],stifea$B[k]) %>% as.character()
      
      gen=which(s=="G")
      pre=which(s=="P")
      
      #create the dataframe
      sti= rbind(data.frame(obj="A",time=A,ori="N"),
                 data.frame(obj="B",time=B,ori="N"),
                 data.frame(obj="E",time=E,ori="E"))
      
      if (length(gen)){
        for (i in gen){
          cc=c("A","B")[i]
          sti=rbind(sti,
                    data.frame(obj="E",time=get(paste(cc,"g",sep="_")),ori=cc))
        }
      }
      
      if (length(pre)){
        for (i in pre){
          cc=c("A","B")[i]
          e=sti$time[sti$obj=="E"] %>% MyRemove(.,get(cc),get(paste(cc,"p",sep="_")))
          sti=subset(sti,obj!="E" | time %in% e)
        }
      }
      
      sti.lis[[paste(paste(s,collapse=""),j,sep="_")]]=sti %>% 
        subset(time<whole_trial_end)%>% 
        MyReshape() 
    }
  }

  return(sti.lis)
}

myStiSave<-function(sti.lis){
  mycsv=data.frame()
  for (k in 1:length(sti.lis)){
    st=sti.lis[[k]]
    a=names(sti.lis)[k]
    st$truth=unlist(strsplit(a,"_"))[1]
    st$condition=c("r"="reliable","u"="unreliable")[unlist(strsplit(a,"_"))[2]] %>% as.character()
    st$id=a
    mycsv=rbind(mycsv,st)
  }
  return(mycsv)
}


#change the number to generate as much as you want
for (i in 1:100){
  set.seed(i)
  or=c("A","A","A","B","B","B")[sample(c(1:6),replace = F)]
  MyStiGen(or) %>% 
    myStiSave() %>%
    write.csv(file=paste0("mySim/",i,".csv"),row.names=F)
}
