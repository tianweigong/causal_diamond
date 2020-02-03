#+ load packages and functions  -------------------
#' # load packages and functions
s_e = 6
s_a=3
s_b=3
k_e = 100
r_e = 20 #e->e: m=5,var=0.25
k_pe = 36
r_pe = 12 #preventative a->e: m=3,var=0.25
k_ge = 9
r_ge = 6  #generative b->e: m=1.5,var=0.25
k_a = 1
r_a = 0.2 #a: m=5,var=5
k_b = 1
r_b = 0.2 #b:m=5,var=5

trial_end=20
sim_sqc=10000
onfor=0.35

myStatsSummary <- function(sqc_s,num_model,type=1){
  A_next=rep(NA,s_a)
  B_next=rep(NA,s_a)
  A_num=rep(NA,s_b)
  B_num=rep(NA,s_b)
  a_count=0
  b_count=0
  for (i in 1:nrow(sqc_s)){
    if (sqc_s$obj[i]=="E"){
      next
    }
    
    if (sqc_s$obj[i]=="A"){
      a_count=a_count+1
      nextflag=1
      numflag=1
      for (j in i:nrow(sqc_s)){
        if (j!=i && sqc_s$obj[j]!="E"){
          numflag=0
          if (is.na(A_num[a_count])){
            A_num[a_count]=0
          }
        }
        if (sqc_s$obj[j]=="E"){
          if (nextflag){
            A_next[a_count]=sqc_s$time[j]-sqc_s$time[i]
            nextflag=0
          }
          if (numflag){
            if (is.na(A_num[a_count])){
              A_num[a_count]=1
            }else{
              A_num[a_count]=A_num[a_count]+1
            }
          }
        }
      }
      if (is.na(A_next[a_count])){
        A_next[a_count]=trial_end-sqc_s$time[i]
      }
    }
    
    if (sqc_s$obj[i]=="B"){
      b_count=b_count+1
      nextflag=1
      numflag=1
      for (j in i:nrow(sqc_s)){
        if (j!=i && sqc_s$obj[j]!="E"){
          numflag=0
          if (is.na(B_num[b_count])){
            B_num[b_count]=0
          }
        }
        if (sqc_s$obj[j]=="E"){
          if (nextflag){
            B_next[b_count]=sqc_s$time[j]-sqc_s$time[i]
            nextflag=0
          }
          if (numflag){
            if (is.na(B_num[b_count])){
              B_num[b_count]=1
            }else{
              B_num[b_count]=B_num[b_count]+1
            }
          }
        }
      }
      if (is.na(B_next[b_count])){
        B_next[b_count]=trial_end-sqc_s$time[i]
      }
    }
  }
  
  if (type==1){
    df.expect$A_delay1[(snum-1)*9+num_model]<<-A_next[1]
    df.expect$A_delay2[(snum-1)*9+num_model]<<-A_next[2]
    df.expect$A_delay3[(snum-1)*9+num_model]<<-A_next[3]
    
    df.expect$B_delay1[(snum-1)*9+num_model]<<-B_next[1]
    df.expect$B_delay2[(snum-1)*9+num_model]<<-B_next[2]
    df.expect$B_delay3[(snum-1)*9+num_model]<<-B_next[3]
    
    df.expect$A_num1[(snum-1)*9+num_model]<<-A_num[1]
    df.expect$A_num2[(snum-1)*9+num_model]<<-A_num[2]
    df.expect$A_num3[(snum-1)*9+num_model]<<-A_num[3]
    
    df.expect$B_num1[(snum-1)*9+num_model]<<-B_num[1]
    df.expect$B_num2[(snum-1)*9+num_model]<<-B_num[2]
    df.expect$B_num3[(snum-1)*9+num_model]<<-B_num[3]
    
  }else{
    df.sqc$A_delay1[1]<<-A_next[1]
    df.sqc$A_delay2[1]<<-A_next[2]
    df.sqc$A_delay3[1]<<-A_next[3]
    
    df.sqc$B_delay1[1]<<-B_next[1]
    df.sqc$B_delay2[1]<<-B_next[2]
    df.sqc$B_delay3[1]<<-B_next[3]
    
    df.sqc$A_num1[1]<<-A_num[1]
    df.sqc$A_num2[1]<<-A_num[2]
    df.sqc$A_num3[1]<<-A_num[3]
    
    df.sqc$B_num1[1]<<-B_num[1]
    df.sqc$B_num2[1]<<-B_num[2]
    df.sqc$B_num3[1]<<-B_num[3]
  }
}

MyRemove <- function (v.org,v.prevent,v.range){
  return(v.org[!((v.org>v.prevent[1] & v.org<v.range[1])|(v.org>v.prevent[2] & v.org<v.range[2])|(v.org>v.prevent[3] & v.org<v.range[3]))])
}


df.expect=as.data.frame(matrix(NA,ncol=15,nrow=sim_sqc*9)) %>%
  setNames(c("simulation","A_pro","B_pro",
             "A_delay1","A_delay2","A_delay3",
             "B_delay1","B_delay2","B_delay3",
             "A_num1","A_num2","A_num3",
             "B_num1","B_num2","B_num3"))

snum=0

for (kk in 1:1000000){
  sti_sig=as.data.frame(matrix(NA,ncol=8,nrow=0)) %>%
    setNames(c("trial_id","A_pro","B_pro","obj","time","seed"))
  
  E_raw = rgamma(s_e, shape = k_e, rate = r_e)
  A_raw = rgamma(s_a, shape = k_a, rate = r_a)
  B_raw = rgamma(s_b, shape = k_b, rate = r_b)
  
  A = A_raw %>% cumsum()
  B = B_raw %>% cumsum()
  E = E_raw %>% cumsum() %>% c(0)
  E=E[E<20]
  
  if(max(A)>trial_end | max(B)>trial_end | min(diff(sort(c(A,B))))<=onfor){
    next
  }
  
  A_g_raw=rgamma(s_a, shape = k_ge, rate = r_ge)
  B_g_raw=rgamma(s_b, shape = k_ge, rate = r_ge)
  A_p_raw=rgamma(s_a, shape = k_pe, rate = r_pe)
  B_p_raw=rgamma(s_b, shape = k_pe, rate = r_pe)
  
  A_g=A_g_raw+A
  B_g=B_g_raw+B
  A_p=A_p_raw+A
  B_p=B_p_raw+B
  
  # if (min(diff(sort(c(A,B,E,A_g,B_g))))<=onfor){
  #   next
  # }
  
  #GG
  gg=data.frame(obj = c(rep("A", length(A)), rep("B", length(B)), 
                        rep("E", length(E)), rep("E",length(A_g)),rep("E",length(B_g))), 
                time =c(A, B, E, A_g, B_g),
                org=c(rep("N", length(A)), rep("N", length(B)), 
                      rep("E", length(E)), rep("A",length(A_g)),rep("B",length(B_g))))
  gg = gg[order(gg$time), ]
  gg = gg[gg$time < 20, ]
  
  #GN
  gn=data.frame(obj = c(rep("A", length(A)), rep("B", length(B)), 
                        rep("E", length(E)), rep("E",length(A_g))),
                time =c(A, B, E, A_g),
                org=c(rep("N", length(A)), rep("N", length(B)), 
                      rep("E", length(E)), rep("A",length(A_g))))
  gn = gn[order(gn$time), ]
  gn = gn[gn$time < 20, ]
  
  #GP
  E_r=MyRemove(E,B,B_p)
  A_g_r=MyRemove(A_g,B,B_p)
  gp=data.frame(obj = c(rep("A", length(A)), rep("B", length(B)), 
                        rep("E", length(E_r)), rep("E",length(A_g_r))),
                time =c(A, B, E_r, A_g_r),
                org=c(rep("N", length(A)), rep("N", length(B)), 
                      rep("E", length(E_r)), rep("A",length(A_g_r))))
  
  gp = gp[order(gp$time), ]
  gp = gp[gp$time < 20, ]
  
  #NG
  ng=data.frame(obj = c(rep("A", length(A)), rep("B", length(B)), 
                        rep("E", length(E)), rep("E",length(B_g))), 
                time =c(A, B, E,B_g),
                org=c(rep("N", length(A)), rep("N", length(B)), 
                      rep("E", length(E)), rep("B",length(B_g))))
  ng = ng[order(ng$time), ]
  ng = ng[ng$time < 20, ]
  
  #NN
  nn=data.frame(obj = c(rep("A", length(A)), rep("B", length(B)), 
                        rep("E", length(E))), 
                time =c(A, B, E),
                org=c(rep("N", length(A)), rep("N", length(B)), 
                      rep("E", length(E))))
  nn = nn[order(nn$time), ]
  nn = nn[nn$time < 20, ] 
  
  #NP
  E_r=MyRemove(E,B,B_p)
  np=data.frame(obj = c(rep("A", length(A)), rep("B", length(B)), 
                        rep("E", length(E_r))),
                time =c(A, B, E_r),
                org=c(rep("N", length(A)), rep("N", length(B)), 
                      rep("E", length(E_r))))
  
  np = np[order(np$time), ]
  np = np[np$time < 20, ]
  
  #PG
  E_r=MyRemove(E,A,A_p)
  B_g_r=MyRemove(B_g,A,A_p)
  pg=data.frame(obj = c(rep("A", length(A)), rep("B", length(B)), 
                        rep("E", length(E_r)), rep("E",length(B_g_r))), 
                time =c(A, B, E_r, B_g_r),
                org=c(rep("N", length(A)), rep("N", length(B)), 
                      rep("E", length(E_r)), rep("B",length(B_g_r))))
  pg = pg[order(pg$time), ]
  pg = pg[pg$time < 20, ]
  
  #PN
  E_r=MyRemove(E,A,A_p)
  pn=data.frame(obj = c(rep("A", length(A)), rep("B", length(B)), 
                        rep("E", length(E_r))), 
                time =c(A, B, E_r),
                org=c(rep("N", length(A)), rep("N", length(B)), 
                      rep("E", length(E_r))))
  pn = pn[order(pn$time), ]
  pn = pn[pn$time < 20, ]
  
  #PP
  E_r=MyRemove(E,A,A_p)
  E_r=MyRemove(E_r,B,B_p)
  pp=data.frame(obj = c(rep("A", length(A)), rep("B", length(B)), 
                        rep("E", length(E_r))), 
                time =c(A, B, E_r),
                org=c(rep("N", length(A)), rep("N", length(B)), 
                      rep("E", length(E_r))))
  pp = pp[order(pp$time), ]
  pp = pp[pp$time < 20, ]
  
  snum=snum+1
  #summary
  df.expect$simulation[(snum-1)*9+1]=snum
  df.expect$A_pro[(snum-1)*9+1]="G"
  df.expect$B_pro[(snum-1)*9+1]="G"
  myStatsSummary(gg,1)
  
  df.expect$simulation[(snum-1)*9+2]=snum
  df.expect$A_pro[(snum-1)*9+2]="P"
  df.expect$B_pro[(snum-1)*9+2]="G"
  myStatsSummary(pg,2)
  
  df.expect$simulation[(snum-1)*9+3]=snum
  df.expect$A_pro[(snum-1)*9+3]="N"
  df.expect$B_pro[(snum-1)*9+3]="G"
  myStatsSummary(ng,3)
  
  df.expect$simulation[(snum-1)*9+4]=snum
  df.expect$A_pro[(snum-1)*9+4]="G"
  df.expect$B_pro[(snum-1)*9+4]="P"
  myStatsSummary(gp,4)
  
  df.expect$simulation[(snum-1)*9+5]=snum
  df.expect$A_pro[(snum-1)*9+5]="P"
  df.expect$B_pro[(snum-1)*9+5]="P"
  myStatsSummary(pp,5)
  
  df.expect$simulation[(snum-1)*9+6]=snum
  df.expect$A_pro[(snum-1)*9+6]="N"
  df.expect$B_pro[(snum-1)*9+6]="P"
  myStatsSummary(np,6)
  
  df.expect$simulation[(snum-1)*9+7]=snum
  df.expect$A_pro[(snum-1)*9+7]="G"
  df.expect$B_pro[(snum-1)*9+7]="N"
  myStatsSummary(gn,7)
  
  df.expect$simulation[(snum-1)*9+8]=snum
  df.expect$A_pro[(snum-1)*9+8]="P"
  df.expect$B_pro[(snum-1)*9+8]="N"
  myStatsSummary(pn,8)
  
  df.expect$simulation[(snum-1)*9+9]=snum
  df.expect$A_pro[(snum-1)*9+9]="N"
  df.expect$B_pro[(snum-1)*9+9]="N"
  myStatsSummary(nn,9)
  
  if (snum>=sim_sqc){
    break
  }
}

# save(df.expect,file="df.expect.Rda")
