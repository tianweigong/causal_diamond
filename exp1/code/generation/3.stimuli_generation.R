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
data.frame(Time = seq(0, 10, length.out = 10000)) %>%
  mutate(exp = dgamma(Time, shape = k_e, rate =r_e )) %>%
  gather(fun, Probability, c(exp)) %>%
  mutate(Form = factor(fun, levels = c('exp'),
                       labels = c('demo'))) %>%
  ggplot(aes(x = Time, y = Probability)) +
  # geom_vline(xintercept = c(3.5)) +
  # annotate("rect",xmin = 0,xmax = 3.5,ymin = 0,ymax = 0.8,alpha = 0.2,fill = "blue") +
  geom_point(size=0.01) +
  theme_bw() +
  theme(text = element_text(size=16),
        panel.grid = element_blank(),
        # axis.text=element_blank(),
        # axis.ticks.x=element_blank(),
        axis.title=element_blank())+
  scale_x_continuous(limits = c(0,7),breaks=c(0:7))

#+ test parameters -------------------
#' # test parameters
data.frame(Time = seq(0, 10, length.out = 10000)) %>%
  mutate(exp = dgamma(Time, shape = k_ge, rate =r_ge )) %>%
  gather(fun, Probability, c(exp)) %>%
  mutate(Form = factor(fun, levels = c('exp'),
                       labels = c('demo'))) %>%
  ggplot(aes(x = Time, y = Probability)) +
  # geom_vline(xintercept = c(3.5)) +
  # annotate("rect",xmin = 0,xmax = 3.5,ymin = 0,ymax = 0.8,alpha = 0.2,fill = "blue") +
  geom_point(size=0.01) +
  theme_bw() +
  theme(text = element_text(size=16),
        panel.grid = element_blank(),
        # axis.text=element_blank(),
        # axis.ticks.x=element_blank(),
        axis.title=element_blank())+
  scale_x_continuous(limits = c(0,4),breaks=c(0:4))

#+ test parameters -------------------
#' # test parameters
data.frame(Time = seq(0, 10, length.out = 10000)) %>%
  mutate(exp = dgamma(Time, shape = k_pe, rate =r_pe )) %>%
  gather(fun, Probability, c(exp)) %>%
  mutate(Form = factor(fun, levels = c('exp'),
                       labels = c('demo'))) %>%
  ggplot(aes(x = Time, y = Probability)) +
  # geom_vline(xintercept = c(3.5)) +
  # annotate("rect",xmin = 0,xmax = 3.5,ymin = 0,ymax = 0.8,alpha = 0.2,fill = "blue") +
  geom_point(size=0.01) +
  theme_bw() +
  theme(text = element_text(size=16),
        panel.grid = element_blank(),
        # axis.text=element_blank(),
        # axis.ticks.x=element_blank(),
        axis.title=element_blank())+
  scale_x_continuous(limits = c(0,5),breaks=c(0:5))

#+ set parameters -------------------
#' # set parameters

k_e = 100
r_e = 20 #e->e: m=5,var=0.25

k_a = 1
r_a = 0.2 #a: m=5,var=5
k_b = 1
r_b = 0.2 #b:m=5,var=5

s_e = 6
s_a = 3
s_b = 3

k_pe = 36
r_pe = 12 #preventative p->e: m=3,var=0.25
k_ge = 9
r_ge = 6  #generative g->e: m=1.5,var=0.25

trial_end=20
onfor=0.35+0.05

MyRemove <- function (v.org,v.prevent,v.range){
  return(v.org[!((v.org>v.prevent[1] & v.org<v.range[1])|(v.org>v.prevent[2] & v.org<v.range[2])|(v.org>v.prevent[3] & v.org<v.range[3]))])
}

MyIdtCheck <- function(){
  x=c(paste(gg$time%>%round(3),collapse=""))
  x=c(x,paste(gn$time%>%round(3),collapse=""))
  x=c(x,paste(gp$time%>%round(3),collapse=""))
  x=c(x,paste(ng$time%>%round(3),collapse=""))
  x=c(x,paste(nn$time%>%round(3),collapse=""))
  x=c(x,paste(np$time%>%round(3),collapse=""))
  x=c(x,paste(pg$time%>%round(3),collapse=""))
  x=c(x,paste(pn$time%>%round(3),collapse=""))
  x=c(x,paste(pp$time%>%round(3),collapse=""))
  return(length(unique(x))==9)
}

flag_e=0
flag_ab=0
flag_gp=0
flag_itv=0
flag_idt=0
for (seed in 1:10000){
  sti_sig=as.data.frame(matrix(NA,ncol=8,nrow=0)) %>%
    setNames(c("trial_id","A_pro","B_pro","obj","time","seed"))
  set.seed(seed)
  E_raw = rgamma(s_e, shape = k_e, rate = r_e)
  A_raw = rgamma(20, shape = k_a, rate = r_a)[1:s_a]
  B_raw = rgamma(20, shape = k_b, rate = r_b)[(s_a+10):(s_a+9+s_b)]
  
  if (min(dgamma(E_raw,shape = k_e, rate = r_e))<0.05){
    flag_e=flag_e+1
    next
  }
  
  A = A_raw %>% cumsum()
  B = B_raw %>% cumsum()
  E = E_raw %>% cumsum() %>% c(0)
  E=E[E<20]
  
  if(max(A)>trial_end | max(B)>trial_end | min(diff(sort(c(A,B))))<=onfor){
    flag_ab=flag_ab+1
    next
  }
  g_raw=rgamma(20, shape = k_ge, rate = r_ge)
  A_g_raw=g_raw[1:s_a]
  B_g_raw=g_raw[(s_a+10):(s_a+9+s_b)]
  p_raw=rgamma(20, shape = k_pe, rate = r_pe)
  A_p_raw=p_raw[1:s_a]
  B_p_raw=p_raw[(s_a+10):(s_a+9+s_b)]
  
  A_g_check=min(dgamma(A_g_raw,shape = k_ge, rate = r_ge))<0.05
  B_g_check=min(dgamma(B_g_raw,shape = k_ge, rate = r_ge))<0.05
  A_p_check=min(dgamma(A_p_raw,shape = k_pe, rate = r_pe))<0.05
  B_p_check=min(dgamma(B_p_raw,shape = k_pe, rate = r_pe))<0.05
  
  if(A_g_check||B_g_check||A_p_check||B_p_check){
    flag_gp=flag_gp+1
    next
  }
  
  A_g=A_g_raw+A
  B_g=B_g_raw+B
  A_p=A_p_raw+A
  B_p=B_p_raw+B
  
  if (min(diff(sort(c(A,B,E,A_g,B_g))))<=onfor){
    flag_itv=flag_itv+1
    next
  }
  
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
  
  if (MyIdtCheck()==0){
    flag_idt=flag_idt+1
    next
  }
  break
}

sti_id="1"
sti_name="gg"
sti_no=1
a_pro="G"
b_pro="G" 
sqc_raw_long=gg
View(sqc_raw_long)
MyOnlineInfer()

fig_a=c(rep(c("1.Generative","2.Preventative","3.Non-causal"),3))
fig_b=c(rep("3.Generative",3),rep("2.Preventative",3),rep("1.Non-causal",3))

fig_data=sim_prob
fig_data_pre= format(fig_data, digits=2,scientific = TRUE, trim = TRUE)
fig_data_pre[fig_data_pre=="0.0e+00"]="trivial"

dt.fig=data.frame(fig_a,fig_b,fig_data,fig_data_pre)

name_summary=paste(sti_name,sti_no,sep = "")

ggplot(dt.fig,aes(x=fig_a, y=fig_b, fill=fig_data)) + 
  geom_tile()+
  scale_fill_gradientn(colours = c("white","steelblue"),trans = "log")+
  geom_text(aes(label = fig_data_pre))+
  ylab("Component B")+
  xlab("Component A")+
  theme(legend.position = "none")+
  ggtitle(name_summary)

# ggsave(paste(sti_id,".",name_summary,".pdf",sep = ""), width = 5, height = 4)

# sqc_raw_long$seed=seed
# write.csv(sqc_raw_long, paste(sti_id,".",sti_name,sti_no,".csv",sep = ""), row.names = F, quote = F)
# save(sqc_raw_long, file = paste(sti_id,".",sti_name,sti_no,".Rda",sep = ""))
