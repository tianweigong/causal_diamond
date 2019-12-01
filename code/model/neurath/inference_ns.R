#+ function setting -------------------
#' # function setting
assignG<- function(act,prob){
  m=runif(1,min=0,max=1)
  if (m<prob){
    if (act=="A"){
      guess["A","type"]<<-"G"
      guess["A","prob"]<<-prob
    }
    if (act=="B"){
      guess["B","type"]<<-"G"
      guess["B","prob"]<<-prob
    }
  }
}

assignP<- function(act,prob){
  m=runif(1,min=0,max=1)
  if (m<prob){
    if (act=="A" && aPFlag!=0){
      guess["A","type"]<<-"P"
      guess["A","prob"]<<-prob
    }
    if (act=="B"&& bPFlag!=0){
      guess["B","type"]<<-"P"
      guess["B","prob"]<<-prob
    }
  }
}

assignN<- function(act,prob){
  if (act=="A" && guess["A","type"]=="P"){
    guess["A","type"]<<-"N"
    guess["A","prob"]<<-prob
  }
  if (act=="B" && guess["B","type"]=="P"){
    guess["B","type"]<<-"N"
    guess["B","prob"]<<-prob
  }
}
#+ infer the structure  -------------------
#' # infer the structure
simulate_result=as.data.frame(matrix(NA,ncol=6,nrow=sampling_point)) %>%
  setNames(c("id","trial_id","A_pro","B_pro","A_state","B_state"))
simulate_result$trial_id=sti_id
simulate_result$A_pro=a_pro
simulate_result$B_pro=b_pro
#sssssimulation
for (ss in 1: sampling_point){

guess=as.data.frame(matrix(NA,ncol=2,nrow=2))
colnames(guess)=c("type","prob")
rownames(guess)=c("A","B")
guess$type="N"

ncs_uni=1/(k_e/r_e)

aPFlag=1
bPFlag=1
q=2
for (i in 2:nrow(sqc)){ #the first one is always baserate
  if (((sqc$obj[i]=="E") && (i==nrow(sqc)||((i+1 <= nrow(sqc) && sqc$obj[i+1]!="E"))))||(i== nrow(sqc))){
    local_sqc=sqc[q:i,]
    
    local_a=local_sqc %>% subset(obj=="A")
    local_b=local_sqc %>% subset(obj=="B")
    local_ab=local_sqc %>% subset(obj %in% c("A","B"))
    local_e=local_sqc %>% subset(obj=="E")
    if (i== nrow(sqc) & sqc$obj[i]!="E"){
      local_e=sqc %>% subset(total_idx==1)
      local_e$time=trial_end+rgamma(1,1,1) #caution!!!
    }
    if (nrow(local_ab)==0){
      q=i+1
      next
    }
    
    if (nrow(local_e)==1){
      local_ab$actual= dgamma(local_e$time[1]-local_ab$time,k_be,r_be) #maybe more noisy here
      if (max(local_ab$actual)>ncs_uni){
        act_cause=local_ab$obj[which.max(local_ab$actual)] %>% as.character()
        assignG(act_cause,max(local_ab$actual))
      }
    }
    
    if (nrow(local_e)>1){
      col_group=c()
      for (j in 1:nrow(local_e)){
        col_name=paste("actual",j,sep = "")
        col_group=c(col_group,col_name)
        local_ab[,col_name]=dgamma(local_e$time[j]-local_ab$time,k_be,r_be)
      }
      local_ab[,"actual"]=rowSums(local_ab[,col_group])
      
      local_ab_dup=local_ab
      for (j in 1:nrow(local_e)){
        if (nrow(local_ab_dup)>0 && max(local_ab_dup$actual)>ncs_uni){
          act_cause=local_ab_dup$obj[which.max(local_ab_dup$actual)] %>% as.character()
          assignG(act_cause,max(local_ab_dup$actual))
        }
        local_ab_dup=local_ab_dup[local_ab_dup$actual!=max(local_ab_dup$actual),]
      }
      
    }# not perfect algorithm here. remain to be improved >_<
    
    if (nrow(local_a)>0){
      local_a$prevent=pgamma(local_e$time[1]-local_a$time,k_ae,r_ae)
      
      if (min(local_a$prevent)>ncs_uni){
        if (nrow(local_a)>nrow(local_b) || (guess["B","type"]!="P")){ #not that good 
          assignP("A",mean(local_a$prevent))
        }else{
          # local_b$prevent=pgamma(local_e$time[1]-local_b$time,k_ae,r_ae)
          # if (min(local_a$prevent)>min(local_b$prevent) || min(local_b$prevent)>0.99){
          #   assignP("A",mean(local_a$prevent))
          # }
        }
      }else{
        if (min(local_a$prevent)<0.01){
          assignN("A",mean(local_a$prevent))
          aPFlag=0
        }
      }
    }#not that make sense 
    
    if (nrow(local_b)>0){
      local_b$prevent=pgamma(local_e$time[1]-local_b$time,k_ae,r_ae)
      if (min(local_b$prevent)>ncs_uni){
        if (nrow(local_b)>nrow(local_a) || (guess["A","type"]!="P") ){ #not that good
          assignP("B",min(local_b$prevent))
        }else{
          # local_a$prevent=pgamma(local_e$time[1]-local_a$time,k_ae,r_ae)
          # if (min(local_b$prevent)>min(local_a$prevent) || min(local_a$prevent)>0.99){
          #   assignP("B",min(local_b$prevent))
          # }
        }
      }else{
        if (min(local_b$prevent)<0.01){
          assignN("B",min(local_b$prevent))
          bPFlag=0
        }
      }
    }#not that make sense
    
    q=i+1
  }
}
#ssssmilation end paraphrase
simulate_result$id=ss
simulate_result$A_state[ss]= guess["A","type"] %>% as.character()
simulate_result$B_state[ss]= guess["B","type"] %>% as.character()
}
# View(simulate_result)
save(simulate_result,file=paste(sti_name,sti_no,".Rda",sep = ""))
