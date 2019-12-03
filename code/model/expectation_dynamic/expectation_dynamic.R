#+ function setting -------------------
#' # function setting
assignG<- function(act,prob){
  #m=runif(1,min=0,max=1)
  m=0
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
  #m=runif(1,min=0,max=1)
  m=0
  if (m<prob){
    if (act=="A" ){
      guess["A","type"]<<-"P"
      guess["A","prob"]<<-prob
    }
    if (act=="B"){
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
simulate_result=as.data.frame(matrix(NA,ncol=6,nrow=sim_subject)) %>%
  setNames(c("id","trial_id","A_pro","B_pro","A_state","B_state"))
simulate_result$trial_id=sti_id
simulate_result$A_pro=a_pro
simulate_result$B_pro=b_pro
#sssssimulation
#for (ss in 1: sim_subject){ #debugging

guess=as.data.frame(matrix(NA,ncol=2,nrow=2))
colnames(guess)=c("type","prob")
rownames(guess)=c("A","B")
guess$type="N"

for (t in seq(0,trial_end,upd_rate)){
  
  
  
  
}



for (i in 1:nrow(sqc)){
  local_sqc=sqc[1:i,]
  local_sqc=local_sqc %>% subset(time > sqc$time[i]-mmr_sec) #limited menmory storage
  local_ab=local_sqc %>% subset(obj %in% c("A","B"))
  ###preventative
  if (!("E" %in% local_sqc$obj)){
    sim_base=local_sqc$time[1]+rgamma(1,k_e,r_e)
    
    local_ab$prevent= 1-pgamma(sim_base-local_ab$time,k_ae,r_ae)
    local_ab$prevent[local_ab$prevent==1]=0
    act_prevent=local_ab$obj[which.max(local_ab$prevent)] %>% as.character()
    cdd_prevent=setdiff(rownames(guess),c(act_prevent))
    if (guess[act_cause,"type"]!="G"){
      assignP(act_prevent,1)
      next
    }
    
    local_cdd=local_ab %>% subset(obj==cdd_prevent)
    
    if (nrow(local_cdd)>0 & guess[cdd_prevent,"type"]!="G"){
      assignP(cdd_prevent,1)
      next
    }
    
  }
  
  ##generative
  
  if (sqc$obj[i]=="A" | sqc$obj[i]=="B"){
    next
  }
  
  if (nrow(local_ab)==0){
    next
  }
  
  local_ab$actual= dgamma(sqc$time[i]-local_ab$time,k_be,r_be)
  
  if (max(local_ab$actual)<ncs_uni){
    next
  }
  
  act_cause=local_ab$obj[which.max(local_ab$actual)] %>% as.character()
  cdd_cause=setdiff(rownames(guess),c(act_cause))
  
  if (guess[act_cause,"type"]=="G"){
    next
  }
  
  if (guess[cdd_cause,"type"]!="G"){
    assignG(act_cause,max(local_ab$actual))
    next
  }
  
  local_cdd=local_ab %>% subset(obj==cdd_cause) #would have "BAEE" problem here
  
  if (nrow(local_cdd)==0){
    assignG(act_cause,max(local_ab$actual))
    next
  }
  
  if (max(local_ab$actual)>max(local_cdd$actual)*ncs_rate){
    assignG(act_cause,max(local_ab$actual))
    next
  }

}
#ssssmilation end paraphrase
simulate_result$id[ss]=ss
simulate_result$A_state[ss]= guess["A","type"] %>% as.character()
simulate_result$B_state[ss]= guess["B","type"] %>% as.character()
#} #dubugging
# View(simulate_result)
save(simulate_result,file=paste(sti_name,sti_no,".Rda",sep = ""))
