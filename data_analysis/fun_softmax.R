MyMdFit<-function(par,vals){
  sf1=exp(par[1])
  
  vals=vals %>% mutate(sf=exp((prob)/sf1))
  vals=vals[,sum_sf:=sum(sf),by="uni_id"]
  
  vals=vals %>% mutate(li=log(sf/sum_sf)*people)
  -sum(vals$li)
}

MyMdFit2<-function(par,vals){
  sf1=exp(par[1])
  sf2=exp(par[2])
  
  vals=vals %>% mutate(sf=exp(((prob1)/sf1) + ((prob2)/sf2)))
  vals=vals[,sum_sf:=sum(sf),by="uni_id"]
  
  vals=vals %>% mutate(li=log(sf/sum_sf)*people)
  -sum(vals$li)
}