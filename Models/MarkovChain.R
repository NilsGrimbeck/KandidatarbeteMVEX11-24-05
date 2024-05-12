MarkovModel1 <- function(P){
  i<-sample(0:(dim(P)[1]-1),1,replace=TRUE)
  path<-c(i)
  sac<-c(NA)
  fix<-rexp(1,0.0047)
  ttot=0
  while (sum(fix)+sum(sac,na.rm=TRUE)<=20*30000){
    i<-sample(0:(dim(P)[1]-1),1,replace=TRUE,prob=P[i+1,])
    path=c(path,i)
    sac<-c(sac,rlnorm(1,3.87,0.75))
    fix<-c(fix,rexp(1,0.0047))
    ttot<-c(ttot,sum(fix)+sum(sac,na.rm=TRUE))
    
  }  
  return(data.frame(ROI=path,duration=fix,presac=sac,timestamp=ttot))
}
