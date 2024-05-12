#Statistika 1: Beräkna antalet fixeringar per ROI
nfix_ROI <- function(P){
  result<-MarkovModel1(P)
  ROInr<-data.frame(ROI=rep(0:(dim(P)[1]-1)))
  nfix<-as.data.frame(table(result$ROI))
  tfix<-as.data.frame(aggregate(result$duration, by=list(Category=result$ROI), FUN=sum))
  colnames(nfix)=c("ROI","nfix")
  nfixROI<-merge(ROInr,nfix,by="ROI",all=TRUE)
  nfixROI$nfix[is.na(nfixROI$nfix)]<-0
  return(nfixROI)
}
replicate_Markov1_nfix<-function(nrepli,transition_matrix){
  repli_nfix<-replicate(nrepli,nfix_ROI(transition_matrix))
  repli_nfix<-data.frame(ROI=repli_nfix[1,1],repli_nfix[2,])
  colnames(repli_nfix)<-c("ROI",sprintf("run %d",1:nrepli))
  repli_nfix$average<-rowSums(repli_nfix[,2:(nrepli+1)])/nrepli
  return(repli_nfix)
}

#Statistika 2: Genomsnittlig fixeringstid per ROI
durationfix_ROI <- function(P){
  result<-MarkovModel1(P)
  ROInr<-data.frame(ROI=rep(0:(dim(P)[1]-1)))
  tfix<-as.data.frame(aggregate(result$duration, by=list(ROInr=result$ROI), FUN=sum))
  colnames(tfix)=c("ROI","duration")
  tfixROI<-merge(ROInr,tfix,by="ROI",all=TRUE)
  tfixROI$duration[is.na(tfixROI$duration)]<-0
  return(tfixROI)
}
replicate_Markov1_dur<-function(nrepli,transition_matrix){
  repli_dur<-replicate(nrepli,durationfix_ROI(transition_matrix))
  repli_dur<-data.frame(ROI=repli_dur[1,1], repli_dur[2,])
  colnames(repli_dur)<-c("ROI",sprintf("run %d",1:nrepli))
  repli_dur$average<-rowSums(repli_dur[,2:(nrepli+1)])/nrepli
  return(repli_dur)
}

#Statistika 3: Scanpath

replicate_scanpath = function(nrep){ #Stores each simulation in a list.
  repli_model = replicate(nrep,MarkovModel2(transition_matrix,KDEret,ROI_labelled_Monet, ROI_Centers_and_Rad))
  scanpaths <- vector("list", nrep)
  for (i in (1:nrep)){
    scanpaths[[i]] = list(Scanpath_sim(repli_model[,i]))
    
  }
  return(scanpaths)
  
}

replicate_convex_hull = function(nrep){
  
  repli_model = replicate(nrep,MarkovModel2(transition_matrix,KDEret,ROI_labelled_Monet, ROI_Centers_and_Rad))
  
  convex_hull_sim <- vector("list", nrep)
  
  for (i in (1:nrep)){
    convex_hull_sim[[i]] = list(Convex_hull_sim(repli_model[,i]))
    
  }
  return(convex_hull_sim)
  
}
