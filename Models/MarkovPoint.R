MarkovModel2 <- function(P,KDE,Sort_data, Centers_and_Rad){
  ROI_fixations<-MarkovModel1(P)
  loc<-Random_point(cluster=ROI_fixations$ROI[1],prepoint=c(Sort_data$locX[1],Sort_data$locY[1],Sort_data$cluster.label[1]), Sort_data, Centers_and_Rad, KDE)
  ROI_fixations$locX[1]<-loc[1]
  ROI_fixations$locY[1]<-loc[2]
  
  
  for(i in 2:dim(ROI_fixations)[1]){
    loc<-Random_point(cluster=ROI_fixations$ROI[i],prepoint=c(ROI_fixations$locX[i-1],ROI_fixations$locY[i-1],ROI_fixations$ROI[i-1]), Sort_data, Centers_and_Rad, KDE)
    ROI_fixations$locX[i]<-loc[1]
    ROI_fixations$locY[i]<-loc[2]
  }
  return(ROI_fixations)
}
