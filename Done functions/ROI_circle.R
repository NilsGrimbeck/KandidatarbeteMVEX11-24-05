library("dplyr")
library("ggplot2")
library("ggforce")
library("jpeg")
library("grid")
library("spatstat")

Sort_function <- function(results) {

ROI_lable_data=results$ROI_lable_data 
Centroid_data=results$Cluster_centroid

#Filtrers all data which belong to a cluster with <10 points
ROI_lable_data_filtered <- ROI_lable_data %>%
  group_by(cluster.label) %>%
  filter(n() > 10) %>%
  ungroup() 

#Creates vector for all labels that should be looped over (i.e all except the removed one(s))
N = t(unique(ROI_lable_data_filtered["cluster.label"]))

#Extracs x and y coord:s for the centroids
Centroid_x = Centroid_data$Centre.1
Centroid_y = Centroid_data$Centre.2

#Radius parameters
BaseRad = 15
RadScale = 1.4

#Pre defining
Centers_and_Rad <- data.frame(x = numeric(), y = numeric(), radius = numeric())

#Loop over all clusters and calculates their corresponding radius
for (cluster_id in N) {
  cluster_data <- ROI_lable_data_filtered[ROI_lable_data_filtered["cluster.label"] == cluster_id, c("locX", "locY")]
  center_x <- Centroid_x[cluster_id]
  center_y <- Centroid_y[cluster_id]
  rad = BaseRad+log(length(t(cluster_data)))*log(length(t(cluster_data)))*RadScale
  Centers_and_Rad <- rbind(Centers_and_Rad, data.frame(x = center_x, y = center_y, radius = rad))
}

#Create new variable for the data to be sorted
Sorted_data = ROI_lable_data
Sorted_data$cluster.label = 0

#Loops over all rows (points) in the data
for (i in 1:nrow(Sorted_data)) {
  #Checks if point is in circle
  for (j in 1:nrow(Centers_and_Rad)) {
    # Calculate the distance from point to circle center
    distance = sqrt((Sorted_data$locX[i] - Centers_and_Rad$x[j])^2 + (Sorted_data$locY[i] - Centers_and_Rad$y[j])^2)
    
    # If the point is within the circle update its label and break
    if (distance <= Centers_and_Rad$radius[j]) {
      Sorted_data$cluster.label[i] = j
      break
    }
  }
}

return(list(Sorted_data,Centers_and_Rad))
}
