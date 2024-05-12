library(LPCM)
library(ggplot2)
library(dplyr)

ms_clustering_ROI <- function(fixationdata, bandwidth=0.05, threshold=0.01, picdim=c(1013,787), plot=FALSE) {
  ##Inputs fixation and outputs ROI labled fixation data and ROI centroid coordinates
  spatdata <- fixationdata[, c("locX", "locY")]
  
  # Perform Mean Shift clustering
  meanshift <- ms(spatdata, h = bandwidth * picdim, thr = threshold, scaled = 0, plot = plot)
  
  # Append cluster label and closest centroid label to input data
  ROI_lable_data <- cbind(fixationdata, cluster.label = meanshift$cluster.label, closest.label = meanshift$closest.label)
  
  # dplyr to vectorize counting of fixations in each cluster and get cluster centroids
  Cluster_centroid <- ROI_lable_data %>%
    group_by(cluster.label) %>%
    summarize(fixations.cluster = n(), # Count number of fixations in each cluster
              #Centre.1 = mean(locX), # Calculate mean locX as centroid x-coordinate
              #Centre.2 = mean(locY), # Calculate mean locY as centroid y-coordinate
              .groups = 'drop') # Drop grouping structure
  Cluster_centroid=cbind(Cluster_centroid, Centre=meanshift$cluster.center)
  colnames(Cluster_centroid)<-c("cluster.label","fixations.cluster","Centre.1","Centre.2")
  
  # Return results
  return(list(ROI_lable_data = ROI_lable_data, Cluster_centroid = Cluster_centroid))
}
