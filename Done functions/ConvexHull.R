Convex_hull_data <- function(MonetData){

id = unique(MonetData[,"id"])
ConvexHull_list = list()

for (k in id) {
  spat_data = MonetData[MonetData["timestamp"]<=30000 & MonetData["inpic"]==1 & MonetData["id"]==k, c("locX","locY", "timestamp")]
  data = as.matrix(spat_data[,c("locX","locY")])
  timestamp= spat_data[,"timestamp",]
  convex_volume = numeric(nrow(data) - 2)
  
  for (i in 3:nrow(data)) {
    
    convex_volume[i-2] = cxhull(data[1:i,])$volume
    
  }
  
  Convex_hull = cbind(timestamp[3:length(timestamp)],convex_volume)
  colnames(Convex_hull) = c("Timestamp","Area")
  
  ConvexHull_list[[as.character(k)]] = Convex_hull
}

return(ConvexHull_list)

}


Convex_hull_sim <- function(Sim_data){
  
  point_data = data.frame(locX = unlist(Sim_data$locX), locY = unlist(Sim_data$locY))
  data = as.matrix(point_data)
  convex_volume = numeric(nrow(data) - 2)
  timestamp = Sim_data$timestamp
    
    for (i in 3:nrow(data)) {
      
      convex_volume[i-2] = cxhull(data[1:i,])$volume
      
    }
    
  Convex_hull = cbind(timestamp[3:length(timestamp)],convex_volume)
  colnames(Convex_hull) = c("Timestamp","Area")
  
  return(Convex_hull)
  
}
