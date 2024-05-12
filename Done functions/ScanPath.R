Scanpath_sim = function(Sim_data){
  
  Monet_points = data.frame(locX = unlist(Sim_data$locX), locY = unlist(Sim_data$locY), timestamp = Sim_data$timestamp) 
  cumulative_length = 0  # Initialize cumulative length
  Saccade_length_cumulative = data.frame(Timestamp = Monet_points[-1, "timestamp"],Length = numeric(nrow(Monet_points) - 1))
  
  # Loop through each fixation point to calculate and accumulate the saccade lengths
  for (i in 1:(nrow(Monet_points) - 1)) {
    diffx = Monet_points$locX[[i+1]] - Monet_points$locX[[i]]
    diffy = Monet_points$locY[[i+1]] - Monet_points$locY[[i]]
    len = sqrt(diffx^2 + diffy^2)
    cumulative_length = cumulative_length + len
    Saccade_length_cumulative$Length[i] = cumulative_length
  }
  return(Saccade_length_cumulative)
}

Scanpath_data = function(monet_data){
  
  Monet_points = data.frame(locX = monet_data$locX, locY = monet_data$locY, timestamp = monet_data$timestamp) 
  cumulative_length = 0  # Initialize cumulative length
  Saccade_length_cumulative = data.frame(Timestamp = Monet_points[-1, "timestamp"],Length = numeric(nrow(Monet_points) - 1))
  len_list = list()
  # Loop through each fixation point to calculate and accumulate the saccade lengths
  for (i in 1:(nrow(Monet_points) - 1)) {
    diffx = Monet_points$locX[[i+1]] - Monet_points$locX[[i]]
    diffy = Monet_points$locY[[i+1]] - Monet_points$locY[[i]]
    len = sqrt(diffx^2 + diffy^2)
    len_list[i] = len
    cumulative_length = cumulative_length + len
    Saccade_length_cumulative$Length[i] = cumulative_length
  }
  return(list(Saccade_length_cumulative,len_list))
}
