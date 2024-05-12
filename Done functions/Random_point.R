library("dplyr")
library("ggplot2")
library("ggforce")
library("jpeg")
library("grid")
library("spatstat")

#Input:
#cluster is an integer where 0 is background and cluster>0 the circles
#Sort_data is the result from Sort_function
#kde_data is the result from kde_function

#Output: a single coordinate/point within the defined state

Random_point <- function(cluster, prepoint, Sort_data, Centers_and_Rad, kde_data) {
  #pre_point=c(locX,locY,cluster)
  #Sorted data is all data sorted into their respective circle or background
  
  #Corrected kde data with only non-negative numbers
  kde = kde_data$kde_corr
  prepoint=as.numeric(prepoint)
  #Array with the max lambda for every circle or background
  #where index 6 is the background
  max_lambda = kde_data$max_lambda
  
  #cluster>0 is all circles
  if(cluster>0){
  
  #Normalizes the KDE with the respective lambda
  norm_kde = kde$v/max_lambda[cluster]
  randpoint <- data.frame(xpos = numeric(), ypos = numeric())
  
    
    while(TRUE){
    
    #Generates random point inside the circle
      if (prepoint[3]==cluster){
        inROI=FALSE
        while(inROI==FALSE){
          theta<-runif(1,0,2*pi)
          l<-rgamma(1,1.556,0.0085)
          xpos<-prepoint[1]+l*cos(theta)
          ypos<-prepoint[2]+l*sin(theta)
          #if(xpos>=5 && xpos<=1017){
          #  if(ypos>=0 && ypos<=767){
          if(sqrt((xpos - Centers_and_Rad$x[cluster])^2 + (ypos - Centers_and_Rad$y[cluster])^2) <= Centers_and_Rad$radius[cluster])
              inROI=TRUE
           # }
          #}
        }
        
      }else{
        theta = runif(1, 0, 2*pi)
        r = Centers_and_Rad$radius[cluster]*(runif(1,0,1))
        xpos = Centers_and_Rad$x[cluster] + r*cos(theta)
        ypos = Centers_and_Rad$y[cluster] + r*sin(theta)
      }
    
    
    #Determines the x and y index for the KDE-matrix
    x_index = round(min(max(xpos/kde$xstep,5),nrow(norm_kde)))
    y_index = round(min(max(ypos/kde$ystep,1),nrow(norm_kde)))
    
    #Chooses lambda for the point
    lambda = norm_kde[x_index,y_index]
  
      #Either saves or discards the point based on lambda
      if (runif(1,0,1) < lambda){
        randpoint=c(xpos,ypos)
        break
      }
  
    }
  
  }
  
  #Runs if in 'background'-state
  if (cluster == 0) {
    #Defines window as the range of the painting
    window <- owin(xrange = c(5, 1017), yrange = c(0, 767))
    #normalizes KDE
    norm_kde = kde$v/max_lambda[6]
    
    while(TRUE) {
      #Generates point in window
      #if (prepoint[3]==0){
        inpic=FALSE
        while(inpic==FALSE){
          theta<-runif(1,0,2*pi)
          l<-rgamma(1,1.556,0.0085)
          point_df<-data.frame(x=prepoint[1]+l*cos(theta),y=prepoint[2]+l*sin(theta))
          if(point_df$x>=5 && point_df$x<=1017){
            if(point_df$y>=0 && point_df$y<=767){
              inpic=TRUE
            }
          }
        }
      #}else{
       # point <- runifpoint(1, win = window)
        #point_df <- as.data.frame(point)
      #}
      
      #Checks if point is inside a circle
      inside_circle <- FALSE
      for (i in 1:nrow(Centers_and_Rad)) {
        if (sqrt((point_df$x - Centers_and_Rad$x[i])^2 + (point_df$y - Centers_and_Rad$y[i])^2) <= Centers_and_Rad$radius[i]) {
          inside_circle <- TRUE
          break
        }
      }
      
      #If point is not inside a circle->calculate indexes and lambda
      if (!inside_circle) {
        
        #Makes sure the index does not exceed 128 or is 0
        x_index <- min(max(round(point_df$x / kde$xstep),1) , nrow(norm_kde))
        y_index <- min(max(round(point_df$y / kde$ystep),1) , nrow(norm_kde))
        
        lambda <- norm_kde[x_index, y_index]
        
        
        if (runif(1, 0, 1) < lambda) {
          return(point_df)
        }
      }
    }
  
  }
  return(randpoint)
}



















