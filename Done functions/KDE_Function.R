library("spatstat")

#input: Result from Sort_function
#output: KDE list and max_lamda

kde_function = function(Sort_data,Centers_and_Rad) {
  
  #Extracts relevant data from the sort_data list
  Monet_data = Sort_data
  Circles = Centers_and_Rad
  
  #Defines window
  win = owin(xrange = c(5, 1017), yrange = c(0, 767))
  #Turns points into point process data
  pppdata = ppp(Monet_data$locX,Monet_data$locY,window = win)
  
  #Creates the KDE list
  kde = density(pppdata,20)
  #Turns all negative values into zeros
  kde_corrected = ifelse(kde$v < 0, 0, kde$v)
  kde$v <- kde_corrected
  
  max_lambda = numeric()
  
  #Calculates the max lambda for each circle.
  #v_index_xxxx is the corresponding indexes in the v-matrix for the circles
  #max_lambda calculates the largest value in v in the created "rectangle".
  for (i in (1:length(Circles$x))){
    
    v_index_xmin = round(Circles$x[i]/kde$xstep)-round(Circles$radius[i]/kde$xstep)
    v_index_xmax = round(Circles$x[i]/kde$xstep)+round(Circles$radius[i]/kde$xstep)
    v_index_ymin = round(Circles$y[i]/kde$ystep)-round(Circles$radius[i]/kde$ystep)
    v_index_ymax = round(Circles$y[i]/kde$ystep)+round(Circles$radius[i]/kde$ystep)
    
    max_lambda[i] = max(kde$v[v_index_xmin:v_index_xmax,v_index_ymin:v_index_ymax])
  }
  #Adds the max value for the entire KDE including the background.
  max_lambda[i+1] = max(kde$v)
  
  return(list(max_lambda=max_lambda, kde_corr=kde))
}
