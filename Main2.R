source("./Done functions/ms_clustering_ROI.R")
source("./Done functions/transistion_probabilities.R")
source("./Done functions/Statistikor.R")
source("./Done functions/KDE_Function.R")
source("./Done functions/Sort_function.R")
source("./Done functions/Random_point.R")
source("./Models/MarkovChain.R")
source("./Models/MarkovPoint2.R")
source("./Done functions/ScanPath.R")
source("./Done functions/ConvexHull.R")
library("spatstat")
library("jpeg")
library("cxhull")
load("Monet.RData")

#Data processing and transition matrix computation.
data<-Monet[Monet$inpic==1 & Monet$timestamp<=30000,]
msret <- ms_clustering_ROI(data,0.045,plot=TRUE) # change to FALSE if want to use ggplot
# Extract the ROI labeled data and cluster centers from the function's output
ROI_labelled_Monet <- as.data.frame(Sort_function(msret)[1])  # The updated dataframe with cluster labels and background
ROI_Centers_and_Rad <- as.data.frame(Sort_function(msret)[2]) # ROI circles
transition_matrix <- transition_probabilities(ROI_labelled_Monet)

#Computation of Intensity
KDEret<-kde_function(ROI_labelled_Monet,ROI_Centers_and_Rad)




#Kör statistikor för att generera resultat







#Kör statistikor nedan för att generera resultat

#------------------Scanpath------------------------------------

nrep = 100 #Number of simulations, can be chosen freely.

scanpath_simulation = replicate_scanpath(nrep) 
sim_data = scanpath_simulation[[1]]
sim_data = sim_data[[1]]

#Plots the simulations.
plot(sim_data$Timestamp, sim_data$Length, type = "l", col = "black",xlab = "Tid (ms)",ylab="Kumulativ sackadlängd")

for (i in 2:length(scanpath_simulation)){
  sim_data = scanpath_simulation[[i]]
  sim_data = sim_data[[1]]
  lines(sim_data$Timestamp, sim_data$Length, col='black', lwd=2)
  
}

Scanpath_list = list()
Len_list = list()
for (i in 1:20){
  
  Scanpath_run = Scanpath_data(Monet[Monet["id"]==i & Monet["inpic"]==1,])
  Scanpath_list[[i]] = Scanpath_run[[1]]
  Len_list[[i]] = Scanpath_run[[2]]
  
}


for (i in 1:length(Scanpath_list)){
  scanpath_data = Scanpath_list[[i]]
  lines(scanpath_data$Timestamp, scanpath_data$Length, col='red', lwd=2)
  
}
legend("topleft", col=c("black","red"), legend=c("Simuleringar","Individdata"),lty=1, cex=1)
#--------------------------------------------------------------#

#----------------Convex Hull-----------------------------------#

nrep = 100

Convex_hull_sim = replicate_convex_hull(nrep)
Convex_hull_init = Convex_hull_sim[[1]]
Convex_hull_init = Convex_hull_init[[1]]

plot(Convex_hull_init[,"Timestamp"],Convex_hull_init[,"Area"],type = "l", col="black", lwd=2,xlab = "Tid (ms)",ylab="Kumulativ area")

for(i in 2:nrep){
  
  Convex_hull_sim_plot = Convex_hull_sim[[i]]
  Convex_hull_sim_plot = Convex_hull_sim_plot[[1]]
  lines(Convex_hull_sim_plot[,"Timestamp"],Convex_hull_sim_plot[,"Area"], col="black", lwd=2)
  
}

#Plots the convex hull for the actual data
ConvexHull_data_plot = Convex_hull_data(Monet)

for(j in 1:20){
  
  ConvexHull = ConvexHull_data_plot[[j]]
  lines(ConvexHull[,"Timestamp"],ConvexHull[,"Area"], col="red", lwd=2)
  
}
legend("bottomright", col=c("black","red"), legend=c("Simuleringar","Individdata"),lty=1, cex=1)
#------------------------------------------------------------------------------------#

#Sackadfördelning
library("fitdistrplus")

sackad_len = unlist(Len_list)

descdist(sackad_len,boot=1000)

fitgamma = fitdist(sackad_len,"gamma")
fitwei = fitdist(sackad_len,"weibull")
plot(fitgamma)

gof_weibull = gofstat(fitwei)
gof_gamma = gofstat(fitgamma)
print(gof_weibull)
print(gof_gamma)

bgamma = bootdist(fitgamma, niter = 100)
summary(bgamma)

bwei = bootdist(fitwei,niter = 100)
summary(bwei)


#-----------------------------------------------------#
#generate result for plotting
result<-MarkovModel2(transition_matrix,KDEret,ROI_labelled_Monet, ROI_Centers_and_Rad)
#plot
img<-readJPEG("./Monet.JPG")
ppp_sim_result<-ppp(x=as.numeric(result$locX), y=as.numeric(result$locY), window = owin(c(5,1017),c(0,767)))
kde <- density(ppp_sim_result, sigma =20)
kde_df=as.data.frame(kde)
p<- ggplot() + annotation_raster(img, xmin=min(kde_df$x), xmax = max(kde_df$x), ymin=min(kde_df$y), ymax=max(kde_df$y))+
  geom_point(data=data.frame(ppp_sim_result), aes(x=x,y=y), color="blue", size=4, alpha=0.5)+
  coord_cartesian(xlim=c(min(kde_df$x),max(kde_df$x)), ylim=c(min(kde_df$y),max(kde_df$y)))+
  theme_void() +geom_circle(data = ROI_Centers_and_Rad, aes(x0=x, y0=y, r=radius), color="red")
print(p)

p2 <- ggplot()+annotation_raster(img, xmin=min(kde_df$x), xmax = max(kde_df$x), ymin=min(kde_df$y), ymax=max(kde_df$y))+
  geom_point(data=data.frame(locX=data$locX, locY=data$locY), aes(x=locX, y=locY))+
  theme_void()
print(p2)


#--------------------------------------------------------------
# Intensity on Monet

ppp_sim_result <- ppp(x=as.numeric(result$locX), y=as.numeric(result$locY), window = owin(c(5,1017), c(0,767)))
# Compute KDE
kde <- density(ppp_sim_result, sigma = 20)

# Correct any negative values in KDE
kde_corrected = ifelse(kde$v < 0, 0, kde$v)
kde$v <- kde_corrected

kde_df = as.data.frame(kde)

intensity_criteria = 0.005

kde_df$value[kde_df$value < intensity_criteria] <- NA

# plot intensity
intensity_plot <- ggplot() +
  annotation_raster(img, xmin = min(kde_df$x), xmax = max(kde_df$x),
                    ymin = min(kde_df$y), ymax = max(kde_df$y)) +
  geom_tile(data = kde_df, aes(x = x, y = y, fill = value), alpha = 0.6) +
  scale_fill_viridis_c(option="turbo", na.value = NA) +
  theme_void() 

plot(intensity_plot)
