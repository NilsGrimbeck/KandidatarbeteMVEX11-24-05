source("./Done functions/ms_clustering_ROI.R")
source("./Done functions/transistion_probabilities.R")
source("./Done functions/Statistikor.R")
source("./Models/MarkovChain1.R")
source("./Done functions/Sort_function.R")
load("Monet.RData")

data<-Monet[Monet$inpic==1 & Monet$timestamp<=30000,]
msret <- ms_clustering_ROI(data,0.045,plot=TRUE) # change to FALSE if want to use ggplot
# Extract the ROI labeled data and cluster centers from the function's output
ROI_labelled_Monet <- as.data.frame(Sort_function(msret)[1])  # The updated dataframe with cluster labels and background
ROI_Centers_and_Rad <- as.data.frame(Sort_function(msret)[2]) # ROI circles
transition_matrix <- transition_probabilities(ROI_labelled_Monet)


sim_nfix<-replicate_Markov1_nfix(100,transition_matrix)
data_average_nfix<-as.data.frame(table(ROI_labelled_Monet$cluster.label)/20)
colnames(data_average_nfix)<-c("ROI","nfix_data")
ROInr<-data.frame(ROI=0:(dim(transition_matrix)[1]-1))
data_average_nfix<-merge(ROInr,data_average_nfix, by="ROI",all=TRUE)
result_fix<-merge(sim_nfix[,c("ROI","average")],data_average_nfix,all=TRUE)
result_fix

sim_duration<-replicate_Markov1_dur(100,transition_matrix)
data_average_dur<-as.data.frame(aggregate(ROI_labelled_Monet$duration, by=list(ROInr=ROI_labelled_Monet$cluster.label), FUN=sum))
colnames(data_average_dur)<-c("ROI","duration_data")
data_average_dur$duration_data=data_average_dur$duration_data/20
data_average_dur<-merge(ROInr,data_average_dur, by="ROI", all=TRUE)
result_duration<-merge(sim_duration[,c("ROI","average")],data_average_dur,all=TRUE)
result_duration
