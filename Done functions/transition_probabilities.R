# ROI-ROI Transition probability

# ROI_labled_data already ordered by participant and then by timestamp already
transition_probabilities <- function(ROI_labled_data) {
  
  # Shift the cluster label column to get next fixations
  # pairs each fixation point with the next one in sequence by their ROI labels
  shifted_labels <- dplyr::lead(ROI_labled_data$cluster.label)
  
  # Create a dataframe of transitions, pairs each 'from' ROI with 'to' ROI
  transitions <- data.frame(
    from = ROI_labled_data$cluster.label[-nrow(ROI_labled_data)],  # Exclude last fixation since no subsequent fixation
    to = shifted_labels[-nrow(ROI_labled_data)]  # Exclude first fixation since no preceding fixation
  )
  
  # Omits rows in transition where column is NA (is the end of each participant, since no subsequent fixation)
  transitions <- na.omit(transitions)
  
  # Count transitions - occurrence of each transition between ROIs
  transition_counts <- table(transitions)
  
  # Compute transition probabilities - calculate probability of transitioning from each 'from' ROI to each 'to' ROI
  transition_probabilities <- prop.table(transition_counts, 1)  # Normalize counts by row
  
  return(as.matrix(transition_probabilities))
}
