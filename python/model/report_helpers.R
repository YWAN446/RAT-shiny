# Report Helpers
library(rlist)
library(plyr)
# REPORT ============================================================
report_results <- function(ps.freq, freq_thresh = 50) {
  # summarize results by neighborhood an age group
  # freq_thresh is used to determine the high/low frequency
  # pathways.  default is 50%
  # this will return a nested list of neighborhoods by age
  # with the dominant pathways identified and a summary table
  
  neighborhoods <- unique(names(list.names(ps.freq, neighborhood)))
  age <- c('Adults', 'Children')
  
  result <- list()
  for (i in neighborhoods) {
    # look at each neighborhood
    # then at each age
    n_result <- list()
    for (a in age) {
      subset <- ps.freq[list.which(ps.freq, neighborhood == i && age == a)]
      subset_result <- list(report_pathwayResult(subset, freq_thresh))
      names(subset_result) <- a
      n_result <- append(n_result, subset_result)
    }
    n_result <- list(n_result)
    names(n_result) <- i # name the neighborhood
    result <- append(result, n_result)
  }
  
  return(result)
  
}

report_pathwayResult <- function(ps.freq, freq_thresh = 50) {
  # identify the dominant pathway(s) and 
  # create a 2x2 table arranging pathways by dose and frequency
  # for each neighborhood and age group
  # intented use to take a list object of samples for a given neighborhood
  # and age
  # freq_thresh is used to determine the high/low frequency
  # pathways.  default is 50%
  
  # calculate an exposure score
  # E = log10( (10 ^ dose) * n/100)
  calc_exposure_score <- function(list_obj, freq_thresh ) {
    E = log10( (10 ^ list_obj$dose) * list_obj$n/100)
    f = (list_obj$n >= freq_thresh)
    return(data.frame(t(c('E' = E, 'freq' = f))))
  }
  
  # calculate the dose and frequency score for each pathway
  pathway_results <- data.frame()
  for (i in ps.freq) {
    pathway_results <- rbind.fill(pathway_results, calc_exposure_score(i, freq_thresh))
  }
  
  # Counting Inf as Low Dose
  pathway_results$E[pathway_results$E == Inf] <- NA
  dose_threshold <- .5*(min(pathway_results$E, na.rm=T)+ max(pathway_results, na.rm=T))
  # create a data frame with binary high dose/high frequency variables for table arrangement
  pathway_results$high_dose <- as.numeric(pathway_results$E > 10 |
                                            pathway_results$E >= dose_threshold)
  # pathway_results$high_dose[is.na(pathway_results$high_dose)] <- 0
  
  # add the pathway names
  pathway_results$pathway <- unlist(lapply(ps.freq, function(a) a$sample))
  pathway_results <- pathway_results[order(pathway_results$E, decreasing=T),]
  
  # reshape the data
  result_matrix <- matrix(NA, nrow=2, ncol=2)
  rownames(result_matrix) <- c("High Dose", "Low Dose")
  colnames(result_matrix) <- c('High Frequency', 'Low Frequency')
  # identify the dominant pathways
  
  pathway_results$dominant <- as.numeric(pathway_results$E >= (max(pathway_results$E, na.rm=T) - 1) | pathway_results$E >= 10)
  
  for (i in 1:nrow(pathway_results)) {
    # false (0) now needs to be 2 to indicate the row or column
    score <- pathway_results[i, c('high_dose', 'freq')]
    score[score == 0] <- 2
    
    score <- unlist(score)
    if (pathway_results$dominant[i] == 1 & !is.na(pathway_results$dominant[i])) {
      dom <- as.character("\\*")
    } else {
      dom <- ""
    }
    
    # now insert the pathway into that spot
    result_matrix[score[1], score[2]] <- paste0(c(result_matrix[score[1], score[2]], paste0(pathway_results[i, 'pathway'],dom)), collapse=", ")
  }
  result_matrix <- gsub('NA, ', '', result_matrix)
  result_matrix[is.na(result_matrix)] <- ""
  
  # Identify the most dominant pathway
  max_dominant <- pathway_results$pathway[pathway_results$E == max(pathway_results$E, na.rm=T)]
  max_dominant <- max_dominant[!is.na(max_dominant)]  
  
  max_dom_info <- ps.freq[list.which(ps.freq, sample == max_dominant)]$path
  
  
  
  return(list('max_dominant' = max_dom_info, # the highest dominant pathway
              'dominant' = pathway_results[pathway_results$dominant == 1 & !is.na(pathway_results$dominant), 'pathway'], 
              'matrix' = result_matrix, 
              'pathway_table' = pathway_results))
  
}