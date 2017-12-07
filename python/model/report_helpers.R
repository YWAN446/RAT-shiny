# Report Helpers
library(rlist)
library(plyr)
library(tidyr)
# REPORT ============================================================

compute_report <- function(params = list(city_name = 'Atlanta, GA',
                                         lab_name = 'Bill Nye, Inc',
                                         start_date = '2017-01-01',
                                         lab_MF = F,
                                         language = "English",
                                         household_data = data.frame(),
                                         school_data = data.frame(),
                                         community_data = data.frame(),
                                         sample_data = data.frame(),
                                         lab_data = data.frame(),
                                         ps_freq = list(), 
                                         pathway_codes = list(),
                                         pathway_labels= list(),
                                         neighborhood_mapping = list(),
                                         freq_thresh=50),
                           out_dir = './',
                           output_format = 'word_document') {
  # Compute all of the necessary parameters for the report 
  # generation
  # ___________________________________
  # city_name => city where the tool was deployed
  # lab_name => lab processing the samples
  # start_date => the day the tool was deployed
  # lab_MF => boolean of whether Membrane Filtration was used for lab
  # pathways => a character vector of the pathways used
  # language => language of the surveys (from configuration of tool)
  # household_data => data collected at households
  # school_data => data collected at schools
  # community_data => data collected at community
  # sample_data => data collected for sample collection
  # ps_freq => results from exposure calcluations
  # neighborhood_mapping => named list of Neighborhood text = coded value
  # pathway_codes => named list of pathway codes = coded value
  # pathway_labels => named list of pathway codes = pathway label name
  # freq_thresh => the threshold for determining if something is dominant
  # ___________________________________
  # returns a list with named attributes related to the report
  # document params
  
  options <- c('word_document', 'pdf_document')
  if (!(output_format %in% options)) stop(sprintf("output_format invalid. options: %s", paste(options, collapse=', ')))

  attach(params)  
  # Set up the pathways param
  pathways = pathway_labels %>% unlist()
  if (length(pathways) > 1) {
    separator <- if (length(pathways) > 2) ', ' else ' ' 
    pathways[length(pathways)] %<>% paste0('and ',.)
    pathways %<>% paste0(collapse=separator)
  }
  print(pathways)
  params$behavior_table <- behavior_table(household_data,
                                          school_data,
                                          community_data,
                                          neighborhood_mapping)
  print(params$behavior_table)
  params$sample_table <- sample_table(sample_data,
                                                    neighborhood_mapping,
                                                    pathway_codes,
                                                    pathway_labels)
  print(params$sample_table)
  params$dominant_exposure_pathways <- dominant_pathway_table(report_results(ps_freq, freq_thresh = freq_thresh))
  print('hi')
  rmarkdown::render('report.Rmd', output_dir = out_dir, params= params, output_format=output_format)
  
  
}

report_results <- function(ps.freq, freq_thresh = 50) {
  # summarize results by neighborhood an age group
  # freq_thresh is used to determine the high/low frequency
  # pathways.  default is 50%
  # this will return a nested list of neighborhoods by age
  # with the dominant pathways identified and a summary table
  
  neighborhoods <- unique(names(list.names(ps.freq, neighborhood)))
  age <- sapply(ps.freq, function(x) x$age) %>% unlist() %>% unique()

  pathway_results <- data.frame()  
  for (i in neighborhoods) {
    # look at each neighborhood
    # then at each age
    # I don't remember why we had to store the data in lists
    # changing that to one dataframe now for ease of use. if 
    # this were to all be refactored, it could be much more 
    # efficient
    for (a in age) {
      subset <- ps.freq[list.which(ps.freq, neighborhood == i && age == a)]
      subset_result <- report_pathwayResult(subset, freq_thresh)
      pathway_results %<>% bind_rows(subset_result$pathway_table)
    }
  }
  
  return(pathway_results)
  
}

dominant_pathway_table <- function(pathway_results) {
  # Make the table with dominant pathways identified
  return(pathway_results %>% group_by(neighborhood, age) %>% 
    filter(dominant == 1) %>%
    summarize(dominant = paste0(pathway, collapse=',\n')) %>%
    spread(age, dominant) %>%
    rename('Neighborhood' = 'neighborhood') %>%
    as.data.frame())
}

behavior_table <- function(household_data, school_data, community_data, neighborhood_mapping) {
  # Make a tabluation of the results by neighborhood
  count_data <- function(df, form_name) {
    x <- as.data.frame(table(df[,grep('_neighborhood$', names(df))]))
    x$data <- form_name
    return(x)
  }
  
  results <- bind_rows(
    count_data(household_data, 'Households Surveyed'),
    count_data(community_data, 'Communities Surveyed'),
    count_data(school_data, 'Schools Surveyed')
  )
  
  n_map <- names(neighborhood_mapping)
  names(n_map) <- neighborhood_mapping %>% unlist()
  results$Var1 %<>% as.character() %>% revalue(n_map)
  
  results %<>% spread(data, Freq)
  names(results)[1] <- 'Neighborhood'
  results[is.na(results)] <- 0  
  results %<>% bind_rows(cbind.data.frame("Neighborhood" = 'Total', as.list(colSums(results[,-1]))))

  return(results)  
}

sample_table <- function(sample_data, neighborhood_mapping, pathway_codes, pathway_labels) {
  # Make a table of the different samples collected
  x <- as.data.frame(table('Neighborhood' = as.character(sample_data$col_neighborhood), 
                           'Sample' = as.character(sample_data$col_sample_type)), stringsAsFactors = F)
  n_map <- names(neighborhood_mapping)
  names(n_map) <- neighborhood_mapping %>% unlist()
  x$Neighborhood %<>% revalue(n_map, warn_missing = F)
  
  p_map <- sapply(names(pathway_codes), function(x) pathway_labels[[x]]) 
  names(p_map) <- unlist(pathway_codes)
  x$Sample %<>% revalue(p_map, warn_missing = F)
  
  x %<>% spread(Sample, Freq)
  x %<>% bind_rows(cbind.data.frame("Neighborhood" = 'Total', as.list(colSums(x[,-1]))))
  
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
    if (list_obj$dose == Inf) list_obj$dose = 0
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
  pathway_results$max_dominant <- as.numeric(pathway_results$E == max(pathway_results$E, na.rm=T))
  pathway_results$age <- sapply(ps.freq, function(x) x$age) %>% unlist() %>% unique()
  pathway_results$neighborhood <- sapply(ps.freq, function(x) x$neighborhood) %>% unlist() %>% unique()
  
  # max_dominant <- max_dominant[!is.na(max_dominant)]  
  
  # max_dom_info <- ps.freq[[list.which(ps.freq, sample == max_dominant)]]
  
  
  
  return(list(#'max_dominant' = max_dom_info, # the highest dominant pathway
              'dominant' = pathway_results[pathway_results$dominant == 1 & !is.na(pathway_results$dominant), 'pathway'], 
              'matrix' = result_matrix, 
              'pathway_table' = pathway_results))
  
}

identify <- function(ps.freq, attribute='dose', FUN=max) {
  # Run a function on the exposure data
  # ______________________________________
  # ps.freq => calculated results from calculate_exposure
  # attribute => the list attribute to examine
  # FUN => function to apply on the attribute selected
  # ______________________________________
  # returns an index value(s) where attribute %in% result of FUN
  
  attr_values <- sapply(ps.freq, function(x) x[attribute]) %>% unlist()
  result <- which(attr_values %in% FUN(attr_values))
  return(result)
}
