# Main functions for SaniPath analysis and plotting functions

compute_frequencies <- function(..., analysis_type=NULL, output_dir='./plots') {
  # Run the frequency counts and generate pie charts for display.
  # pie charts are saved with the same naming convention as the survey pathway 
  # configurations (ie pie_c_d_a.png)
  # _____________________________________________________
  # ... => household, community, or/and school dataframe. can be one or all three
  # analysis_type => the survey type for analysis purposes (if multiple passed)
  #   can be 'household', 'community', 'school', or 'combined'
  # output_dir => file location where the plots will be saved. 
  # _____________________________________________________
  # returns a list converted to json with frequencies and file location of corresponding plot
  
  freq <- calculate_freq(..., analysis_type = analysis_type, type = 'pie')
  
  freq <- make_plots(freq, type='pie')
  
}

compute_concentrations <- function(collection, lab, config, output_dir='./plots/') {
  # Run the frequency counts and generate pie charts for display.
  # pie charts are saved with the same naming convention as the survey pathway 
  # configurations (ie pie_c_d_a.png)
  # _____________________________________________________
  # ... => household, community, or/and school dataframe. can be one or all three
  # analysis_type => the survey type for analysis purposes (if multiple passed)
  #   can be 'household', 'community', 'school', or 'combined'
  # output_dir => file location where the plots will be saved. 
  # _____________________________________________________
  # returns a list converted to json with frequencies and file location of corresponding plot
  
  conc <- create_concData(collection, lab, mpn_tb)
  
}

fname <- function(dir, plot_type, fn) {
  # concatenate parts to make a filename for plots
  # ___________________________________________________
  # dir => output directory to be prepended to the filename
  # plot_type => 'pie', 'hist', 'ppl' 
  # fn => file name already created in the list object
  # ___________________________________________________
  # returns filename with png appended
  return(sprintf('%s%s_%s', dir, plot_type, fn))
}
