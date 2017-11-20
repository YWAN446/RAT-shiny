# Utility functions


factor_to_numeric <- function(x) {
  # convert factor or character data to numeric
  return(suppressWarnings(as.numeric(as.character(x))))
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