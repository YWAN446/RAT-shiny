# Utility functions
save_json <- function(x, fn) {
  # save a list object to json
  write(toJSON(x), fn)
}

read_json <- function(x) {
  # This one is just a wrapper around fromJSON
  return(fromJSON(x))
}

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

build_requirements_file <- function(base_directory='./', out_dir = './', out_name = 'r-requirements.txt') {
  # Read lines from R or r files and find any time "library(.)" occurs. 
  # strip the library lines out and make a requirements file commented
  # by file name
  files <- grep('\\.R$|\\.r$|\\.Rmd', list.files(base_directory, recursive = T), value=T)
  f <- file(file.path(out_dir, out_name))

  libraries <- lapply(files, function(x) {
    x_lines <- readLines(x)
    x_lines <- unlist(strsplit(x_lines, ' '))
    libs <- grep('library\\(.*\\)', x_lines, value=T, perl=T)
    libs <- gsub('library\\(|\\)', '', libs, perl = T)
    libs <- libs[libs != "\".\""]
    # cat(sprintf("# %s", x), libs, file= f, sep='\n', append=T)
    paste(c(sprintf("# %s", x), libs), collapse="\n")
  })
  
  libraries <- paste(libraries, collapse="\n\n")
  cat('# R Requirements File',
      sprintf('# Generated %s', Sys.Date()),
      "# Use install_requirements() to install",
      "# _____________________________________",
      "", 
      libraries,
      file= f, sep='\n', append=T)
  close(f)
  return(libraries)
}

install_requirements <- function(requirements_file, only_missing=T) {
  # Install requirements listed in an file of libraries
  # one per line
  reqs <- readLines(requirements_file)
  reqs <- reqs[!grepl("#", reqs) & reqs != ""]
  
  if (only_missing) {
    reqs = reqs[!(reqs %in% installed.packages())]
  }
  
  install.packages(reqs)
  
}