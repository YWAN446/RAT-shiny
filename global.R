## Server Settings

baseURL <- 'https://rat.sanipath.org/'

# Test form
test_form <- 'Register_Enumerator'

# Default forms
collection_form <- 'Training_Sample_d'
school_form <- 'School_Current'
community_form <- 'Community'
lab_form <- 'Training_Lab_e'
household_form <- 'Training_Household_d'

## Load the files we need
source("model/PS_Plot.r")
options(shiny.maxRequestSize = 9*1024^2)
source('model/api_helpers.R') # Formhub interactions
source('model/analysis_helpers.R') # Number crunching
source('model/plotting_helpers.R') # Number presenting
source('model/report_helpers.R') # Specifically for reporting

ui_elements <- 'ui_elements/'
ui_locations <- paste0(ui_elements, list.files(ui_elements))
sapply(ui_locations, function(x) source(x))
