## Server Settings

baseURL <- 'http://54.210.2.87/'

# Test form
test_form <- 'sp_sample_collection_form_1_c'

# Default forms
collection_form <- 'sp_sample_collection_form_1_c'
school_form <- 'school_d'
community_form <- 'community_d'
lab_form <- 'sp_sample_lab_form_1_i'
household_form <- 'sp_household_form_2_01b'

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
