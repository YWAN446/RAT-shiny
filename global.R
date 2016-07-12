## Server Settings

baseURL <- 'http://address/'
apiUrl <- 'http://address/api/v1/data' # this is the main access point to use for data
apiToken <- 'token_key'
usr <- 'usrname'
pwd <- 'password'

## Load the files we need

source("./model/PS_Plot.r")
options(shiny.maxRequestSize = 9*1024^2)
source('api_helpers.R') # Formhub interactions
source('analysis_helpers.R') # Number crunching
source('plotting_helpers.R') # Number presenting
source('report_helpers.R') # Specifically for reporting
