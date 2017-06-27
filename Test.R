# baseURL <- 'http://54.210.2.87/'
# apiUrl <- 'http://54.210.2.87/api/v1/data' # this is the main access point to use for data
# apiToken <- '8d0336d37ef28df590574f1cd4531f142e31ca02'
# usr <- 'sp'
# pwd <- '2007beagle'

baseURL <- 'https://tool.sanipath.org/'
apiUrl <- 'http://54.210.2.87/api/v1/data' # this is the main access point to use for data
apiToken <- '8d0336d37ef28df590574f1cd4531f142e31ca02'
usr <- "cambodia_8_16"
pwd <- "jamaica_plain"

source("~/stat/RAT-shiny/model/analysis_helpers.R")
source("~/stat/RAT-shiny/model/api_helpers.R")
source("~/stat/RAT-shiny/model/plotting_helpers.R")
source("~/stat/RAT-shiny/model/PS_Plot.r")
source("~/stat/RAT-shiny/model/report_helpers.R")

# school_data <- formhubGET_csv(baseURL, usr, pwd, 'school_d')
# collection_data <- formhubGET_csv(baseURL, usr, pwd, 'sp_sample_collection_form_1_c')
# lab_data <- formhubGET_csv(baseURL, usr, pwd, 'sp_sample_lab_form_1_i')
# household_data <- formhubGET_csv(baseURL, usr, pwd, 'sp_household_form_2_01b')
# community_data <- formhubGET_csv(baseURL, usr, pwd, 'community_d')

school_data <- formhubGET_csv(baseURL, usr, pwd, 'School_Current')
collection_data <- formhubGET_csv(baseURL, usr, pwd, 'Training_Sample_d')
lab_data <- formhubGET_csv(baseURL, usr, pwd, 'Training_Lab_e')
household_data <- formhubGET_csv(baseURL, usr, pwd, 'Training_Household_d')
community_data <- formhubGET_csv(baseURL, usr, pwd, 'Community')

ec_data <- create_ecData(collection_data, lab_data)
conc <- create_concData(ec_data)
gen_hist(conc)

path<-c("Drain Water", "Produce", "Piped Water", 
        "Ocean Water", "Surface Water", "Flood Water",
        "Public Latrine Surfaces", "Particulate", "Bathing",
        "Ice", "Water Supply", "Bottled Water", "Well Water")

age<-c("Adults","Children")
neighborhood<-c("Chong Kaosou (Informal)","Kumruthemey (Informal)","Kumruthemey (Formal)","Steung Thumey (Formal)","Veal/Trapangses (Formal)")

freq<-calculate_freq(household_data,survey_type = "household")

i=1
my_pie<-create_pieChart(freq[[i]]$data,freq[[i]]$sample,paste0(neighborhood[as.numeric(substring(freq[[i]]$neighborhood,14,14))],"\n",freq[[i]]$age," ",freq[[i]]$sample))
print(my_pie)

ps.freq <- calculate_freq(household_data, school_data, community_data, type='ppl plot', survey_type='combined')

ps.freq <- calculate_pplPlotData(ps.freq, conc)

report_r <- report_results(ps.freq)

report <- function() {report_r}

rmarkdown::render('report.Rmd')

freq <- calculate_freq(household_data, school_data, community_data, survey_type='combined')
