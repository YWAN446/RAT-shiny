baseURL <- 'http://54.210.2.87/'
apiUrl <- 'http://54.210.2.87/api/v1/data' # this is the main access point to use for data
apiToken <- '8d0336d37ef28df590574f1cd4531f142e31ca02'
usr <- 'sp'
pwd <- '2007beagle'

school_data <- formhubGET_csv(baseURL, usr, pwd, 'school_d')
collection_data <- formhubGET_csv(baseURL, usr, pwd, 'sp_sample_collection_form_1_c')
lab_data <- formhubGET_csv(baseURL, usr, pwd, 'sp_sample_lab_form_1_i')
household_data <- formhubGET_csv(baseURL, usr, pwd, 'sp_household_form_2_01b')
community_data <- formhubGET_csv(baseURL, usr, pwd, 'community_d')

ec_data <- create_ecData(collection_data, lab_data)
conc <- create_concData(ec_data)

ps.freq <- calculate_freq(household_data, school_data, community_data, type='ppl plot', survey_type='combined')

ps.freq <- calculate_pplPlotData(ps.freq, conc)


report_r <- report_results(ps.freq)

report <- function() {report_r}

rmarkdown::render('report.Rmd')

freq <- calculate_freq(household_data, school_data, community_data, survey_type='combined')
