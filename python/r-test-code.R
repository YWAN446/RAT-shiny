hh <- read.csv('rsrc/HOUSEHOLD_EXAMPLE.csv', stringsAsFactors = F)
cc <- read.csv('rsrc/COMMUNITY_EXAMPLE.csv', stringsAsFactors = F)
sc <- read.csv('rsrc/SCHOOL_EXAMPLE.csv', stringsAsFactors = F)

col <- read.csv('rsrc/SAMPLE_EXAMPLE.csv', stringsAsFactors = F)
lab <- read.csv('rsrc/LAB_EXAMPLE.csv', stringsAsFactors = F)

sapply(grep('.R$', list.files('model', full.names = T), value=T), source)
source('config.R')

# Frequency calculations
hh_freq <- calculate_freq(hh,survey_type = 'household')
sc_freq <- calculate_freq(sc, type='pie chart', survey_type='school')
sc_freq <- make_plots(sc_freq, 'pie')

# community is failing because there are a few columns such as c_c_y and c_p_a2
# should these still be here or can we standardize those away?
# cc_freq <- calculate_freq(cc, type = 'ppl plot')


# calculate the concentration data (which does ecData too)
conc_data <- create_concData(col, lab, config=config)
test <- make_plots(conc_data, 'hist')

# lazy defaulting to standards here
# is failing on bayesian calculations.  why?
sc_freq <- calculate_freq(sc, type='ppl plot', survey_type='school')
data_for_ppl_plots <- calculate_pplPlotData(sc_freq, conc_data,parallel = T)
ppl <- make_plots(data_for_ppl_plots, 'ppl')



# seems to be failing after the setup in bayesian_behavior_estimates()
undebug(bayesian_behavior_estimates)
# is it because of how the data are setup before being passed to the jags model?
behavior <- bayesian_behavior_estimates(sc_freq) # lazy load the other defaults for now
