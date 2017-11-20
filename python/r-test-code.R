source('main.R')

hh <- read.csv('rsrc/HOUSEHOLD_EXAMPLE.csv', stringsAsFactors = F)
cc <- read.csv('rsrc/COMMUNITY_EXAMPLE.csv', stringsAsFactors = F)
sc <- read.csv('rsrc/SCHOOL_EXAMPLE.csv', stringsAsFactors = F)

col <- read.csv('rsrc/SAMPLE_EXAMPLE.csv', stringsAsFactors = F)
lab <- read.csv('rsrc/LAB_EXAMPLE.csv', stringsAsFactors = F)



# Frequency calculations
hh_freq <- compute_frequencies(hh,survey_type = 'household')
sc_freq <- compute_frequencies(sc, type='pie', analysis_type='school')
sc_freq <- make_plots(sc_freq, 'pie')

# community is failing because there are a few columns such as c_c_y and c_p_a2
# should these still be here or can we standardize those away?
# cc_freq <- calculate_freq(cc, type = 'ppl plot')


# calculate the concentration data (which does ecData too)
conc_data <- compute_concentrations(col, lab, config=config, pathway_codes = pathway_codes)
test <- make_plots(conc_data, 'hist')

# lazy defaulting to standards here
# is failing on bayesian calculations.  why?
sc_freq <- compute_frequencies(sc, type='ppl', analysis_type='school', config=config)
exposed <- compute_exposure(sc_freq, conc_data,parallel = T, config=config)
ppl <- make_plots(exposed, 'ppl')


# Defining new sets of pathway codes and labels will override what is in config
# and the functions will only search for those declared.  SP tool can create these
# as necessary and pass the appropriate codes and labels as needed. if nothing is passed
# the default to config values. 
pathway_codes = list('d' = 1, 'p' = 2, 'dw' = 3, 'f' = 6, 'l' = 7)
pathway_labels = list('d' = 'Drain Water', 'p' = 'Produce', 'dw' = 'Municipal and Piped Water', 'o' = 'Ocean Water',
                        'f' = 'Flood Water', 'l' = 'Public Latrine')
                        
sc_freq <- compute_frequencies(sc, type='pie', analysis_type='school', pathway_labels = pathway_labels)
sc_freq <- make_plots(sc_freq, 'ppl')

# calculate the concentration data (which does ecData too)
conc_data <- compute_concentrations(col, lab, config=config, pathway_codes = pathway_codes)
test <- make_plots(conc_data, 'hist')

exposed <- compute_exposure(sc_freq, conc_data,parallel = T, config=config)
ppl <- make_plots(exposed, 'ppl')


