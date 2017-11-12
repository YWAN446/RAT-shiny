hh <- read.csv('rsrc/HOUSEHOLD_EXAMPLE.csv', stringsAsFactors = F)
cc <- read.csv('rsrc/COMMUNITY_EXAMPLE.csv', stringsAsFactors = F)
sc <- read.csv('rsrc/SCHOOL_EXAMPLE.csv', stringsAsFactors = F)

col <- read.csv('rsrc/SAMPLE_EXAMPLE.csv', stringsAsFactors = F)
lab <- read.csv('rsrc/LAB_EXAMPLE.csv', stringsAsFactors = F)

sapply(list.files('model', full.names = T), source)
source('config.R')

# Frequency calculations
hh_freq <- calculate_freq(hh)

# community is failing because there are a few columns such as c_c_y and c_p_a2
# should these still be here or can we standardize those away?
cc_freq <- calculate_freq(cc, type = 'ppl plot')

conc_data <- create_concData(col, lab, mpn_tbl)


