import pandas as pd
import SaniPath as sp
from rpy2.robjects import vectors
# test the analysis settings
analysis = sp.Analysis('./')
hh = pd.read_csv('rsrc/HOUSEHOLD_EXAMPLE.csv')
cc = pd.read_csv('rsrc/COMMUNITY_EXAMPLE.csv')
sc = pd.read_csv('rsrc/SCHOOL_EXAMPLE.csv')
col = pd.read_csv('rsrc/SAMPLE_EXAMPLE.csv')
lab = pd.read_csv('rsrc/LAB_EXAMPLE.csv')



# Example with school data collected

# Frequency data are used with pie charts
sc_freq = analysis.compute_frequencies(sc)
sc_freq = analysis.make_plots(sc_freq, 'pie')

# concentration data are used with histograms
conc = analysis.compute_concentrations(col, lab)
conc = analysis.make_plots(conc, 'hist')

# Exposure data uses people plots
# we also need to make the frequencies again but for people plots
sc_freq_ppl = analysis.compute_frequencies(sc, type='ppl', survey_type='school')
# we can use the same concentration data already calculated
ppl_plots = analysis.compute_exposure(sc_freq_ppl, conc)
ppl_plots = analysis.make_plots(ppl_plots)


# the analysis.config object has default values for pathway labels and codes
# but it's better to explicitly pass the values from SP tool as dictionaries converted to R lists/vectors
# IMPORTANT: Note that this will reduce the number of analyses performed since the R functions only look for
# the data included in pathway_codes and pathway_labels

# pass the configured pathways using a python dicctionary and convert to an R list
pathway_labels = vectors.ListVector({'p' : 'Produce', 'dw' : 'Municipal and Drain Water', 'o' : 'Ocean Water',   'l' : 'Public Latrine'})
sc_freq = analysis.compute_frequencies(sc, type='ppl', survey_type='school', pathway_labels= pathway_labels)


# the sample collection form codes each pathway selected as a numeric value, we can pass these also as a named lists
pathway_codes = vectors.ListVector({'p' : 2, 'dw' : 3, 'o' : 4, 'l' : 7})
conc = analysis.compute_concentrations(col, lab, pathway_labels=pathway_labels, pathway_codes=pathway_codes)

# then pass those to compute_exposure
ppl_plots = analysis.compute_exposure(sc_freq, conc)

# compute_exposure automatically will attempt to run in parallel, but you can set parallel = False to run in sequence
# or set nc = X, where X is the number of cores to utlize when running in parallel.  nc does nothing if parallel is false.
ppl_plots = analysis.compute_exposure(sc_freq, conc, nc=2)
