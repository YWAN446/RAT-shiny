import pandas as pd
import SaniPath as sp
import pdb; pdb.set_trace()
# test the analysis settings
analysis = sp.Analysis()
import pdb; pdb.set_trace()
hh = pd.read_csv('rsrc/HOUSEHOLD_EXAMPLE.csv')
cc = pd.read_csv('rsrc/COMMUNITY_EXAMPLE.csv')
sc = pd.read_csv('rsrc/SCHOOL_EXAMPLE.csv')

hh_freq = analysis.calculate_frequencies(hh)
cc_freq = analysis.calculate_frequencies(cc)
sc_freq = analysis.calculate_frequencies(sc)
