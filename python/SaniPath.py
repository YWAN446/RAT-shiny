'''
Python wrapper class around analysis.R
for the SaniPath Rapid Assessment tool.

Analysis:
-> create the necessary frequency calculations for
   pie charts and histograms.  Return formatted data
   ready for charting.
-> run bayesian calculations on frequency data for
   people plot displays.  Return formatted data
   ready for charting
Plotting:
-> create pie charts with the data
-> create histograms
-> create people plots
RSetup:
-> init R environment if first time, ensure proper
   packages installed listed in r-requirements.txt

Important:
The R functions interface with JAGS (Just Another Gibbs Sampler)
to run the Bayesian calculations for people plots.  The env will
need to have that installed.  JAGS also only works on Linux to make
things more complicated.

Ubuntu dependencies:
libcurl4-openssl-dev

Using conda to make sure necessary programs are installed:
# we need jags 4.x
conda install -c conda-forge jags

# latest version of r. this will install most of the
# libraries we need, but use the RSetup() class to make sure
# everything is installed.
conda install r-essentials

# the connection between r and python
conda install krb5
conda install libssh2
conda install -c r rpy2
'''
from rpy2.robjects import pandas2ri, r as rcon
import pandas as pd
import numpy as np

# make sure we're translating things back and forth correctly
class Analysis():
	def __init__(self):
		pandas2ri.activate()
		# import the proper functions so we can do stuff
		rcon("source('model/analysis_helpers.R')")
        rcon("source('model/ec_helpers.R')")

		# TODO analysis_helpers.R currently tries to load an
		# r data file from rsrc.  Assuming the current working directory
		# is set to ./python this should be ok, but it's not the best.

		# we're essentially creating class methods here.
		# we need to calculate frequecies of answers to then use either for
		# plotting data in the application or for further analysis
		# pie chart uses raw answers, ppl plot takes all answers and centers
		# them around 0.  I never learned why.  For some reason we need it
		# that way. Using slightly more intuitive names for the functions here
		self.calculate_frequencies = rcon('calculate_freq')
		# if not using this dynamically, convert pandas df to R df using
		# pandas2ri.py2ri(df) before calling the function!
		'''
		____________________________________________
		household => df of household data gathered
		community => df of community data gathered
		school => df of school data gathered
		type => 'pie chart' or 'ppl plot'. defaults to 'pie chart'
		survey_type => optional. can be 'household', 'community', 'school'
		____________________________________________
		returns an R list of lists of answer frequency data.
		passed to calculate_exposure

		computes the frequencies of answers to pathway questions. .

		calculate_freq can work with just one data frame if all other args are
		explicitly declared. ie calculate_freq(houeshold, type='pie chart', survey_type='household')
		if survey type is missing, the function tries to infer which data are
		being processed by looking at the column names since they are coded by survey type.
		if survey_type is declared, it will do the desired calculations.  either pass
		one or all data frames at once.  it will get angry if you only give it two.
		if all three are provdied and survey_type == 'combined' it will create a combined score.
		'''

		self.calculate_concentrations = rcon('create_concData')
		# if not using this dynamically, convert pandas df to R df using
		# pandas2ri.py2ri(df) before calling the function!
		'''
		____________________________________________
		collection_data => df of collection (sample) data
		lab_data => df of lab samples processed
		mpn_loc => path to mpn_tbl.rda in rsrc folder
		____________________________________________
		returns an R list of concentration values by neighborhood and pathway
		'''


		self.calc_exposure = rcon('calculate_pplPlotData')
		# if not using this dynamically, convert pandas df to R df using
		# pandas2ri.py2ri(df) before calling the function!
		'''
		____________________________________________
		freq => frequency values calculated using calculate_frequencies(type='ppl plot')
		conc => concentration scores from calculate_concentrations
		------ JAGS arguments -------
		nburn => param for JAGS model. defaults to 1000
		niter => num of iterations for model. defaults to 10,000
		thin => defaults to 1
		cutpoint => defaults to [0, 5, 10]
		____________________________________________
		returns a list of dicts used for people plot creation
		'''

class Plotting():
	# wrappers around plotting functions for pie charts, histograms and people plots
	def __init__(self):
		# this is technically in the same environment for R, which is normal.
		# but should we make it fully separate?
		rcon('source("model/plotting_helpers.R")')
		# self.create_pie_charts = rcon('')
		pass

class RSetup():
	'''
	Make sure proper packages are installed to r-base.
	If missing, install them. Otherwise, do nothing.
	'''
	def __init__(self, r_requirements_file):
		pandas2ri.activate()
		rcon('options(download.file.method="wget")')
		# get a numpy 1d array of the installed packages
		installed_packages = self._check_pkgs()
		# get the libraries we need
		requirements = self._read(r_requirements_file)
		pkgs_to_install = self._filter_pkgs(installed_packages, requirements)
		# pull the installer function
		if  all([x != '' for x in pkgs_to_install]):
			# we have some things to install, pull the install function
			installR = rcon('install.packages')
			# some window dressing so we know it's working
			print("Packages to install:" + ", ".join(pkgs_to_install))
			# now install things from the cloud mirror
			installR(pkgs_to_install, repos='https://cloud.r-project.org/', dependencies=True)
			try:
				pkgs = self._filter_pkgs(self._check_pkgs(), requirements)
				assert(len(pkgs) == 0)
			except:
				raise ValueError('Not all packages successfully installed.')

		else:
			print('Everything is installed!')

	def _check_pkgs(self):
		return rcon('rownames(installed.packages())')

	def _filter_pkgs(self, installed, requirements):
		# just return packages that don't already show up in the r
		# library of packages so we can install them.
		return np.asarray([r for r in requirements if r not in installed])

	def _read(self, r_requirements_file):
		# TODO need to check how line breaks are stripped here
		f = open(r_requirements_file)
		reqs = f.readlines()
		f.close()
		# remove commented lines and line breaks
		return [r.replace('\n','').replace('\r','') for r in filter(lambda x: '#' not in x, reqs)]
