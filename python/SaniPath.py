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

RSetup:
-> init R environment if first time, ensure proper
   packages installed listed in r-requirements.txt

Important:
The R functions interface with JAGS (Just Another Gibbs Sampler)
to run the Bayesian calculations for people plots.  The env will
need to have that installed.  JAGS also only works on Linux to make
things more complicated.

Use the env.txt file to setup Conda appropriately with all of the necessary
packages.
'''
from rpy2.robjects import pandas2ri, r as rcon, vectors
import pandas as pd
import numpy as np
from functools import partial

# make sure we're translating things back and forth correctly
class Analysis():
	def __init__(self,
				 r_dir='./',
				 plot_dir = './plots/',
				 analysis_type = 'combined',
				 # used for analysis ----
				 pathway_codes = {},
				 pathway_labels = {},
				 neighborhood_mapping = {},
				 # report specific arguments ----
				 city_name = 'Atlanta, GA',
				 lab_name = 'Bill Nye, Inc',
				 start_date = '2017-01-01',
				 lab_MF = False,
				 language = "English",
				 freq_thresh= 50
				 ):
		'''
		____________________________________________
		r_dir => the root location of the R files (where main.R sits)
		plot_dir => where to save plots created (defaults to r_dir/plots/)
		pathway_codes => a dict of alpha codes to numeric codes used in the sample form ({'d' : 1})
		pathway_labels => a dict of alpha codes to str labels ({'d' : 'Drain Water'})
		neighborhood_mapping => a dict of neighborhood numeric codes to label value
			({'Jamaica Plain' : 1, 'Brighton' : 2, "Los Robles" : 3})
		city_name => the city where deployment happens, should come from user account (need to add)
		lab_name => the lab name where samples are processed. also comes from user account information
		start_date => the date that a user moved from Training -> Deployment
		lab_MF => if Membrane Filtration is the selected lab analysis method
		language => the selected language from user acount info
		freq_thres => the number of responses necessary to determine significant
			exposure (does not change much)
		____________________________________________
		instantiates the Analysis class with connections to R for analysis
		if just analyzing data and not generating the report, all arguments
		after neighborhood_mapping can be ignored.
		'''
		pandas2ri.activate()
		# import the proper functions so we can do stuff
		rcon("setwd('"+r_dir+"')")
		rcon("source('main.R')")
		self.plot_dir = plot_dir
		self.config = rcon('config')
		self.analysis_type = analysis_type
		# data objects
		self.household_data = pd.DataFrame()
		self.school_data = pd.DataFrame()
		self.community_data = pd.DataFrame()
		self.sample_data = pd.DataFrame()
		self.lab_data = pd.DataFrame()
		self.frequencies = vectors.ListVector({})
		self.concentrations = vectors.ListVector({})
		self.exposures = vectors.ListVector({})

		# analysis objects
		self.pathway_codes = self._convert_params(pathway_codes)
		self.pathway_labels = self._convert_params(pathway_labels)
		self.neighborhood_mapping = self._convert_params(neighborhood_mapping)

		# report objects
		self.city_name = city_name
		self.lab_name = lab_name
		self.start_date = start_date
		self.lab_MF = lab_MF
		self.language = language
		self.freq_thresh = freq_thresh

		# some behind the scenes R stuff
		self._setup()

	def add_data(self, name, df):
		'''
		method to add collected data
		____________________________________________
		name => name of data being added
		df => dataframe of data to add
		____________________________________________
		'''
		acceptable_options = ['household',
							  'school',
							  'community',
							  'sample',
							  'lab']
		try:
			assert(name in acceptable_options)
			setattr(self, "{}_data".format(name), df)

		except:
			raise ValueError("That name isn't allowed.  Acceptable options: {}".format(', '.join(acceptable_options)))

	def set_analysis(analysis_type):
		self._check_analysis_type(analysis_type)
		self.analysis_type = analysis_type

	def has_data(self, data_name):
		if data_name == 'combined':
			data_name = ['household', 'school', 'community']
			x = [getattr(self, "{}_data".format(i)).empty for i in data_name]
			assert(any(x), 'No data seem to exist!')

		else:
			assert(getattr(self, "{}_data".format(data_name)).empty, 'Not enough data! Have you added everything?')

	def _check_analysis_type(analysis_type):
		acceptable_options = ['household', 'school', 'community', 'combined']
		assert(analysis_type in acceptable_options,
				'Unacceptable analysis type. Options: {}', ', '.join(acceptable_options)
		)


	# compute_frequencies -------------------------------------
	def compute_frequencies(self, gen_plots=True):
		'''
		calculate the data necessary for pie charts and make plots
		if necessary
		____________________________________________
		gen_plots => whether to make the plots
		____________________________________________
		returns nothing
		'''
		try:
			self.has_data(self.analysis_type)
		finally:
			self.frequencies = self._compute_frequencies(household_data = self.household_data,
											schoold_data = self.school_data,
											community_data = self.community_data,
											analysis_type= self.analysis_type,
											type='pie'
											)

			if gen_plots:
				self.frequencies = self._make_plots(self.frequencies, 'pie')

	def get_frequencies(self):
		'''
		get data from frequency calculations to store
		____________________________________________
		returns a json with the calcuated values
		'''
		return self._to_json(self.frequencies)

	def set_frequencies(self, frequency_json):
		'''
		add data back from json format
		____________________________________________
		frequency_json => json formatted data to import
			can be a path to a file or a literal json string
		____________________________________________
		'''
		self.frequencies = self._from_json(frequency_json)

	def compute_concentrations(self, gen_plots = True):
		'''
		calculate the data necessary for histograms and make plots
		if necessary
		____________________________________________
		gen_plots => whether to make the plots
		____________________________________________
		returns nothing
		'''
		try:
			self.has_data('sample')
			self.has_data('lab')
		finally:
			self.concentrations = self._compute_concentrations(self.sample_data,
															   self.lab_data
															   )
			if gen_plots:
				self.concentrations = self._make_plots(self.concentrations, 'hist')

	def get_concentrations(self):
		'''
		get data from concentration calculations to store
		____________________________________________
		returns a json with the calcuated values
		'''
		return self._to_json(self.concentrations)

	def set_concentrations(self, concentration_json):
		'''
		add data back from json format
		____________________________________________
		concentration_json => json formatted data to import
			can be a path to a file or a literal json string
		____________________________________________
		'''
		self.concentrations = self._from_json(concentration_json)


	def compute_exposures(self, gen_plots = True, parallel = True):
		'''
		calculate the data necessary for histograms and make plots
		if necessary
		____________________________________________
		gen_plots => whether to make the plots
		parallel => whether to run the analysis in parallel.
			if true, R will use as many cores as are available on the machine
		____________________________________________
		returns nothing
		'''
		try:
			self.has_data(self.analysis_type)
			self.has_data('sample')
			self.has_data('lab')
		finally:
			freq = self._compute_frequencies(household=self.household_data,
											 school=self.school_data,
											 community=self.community_data,
											 type='ppl',
											 analysis_type=self.analysis_type)

			conc = self._compute_concentrations(self.sample_data,
											   self.lab_data)

			self.exposures = self._compute_exposures(freq, conc, parallel = parallel)

			if gen_plots:
				self.exposures = self._make_plots(self.exposures, 'ppl')

	def get_exposures(self):
		'''
		get data from exposure calculations to store
		____________________________________________
		returns a json with the calcuated values
		'''
		return self._to_json(self.exposures)

	def set_exposures(self, exposure_json):
		'''
		add data back from json format
		____________________________________________
		exposure_json => json formatted data to import
			can be a path to a file or a literal json string
		____________________________________________
		'''
		self.exposures = self._from_json(exposure_json)

	# compute_report --------------------------------------------
	def compute_report(out_dir = './', output_format = 'word_document'):
		'''
		Knit the RMarkdown report.  This needs a lot of information, which we
		can update. These should all be contained in the analysis object
		at this point.
		____________________________________________
		out_dir => directory where the file will be saved
		output_format => rendered filetype either "word_document" or "pdf_document"
		____________________________________________
		saves a file output in out_dir
		'''
		_compute_report = rcon('compute_report')
		params = {'city_name' : self.city_name,
				  'lab_name' : self.lab_name,
				  'start_date' : self.start_date,
				  'lab_MF' : self.lab_MF,
				  'language' : self.language,
				  'household_data' : self.household_data,
				  'school_data' : self.school_data,
				  'community_data' : self.community_data,
				  'sample_data' : self.sample_data,
				  'lab_data' : self.lab_data,
				  #'freq' : self.frequencies, # TODO: Add to R
				  #'conc' : self.concentrations, # TODO: Add to R
				  'ps_freq' : self.exposures,
				  'neighborhood_mapping' : self.neighborhood_mapping,
				  'pathway_codes' : self.pathway_codes,
				  'pathway_labels' : self.pathway_labels,
				  'freq_thresh' : self.freq_thresh
				  }
		try:
			_compute_report(params = vectors.ListVector(params),
							out_dir = out_dir)
		except:
			raise ValueError('This error message could be more helpful')

	def _setup(self):
		# we're essentially creating class methods here.
		# we need to calculate frequecies of answers to then use either for
		# plotting data in the application or for further analysis
		# pie chart uses raw answers, ppl plot takes all answers and centers
		# them around 0.  I never learned why.  For some reason we need it
		# that way. Using slightly more intuitive names for the functions here

		# this is the easiest way to translate data back and forth
		self._to_json = rcon('toJSON')
		self._from_json = rcon('fromJSON')

		self._compute_frequencies = self._add_arguments(rcon('compute_frequencies'))
		# if not using this dynamically, convert pandas df to R df using
		# pandas2ri.py2ri(df) before calling the function!
		'''
		____________________________________________
		household => df of household data gathered
		community => df of community data gathered
		school => df of school data gathered
		type => 'pie chart' or 'ppl plot'. defaults to 'pie chart'
		analysis_type => optional. can be 'household', 'community', 'school'
		---- these are added by _add_arguments() -----
		config => the config list with options. defaults to those defined in config.R
		pathway_code => a mapping of pathway code to numeric value in sample
		pathway_label => a mapping of pathway code to pathway label
		neighborhood_mapping => a mapping of neighborhood name to numeric code
		____________________________________________
		returns an R list of lists of answer frequency data.
		passed to calculate_exposure

		computes the frequencies of answers to pathway questions.

		calculate_freq can work with just one data frame if all other args are
		explicitly declared. ie calculate_freq(houeshold, type='pie chart', survey_type='household')
		if survey type is missing, the function tries to infer which data are
		being processed by looking at the column names since they are coded by survey type.
		if survey_type is declared, it will do the desired calculations.  either pass
		one or all data frames at once.  it will get angry if you only give it two.
		if all three are provdied and survey_type == 'combined' it will create a combined score.
		'''


		# compute_concentrations -----------------------------------
		self._compute_concentrations = self._add_arguments(rcon('compute_concentrations'))
		'''
		____________________________________________
		collection_data => df of collection (sample) data
		lab_data => df of lab samples processed
		---- these are added by _add_arguments() -----
		config => the config list with options. defaults to those defined in config.R
		pathway_code => a mapping of pathway code to numeric value in sample
		pathway_label => a mapping of pathway code to pathway label
		neighborhood_mapping => a mapping of neighborhood name to numeric code
		____________________________________________
		returns an R list of concentration values by neighborhood and pathway
		'''

		# compute_exposure -------------------------------------------
		self._compute_exposures = self._add_arguments(rcon('compute_exposure'))
		'''
		____________________________________________
		freq => frequency values calculated using calculate_frequencies(type='ppl plot')
		conc => concentration scores from calculate_concentrations
		------ JAGS arguments -------
		nburn => param for JAGS model. defaults to 1000
		niter => num of iterations for model. defaults to 10,000
		thin => defaults to 1
		cutpoint => defaults to [0, 5, 10]
		---- these are added by _add_arguments() -----
		config => the config list with options. defaults to those defined in config.R
		pathway_code => a mapping of pathway code to numeric value in sample
		pathway_label => a mapping of pathway code to pathway label
		neighborhood_mapping => a mapping of neighborhood name to numeric code
		____________________________________________
		returns a list of dicts used for people plot creation
		'''

		# make_plots ----------------------------
		self._make_plots = partial(rcon('make_plots'), output_dir = self.plot_dir)
		'''
		____________________________________________
		obj => r object from one of the compute functions
		type => type of plot to make. "pie", "hist", or "ppl" are valid options
		____________________________________________
		returns an R list with updated information
		'''

	def _add_arguments(self, x):
		# creates a partial R function with some arguments
		# already complete
		return partial(x, config= self.config,
						  pathway_codes = self._convert_params(self.pathway_codes),
						  pathway_labels = self._convert_params(self.pathway_labels),
						  neighborhood_mapping = self._convert_params(self.neighborhood_mapping))

	def _convert_params(self, param_dict):
		if param_dict != None:
			try:
				return vectors.ListVector(param_dict)
			except:
				raise ValueError("Failed to convert. Is it a dict?")



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
