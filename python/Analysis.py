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
   This could be a separate class?

Important:
The R functions interface with JAGS (Just Another Gibbs Sampler)
to run the Bayesian calculations for people plots.  The env will
need to have that installed.  JAGS also only works on Linux to make
things more complicated.

Using conda:
conda install -c trent jags

'''
import rpy2.robjects as rcon
import pandas as pd
import numpy as np

# make sure we're translating things back and forth correctly
rcon.pandas2ri.activate()

class Analysis():
    pass

class RSetup():
    '''
    Make sure proper packages are installed to r-base.
    If missing, install them. Otherwise, do nothing.
    This could be a subclass maybe?
    '''
    def __init__(self, r_requirements_file):
        # get a numpy 1d array of the installed packages
        installed_packages = rcon.r('rownames(installed.packages())')
        # get the libraries we need
        requirements = self._read(r_requirements_file)
        pkgs_to_install = np.asarray([r for r in requirements if r not in installed_packages])
        # pull the installer function
        if len(pkgs_to_install) > 0:
            installR = rcon.r('install.packages')
            print("Packages to install:", ", ".join(pkgs_to_install))
            # now install things from the cloud mirror
            installR(pkgs_to_install, repos='https://cloud.r-project.org/')
        else:
            print('Everything is installed!')
    def _filter_pkgs(self, installed, requirements):


    def _read(self, r_requirements_file):
        f = open(r_requirements_file)
        reqs = f.readlines()
        f.close()
        # remove commented lines and line breaks
        return [r.replace('\n', '') for r in filter(lambda x: '#' not in x, reqs)]
