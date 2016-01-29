## API Helpers -------------------------------------------------------------
# These functions are designed to help the Shiny Server interact with 
# its FormHub counterpart across the web.  This is currently over an 
# unecrypted port, and something for which we should look into setting better 
# security
# FormHub API documentation can be accessed at:
# https://formhub.org/api/v1/


# TOKEN --------------------------------------------------------------------
# Available by logging into FormHub, selecting the user account in the top right
# and selecting "API Key"

# This is specific to the SP user at the moment
api_token <- '8d0336d37ef28df590574f1cd4531f142e31ca02' 

formhubGET <- function(api_)

getAPI_forms <- function(api_url, api_token) {
  # This accesses the /api/v1/data access point and downloads
  # the available forms that the user has created and the appropriate
  # web links for downloading data.  We'll output a dataframe with 
  # two columns, formName, apiLink.
  # Ex. 
  # > getAPI_forms('http://formhub.cgsw.org/', api_token)
  # formName    apiLink
  # community   /api/v1/data/sp/92
  
  response
  
}