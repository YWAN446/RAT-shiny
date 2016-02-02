library(httr)
library(RJSONIO)

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

# This is specific to the SP user at the moment.  Ideally, we modify this so
# each user has a stored key and the system can tell who is logged in and 
# changes the key accordingly. 
api_token <- '8d0336d37ef28df590574f1cd4531f142e31ca02' 


# FORMHUB INTERACTIONS -----------------------------------------------------
formhubGET <- function(api_url, api_token) {
  # This will be used a lot to download data from various access points 
  # from the form hub server. It wraps the end point and api token
  # into a httr curl request.  This won't do any processing. It just
  # downloads. 
  #
  # Ex.
  # > formhubGET('http://formhub.cgsw.org/api/v1/data', token)
  # API request response and content
  # ...
  
  response <- GET(api_url, add_headers(Authorization = paste0('Token ',api_token)))
  if (response$status_code != 200) {
    # 200 indicates successful communication and authentication with the server
    stop("API Authentication failed.  Check credentials.") 
  }
  
  return(response)
}


getAPI_forms <- function(api_url, api_token) {
  # This accesses the /api/v1/data access point and downloads
  # the available forms that the user has created and the appropriate
  # web links for downloading data.  We'll output a list with 
  # with named elements matching the form names in formhub.
  # If 'find' is specified, the function will return a filtered
  # list of options that match the criteria specified. 
  #
  # Ex. 
  # > getAPI_forms('http://formhub.cgsw.org/', api_token)
  # $community_a
  # 'http://formhub.cgsw.org/api/v1/data/sp/92'
  
  response <- formhubGET(api_url, api_token)
  
  return(content(response))
}

filterAPI_forms <- function(filter, forms_list) {
  # This is a wrapper function around a grep command.
  # We'll filter based on the list element names from the form 
  # information downloaded using getAPI_forms(). This function
  # uses the same style as grep in stating the search criteria
  # first and then the object in which to look. 
  #
  # Ex.
  # > filterAPI_forms('community', downloaded_forms)
  # $sp_community_form_1_x
  # 'http://formhub.cgsw.org/api/v1/data/sp/93'
  # $sp_community_form_2_a
  # ...
  return(forms_list[grep(filter, names(forms_list))])
}

getAPI_data <- function(form_url, api_token) {
  # This will take a specific form link that is returned from
  # getAPI_forms() and download that data. It will return a
  # dataframe as though we had imported it as a csv (to make
  # integration with existing code as easy as possible).
  #
  # Ex.
  # > getAPI_data('http://formhub.cgsw.org/api/v1/data/sp/93', api_token)
  # col1 col2 col3 ...
  # a    b    c
  
  # download the data
  response <- formhubGET(form_url, api_token)
  
  # convert the json response to a dataframe
  results <- fromJSON(content(response, 'text'))
  results <- as.data.frame(apply(results, 2, function(a) as.character(a)))
  
  
  return(results)
}