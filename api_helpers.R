library(httr)
library(jsonlite)

## API Helpers -------------------------------------------------------------
# These functions are designed to help the Shiny Server interact with 
# its FormHub counterpart across the web.  This is currently over an 
# unecrypted port, and something for which we should look into setting better 
# security
# FormHub API documentation can be accessed at:
# https://formhub.org/api/v1/
# and at:
# https://github.com/SEL-Columbia/formhub/wiki/Formhub-Access-Points-(API)


# TOKEN --------------------------------------------------------------------
# Available by logging into FormHub, selecting the user account in the top right
# and selecting "API Key"

# This is specific to the SP user at the moment.  Ideally, we modify this so
# each user has a stored key and the system can tell who is logged in and 
# changes the key accordingly. 


# FORMHUB INTERACTIONS -----------------------------------------------------
formhubGET <- function(api_url, api_token) {
  # This will be used a lot to download data from various access points 
  # from the form hub server. It wraps the end point and api token
  # into a httr curl request.  This won't do any processing. It just
  # downloads. The response is a json.
  #
  # Ex.
  # > formhubGET('http://formhub.cgsw.org/api/v1/data', token)
  # API request response and content
  # ...
  
  response <- GET(api_url, accept_json(), add_headers(Authorization = paste0('Token ',api_token)))
  if (response$status_code != 200) {
    # 200 indicates successful communication and authentication with the server
    stop(paste("Something went wrong.  Try checking the form names and credentials.\n
         api_url= ",api_url,"\n"
         )) 
  }
  
  return(response)
}



formhubGET_csv <- function(base_url, usr, pwd, form_name) {
  # This is a work around for the moment because of data type conversion issues
  # when downloading a JSON from the api access point. 
  # function returns the equivalent of downloading a csv from the UI.
  
  response <- GET(paste0(base_url,'/',usr, '/forms/',form_name, '/data.csv'),
                  authenticate(usr, pwd)
                  )
  if (response$status_code == 404) { # this is when the form doesn't have any data
    # 200 indicates successful communication and authentication with the server
    warning(paste(form_name, "does not have any data uploaded yet!"))
  }
  else if (response$status_code != 200) { # check to see if we hit some other error
    stop(content(response)) 
  }
  else { # response will be a successful 200
    return(content(response, as='parsed', type='text/csv'))
    
  }
}

formhubGET_formCount <- function(base_url, usr, pwd, form_name) {
  # simple function to poll the api and see if any data exists for
  # the form
  
  response <- GET(paste0(base_url,'/',usr, '/forms/',form_name,'/api'),
                  authenticate(usr, pwd), query=list(count = 1)
  )
  return(unlist(content(response)))
}

getAPI_forms <- function(base_url, api_url, usr, pwd, api_token) {
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
  
  forms <- cbind.data.frame('form_name'=names(content(response)), 'api_link'=unlist(content(response)))
  for (f in 1:nrow(forms)) {
    forms[f, 'submission_count'] <- formhubGET_formCount(base_url, usr, pwd, form_name = forms$form_name[f])
  }
  forms$menu_option <- paste(forms$form_name, paste0("(",forms$submission_count,")"))
  
  return(forms)
}

filterAPI_forms <- function(filter, forms_list) {
  # This is a wrapper function around a grep command.
  # We'll filter based on the list element names from the form 
  # information downloaded using getAPI_forms(). This function
  # uses the same style as grep in stating the search criteria
  # first and then the object in which to look. The function returns
  # a list with two parts= menu_items and forms.  The options
  # are sorted with the form with the highest submission count
  # first. 
  #
  # Ex.
  # > filterAPI_forms('community', downloaded_forms)
  # $menu_items
  # sp_community_form_1_x (0)
  # "sp_community_form_1_x"
  # $forms
  #                       form_name              api_link                                     submission_count                 menu_option
  # sp_community_form_1_x sp_community_form_1_x 'http://formhub.cgsw.org/api/v1/data/sp/93'                  0   sp_community_form_1_x (0)
  # ...
  
  forms_list <- forms_list[grep(filter, forms_list$form_name),]
  forms_list <- forms_list[order(forms_list$submission_count, decreasing=T),]
  
  filtered_forms <- as.character(forms_list$form_name)
  names(filtered_forms) <- forms_list$menu_option
  
  
  return(list('menu_items' = filtered_forms, 'forms' = forms_list))
}

## DEPRECATED ---------------------------------------------------------------
# this isn't working properly right now
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
  
  # these columns are coming in as nested lists and seem to be causing
  # problems at the moment.  we'll drop them unless they're absolutely necessary
  results <- results[,-c(grep('_tags', names(results)), grep('_attachments', names(results)),grep('_geolocation', names(results)))]
  
  return(results)
}