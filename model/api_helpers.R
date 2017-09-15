library(httr)
library(dplyr)
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

# KoboToolBox Interactions

kt_getForms <- function(base_url, usr, pwd) {
  # Get the list of forms available for download for a specific user
  # somewhat misleadingly uses the data endpoint to get that list of options
  response <- tryCatch(
    resp <- GET(sprintf("%sapi/v1/data.csv", base_url), authenticate(usr, pwd)),
    error = function(e) {print(e); return(resp)},
    finally = function() return(resp)
  )
  return(content(response))
}

kt_getData <- function(base_url, usr, pwd,  form_id) {
  # get data for a specific form after having determined the appropriate reference url
  response <- tryCatch(
    resp <- GET(sprintf("%sapi/v1/data/%s.csv", base_url, form_id), authenticate(usr, pwd)),
    error = function(e) {print(e); return(resp)},
    finally = function() return(resp)
  )
  r <- content(response, type = 'application/csv')
  # server sends it back in raw format for some reason
  # make a temp file, save the file and then reload it
  tmp <- tempfile()
  write(rawToChar(r), tmp)
  r <- read.csv(tmp)
  file.remove(tmp)
  return(r)
}

kt_checkUser <- function(base_url, usr, pwd) {
  # check that a user exists by hitting the base api end point
  exists <- tryCatch(
    resp <- GET(sprintf('%sapi/v1/', base_url), authenticate(usr, pwd)),
    error = function(e) {print(e); return(resp)},
    finally = function() return(resp)
  )
  if (resp$status_code == 200) return(T)
  else return(F)
}


filterAPI_forms <- function(filter, forms_list) {

  forms_list <- forms_list[grep(filter, forms_list$title, ignore.case = T),]
  forms_list <- forms_list[order(forms_list$id, decreasing=F),]
  
  filtered_forms <- as.character(forms_list$id)
  names(filtered_forms) <- forms_list$title
  
  
  return(filtered_forms)
}


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
  
  # error handling request 
  response <- tryCatch(
    # first try to get the data
    req <- GET(api_url, accept_json(), add_headers(Authorization = paste0('Token ',api_token))),
    # If something goes wrong, return a null value
    error= function(e) {print(e); return(NULL)}, 
    # if all goes well, return the response we got
    finally =  function() return(req)
    )
  
  # if we got a response, it still may not be what we want. 200 means a successful response.
  if (response$status_code != 200 & !is.null(response)) {
    # 200 indicates successful communication and authentication with the server
    stop(paste("Something went wrong.  Try checking the form names and credentials.\n
         api_url= ",api_url,"\n"
         )) 
  }
  
  return(response)
}

formhubCheck_user <- function(base_url, usr, pwd, test_form) {
  # Check to see if a user exists, if they do, a profile will be 
  # displayed at the url/+ username location, even if they don't 
  # have any shared forms. This probably isn't the most secure thing
  # in the world...
  # test_form specifies what form to use to test if the user
  # actually has some data to use. 
  
  req <- GET(paste0(base_url, usr, "/forms/",test_form,"/data.csv"), accept_json(),
             authenticate(usr, pwd, 'basic')
            )
  
  if (req$status_code == 200) {
    return(T)
  }
  else {
    return(F)
  }
  
  
  
}

formhubGET_csv <- function(base_url, usr, pwd, form_name) {
  # This is a work around for the moment because of data type conversion issues
  # when downloading a JSON from the api access point. 
  # function returns the equivalent of downloading a csv from the UI.
  
  response <- GET(paste0(base_url,usr, '/forms/',form_name, '/data.csv'),
                  authenticate(usr, pwd)
                  )
 if (response$status_code != 200) { # check to see if we hit some other error
   print("Either the form name is wrong or there is no data available.")
    return(data.frame())
    
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

# #getAPI_forms <- function(base_url, api_url, usr, pwd, api_token) {
#   # This accesses the /api/v1/data access point and downloads
#   # the available forms that the user has created and the appropriate
#   # web links for downloading data.  We'll output a list with 
#   # with named elements matching the form names in formhub.
#   # If 'find' is specified, the function will return a filtered
#   # list of options that match the criteria specified. 
#   #
#   # Ex. 
#   # > getAPI_forms('http://formhub.cgsw.org/', api_token)
#   # $community_a
#   # 'http://formhub.cgsw.org/api/v1/data/sp/92'
#   
#   cat('Get forms available.\n')
#   response <- formhubGET(api_url, api_token)
#   
#   forms <- cbind.data.frame('form_name'=names(content(response)), 'api_link'=unlist(content(response)))
#   
#   cat('Download form submission counts\n')
#   for (f in 1:nrow(forms)) {
#     cat(forms[f,'form_name'],'\n')
#     forms[f, 'submission_count'] <- formhubGET_formCount(base_url, usr, pwd, form_name = forms$form_name[f])
#   }
#   forms$menu_option <- paste(forms$form_name, paste0("(",forms$submission_count,")"))
#   
#   return(forms)
# }
# #
# filterAPI_forms <- function(filter, forms_list) {
#   # This is a wrapper function around a grep command.
#   # We'll filter based on the list element names from the form 
#   # information downloaded using getAPI_forms(). This function
#   # uses the same style as grep in stating the search criteria
#   # first and then the object in which to look. The function returns
#   # a list with two parts= menu_items and forms.  The options
#   # are sorted with the form with the highest submission count
#   # first. 
#   #
#   # Ex.
#   # > filterAPI_forms('community', downloaded_forms)
#   # $menu_items
#   # sp_community_form_1_x (0)
#   # "sp_community_form_1_x"
#   # $forms
#   #                       form_name              api_link                                     submission_count                 menu_option
#   # sp_community_form_1_x sp_community_form_1_x 'http://formhub.cgsw.org/api/v1/data/sp/93'                  0   sp_community_form_1_x (0)
#   # ...
#   
#   forms_list <- forms_list[grep(filter, forms_list$form_name),]
#   forms_list <- forms_list[order(forms_list$submission_count, decreasing=T),]
#   
#   filtered_forms <- as.character(forms_list$form_name)
#   names(filtered_forms) <- forms_list$menu_option
#   
#   
#   return(list('menu_items' = filtered_forms, 'forms' = forms_list))
# }
