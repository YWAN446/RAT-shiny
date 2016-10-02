tab_formhubSettings <- 
  tabPanel(
  "Formhub Settings",
  h3('Forms:'),
  helpText("Manually override the forms by typing the form name in the corresponding location.  Then press 'Update Forms' to refresh the available data."),
  selectizeInput(
    'col_file',
    'Collection Data',
    multiple = F,
    choices = 'sp_sample_collection_form_1_c',
    selected = 'sp_sample_collection_form_1_c',
    options= list(create=T)
  ),
  selectizeInput(
    'lab_file',
    'Lab Data',
    multiple = F,
    choices = 'sp_sample_lab_form_1_i',
    selected = 'sp_sample_lab_form_1_i',
    options= list(create=T)
  ),
  selectizeInput(
    'hh_file',
    'Household Data',
    multiple = F,
    choices = 'sp_household_form_2_01b',
    selected = 'sp_household_form_2_01b',
    options= list(create=T)
  ),
  selectizeInput(
    'sch_file',
    'School Data',
    multiple = F,
    choices = 'school_d',
    selected = 'school_d',
    options= list(create=T)
  ),
  selectizeInput(
    'com_file',
    'Community Data',
    multiple = F,
    choices = 'community_d',
    selected = 'community_d',
    options= list(create=T)
  ),
  actionButton("update_forms", "Update Forms", icon=icon('refresh'))
)
