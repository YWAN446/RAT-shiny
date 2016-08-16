tab_formhubSettings <- 
  tabPanel(
  "Formhub Settings",
  h3('Forms:'),
  selectizeInput(
    'col_file',
    'Collection Data',
    multiple = F,
    choices = 'sp_sample_collection_form_1_c',
    selected = 'sp_sample_collection_form_1_c'
  ),
  selectizeInput(
    'lab_file',
    'Lab Data',
    multiple = F,
    choices = 'sp_sample_lab_form_1_i',
    selected = 'sp_sample_lab_form_1_i'
  ),
  selectizeInput(
    'hh_file',
    'Household Data',
    multiple = F,
    choices = 'sp_household_form_2_01b',
    selected = 'sp_household_form_2_01b'
  ),
  selectizeInput(
    'sch_file',
    'School Data',
    multiple = F,
    choices = 'school_d',
    selected = 'school_d'
  ),
  selectizeInput(
    'com_file',
    'Community Data',
    multiple = F,
    choices = 'community_d',
    selected = 'community_d'
  )
)
