tab_formhubSettings <-
  tabPanel(
  "Formhub Settings",
  h3('Forms:'),
  helpText("Override the forms by typing the form name as it is entered in Formhub in the corresponding location to download.  Alternatively, specify a csv file for any form.
           If a CSV has been uploaded, it will override any form settings to download from Formhub.  Then press 'Update Forms' to refresh the available data."),

    column(6,
      selectizeInput(
        'col_file',
        'Collection Data',
        multiple = F,
        choices = collection_form,
        selected = lab_form,
        options= list(create=T)
      )
    ),
    column(6,
     fileInput('col_csv', 'CSV override:', accept='.csv')
    ),
  column(6,
  selectizeInput(
    'lab_file',
    'Lab Data',
    multiple = F,
    choices = lab_form,
    selected = lab_form,
    options= list(create=T)
  )
  ),
  column(6,
         fileInput('lab_csv', 'CSV override:', accept='.csv')
         ),
  column(6,
  selectizeInput(
    'hh_file',
    'Household Data',
    multiple = F,
    choices = household_form,
    selected = household_form,
    options= list(create=T)
  )
  ),
  column(6,
         fileInput('hh_csv', 'CSV override:', accept='.csv')
  ),
  column(6,
  selectizeInput(
    'sch_file',
    'School Data',
    multiple = F,
    choices = school_form,
    selected = school_form,
    options= list(create=T)
  )
  ),
  column(6,
         fileInput('sch_csv', 'CSV override:', accept='.csv')
  ),
  column(6,
  selectizeInput(
    'com_file',
    'Community Data',
    multiple = F,
    choices = community_form,
    selected = community_form,
    options= list(create=T)
  )
  ),
  column(6,
         fileInput('com_csv', 'CSV override:', accept='.csv')
  )
  # actionButton("update_forms", "Update Forms", icon=icon('refresh'))
)
