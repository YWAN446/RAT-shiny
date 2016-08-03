tab_rawData <- tabPanel("Raw Data", 
                        selectInput('raw_view', 'Select table to view:', 
                                    c('Household', 'Community', 'School', 'E. Coli'), selected='Household'),
                        dataTableOutput('raw_table'))