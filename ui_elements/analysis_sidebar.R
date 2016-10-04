
analysis_sidebar <- fluidRow(
    selectInput('surtype','Survey Type', selected="1", choices=c("All"=0,"Household"=1,"School"=2,"Community"=3)),
    actionButton("update_forms", "Refresh Data", icon=icon('refresh')),
    helpText("Press to download new data from Formhub or use uploaded CSVs to generate 
             analysis figures."),
    
    #       selectInput('samtype', 'Sample Type', choices = c("All"=0,c("Drain Water"=1, "Produce"=2, "Piped Water"=3, 
    #                                                                           "Ocean Water"=4, "Surface Water"=5, "Flood Water"=6,
    #                                                                           "Public Latrine Surfaces"=7, "Particulate"=8, "Bathing"=9))),
    # 
    #       selectInput('neighb', 'Neighborhood', choices=""),
    #       selectInput('ad_ch','Adults or Children',choices=c("All"=0,"Adults"=1,"Children"=2)),
    radioButtons('format', 'Document format', c('PDF', 'HTML', 'Word'),
                 inline = TRUE),
    downloadButton('downloadReport'),
    br(),
    br(),
    br(),
    img(src="RGB-horiz.png", width = 250)
  )