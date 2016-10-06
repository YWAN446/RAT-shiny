
analysis_sidebar <- fluidRow(
    selectInput('surtype','Survey Type', selected="1", choices=c("All"=0,"Household"=1,"School"=2,"Community"=3)),
    actionButton("update_forms", "Refresh Data", icon=icon('refresh')),
    helpText("Press to download new data from Formhub or use uploaded CSVs to generate 
             analysis figures."),

    radioButtons('format', 'Document format', c('PDF', 'HTML', 'Word'),
                 inline = TRUE),
    downloadButton('downloadReport'),
    br(),
    br(),
    br(),
    img(src="RGB-horiz.png", width = 250)
  )