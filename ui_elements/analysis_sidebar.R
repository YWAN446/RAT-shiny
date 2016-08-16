
analysis_sidebar <- fluidRow(
    h3('Analysis Settings:'),
    selectInput('surtype','Survey Type', selected="0", choices=c("All"=0,"Household"=1,"School"=2,"Community"=3)),
    hr(),
    h4('Order Results:'),
    selectizeInput('level1', 'Level 1', choices=c('Sample' = 'sample', 'Neighborhood' = 'neighborhood', 'Age' = 'age'), selected='sample'),
    selectInput('level2', 'Level 2', choices=c('Neighborhood' = 'neighborhood', 'Age' = 'age'), selected='neighborhood'),
    selectInput('level3', 'Level 3', choices=c('Age' = 'age'), selected='age'),
    
    
    tags$b(textOutput('plot_display')),
    # actionButton('make_pie', 'Make pies'),
    fluidRow(
      column(6,
             checkboxGroupInput('sample', 'Sample Choices:', choices="", selected="")
      ),
      column(6,
             checkboxGroupInput('age', 'Age Choices:',choices="", selected="")
             
      )
      
    ),
    
    checkboxGroupInput('neighborhood', 'Neighborhood Choices:',choices="", selected=""),
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
    img(src="sanipath_logo.png", height = 80, width = 250)
  )