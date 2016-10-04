tab_visualizationSettings <- 
  tabPanel(
    'Visualization Settings',
    helpText("General settings for how the data are displayed."),
    hr(),
    fluidRow(
      column(6, 
             h4("Filter Results:"),
             helpText("Filter out specific neighborhoods, samples, or age groups to limit what results are displayed."),
             fluidRow(
               column(6,
                      checkboxGroupInput('sample', 'Sample Choices:', choices="", selected="")
               ),
               column(6,
                      checkboxGroupInput('age', 'Age Choices:',choices="", selected="")
                      
               )
               
             ),
             checkboxGroupInput('neighborhood', 'Neighborhood Choices:',choices="", selected="")
      ),
      column(6,
             h4('Order Results:'),
             helpText('Set the order and hierarchy of how the results are displayed'),
             selectizeInput('level1', 'Level 1', choices=c('Sample' = 'sample', 'Neighborhood' = 'neighborhood', 'Age' = 'age'), selected='sample'),
             selectInput('level2', 'Level 2', choices=c('Neighborhood' = 'neighborhood', 'Age' = 'age'), selected='neighborhood'),
             selectInput('level3', 'Level 3', choices=c('Age' = 'age'), selected='age'),
             
             
             tags$b(textOutput('plot_display'))
             # actionButton('make_pie', 'Make pies'),
             
      )
             
    ),
    hr(),
    fluidRow(
      column(6,
             h4('Pie Chart Size:'),
             helpText('Change the dimensions of the pie charts displayed.'),
             sliderInput(
               'pw',"Pie Width:", min = 200, max = 500, value = 350, step = 25
             ),
             sliderInput(
               'ph',"Pie Height:", min = 200, max = 500, value = 400, step = 25
             )
      ),
      column(6,
             h4("Layout Settings:"),
             helpText(
               "Set the number of columns displayed for each of the visualization tabs.  As this increases,
                the number of rows on the page will shrink, but the plots will shrink as well. "
             ),
             sliderInput('num_columns', 'Number of Columns', 1, 6, value =
                           2)

             )
    )
    )

