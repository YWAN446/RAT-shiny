library(shiny)

shinyUI(fluidPage(theme = "bootstrap.css",
                  tags$head(
                    tags$style(HTML("
                                    @import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');
                                    "))
                    ),
  h1("SaniPath", span("Exposure Assessment Tool", style = "font-weight: 300"), 
      style = "font-family: 'Lobster', cursive;
  color: #fff; text-align: center;
  background-image: url('texturebg.png');
  padding: 20px"),
  br(),
#  titlePanel("SaniPath"),
  sidebarLayout(
    sidebarPanel(
      h3('Analysis Settings:'),
      selectizeInput('level1', 'Level 1', choices=c('Sample' = 'sample', 'Neighborhood' = 'neighborhood', 'Age' = 'age'), selected='sample'),
      selectInput('level2', 'Level 2', choices='', selected=''),
      selectInput('level3', 'Level 3', choices='', selected=''),
      
      
      tags$b(textOutput('plot_display')),
      # actionButton('make_pie', 'Make pies'),
      checkboxGroupInput('sample', 'Sample Choices:', choices="", selected=""),
      checkboxGroupInput('neighborhood', 'Neighborhood Choices:',choices="", selected=""),
      checkboxGroupInput('age', 'Age Choices:',choices="", selected=""),
      selectInput('samtype', 'Sample Type', choices = c("All"=0,c("Drain Water"=1, "Produce"=2, "Piped Water"=3, 
                                                                          "Ocean Water"=4, "Surface Water"=5, "Flood Water"=6,
                                                                          "Public Latrine Surfaces"=7, "Particulate"=8, "Bathing"=9))),
      selectInput('surtype','Survey Type', selected="1", choices=c("All"=0,"Household"=1,"School"=2,"Community"=3)),
      selectInput('neighb', 'Neighborhood', choices=""),
      selectInput('ad_ch','Adults or Children',choices=c("All"=0,"Adults"=1,"Children"=2)),
      radioButtons('format', 'Document format', c('PDF', 'HTML', 'Word'),
                   inline = TRUE),
      downloadButton('downloadReport'),
      br(),
      br(),
      br(),
      img(src="sanipath_logo.png", height = 80, width = 250)
    ),
    mainPanel(
      tabsetPanel(type = "tabs", 
                  tabPanel("Pie Chart", 
                           uiOutput("pie_charts")),
                  tabPanel("Histogram", uiOutput('histograms')),
#                            plotOutput("hist1",height = 500, width = 1100),plotOutput("hist2",height = 500, width = 1100),plotOutput("hist3",height = 500, width = 1100),
#                            plotOutput("hist4",height = 500, width = 1100),plotOutput("hist5",height = 500, width = 1100),plotOutput("hist6",height = 500, width = 1100),
#                            plotOutput("hist7",height = 500, width = 1100),plotOutput("hist8",height = 500, width = 1100),plotOutput("hist9",height = 500, width = 1100)),
                  tabPanel("People Plot", plotOutput("ps_plot1",height = 500, width = 1100),plotOutput("ps_plot2",height = 500, width = 1100),plotOutput("ps_plot3",height = 500, width = 1100),
                           plotOutput("ps_plot4",height = 500, width = 1100),plotOutput("ps_plot5",height = 500, width = 1100),plotOutput("ps_plot6",height = 500, width = 1100),
                           plotOutput("ps_plot7",height = 500, width = 1100)),
                  tabPanel("Raw Data", 
                           selectInput('raw_view', 'Select table to view:', 
                                       c('Household', 'Community', 'School', 'E. Coli'), selected='Household'),
                           dataTableOutput('raw_table')),
                  tabPanel('Vizualization Settings',
                           helpText("General settings for how the data are displayed."),
                           h3('Pie Chart Size'),
                           sliderInput('pw',"Pie Width:", min=200, max=500, value=350, step=25),
                           sliderInput('ph',"Pie Height:", min=200, max=500, value=400, step=25),
                           h3("Layout Settings"),
                           sliderInput('num_columns', 'Number of Columns', 1, 6, value=2),
                           helpText("Sets the number of columns displayed for each set of visualizations on a tab.  As this increases,
                                    the number of rows on the page will shrink, but the plots will shrink as well. ")
                           
                           ),
                  tabPanel("Formhub Settings",
                           h3('Forms:'),
                           selectizeInput('col_file', 'Collection Data', multiple=F, choices='sp_sample_collection_form_1_c', selected='sp_sample_collection_form_1_c'),
                           selectizeInput('lab_file', 'Lab Data', multiple=F, choices='sp_sample_lab_form_1_i', selected='sp_sample_lab_form_1_i'),
                           selectizeInput('hh_file', 'Household Data', multiple=F, choices='sp_household_form_2_01b', selected='sp_household_form_2_01b'),
                           selectizeInput('sch_file', 'School Data', multiple=F, choices='school_d', selected='school_d'),
                           selectizeInput('com_file', 'Community Data', multiple=F, choices='community_d', selected='community_d')
                  )
                  
                )
      
    )
  )
))