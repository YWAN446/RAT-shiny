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
      h3('FormHub Settings:'),
      selectizeInput('col_file', 'Collection Data', multiple=F, choices=NULL, selected=NULL),
      selectizeInput('lab_file', 'Lab Data', multiple=F, choices=NULL, selected=NULL),
      selectizeInput('hh_file', 'Household Data', multiple=F, choices=NULL, selected=NULL),
      selectizeInput('sch_file', 'School Data', multiple=F, choices=NULL, selected=NULL),
      selectizeInput('com_file', 'Community Data', multiple=F, choices=NULL, selected=NULL),
      
      h3('Analysis Settings:'),
      selectInput('neighb', 'Neighborhood', choices=""),
      selectInput('samtype', 'Sample Type', choices=""),
      selectInput('surtype','Survey Type',choices=c("All"=0,"Household"=1,"School"=2,"Community"=3)),
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
                  tabPanel("Raw Data", 
                           textOutput('collection'),
                           textOutput('school'),
                           textOutput('community'),
                           textOutput('hh'),
                           textOutput('lab')),
                           # tableOutput('ec_table'),
                           # tableOutput('hh_table'),
                           # tableOutput('sch_table')),
                           # tableOutput('com_table')),
                  tabPanel("Pie Chart", plotOutput("pie_chart1",height = 300, width = 1400),plotOutput("pie_chart2",height = 300, width = 1400),
                           plotOutput("pie_chart3",height = 300, width = 1400),plotOutput("pie_chart4",height = 300, width = 1400),
                           plotOutput("pie_chart5",height = 300, width = 1400),plotOutput("pie_chart6",height = 300, width = 1400),
                           plotOutput("pie_chart7",height = 300, width = 1400)),
                  tabPanel("Histogram", plotOutput("hist1",height = 500, width = 1100),plotOutput("hist2",height = 500, width = 1100),plotOutput("hist3",height = 500, width = 1100),
                           plotOutput("hist4",height = 500, width = 1100),plotOutput("hist5",height = 500, width = 1100),plotOutput("hist6",height = 500, width = 1100),
                           plotOutput("hist7",height = 500, width = 1100),plotOutput("hist8",height = 500, width = 1100),plotOutput("hist9",height = 500, width = 1100)),
                  tabPanel("People Plot", plotOutput("ps_plot1",height = 500, width = 1100),plotOutput("ps_plot2",height = 500, width = 1100),plotOutput("ps_plot3",height = 500, width = 1100),
                           plotOutput("ps_plot4",height = 500, width = 1100),plotOutput("ps_plot5",height = 500, width = 1100),plotOutput("ps_plot6",height = 500, width = 1100),
                           plotOutput("ps_plot7",height = 500, width = 1100))
                  )
    )
  )
))