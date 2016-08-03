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
    sidebarPanel(analysis_sidebar),
    mainPanel(
      tabsetPanel(type = "tabs", 
                  tab_pieChart,
                  tab_histogram,
                  # tab_peoplePlot,
                  tab_rawData,
                  tab_visualizationSettings,
                  tab_formhubSettings

                  
                )
      
    )
  )
)
)
