# MAIN UI
# This is the layout once a user authenticates appropriately
# with the system. Each tab in the UI is saved as a different
# file to make organization easier.  they follow the format
# tab_NAME.R and correspond to the tab names.

main_ui <-
  fluidPage(
    theme = "bootstrap.css",
    tags$head(tags$style(
      HTML(
        "
        @import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');
        "
      )
      )),
    h1(
      span("SaniPath Exposure Assessment Tool", style = "font-weight: 300"),
      style = "font-family: 'Lobster', cursive;
      color: #fff; text-align: center;
      background-image: url('texturebg.png');
      padding: 20px"
    ),
    br(),
    #  titlePanel("SaniPath"),
    
    column(3, analysis_sidebar),
    column(1),
    column(8,
      tabsetPanel(
        type = "tabs",
        tab_pieChart,
        tab_histogram,
        tab_peoplePlot,
        tab_rawData,
        tab_visualizationSettings,
        tab_formhubSettings
        
        
      )
      
    )
    )
