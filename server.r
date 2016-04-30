library(shiny)
library(rjags)
source("./model/PS_Plot.r")
options(shiny.maxRequestSize = 9*1024^2)
source('api_helpers.R')
source('analysis_helpers.R')
source('plotting_helpers.R')

baseURL <- 'http://54.210.2.87/'
apiUrl <- 'http://54.210.2.87/api/v1/data' # this is the main access point to use for data
apiToken <- '8d0336d37ef28df590574f1cd4531f142e31ca02'
usr <- 'sp'
pwd <- '2007beagle'

# download the list of forms before Shiny starts
forms <- getAPI_forms(baseURL, apiUrl, usr, pwd, apiToken)

sampleTypes <- c("Drain Water"=1, "Produce"=2, "Piped Water"=3, 
                 "Ocean Water"=4, "Surface Water"=5, "Flood Water"=6,
                 "Public Latrine Surfaces"=7, "Particulate"=8, "Bathing"=9)

options <- c('Sample' = 'sample', 'Neighborhood' = 'neighborhood', 'Age' = 'age')


shinyServer(function(input, output, session) {
  
  # will stop shiny when the window closes
  session$onSessionEnded(function() {
    stopApp()
  })
  
  # Update the form options ---------------------------------------------------------
  observe(autoDestroy = T, {
    # URL and API token are currently defined in the API Helpers script.
    # This returns a list of available forms based on the user token
    # provided.
    
    # update the options
    updateSelectizeInput(session, 'col_file', choices=filterAPI_forms('collection', forms)$menu_items, selected='sp_sample_collection_form_1_c')
    updateSelectizeInput(session, 'lab_file', choices=filterAPI_forms('lab', forms)$menu_items, selected='sp_sample_lab_form_1_i')
    updateSelectizeInput(session, 'hh_file', choices=filterAPI_forms('household', forms)$menu_items, selected='sp_household_form_2_01b')
    updateSelectizeInput(session, 'sch_file', choices=filterAPI_forms('school', forms)$menu_items, selected='school_d')
    updateSelectizeInput(session, 'com_file', choices=filterAPI_forms('community', forms)$menu_items, selected='community_d')
    
  })
#   # Download the data ----------------------------------------------------------------
  school_data <- eventReactive(input$col_file, {
    withProgress(
    formhubGET_csv(baseURL, usr, pwd, input$sch_file),
    message = 'Downloading School Data', value = 100)
  })
  
  community_data <- eventReactive(input$com_file, {
    formhubGET_csv(baseURL, usr, pwd, input$com_file)
  })

  
  household_data <- eventReactive(input$hh_file, { # household data, keeping name for consistency
    formhubGET_csv(baseURL, usr, pwd, input$hh_file)
  })
  
  collection_data <- eventReactive(input$col_file, {
    formhubGET_csv(baseURL, usr, pwd, input$col_file)
  })
  

  lab_data <- eventReactive(input$lab_file, {
    formhubGET_csv(baseURL, usr, pwd, input$lab_file)
  })
  
  ec_data <- reactive({
    create_ecData(collection_data(), lab_data())
  })
  
  conc <- reactive({
    conc <- create_concData(ec_data())
    print(conc)
  })

#   
#   # PIE CHART PLOTTING INFO ----------------------------------------------------------------
  freq <- reactive({
    types <- c('combined', 'household', 'school', 'community')
    freq <- calculate_freq(household_data(), school_data(), community_data(), survey_type= types[as.numeric(input$surtype)+1])
  })
#   
  observeEvent(input$level1, {
    opts2 <- options[options != input$level1]
    updateSelectInput(session, inputId = 'level2', label='Level 2', choices= opts2, selected=opts2[1])
  })
  observeEvent(c(input$level1, input$level2), {
    opts3 <- options[!(options %in% c(input$level2, input$level1))]
    updateSelectInput(session, inputId = 'level3', label='Level 3', choices= opts3)
  })
  
  observe(autoDestroy = T, {
    freq()
    age <- unique(names(list.names(freq(), age)))
    neighborhood <- unique(names(list.names(freq(), neighborhood)))
    sample <- unique(names(list.names(freq(), sample)))
    
    updateCheckboxGroupInput(session, 'sample',  choices= sample, selected=sample)
    updateCheckboxGroupInput(session, 'neighborhood', choices= neighborhood, selected=neighborhood)
    updateCheckboxGroupInput(session, 'age', choices= age, selected= age)
    
    
  })
  
  # BUILD THE UI FOR THE PIE CHARTS ---------------------------------------------------------
  # this will lay out the plots in the appropriate order
  pie_chart_order <- reactive({
    ordered_shinyCharts(freq(), columns= input$num_columns, level1_type = input$level1, level2_type = input$level2,
                        sample_filter=input$sample, neighborhood_filter = input$neighborhood, age_filter = input$age,
                        height = input$ph, width = input$pw, shinySession=session)
  })
  
  # this will actually render them for the UI
  output$pie_charts <- renderUI({
    pie_chart_order()
    do.call(tagList, pie_chart_order())
  })
  
  # generate the ggplot objects
  observeEvent(c(input$level3, input$surtype), {
    dat <- freq()
    dat <- dat[list.which(dat, sample %in% input$sample && 
                            neighborhood %in% input$neighborhood &&
                            age %in% input$age)]

      count <- 1
      for (i in dat) {
        local({
          withProgress({
          my_i <- i
          p_name <- paste0('pie-', my_i$sample,"-",my_i$neighborhood, '-', my_i$age)
          p_name <- gsub(' ', '', p_name)
          output[[p_name]] <- renderPlot({
            create_pieChart(my_i$data, my_i$sample, '') 
          })
        },message = 'Generating Pie Charts', session=session, value= count/length(dat)
        )
        })
        # incProgress(count/length(dat), session = session)
        count <- count + 1
      }

    
  })
  
  hist_order <- reactive({
    ordered_shinyHists(conc(), input$num_columns, level1_type = input$level1, sample_filter = input$sample, neighborhood_filter = input$neighborhood)
  })
  
  output$histograms <- renderUI({
    hist_order()
    do.call(tagList, hist_order())
  })
  
  observeEvent(c(input$level1, input$level2), {
    if (length(conc()) > 0 ) {
      
      dat <- conc()
      dat <- dat[list.which(dat, sample %in% input$sample && 
                              neighborhood %in% input$neighborhood)]

        count <- 1
        for (i in dat) {
          local({
            ses = session
            withProgress({
            my_i <- i
            p_name <- paste0('hist-', my_i$sample,"-",my_i$neighborhood)
            p_name <- gsub(' ', '', p_name)
            output[[p_name]] <- renderPlot({
              make_histogram(my_i$data, paste0(my_i$neighborhood,", ", my_i$sample, '\n(N=',length(my_i$data),")"))
            })
            }, message = 'Generating Histograms', session=ses, value=count/length(dat)
            )
          })
          # incProgress(count/length(dat), session = session)
          count <- count + 1
        }

    }
    
  })
  
  ps.freq <- reactive({
    types <- c('combined', 'household', 'school', 'community')
    freq <- calculate_freq(household_data(), school_data(), community_data(), type='ppl plot', survey_type= types[as.numeric(input$surtype)+1])
    
    calculate_pplPlotData(freq, conc(), shinySession=session) # letting the defaults lazy load 
    
  })

  ppl_plot_order <- reactive({
    ordered_shinyCharts(ps.freq(), columns= input$num_columns, level1_type = input$level1, level2_type = input$level2,
                        sample_filter=input$sample, neighborhood_filter = input$neighborhood, age_filter = input$age,
                        height = input$ph, width = input$pw, shinySession=session, chart_prefix = 'ppl-')
  })
  
  # this will actually render them for the UI
  output$ppl_plots <- renderUI({
    ppl_plot_order()
    do.call(tagList, ppl_plot_order())
  })
  
  # generate the people plots
  observeEvent(c(input$level3, input$surtype), {
    dat <- ps.freq()
    dat <- dat[list.which(dat, sample %in% input$sample && 
                            neighborhood %in% input$neighborhood &&
                            age %in% input$age)]
    
    count <- 1
    for (i in dat) {
      local({
        withProgress({
          my_i <- i
          p_name <- paste0('ppl-',my_i$sample,"-",my_i$neighborhood, '-', my_i$age)
          p_name <- gsub(' ', '', p_name)
          output[[p_name]] <- renderPlot({
            PS_Plot(paste0(my_i$neighborhood,", ", my_i$sample, ', ', my_i$age), my_i$n, my_i$dose) 
          })
        }, message = 'Generating People Plots', session=session, value= count/length(dat)
        )
      })
      # incProgress(count/length(dat), session = session)
      count <- count + 1
    }
    
    
  })

  
  ## Tables for raw printing ------------------------------------------------------------------------------------------------------------
  output$raw_table <- renderDataTable({
    switch(input$raw_view,
           'Household' = household_data(),
           'Community' = community_data(),
           'School' = school_data(),
           'E. Coli' = ec_data()
           )
  })
  

  
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste('my-report', sep = '.', switch(
        input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
      ))
    },
    
    content = function(file) {
      src <- normalizePath('report.Rmd')
      
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'report.Rmd')
      
      library(rmarkdown)
      out <- render('report.Rmd', switch(
        input$format,
        PDF = pdf_document(), HTML = html_document(), Word = word_document()
      ))
      file.rename(out, file)
    }
  )
  
})