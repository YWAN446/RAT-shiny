library(shiny)
source('global.R') # this is supposed to automatically be loaded, but it doesn't seem to be. 

# see global.R for configuration files and settings 

# intervention list for later
interventions <- read.csv('interventions.csv')

sampleTypes <- c("Drain Water"=1, "Produce"=2, "Piped Water"=3, 
                 "Ocean Water"=4, "Surface Water"=5, "Flood Water"=6,
                 "Public Latrine Surfaces"=7, "Particulate"=8, "Bathing"=9)

options <- c('Sample' = 'sample', 'Neighborhood' = 'neighborhood', 'Age' = 'age')


jscode <- '
$(function() {
  var $els = $("[data-proxy-click]");
  $.each(
    $els,
    function(idx, el) {
      var $el = $(el);
      var $proxy = $("#" + $el.data("proxyClick"));
      $el.keydown(function (e) {
        if (e.keyCode == 13) {
          $proxy.click();
        }
      });
    }
  );
});
'



shinyServer(function(input, output, session) {
  
  # will stop shiny when the window closes
  session$onSessionEnded(function() {
    stopApp()
  })
  
  # Login observers
  USER <- reactiveValues(Logged = FALSE)
  # Download value to update form data

  # Observer to check the login credentials given
  observe({ 
    if (USER$Logged == FALSE) {
      if (!is.null(input$Login)) {
        
        if (input$Login > 0) {
          Username <- isolate(input$userName)
          Password <- isolate(input$passwd)
          check <- formhubCheck_user(baseURL, Username, Password, test_form)
          print(check)
          if (check == T) {
            USER$Logged <- TRUE

          }
          else {
            output$login_status <- renderText("Invalid credentials! Please check the username and password.")
          }

        } 
      }
    }    
  })
  
  # Render the login page and then render the main_ui if login is successful
  # need to add error message if login fails
  observe({
    if (USER$Logged == FALSE) {
      
      output$page <- renderUI({
        bootstrapPage(tags$head(tags$script(HTML(jscode))),
                      
                      
                      
                      div(id = "login",
                          img(src="RGB-horiz.png", width=250),
                          wellPanel(textInput("userName", "Username"),
                                    tagAppendAttributes(
                                      passwordInput("passwd", "Password"),
                                      `data-proxy-click` = "passwd"
                                    ),
                                    br(),actionButton("Login", "Log in")), textOutput('login_status')),
                      tags$style(type="text/css", "#login {font-size:10px;   text-align: left;position:absolute;top: 40%;left: 50%;margin-top: -100px;margin-left: -150px;}")
        )
      })
    }
    if (USER$Logged == TRUE) 
    {
      output$page <- renderUI({
        main_ui
      })
      updateSelectizeInput(session, 'col_file', selected=collection_form)
      updateSelectizeInput(session, 'lab_file', selected=lab_form)
      updateSelectizeInput(session, 'hh_file', selected=household_form)
      updateSelectizeInput(session, 'sch_file', selected=school_form)
      updateSelectizeInput(session, 'com_file', selected=community_form)

    }
  })
  usr <- reactive({isolate(input$userName)})
  pwd <- reactive({isolate(input$passwd)})

  
#   forms <- reactive({
#     if (USER$Logged == T) {
#       
#       getAPI_forms(baseURL, apiUrl, usr(), pwd(), apiToken)
#     }
#     else {
#       validate(need(USER$Logged == T, 'Not logged in'))
#     }
# 
#     })
#   
#   
#   
  # Update the form options ---------------------------------------------------------
#   observe(autoDestroy = T, {
#     # Update the form options based on the defaults specified in global.R if
#     # login is successful
#     if (USER$Logged == T) {
#       # update the options

#       
#       
#     }
# 
#   })
  
#   # Download the data ----------------------------------------------------------------
  school_data <- eventReactive(input$update_forms, {
    if (is.null(input$sch_csv)) {
      
      withProgress(
        formhubGET_csv(baseURL, usr(), pwd(), input$sch_file),
        message = 'Downloading Data', value = 20)
    }
    else {
      print('School csv override.')
      withProgress(
        read.csv(input$sch_csv$datapath, as.is=T),
        message= 'Overriding school form with csv upload', value=20)
      
    }
  })

   community_data <- eventReactive(input$update_forms, {
    if (is.null(input$com_csv)) {
      withProgress(
        formhubGET_csv(baseURL, usr(), pwd(), input$com_file),
        message = 'Downloading Data', value = 40)
    }
    else {
      print('Community csv override.')
      withProgress(
        read.csv(input$com_csv$datapath, as.is=T),
        message= 'Overriding community form with csv upload', value=40)
      
    }
  })
  
  
  household_data <- eventReactive(input$update_forms,  { 
    if (is.null(input$hh_csv)) {
      withProgress(
        formhubGET_csv(baseURL, usr(), pwd(), input$hh_file),
        message = 'Downloading Data', value = 60)
    }
    else {
      print('Household csv override.')
      withProgress(
        read.csv(input$hh_csv$datapath, as.is=T),
        message= 'Overriding household form with csv upload', value=60)
      
    }
     
  })
  
  collection_data <- eventReactive(input$update_forms, {
    if (is.null(input$col_csv)) {
      
      withProgress(
        formhubGET_csv(baseURL, usr(), pwd(), input$col_file),
        message = 'Downloading Data', value = 80)
    }
    else {
      print('Collection csv override.')
      
      withProgress(
        read.csv(input$col_csv$datapath, as.is=T),
        message= 'Overriding collection form with csv upload', value=80)
      
    }
  })
  

  lab_data <- eventReactive(input$update_forms, {
    if (is.null(input$lab_csv)) {
      withProgress(
        formhubGET_csv(baseURL, usr(), pwd(), input$lab_file),
        message = 'Downloading Data', value = 100)
    }
    else {
      print('Lab csv override.')
      
      withProgress(
        read.csv(input$lab_csv$datapath, as.is=T),
        message= 'Overriding lab form with csv upload', value=100)
      
    }
  })


  
  ec_data <- eventReactive(lab_data(), {
    withProgress(
      create_ecData(collection_data(), lab_data()),
      message = 'Calculating Concentration Data', value = 50)
  })
  
  conc <- eventReactive(ec_data(), {
    withProgress(
      create_concData(ec_data()),
      message = 'Calculating Concentration Data', value = 100)
    
  })
  
# Raw Data Summary Dashboard 
#   survey_submissions <- DT::renderDataTable({
#     school <- as.data.frame(table(school_data()[,grep('neighbor', names(school_data()))]))
#     school$Survey <- 'School'
#     community <- as.data.frame(table(community_data()[,grep('neighbor', names(community_data()))]))
#     community$Survey <- 'Community'
#     household <- as.data.frame(table(household_data()[,grep('neighbor', names(household_data()))]))
#     household$Survey <- 'Household'
#     
#     summary <- rbind.fill(school, community, household)
#     summary <- dcast(summary, surv ~ Var1, value.var = 'Freq')
#     summary
#   })
#   
#   sample_collection <- DT::renderDataTable({
#     
#   })
  
  
  

  #   # PIE CHART PLOTTING INFO ----------------------------------------------------------------
  freq <- reactive({
    types <- c('combined', 'household', 'school', 'community')
    if (USER$Logged == T) {
      calculate_freq(household_data(), school_data(), community_data(), survey_type= types[as.numeric(input$surtype)+1])
      
    }
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
  
  observe({
    if (USER$Logged == T) {
      age <- unique(names(list.names(freq(), age)))
      neighborhood <- unique(names(list.names(freq(), neighborhood))) 
      sample <- unique(c(names(list.names(freq(), sample)), names(list.names(conc(), sample)))) # need to add 2 missing samples !!! 
      
      updateCheckboxGroupInput(session, 'sample',  choices= sample, selected=sample)
      updateCheckboxGroupInput(session, 'neighborhood', choices= neighborhood, selected=neighborhood)
      updateCheckboxGroupInput(session, 'age', choices= age, selected= age)
      
    }

    
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
    cat("Pie chart order:\n")
    print(pie_chart_order())
    # pie_chart_order()
    do.call(tagList, pie_chart_order())
  })
  # generate the ggplot objects
  observe({
    dat <- freq()
    dat <- dat[list.which(dat, sample %in% input$sample && 
                            neighborhood %in% input$neighborhood &&
                            age %in% input$age)]
    
    count <- 1
    for (i in dat) {
      local({
        withProgress({
          my_i <- i
          p_name <- paste0("pie-",my_i$sample,"-",my_i$neighborhood, '-', my_i$age)
          p_name <- gsub(' ', '', p_name)
          print(p_name)
          output[[p_name]] <- renderPlot({
            create_pieChart(my_i$data, my_i$sample, '')
          })
        },message = 'Generating Pie Charts', session=session, value= count/length(dat)
        )
      })
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
  
  observe({
    if (length(conc()) > 0 ) {
      
      dat <- conc()
      dat <- dat[list.which(dat, sample %in% input$sample && 
                              neighborhood %in% input$neighborhood)]

        count <- 1
        for (i in dat) {
          local({
            withProgress({
            my_i <- i
            p_name <- paste0('hist-', my_i$sample,"-",my_i$neighborhood)
            p_name <- gsub(' ', '', p_name)
            output[[p_name]] <- renderPlot({
              make_histogram(my_i$data, paste0(my_i$neighborhood,", ", my_i$sample, '\n(N=',length(my_i$data),")"))
            })
            }, message = 'Generating Histograms', session=session, value=count/length(dat)
            )
          })
          # incProgress(count/length(dat), session = session)
          count <- count + 1
        }

    }
    
  })
  
  ps.freq <- reactive({
    print('bayesian calculations')
    
    types <- c('combined', 'household', 'school', 'community')
    if (USER$Logged == T) {
      
      freq <- calculate_freq(household_data(), school_data(), community_data(), type='ppl plot', survey_type= types[as.numeric(input$surtype)+1])
      calculate_pplPlotData(freq, conc(), shinySession=session) # letting the defaults lazy load 
    }
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
  observe({
    print('triggered---------------------------------------------------------')
    dat <- ps.freq()
    dat <- dat[list.which(dat, sample %in% input$sample && 
                            neighborhood %in% input$neighborhood &&
                            age %in% input$age)]
    
    count <- 1
    for (i in dat) {
      local({
        withProgress({
          my_i <- i
          print(my_i)
          p_name <- paste0('ppl-',my_i$sample,"-",my_i$neighborhood, '-', my_i$age)
          p_name <- gsub(' ', '', p_name)
          output[[p_name]] <- renderPlot({
            PS_Plot(paste0(my_i$neighborhood,", ", my_i$sample, ', ', my_i$age), as.numeric(my_i$n), as.numeric(my_i$dose))
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