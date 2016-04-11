library(shiny)
# library(rjags)
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
    create_concData(ec_data())
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
  pie_chart_order <- eventReactive(input$level3, {
    ordered_shinyCharts(freq(), columns= 2, level1_type = input$level1, level2_type = input$level2,
                        sample_filter=input$sample, neighborhood_filter = input$neighborhood, age_filter = input$age)
  })
  
  # this will actually render them for the UI
  output$plots <- renderUI({
    pie_chart_order()
    do.call(tagList, pie_chart_order())
  })
  
  observe({
    dat <- freq()
    dat <- dat[list.which(dat, sample %in% input$sample && 
                            neighborhood %in% input$neighborhood &&
                            age %in% input$age)]
    
    withProgress({
      
      count <- 1
      for (i in dat) {
        local({
          my_i <- i
          p_name <- paste0(my_i$sample,"-",my_i$neighborhood, '-', my_i$age)
          p_name <- gsub(' ', '', p_name)
          output[[p_name]] <- renderPlot({
            create_pieChart(my_i$data, my_i$sample, '')
          })
          
        })
        incProgress(count/length(dat), session = session)
        count <- count + 1
      }
    },message = 'Generating Pie Charts', session=session
    )
    
  })
  
  
  ps.freq <- reactive({
    types <- c('combined', 'household', 'school', 'community')
    freq <- calculate_freq(household_data(), school_data(), community_data(), type='ppl plot', survey_type= types[as.numeric(input$surtype)+1])
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
  

  output$hist1 <- renderPlot({
    ec_data<-ec_data()
    conc<-conc()
    n.neighb=ifelse(input$neighb==0, length(unique(as.numeric(ec_data$neighbor))),1)
    n.path=1
    if (n.neighb==1 & input$neighb!=0) {k.neighb=as.numeric(input$neighb)}
    else {k.neighb=sort(unique(as.numeric(ec_data$neighbor)))}
    if (input$samtype!=0){k.path=as.numeric(input$samtype)} 
    else{k.path=1}
    nrow=n.path
    ncol=n.neighb
    par(mfrow=c(nrow,ncol))
    par(mar=c(4,2,4,1))
    par(pin=c(6,5))
    label3<-c("Drain Water", "Produce", "Piped Water", "Ocean Water", "Surface Water", "Flood Water", "Public Latrine Surfaces", "Particulate", "Bathing")
    for (j in 1:n.neighb){
      hist(log10(as.numeric(conc[[9*(k.neighb[j]-1)+k.path]])),breaks=seq(0,10,by=1),col="skyblue",ylim=c(0,1),freq=FALSE,yaxt="n",ylab="percent",
           main=paste("Neighborhood ",k.neighb[j],", Sample Type:",label3[k.path],"( N =",length(which(!is.na(conc[[9*(k.neighb[j]-1)+k.path]]))),")"),cex.main=1.3,xlab=expression(paste("log10 ", italic("E. coli"), "concentration (CFU/100mL)")))
      axis(2,at=seq(0,1,0.2),labels=paste(c(0,20,40,60,80,100),"%",sep=""))
    }
  })
  
  output$hist2 <- renderPlot({
    if (input$samtype!=0) return(NULL)
    ec_data<-ec_data()
    conc<-conc()
    n.neighb=ifelse(input$neighb==0, length(unique(as.numeric(ec_data$neighbor))),1)
    n.path=1
    if (n.neighb==1 & input$neighb!=0) {k.neighb=as.numeric(input$neighb)}
    else {k.neighb=sort(unique(as.numeric(ec_data$neighbor)))}
    k.path=2
    nrow=n.path
    ncol=n.neighb
    par(mfrow=c(nrow,ncol))
    par(mar=c(4,2,4,1))
    par(pin=c(6,5))
    label3<-c("Drain Water", "Produce", "Piped Water", "Ocean Water", "Surface Water", "Flood Water", "Public Latrine Surfaces", "Particulate", "Bathing")
    for (j in 1:n.neighb){
      hist(log10(as.numeric(conc[[9*(k.neighb[j]-1)+k.path]])),breaks=seq(0,10,by=1),col="skyblue",ylim=c(0,1),freq=FALSE,yaxt="n",ylab="percent",
           main=paste("Neighborhood ",k.neighb[j],", Sample Type:",label3[k.path],"( N =",length(which(!is.na(conc[[9*(k.neighb[j]-1)+k.path]]))),")"),cex.main=1.3,xlab=expression(paste("log10 ", italic("E. coli"), "concentration (CFU/serving)")))
      axis(2,at=seq(0,1,0.2),labels=paste(c(0,20,40,60,80,100),"%",sep=""))
    }
  })
  
  output$hist3 <- renderPlot({
    if (input$samtype!=0) return(NULL)
    ec_data<-ec_data()
    conc<-conc()
    n.neighb=ifelse(input$neighb==0, length(unique(as.numeric(ec_data$neighbor))),1)
    n.path=1
    if (n.neighb==1 & input$neighb!=0) {k.neighb=as.numeric(input$neighb)}
    else {k.neighb=sort(unique(as.numeric(ec_data$neighbor)))}
    k.path=3
    nrow=n.path
    ncol=n.neighb
    par(mfrow=c(nrow,ncol))
    par(mar=c(4,2,4,1))
    par(pin=c(6,5))
    label3<-c("Drain Water", "Produce", "Piped Water", "Ocean Water", "Surface Water", "Flood Water", "Public Latrine Surfaces", "Particulate", "Bathing")
    for (j in 1:n.neighb){
      hist(log10(as.numeric(conc[[9*(k.neighb[j]-1)+k.path]])),breaks=seq(0,10,by=1),col="skyblue",ylim=c(0,1),freq=FALSE,yaxt="n",ylab="percent",
           main=paste("Neighborhood ",k.neighb[j],", Sample Type:",label3[k.path],"( N =",length(which(!is.na(conc[[9*(k.neighb[j]-1)+k.path]]))),")"),cex.main=1.3,xlab=expression(paste("log10 ", italic("E. coli"), "concentration (CFU/100mL)")))
      axis(2,at=seq(0,1,0.2),labels=paste(c(0,20,40,60,80,100),"%",sep=""))
    }
  })
  
  output$hist4 <- renderPlot({
    if (input$samtype!=0) return(NULL)
    ec_data<-ec_data()
    conc<-conc()
    n.neighb=ifelse(input$neighb==0, length(unique(as.numeric(ec_data$neighbor))),1)
    n.path=1
    if (n.neighb==1 & input$neighb!=0) {k.neighb=as.numeric(input$neighb)}
    else {k.neighb=sort(unique(as.numeric(ec_data$neighbor)))}
    k.path=4
    nrow=n.path
    ncol=n.neighb
    par(mfrow=c(nrow,ncol))
    par(mar=c(4,2,4,1))
    par(pin=c(6,5))
    label3<-c("Drain Water", "Produce", "Piped Water", "Ocean Water", "Surface Water", "Flood Water", "Public Latrine Surfaces", "Particulate", "Bathing")
    for (j in 1:n.neighb){
      hist(log10(as.numeric(conc[[9*(k.neighb[j]-1)+k.path]])),breaks=seq(0,10,by=1),col="skyblue",ylim=c(0,1),freq=FALSE,yaxt="n",ylab="percent",
           main=paste("Neighborhood ",k.neighb[j],", Sample Type:",label3[k.path],"( N =",length(which(!is.na(conc[[9*(k.neighb[j]-1)+k.path]]))),")"),cex.main=1.3,xlab=expression(paste("log10 ", italic("E. coli"), "concentration (CFU/100mL)")))
      axis(2,at=seq(0,1,0.2),labels=paste(c(0,20,40,60,80,100),"%",sep=""))
    }
  })
  
  output$hist5 <- renderPlot({
    if (input$samtype!=0) return(NULL)
    ec_data<-ec_data()
    conc<-conc()
    n.neighb=ifelse(input$neighb==0, length(unique(as.numeric(ec_data$neighbor))),1)
    n.path=1
    if (n.neighb==1 & input$neighb!=0) {k.neighb=as.numeric(input$neighb)}
    else {k.neighb=sort(unique(as.numeric(ec_data$neighbor)))}
    k.path=5
    nrow=n.path
    ncol=n.neighb
    par(mfrow=c(nrow,ncol))
    par(mar=c(4,2,4,1))
    par(pin=c(6,5))
    label3<-c("Drain Water", "Produce", "Piped Water", "Ocean Water", "Surface Water", "Flood Water", "Public Latrine Surfaces", "Particulate", "Bathing")
    for (j in 1:n.neighb){
      hist(log10(as.numeric(conc[[9*(k.neighb[j]-1)+k.path]])),breaks=seq(0,10,by=1),col="skyblue",ylim=c(0,1),freq=FALSE,yaxt="n",ylab="percent",
           main=paste("Neighborhood ",k.neighb[j],", Sample Type:",label3[k.path],"( N =",length(which(!is.na(conc[[9*(k.neighb[j]-1)+k.path]]))),")"),cex.main=1.3,xlab=expression(paste("log10 ", italic("E. coli"), "concentration (CFU/100mL)")))
      axis(2,at=seq(0,1,0.2),labels=paste(c(0,20,40,60,80,100),"%",sep=""))
    }
  })
  
  output$hist6 <- renderPlot({
    if (input$samtype!=0) return(NULL)
    ec_data<-ec_data()
    conc<-conc()
    n.neighb=ifelse(input$neighb==0, length(unique(as.numeric(ec_data$neighbor))),1)
    n.path=1
    if (n.neighb==1 & input$neighb!=0) {k.neighb=as.numeric(input$neighb)}
    else {k.neighb=sort(unique(as.numeric(ec_data$neighbor)))}
    k.path=6
    nrow=n.path
    ncol=n.neighb
    par(mfrow=c(nrow,ncol))
    par(mar=c(4,2,4,1))
    par(pin=c(6,5))
    label3<-c("Drain Water", "Produce", "Piped Water", "Ocean Water", "Surface Water", "Flood Water", "Public Latrine Surfaces", "Particulate", "Bathing")
    for (j in 1:n.neighb){
      hist(log10(as.numeric(conc[[9*(k.neighb[j]-1)+k.path]])),breaks=seq(0,10,by=1),col="skyblue",ylim=c(0,1),freq=FALSE,yaxt="n",ylab="percent",
           main=paste("Neighborhood ",k.neighb[j],", Sample Type:",label3[k.path],"( N =",length(which(!is.na(conc[[9*(k.neighb[j]-1)+k.path]]))),")"),cex.main=1.3,xlab=expression(paste("log10 ", italic("E. coli"), "concentration (CFU/100mL)")))
      axis(2,at=seq(0,1,0.2),labels=paste(c(0,20,40,60,80,100),"%",sep=""))
    }
  })
  
  output$hist7 <- renderPlot({
    if (input$samtype!=0) return(NULL)
    ec_data<-ec_data()
    conc<-conc()
    n.neighb=ifelse(input$neighb==0, length(unique(as.numeric(ec_data$neighbor))),1)
    n.path=1
    if (n.neighb==1 & input$neighb!=0) {k.neighb=as.numeric(input$neighb)}
    else {k.neighb=sort(unique(as.numeric(ec_data$neighbor)))}
    k.path=7
    nrow=n.path
    ncol=n.neighb
    par(mfrow=c(nrow,ncol))
    par(mar=c(4,2,4,1))
    par(pin=c(6,5))
    label3<-c("Drain Water", "Produce", "Piped Water", "Ocean Water", "Surface Water", "Flood Water", "Public Latrine Surfaces", "Particulate", "Bathing")
    for (j in 1:n.neighb){
      hist(log10(as.numeric(conc[[9*(k.neighb[j]-1)+k.path]])),breaks=seq(0,10,by=1),col="skyblue",ylim=c(0,1),freq=FALSE,yaxt="n",ylab="percent",
           main=paste("Neighborhood ",k.neighb[j],", Sample Type:",label3[k.path],"( N =",length(which(!is.na(conc[[9*(k.neighb[j]-1)+k.path]]))),")"),cex.main=1.3,xlab=expression(paste("log10 ", italic("E. coli"), "concentration (CFU/swab)")))
      axis(2,at=seq(0,1,0.2),labels=paste(c(0,20,40,60,80,100),"%",sep=""))
    }
  })
  
  output$hist8 <- renderPlot({
    if (input$samtype!=0) return(NULL)
    ec_data<-ec_data()
    conc<-conc()
    n.neighb=ifelse(input$neighb==0, length(unique(as.numeric(ec_data$neighbor))),1)
    n.path=1
    if (n.neighb==1 & input$neighb!=0) {k.neighb=as.numeric(input$neighb)}
    else {k.neighb=sort(unique(as.numeric(ec_data$neighbor)))}
    k.path=8
    nrow=n.path
    ncol=n.neighb
    par(mfrow=c(nrow,ncol))
    par(mar=c(4,2,4,1))
    par(pin=c(6,5))
    label3<-c("Drain Water", "Produce", "Piped Water", "Ocean Water", "Surface Water", "Flood Water", "Public Latrine Surfaces", "Particulate", "Bathing")
    for (j in 1:n.neighb){
      hist(log10(as.numeric(conc[[9*(k.neighb[j]-1)+k.path]])),breaks=seq(0,10,by=1),col="skyblue",ylim=c(0,1),freq=FALSE,yaxt="n",ylab="percent",
           main=paste("Neighborhood ",k.neighb[j],", Sample Type:",label3[k.path],"( N =",length(which(!is.na(conc[[9*(k.neighb[j]-1)+k.path]]))),")"),cex.main=1.3,xlab=expression(paste("log10 ", italic("E. coli"), "concentration (CFU/gram)")))
      axis(2,at=seq(0,1,0.2),labels=paste(c(0,20,40,60,80,100),"%",sep=""))
    }
  })
  
  output$hist9 <- renderPlot({
    if (input$samtype!=0) return(NULL)
    ec_data<-ec_data()
    conc<-conc()
    n.neighb=ifelse(input$neighb==0, length(unique(as.numeric(ec_data$neighbor))),1)
    n.path=1
    if (n.neighb==1 & input$neighb!=0) {k.neighb=as.numeric(input$neighb)}
    else {k.neighb=sort(unique(as.numeric(ec_data$neighbor)))}
    k.path=9
    nrow=n.path
    ncol=n.neighb
    par(mfrow=c(nrow,ncol))
    par(mar=c(4,2,4,1))
    par(pin=c(6,5))
    label3<-c("Drain Water", "Produce", "Piped Water", "Ocean Water", "Surface Water", "Flood Water", "Public Latrine Surfaces", "Particulate", "Bathing")
    for (j in 1:n.neighb){
      hist(log10(as.numeric(conc[[9*(k.neighb[j]-1)+k.path]])),breaks=seq(0,10,by=1),col="skyblue",ylim=c(0,1),freq=FALSE,yaxt="n",ylab="percent",
           main=paste("Neighborhood ",k.neighb[j],", Sample Type:",label3[k.path],"( N =",length(which(!is.na(conc[[9*(k.neighb[j]-1)+k.path]]))),")"),cex.main=1.3,xlab=expression(paste("log10 ", italic("E. coli"), "concentration (CFU/100mL)")))
      axis(2,at=seq(0,1,0.2),labels=paste(c(0,20,40,60,80,100),"%",sep=""))
    }
  })
  
  output$ps_plot1 <- renderPlot({
    #if (input$samtype!=0 & input$samtype!=1) return(NULL)
    if (input$surtype==0){
      be_data1<-household_data()
      be_data2<-school_data()
      be_data3<-community_data()
      num.neighb<-sort(unique(c(as.numeric(be_data1$neighbor),as.numeric(be_data2$neighbor),as.numeric(be_data3$neighbor))))
    }
    if (input$surtype==1){
      be_data1<-household_data()
      num.neighb<-sort(unique(as.numeric(be_data1$neighbor)))
    }
    if (input$surtype==2){
      be_data2<-school_data()
      num.neighb<-sort(unique(as.numeric(be_data2$neighbor)))
    }
    if (input$surtype==3){
      be_data3<-community_data()
      num.neighb<-sort(unique(as.numeric(be_data3$neighbor)))
    }
    freq <- convert_to_old_freq(ps.freq())
    ec_data<-ec_data()
    conc<-conc()
    #be_data<-be_data()
    #freq<-ps.frq()
    label_path<-c("Drain Water", "Produce", "Piped Water", "Ocean Water", "Surface Water", "Flood Water", "Public Latrine Surfaces", "Particulate", "Bathing")
    label_age<-c("Adults","Children")
    
    n.neighb=ifelse(input$neighb==0, length(num.neighb),1)
    n.path=1
    n.age=ifelse(input$ad_ch==0, 2, 1)
    if (n.neighb==1 & input$neighb!=0) {k.neighb=as.numeric(input$neighb)}
    else {k.neighb=num.neighb}
    if (input$samtype!=0){k.path=as.numeric(input$samtype)} 
    else{k.path=1}
    if (n.age==1 & input$ad_ch!=0) {k.age=as.numeric(input$ad_ch)}
    else {k.age=c(1,2)}
    
    source("./model/PS_Plot.r")
    ".RNG.state" <- c(19900, 14957, 25769)
    nburn <- 1000;
    niter <- 10000;
    thin <- 1;
    calcul <- paste(nburn,niter,thin,sep="|");
    
    # environmental samples
    tomonitor <- c("mu","sigma");
    mu<-array(NA,c(9,n.neighb))
    sigma<-array(NA,c(9,n.neighb)) 
    for (j in 1:n.neighb){
      log_ec<-log10(as.numeric(conc[[9*(k.neighb[j]-1)+k.path]]))
      env_data<-list(lnconc=log_ec,N=length(log_ec))
      
      modelpos <- jags.model(file="./model/env_model.jags",data=env_data,n.chains=3);
      update(modelpos,n.burn=nburn);
      env_mcmcpos <- coda.samples(modelpos,tomonitor,n.iter=niter,thin=thin);
      #Bayesian estimators of mu and sigma
      mu[k.path,j]<-summary(env_mcmcpos)$statistics[1,1];
      sigma[k.path,j]<-summary(env_mcmcpos)$statistics[2,1];
    }
    
    cutpoint<-c(0,5,10) #cut point changed
    tomonitor <- c("p","r");
    p<-array(NA,c(9,n.neighb,2))
    r<-array(NA,c(9,n.neighb,2)) 
    for (j in 1:n.neighb){
      for (k in 1:n.age){
        freq_be0=freq[[14*(k.neighb[j]-1)+2*(k.path-1)+k.age[k]]]
        freq_be<-freq_be0[which(freq_be0>=0)]
        #initial values
        init_be<-as.numeric(rep(0,length(freq_be)))
        init_be[which(freq_be==1)]<-1
        init_be[which(freq_be==2)]<-2
        init_be[which(freq_be==3)]<-3
        
        init_freq_be<-as.numeric(rep(NA,length(freq_be)))
        init_freq_be[which(freq_be==1)]<-2
        init_freq_be[which(freq_be==2)]<-7
        init_freq_be[which(freq_be==3)]<-12
        
        be_data<-list(select=freq_be,N=length(freq_be),cut=cutpoint)
        init<-list(freq=init_freq_be,r=1,p=0.2)
        
        modelpos <- jags.model(file="./model/be_model.jags",data=be_data,n.chains=3,inits=init);
        update(modelpos,n.burn=nburn);
        be_mcmcpos <- coda.samples(modelpos,tomonitor,n.iter=niter,thin=thin);
        #Bayesian estimators of p and r
        p[k.path,j,k]<-summary(be_mcmcpos)$statistics[1,1]
        r[k.path,j,k]<-summary(be_mcmcpos)$statistics[2,1]
      }
    }
    nrow=n.path
    ncol=n.age*n.neighb
    par(mfrow=c(nrow,ncol))
    par(mar=c(2,3,5.5,2))
    e<-array(NA,c(9,n.neighb,1000)) 
    f<-array(NA,c(9,n.neighb,2,1000)) 
    risk<-array(NA,c(9,n.neighb,2,1000))
    n<-array(NA,c(9,n.neighb,2))
    dose<-array(NA,c(9,n.neighb,2))
    intake<-array(c(0.0006,1,10.43,0.0154,0.037,0.0006,0.034,NA,0.0499,
                    0.01,0.5,4.14,0.2042,0.2042,0.01,0.034,NA,0.09975),c(9,2)) #need to input this information!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    for (j in 1:n.neighb){
      for (k in 1:n.age){
        for (m in 1:1000){
          e[k.path,j,m]<-rnorm(1,mu[k.path,j],sigma[k.path,j])
          f[k.path,j,k,m]<-rnbinom(1,size=r[k.path,j,k],prob=p[k.path,j,k])
          risk[k.path,j,k,m]<-f[k.path,j,k,m]*(10^e[k.path,j,m])*intake[k.path,k]
        }
        
        non0 <- function(mc){
          tmp <- mc; tmp[!(tmp>0)] <- NA; return(tmp);
        }
        
        n[k.path,j,k]<-(1-length(which(f[k.path,j,k,]==0))/1000)*100;
        dose[k.path,j,k]<-log10(mean(non0(risk[k.path,j,k,]),na.rm=TRUE))
        
        PS_Plot(paste("Neighborhood: ",k.neighb[j],"\n",label_path[k.path],label_age[k.age[k]]),n[k.path,j,k],dose[k.path,j,k])
      }
    }
  })
  
  output$ps_plot2 <- renderPlot({
    if (input$samtype!=0) return(NULL)
    if (input$surtype==0){
      be_data1<-household_data()
      be_data2<-school_data()
      be_data3<-community_data()
      freq<-ps.frq0()
      num.neighb<-sort(unique(c(as.numeric(be_data1$neighbor),as.numeric(be_data2$neighbor),as.numeric(be_data3$neighbor))))
    }
    if (input$surtype==1){
      be_data1<-household_data()
      freq<-ps.frq1()
      num.neighb<-sort(unique(as.numeric(be_data1$neighbor)))
    }
    if (input$surtype==2){
      be_data2<-school_data()
      freq<-ps.frq2()
      num.neighb<-sort(unique(as.numeric(be_data2$neighbor)))
    }
    if (input$surtype==3){
      be_data3<-community_data()
      freq<-ps.frq3()
      num.neighb<-sort(unique(as.numeric(be_data3$neighbor)))
    }
    ec_data<-ec_data()
    conc<-conc()
    #be_data<-be_data()
    #freq<-ps.frq()
    label_path<-c("Drain Water", "Produce", "Piped Water", "Ocean Water", "Surface Water", "Flood Water", "Public Latrine Surfaces", "Particulate", "Bathing")
    label_age<-c("Adults","Children")
    
    n.neighb=ifelse(input$neighb==0, length(num.neighb),1)
    n.path=1
    n.age=ifelse(input$ad_ch==0, 2, 1)
    if (n.neighb==1 & input$neighb!=0) {k.neighb=as.numeric(input$neighb)}
    else {k.neighb=num.neighb}
    k.path=2
    if (n.age==1 & input$ad_ch!=0) {k.age=as.numeric(input$ad_ch)}
    else {k.age=c(1,2)}
    
    source("./model/PS_Plot.r")
    ".RNG.state" <- c(19900, 14957, 25769)
    nburn <- 1000;
    niter <- 10000;
    thin <- 1;
    calcul <- paste(nburn,niter,thin,sep="|");
    
    # environmental samples
    tomonitor <- c("mu","sigma");
    mu<-array(NA,c(9,n.neighb))
    sigma<-array(NA,c(9,n.neighb)) 
    for (j in 1:n.neighb){
      log_ec<-log10(as.numeric(conc[[9*(k.neighb[j]-1)+k.path]]))
      env_data<-list(lnconc=log_ec,N=length(log_ec))
      
      modelpos <- jags.model(file="./model/env_model.jags",data=env_data,n.chains=3);
      update(modelpos,n.burn=nburn);
      env_mcmcpos <- coda.samples(modelpos,tomonitor,n.iter=niter,thin=thin);
      #Bayesian estimators of mu and sigma
      mu[k.path,j]<-summary(env_mcmcpos)$statistics[1,1];
      sigma[k.path,j]<-summary(env_mcmcpos)$statistics[2,1];
    }
    
    cutpoint<-c(0,5,10) #cut point changed
    tomonitor <- c("p","r");
    p<-array(NA,c(9,n.neighb,2))
    r<-array(NA,c(9,n.neighb,2)) 
    for (j in 1:n.neighb){
      for (k in 1:n.age){
        freq_be0=freq[[14*(k.neighb[j]-1)+2*(k.path-1)+k.age[k]]]
        freq_be<-freq_be0[which(freq_be0>=0)]
        #initial values
        init_be<-as.numeric(rep(0,length(freq_be)))
        init_be[which(freq_be==1)]<-1
        init_be[which(freq_be==2)]<-2
        init_be[which(freq_be==3)]<-3
        
        init_freq_be<-as.numeric(rep(NA,length(freq_be)))
        init_freq_be[which(freq_be==1)]<-2
        init_freq_be[which(freq_be==2)]<-7
        init_freq_be[which(freq_be==3)]<-12
        
        be_data<-list(select=freq_be,N=length(freq_be),cut=cutpoint)
        init<-list(freq=init_freq_be,r=1,p=0.2)
        
        modelpos <- jags.model(file="./model/be_model.jags",data=be_data,n.chains=3,inits=init);
        update(modelpos,n.burn=nburn);
        be_mcmcpos <- coda.samples(modelpos,tomonitor,n.iter=niter,thin=thin);
        #Bayesian estimators of p and r
        p[k.path,j,k]<-summary(be_mcmcpos)$statistics[1,1]
        r[k.path,j,k]<-summary(be_mcmcpos)$statistics[2,1]
      }
    }
    nrow=n.path
    ncol=n.age*n.neighb
    par(mfrow=c(nrow,ncol))
    par(mar=c(2,3,5.5,2))
    e<-array(NA,c(9,n.neighb,1000)) 
    f<-array(NA,c(9,n.neighb,2,1000)) 
    risk<-array(NA,c(9,n.neighb,2,1000))
    n<-array(NA,c(9,n.neighb,2))
    dose<-array(NA,c(9,n.neighb,2))
    intake<-array(c(0.0006,1,10.43,0.0154,0.037,0.0006,0.034,NA,0.0499,
                    0.01,0.5,4.14,0.2042,0.2042,0.01,0.034,NA,0.09975),c(9,2)) #need to input this information!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    for (j in 1:n.neighb){
      for (k in 1:n.age){
        for (m in 1:1000){
          e[k.path,j,m]<-rnorm(1,mu[k.path,j],sigma[k.path,j])
          f[k.path,j,k,m]<-rnbinom(1,size=r[k.path,j,k],prob=p[k.path,j,k])
          risk[k.path,j,k,m]<-f[k.path,j,k,m]*(10^e[k.path,j,m])*intake[k.path,k]
        }
        
        non0 <- function(mc){
          tmp <- mc; tmp[!(tmp>0)] <- NA; return(tmp);
        }
        
        n[k.path,j,k]<-(1-length(which(f[k.path,j,k,]==0))/1000)*100;
        dose[k.path,j,k]<-log10(mean(non0(risk[k.path,j,k,]),na.rm=TRUE))
        
        PS_Plot(paste("Neighborhood: ",k.neighb[j],"\n",label_path[k.path],label_age[k.age[k]]),n[k.path,j,k],dose[k.path,j,k])
      }
    }
  })
  
  output$ps_plot3 <- renderPlot({
    if (input$samtype!=0) return(NULL)
    if (input$surtype==0){
      be_data1<-household_data()
      be_data2<-school_data()
      be_data3<-community_data()
      freq<-ps.frq0()
      num.neighb<-sort(unique(c(as.numeric(be_data1$neighbor),as.numeric(be_data2$neighbor),as.numeric(be_data3$neighbor))))
    }
    if (input$surtype==1){
      be_data1<-household_data()
      freq<-ps.frq1()
      num.neighb<-sort(unique(as.numeric(be_data1$neighbor)))
    }
    if (input$surtype==2){
      be_data2<-school_data()
      freq<-ps.frq2()
      num.neighb<-sort(unique(as.numeric(be_data2$neighbor)))
    }
    if (input$surtype==3){
      be_data3<-community_data()
      freq<-ps.frq3()
      num.neighb<-sort(unique(as.numeric(be_data3$neighbor)))
    }
    ec_data<-ec_data()
    conc<-conc()
    #be_data<-be_data()
    #freq<-ps.frq()
    label_path<-c("Drain Water", "Produce", "Piped Water", "Ocean Water", "Surface Water", "Flood Water", "Public Latrine Surfaces", "Particulate", "Bathing")
    label_age<-c("Adults","Children")
    
    n.neighb=ifelse(input$neighb==0, length(num.neighb),1)
    n.path=1
    n.age=ifelse(input$ad_ch==0, 2, 1)
    if (n.neighb==1 & input$neighb!=0) {k.neighb=as.numeric(input$neighb)}
    else {k.neighb=num.neighb}
    k.path=3
    if (n.age==1 & input$ad_ch!=0) {k.age=as.numeric(input$ad_ch)}
    else {k.age=c(1,2)}
    
    source("./model/PS_Plot.r")
    ".RNG.state" <- c(19900, 14957, 25769)
    nburn <- 1000;
    niter <- 10000;
    thin <- 1;
    calcul <- paste(nburn,niter,thin,sep="|");
    
    # environmental samples
    tomonitor <- c("mu","sigma");
    mu<-array(NA,c(9,n.neighb))
    sigma<-array(NA,c(9,n.neighb)) 
    for (j in 1:n.neighb){
      log_ec<-log10(as.numeric(conc[[9*(k.neighb[j]-1)+k.path]]))
      env_data<-list(lnconc=log_ec,N=length(log_ec))
      
      modelpos <- jags.model(file="./model/env_model.jags",data=env_data,n.chains=3);
      update(modelpos,n.burn=nburn);
      env_mcmcpos <- coda.samples(modelpos,tomonitor,n.iter=niter,thin=thin);
      #Bayesian estimators of mu and sigma
      mu[k.path,j]<-summary(env_mcmcpos)$statistics[1,1];
      sigma[k.path,j]<-summary(env_mcmcpos)$statistics[2,1];
    }
    
    cutpoint<-c(0,5,10) #cut point changed
    tomonitor <- c("p","r");
    p<-array(NA,c(9,n.neighb,2))
    r<-array(NA,c(9,n.neighb,2)) 
    for (j in 1:n.neighb){
      for (k in 1:n.age){
        freq_be0=freq[[14*(k.neighb[j]-1)+2*(k.path-1)+k.age[k]]]
        freq_be<-freq_be0[which(freq_be0>=0)]
        #initial values
        init_be<-as.numeric(rep(0,length(freq_be)))
        init_be[which(freq_be==1)]<-1
        init_be[which(freq_be==2)]<-2
        init_be[which(freq_be==3)]<-3
        
        init_freq_be<-as.numeric(rep(NA,length(freq_be)))
        init_freq_be[which(freq_be==1)]<-2
        init_freq_be[which(freq_be==2)]<-7
        init_freq_be[which(freq_be==3)]<-12
        
        be_data<-list(select=freq_be,N=length(freq_be),cut=cutpoint)
        init<-list(freq=init_freq_be,r=1,p=0.2)
        
        modelpos <- jags.model(file="./model/be_model.jags",data=be_data,n.chains=3,inits=init);
        update(modelpos,n.burn=nburn);
        be_mcmcpos <- coda.samples(modelpos,tomonitor,n.iter=niter,thin=thin);
        #Bayesian estimators of p and r
        p[k.path,j,k]<-summary(be_mcmcpos)$statistics[1,1]
        r[k.path,j,k]<-summary(be_mcmcpos)$statistics[2,1]
      }
    }
    nrow=n.path
    ncol=n.age*n.neighb
    par(mfrow=c(nrow,ncol))
    par(mar=c(2,3,5.5,2))
    e<-array(NA,c(9,n.neighb,1000)) 
    f<-array(NA,c(9,n.neighb,2,1000)) 
    risk<-array(NA,c(9,n.neighb,2,1000))
    n<-array(NA,c(9,n.neighb,2))
    dose<-array(NA,c(9,n.neighb,2))
    intake<-array(c(0.0006,1,10.43,0.0154,0.037,0.0006,0.034,NA,0.0499,
                    0.01,0.5,4.14,0.2042,0.2042,0.01,0.034,NA,0.09975),c(9,2)) #need to input this information!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    for (j in 1:n.neighb){
      for (k in 1:n.age){
        for (m in 1:1000){
          e[k.path,j,m]<-rnorm(1,mu[k.path,j],sigma[k.path,j])
          f[k.path,j,k,m]<-rnbinom(1,size=r[k.path,j,k],prob=p[k.path,j,k])
          risk[k.path,j,k,m]<-f[k.path,j,k,m]*(10^e[k.path,j,m])*intake[k.path,k]
        }
        
        non0 <- function(mc){
          tmp <- mc; tmp[!(tmp>0)] <- NA; return(tmp);
        }
        
        n[k.path,j,k]<-(1-length(which(f[k.path,j,k,]==0))/1000)*100;
        dose[k.path,j,k]<-log10(mean(non0(risk[k.path,j,k,]),na.rm=TRUE))
        
        PS_Plot(paste("Neighborhood: ",k.neighb[j],"\n",label_path[k.path],label_age[k.age[k]]),n[k.path,j,k],dose[k.path,j,k])
      }
    }
  })
  
  output$ps_plot4 <- renderPlot({
    if (input$samtype!=0) return(NULL)
    if (input$surtype==0){
      be_data1<-household_data()
      be_data2<-school_data()
      be_data3<-community_data()
      freq<-ps.frq0()
      num.neighb<-sort(unique(c(as.numeric(be_data1$neighbor),as.numeric(be_data2$neighbor),as.numeric(be_data3$neighbor))))
    }
    if (input$surtype==1){
      be_data1<-household_data()
      freq<-ps.frq1()
      num.neighb<-sort(unique(as.numeric(be_data1$neighbor)))
    }
    if (input$surtype==2){
      be_data2<-school_data()
      freq<-ps.frq2()
      num.neighb<-sort(unique(as.numeric(be_data2$neighbor)))
    }
    if (input$surtype==3){
      be_data3<-community_data()
      freq<-ps.frq3()
      num.neighb<-sort(unique(as.numeric(be_data3$neighbor)))
    }
    ec_data<-ec_data()
    conc<-conc()
    #be_data<-be_data()
    #freq<-ps.frq()
    label_path<-c("Drain Water", "Produce", "Piped Water", "Ocean Water", "Surface Water", "Flood Water", "Public Latrine Surfaces", "Particulate", "Bathing")
    label_age<-c("Adults","Children")
    
    n.neighb=ifelse(input$neighb==0, length(num.neighb),1)
    n.path=1
    n.age=ifelse(input$ad_ch==0, 2, 1)
    if (n.neighb==1 & input$neighb!=0) {k.neighb=as.numeric(input$neighb)}
    else {k.neighb=num.neighb}
    k.path=4
    if (n.age==1 & input$ad_ch!=0) {k.age=as.numeric(input$ad_ch)}
    else {k.age=c(1,2)}
    
    source("./model/PS_Plot.r")
    ".RNG.state" <- c(19900, 14957, 25769)
    nburn <- 1000;
    niter <- 10000;
    thin <- 1;
    calcul <- paste(nburn,niter,thin,sep="|");
    
    # environmental samples
    tomonitor <- c("mu","sigma");
    mu<-array(NA,c(9,n.neighb))
    sigma<-array(NA,c(9,n.neighb)) 
    for (j in 1:n.neighb){
      log_ec<-log10(as.numeric(conc[[9*(k.neighb[j]-1)+k.path]]))
      env_data<-list(lnconc=log_ec,N=length(log_ec))
      
      modelpos <- jags.model(file="./model/env_model.jags",data=env_data,n.chains=3);
      update(modelpos,n.burn=nburn);
      env_mcmcpos <- coda.samples(modelpos,tomonitor,n.iter=niter,thin=thin);
      #Bayesian estimators of mu and sigma
      mu[k.path,j]<-summary(env_mcmcpos)$statistics[1,1];
      sigma[k.path,j]<-summary(env_mcmcpos)$statistics[2,1];
    }
    
    cutpoint<-c(0,5,10) #cut point changed
    tomonitor <- c("p","r");
    p<-array(NA,c(9,n.neighb,2))
    r<-array(NA,c(9,n.neighb,2)) 
    for (j in 1:n.neighb){
      for (k in 1:n.age){
        freq_be0=freq[[14*(k.neighb[j]-1)+2*(k.path-1)+k.age[k]]]
        freq_be<-freq_be0[which(freq_be0>=0)]
        #initial values
        init_be<-as.numeric(rep(0,length(freq_be)))
        init_be[which(freq_be==1)]<-1
        init_be[which(freq_be==2)]<-2
        init_be[which(freq_be==3)]<-3
        
        init_freq_be<-as.numeric(rep(NA,length(freq_be)))
        init_freq_be[which(freq_be==1)]<-2
        init_freq_be[which(freq_be==2)]<-7
        init_freq_be[which(freq_be==3)]<-12
        
        be_data<-list(select=freq_be,N=length(freq_be),cut=cutpoint)
        init<-list(freq=init_freq_be,r=1,p=0.2)
        
        modelpos <- jags.model(file="./model/be_model.jags",data=be_data,n.chains=3,inits=init);
        update(modelpos,n.burn=nburn);
        be_mcmcpos <- coda.samples(modelpos,tomonitor,n.iter=niter,thin=thin);
        #Bayesian estimators of p and r
        p[k.path,j,k]<-summary(be_mcmcpos)$statistics[1,1]
        r[k.path,j,k]<-summary(be_mcmcpos)$statistics[2,1]
      }
    }
    nrow=n.path
    ncol=n.age*n.neighb
    par(mfrow=c(nrow,ncol))
    par(mar=c(2,3,5.5,2))
    e<-array(NA,c(9,n.neighb,1000)) 
    f<-array(NA,c(9,n.neighb,2,1000)) 
    risk<-array(NA,c(9,n.neighb,2,1000))
    n<-array(NA,c(9,n.neighb,2))
    dose<-array(NA,c(9,n.neighb,2))
    intake<-array(c(0.0006,1,10.43,0.0154,0.037,0.0006,0.034,NA,0.0499,
                    0.01,0.5,4.14,0.2042,0.2042,0.01,0.034,NA,0.09975),c(9,2)) #need to input this information!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    for (j in 1:n.neighb){
      for (k in 1:n.age){
        for (m in 1:1000){
          e[k.path,j,m]<-rnorm(1,mu[k.path,j],sigma[k.path,j])
          f[k.path,j,k,m]<-rnbinom(1,size=r[k.path,j,k],prob=p[k.path,j,k])
          risk[k.path,j,k,m]<-f[k.path,j,k,m]*(10^e[k.path,j,m])*intake[k.path,k]
        }
        
        non0 <- function(mc){
          tmp <- mc; tmp[!(tmp>0)] <- NA; return(tmp);
        }
        
        n[k.path,j,k]<-(1-length(which(f[k.path,j,k,]==0))/1000)*100;
        dose[k.path,j,k]<-log10(mean(non0(risk[k.path,j,k,]),na.rm=TRUE))
        
        PS_Plot(paste("Neighborhood: ",k.neighb[j],"\n",label_path[k.path],label_age[k.age[k]]),n[k.path,j,k],dose[k.path,j,k])
      }
    }
  })
  
  output$ps_plot5 <- renderPlot({
    if (input$samtype!=0) return(NULL)
    if (input$surtype==0){
      be_data1<-household_data()
      be_data2<-school_data()
      be_data3<-community_data()
      freq<-ps.frq0()
      num.neighb<-sort(unique(c(as.numeric(be_data1$neighbor),as.numeric(be_data2$neighbor),as.numeric(be_data3$neighbor))))
    }
    if (input$surtype==1){
      be_data1<-household_data()
      freq<-ps.frq1()
      num.neighb<-sort(unique(as.numeric(be_data1$neighbor)))
    }
    if (input$surtype==2){
      be_data2<-school_data()
      freq<-ps.frq2()
      num.neighb<-sort(unique(as.numeric(be_data2$neighbor)))
    }
    if (input$surtype==3){
      be_data3<-community_data()
      freq<-ps.frq3()
      num.neighb<-sort(unique(as.numeric(be_data3$neighbor)))
    }
    ec_data<-ec_data()
    conc<-conc()
    #be_data<-be_data()
    #freq<-ps.frq()
    label_path<-c("Drain Water", "Produce", "Piped Water", "Ocean Water", "Surface Water", "Flood Water", "Public Latrine Surfaces", "Particulate", "Bathing")
    label_age<-c("Adults","Children")
    
    n.neighb=ifelse(input$neighb==0, length(num.neighb),1)
    n.path=1
    n.age=ifelse(input$ad_ch==0, 2, 1)
    if (n.neighb==1 & input$neighb!=0) {k.neighb=as.numeric(input$neighb)}
    else {k.neighb=num.neighb}
    k.path=5
    if (n.age==1 & input$ad_ch!=0) {k.age=as.numeric(input$ad_ch)}
    else {k.age=c(1,2)}
    
    source("./model/PS_Plot.r")
    ".RNG.state" <- c(19900, 14957, 25769)
    nburn <- 1000;
    niter <- 10000;
    thin <- 1;
    calcul <- paste(nburn,niter,thin,sep="|");
    
    # environmental samples
    tomonitor <- c("mu","sigma");
    mu<-array(NA,c(9,n.neighb))
    sigma<-array(NA,c(9,n.neighb)) 
    for (j in 1:n.neighb){
      log_ec<-log10(as.numeric(conc[[9*(k.neighb[j]-1)+k.path]]))
      env_data<-list(lnconc=log_ec,N=length(log_ec))
      
      modelpos <- jags.model(file="./model/env_model.jags",data=env_data,n.chains=3);
      update(modelpos,n.burn=nburn);
      env_mcmcpos <- coda.samples(modelpos,tomonitor,n.iter=niter,thin=thin);
      #Bayesian estimators of mu and sigma
      mu[k.path,j]<-summary(env_mcmcpos)$statistics[1,1];
      sigma[k.path,j]<-summary(env_mcmcpos)$statistics[2,1];
    }
    
    cutpoint<-c(0,5,10) #cut point changed
    tomonitor <- c("p","r");
    p<-array(NA,c(9,n.neighb,2))
    r<-array(NA,c(9,n.neighb,2)) 
    for (j in 1:n.neighb){
      for (k in 1:n.age){
        freq_be0=freq[[14*(k.neighb[j]-1)+2*(k.path-1)+k.age[k]]]
        freq_be<-freq_be0[which(freq_be0>=0)]
        #initial values
        init_be<-as.numeric(rep(0,length(freq_be)))
        init_be[which(freq_be==1)]<-1
        init_be[which(freq_be==2)]<-2
        init_be[which(freq_be==3)]<-3
        
        init_freq_be<-as.numeric(rep(NA,length(freq_be)))
        init_freq_be[which(freq_be==1)]<-2
        init_freq_be[which(freq_be==2)]<-7
        init_freq_be[which(freq_be==3)]<-12
        
        be_data<-list(select=freq_be,N=length(freq_be),cut=cutpoint)
        init<-list(freq=init_freq_be,r=1,p=0.2)
        
        modelpos <- jags.model(file="./model/be_model.jags",data=be_data,n.chains=3,inits=init);
        update(modelpos,n.burn=nburn);
        be_mcmcpos <- coda.samples(modelpos,tomonitor,n.iter=niter,thin=thin);
        #Bayesian estimators of p and r
        p[k.path,j,k]<-summary(be_mcmcpos)$statistics[1,1]
        r[k.path,j,k]<-summary(be_mcmcpos)$statistics[2,1]
      }
    }
    nrow=n.path
    ncol=n.age*n.neighb
    par(mfrow=c(nrow,ncol))
    par(mar=c(2,3,5.5,2))
    e<-array(NA,c(9,n.neighb,1000)) 
    f<-array(NA,c(9,n.neighb,2,1000)) 
    risk<-array(NA,c(9,n.neighb,2,1000))
    n<-array(NA,c(9,n.neighb,2))
    dose<-array(NA,c(9,n.neighb,2))
    intake<-array(c(0.0006,1,10.43,0.0154,0.037,0.0006,0.034,NA,0.0499,
                    0.01,0.5,4.14,0.2042,0.2042,0.01,0.034,NA,0.09975),c(9,2)) #need to input this information!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    for (j in 1:n.neighb){
      for (k in 1:n.age){
        for (m in 1:1000){
          e[k.path,j,m]<-rnorm(1,mu[k.path,j],sigma[k.path,j])
          f[k.path,j,k,m]<-rnbinom(1,size=r[k.path,j,k],prob=p[k.path,j,k])
          risk[k.path,j,k,m]<-f[k.path,j,k,m]*(10^e[k.path,j,m])*intake[k.path,k]
        }
        
        non0 <- function(mc){
          tmp <- mc; tmp[!(tmp>0)] <- NA; return(tmp);
        }
        
        n[k.path,j,k]<-(1-length(which(f[k.path,j,k,]==0))/1000)*100;
        dose[k.path,j,k]<-log10(mean(non0(risk[k.path,j,k,]),na.rm=TRUE))
        
        PS_Plot(paste("Neighborhood: ",k.neighb[j],"\n",label_path[k.path],label_age[k.age[k]]),n[k.path,j,k],dose[k.path,j,k])
      }
    }
  })
  
  output$ps_plot6 <- renderPlot({
    if (input$samtype!=0) return(NULL)
    if (input$surtype==0){
      be_data1<-household_data()
      be_data2<-school_data()
      be_data3<-community_data()
      freq<-ps.frq0()
      num.neighb<-sort(unique(c(as.numeric(be_data1$neighbor),as.numeric(be_data2$neighbor),as.numeric(be_data3$neighbor))))
    }
    if (input$surtype==1){
      be_data1<-household_data()
      freq<-ps.frq1()
      num.neighb<-sort(unique(as.numeric(be_data1$neighbor)))
    }
    if (input$surtype==2){
      be_data2<-school_data()
      freq<-ps.frq2()
      num.neighb<-sort(unique(as.numeric(be_data2$neighbor)))
    }
    if (input$surtype==3){
      be_data3<-community_data()
      freq<-ps.frq3()
      num.neighb<-sort(unique(as.numeric(be_data3$neighbor)))
    }
    ec_data<-ec_data()
    conc<-conc()
    #be_data<-be_data()
    #freq<-ps.frq()
    label_path<-c("Drain Water", "Produce", "Piped Water", "Ocean Water", "Surface Water", "Flood Water", "Public Latrine Surfaces", "Particulate", "Bathing")
    label_age<-c("Adults","Children")
    
    n.neighb=ifelse(input$neighb==0, length(num.neighb),1)
    n.path=1
    n.age=ifelse(input$ad_ch==0, 2, 1)
    if (n.neighb==1 & input$neighb!=0) {k.neighb=as.numeric(input$neighb)}
    else {k.neighb=num.neighb}
    k.path=6
    if (n.age==1 & input$ad_ch!=0) {k.age=as.numeric(input$ad_ch)}
    else {k.age=c(1,2)}
    
    source("./model/PS_Plot.r")
    ".RNG.state" <- c(19900, 14957, 25769)
    nburn <- 1000;
    niter <- 10000;
    thin <- 1;
    calcul <- paste(nburn,niter,thin,sep="|");
    
    # environmental samples
    tomonitor <- c("mu","sigma");
    mu<-array(NA,c(9,n.neighb))
    sigma<-array(NA,c(9,n.neighb)) 
    for (j in 1:n.neighb){
      log_ec<-log10(as.numeric(conc[[9*(k.neighb[j]-1)+k.path]]))
      env_data<-list(lnconc=log_ec,N=length(log_ec))
      
      modelpos <- jags.model(file="./model/env_model.jags",data=env_data,n.chains=3);
      update(modelpos,n.burn=nburn);
      env_mcmcpos <- coda.samples(modelpos,tomonitor,n.iter=niter,thin=thin);
      #Bayesian estimators of mu and sigma
      mu[k.path,j]<-summary(env_mcmcpos)$statistics[1,1];
      sigma[k.path,j]<-summary(env_mcmcpos)$statistics[2,1];
    }
    
    cutpoint<-c(0,5,10) #cut point changed
    tomonitor <- c("p","r");
    p<-array(NA,c(9,n.neighb,2))
    r<-array(NA,c(9,n.neighb,2)) 
    for (j in 1:n.neighb){
      for (k in 1:n.age){
        freq_be0=freq[[14*(k.neighb[j]-1)+2*(k.path-1)+k.age[k]]]
        freq_be<-freq_be0[which(freq_be0>=0)]
        #initial values
        init_be<-as.numeric(rep(0,length(freq_be)))
        init_be[which(freq_be==1)]<-1
        init_be[which(freq_be==2)]<-2
        init_be[which(freq_be==3)]<-3
        
        init_freq_be<-as.numeric(rep(NA,length(freq_be)))
        init_freq_be[which(freq_be==1)]<-2
        init_freq_be[which(freq_be==2)]<-7
        init_freq_be[which(freq_be==3)]<-12
        
        be_data<-list(select=freq_be,N=length(freq_be),cut=cutpoint)
        init<-list(freq=init_freq_be,r=1,p=0.2)
        
        modelpos <- jags.model(file="./model/be_model.jags",data=be_data,n.chains=3,inits=init);
        update(modelpos,n.burn=nburn);
        be_mcmcpos <- coda.samples(modelpos,tomonitor,n.iter=niter,thin=thin);
        #Bayesian estimators of p and r
        p[k.path,j,k]<-summary(be_mcmcpos)$statistics[1,1]
        r[k.path,j,k]<-summary(be_mcmcpos)$statistics[2,1]
      }
    }
    nrow=n.path
    ncol=n.age*n.neighb
    par(mfrow=c(nrow,ncol))
    par(mar=c(2,3,5.5,2))
    e<-array(NA,c(9,n.neighb,1000)) 
    f<-array(NA,c(9,n.neighb,2,1000)) 
    risk<-array(NA,c(9,n.neighb,2,1000))
    n<-array(NA,c(9,n.neighb,2))
    dose<-array(NA,c(9,n.neighb,2))
    intake<-array(c(0.0006,1,10.43,0.0154,0.037,0.0006,0.034,NA,0.0499,
                    0.01,0.5,4.14,0.2042,0.2042,0.01,0.034,NA,0.09975),c(9,2)) #need to input this information!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    for (j in 1:n.neighb){
      for (k in 1:n.age){
        for (m in 1:1000){
          e[k.path,j,m]<-rnorm(1,mu[k.path,j],sigma[k.path,j])
          f[k.path,j,k,m]<-rnbinom(1,size=r[k.path,j,k],prob=p[k.path,j,k])
          risk[k.path,j,k,m]<-f[k.path,j,k,m]*(10^e[k.path,j,m])*intake[k.path,k]
        }
        
        non0 <- function(mc){
          tmp <- mc; tmp[!(tmp>0)] <- NA; return(tmp);
        }
        
        n[k.path,j,k]<-(1-length(which(f[k.path,j,k,]==0))/1000)*100;
        dose[k.path,j,k]<-log10(mean(non0(risk[k.path,j,k,]),na.rm=TRUE))
        
        PS_Plot(paste("Neighborhood: ",k.neighb[j],"\n",label_path[k.path],label_age[k.age[k]]),n[k.path,j,k],dose[k.path,j,k])
      }
    }
  })
  
  output$ps_plot7 <- renderPlot({
    if (input$samtype!=0) return(NULL)
    if (input$surtype==0){
      be_data1<-household_data()
      be_data2<-school_data()
      be_data3<-community_data()
      freq<-ps.frq0()
      num.neighb<-sort(unique(c(as.numeric(be_data1$neighbor),as.numeric(be_data2$neighbor),as.numeric(be_data3$neighbor))))
    }
    if (input$surtype==1){
      be_data1<-household_data()
      freq<-ps.frq1()
      num.neighb<-sort(unique(as.numeric(be_data1$neighbor)))
    }
    if (input$surtype==2){
      be_data2<-school_data()
      freq<-ps.frq2()
      num.neighb<-sort(unique(as.numeric(be_data2$neighbor)))
    }
    if (input$surtype==3){
      be_data3<-community_data()
      freq<-ps.frq3()
      num.neighb<-sort(unique(as.numeric(be_data3$neighbor)))
    }
    ec_data<-ec_data()
    conc<-conc()
    #be_data<-be_data()
    #freq<-ps.frq()
    label_path<-c("Drain Water", "Produce", "Piped Water", "Ocean Water", "Surface Water", "Flood Water", "Public Latrine Surfaces", "Particulate", "Bathing")
    label_age<-c("Adults","Children")
    
    n.neighb=ifelse(input$neighb==0, length(num.neighb),1)
    n.path=1
    n.age=ifelse(input$ad_ch==0, 2, 1)
    if (n.neighb==1 & input$neighb!=0) {k.neighb=as.numeric(input$neighb)}
    else {k.neighb=num.neighb}
    k.path=7
    if (n.age==1 & input$ad_ch!=0) {k.age=as.numeric(input$ad_ch)}
    else {k.age=c(1,2)}
    
    source("./model/PS_Plot.r")
    ".RNG.state" <- c(19900, 14957, 25769)
    nburn <- 1000;
    niter <- 10000;
    thin <- 1;
    calcul <- paste(nburn,niter,thin,sep="|");
    
    # environmental samples
    tomonitor <- c("mu","sigma");
    mu<-array(NA,c(9,n.neighb))
    sigma<-array(NA,c(9,n.neighb)) 
    for (j in 1:n.neighb){
      log_ec<-log10(as.numeric(conc[[9*(k.neighb[j]-1)+k.path]]))
      env_data<-list(lnconc=log_ec,N=length(log_ec))
      
      modelpos <- jags.model(file="./model/env_model.jags",data=env_data,n.chains=3);
      update(modelpos,n.burn=nburn);
      env_mcmcpos <- coda.samples(modelpos,tomonitor,n.iter=niter,thin=thin);
      #Bayesian estimators of mu and sigma
      mu[k.path,j]<-summary(env_mcmcpos)$statistics[1,1];
      sigma[k.path,j]<-summary(env_mcmcpos)$statistics[2,1];
    }
    
    cutpoint<-c(0,5,10) #cut point changed
    tomonitor <- c("p","r");
    p<-array(NA,c(9,n.neighb,2))
    r<-array(NA,c(9,n.neighb,2)) 
    for (j in 1:n.neighb){
      for (k in 1:n.age){
        freq_be0=freq[[14*(k.neighb[j]-1)+2*(k.path-1)+k.age[k]]]
        freq_be<-freq_be0[which(freq_be0>=0)]
        #initial values
        init_be<-as.numeric(rep(0,length(freq_be)))
        init_be[which(freq_be==1)]<-1
        init_be[which(freq_be==2)]<-2
        init_be[which(freq_be==3)]<-3
        
        init_freq_be<-as.numeric(rep(NA,length(freq_be)))
        init_freq_be[which(freq_be==1)]<-2
        init_freq_be[which(freq_be==2)]<-7
        init_freq_be[which(freq_be==3)]<-12
        
        be_data<-list(select=freq_be,N=length(freq_be),cut=cutpoint)
        init<-list(freq=init_freq_be,r=1,p=0.2)
        
        modelpos <- jags.model(file="./model/be_model.jags",data=be_data,n.chains=3,inits=init);
        update(modelpos,n.burn=nburn);
        be_mcmcpos <- coda.samples(modelpos,tomonitor,n.iter=niter,thin=thin);
        #Bayesian estimators of p and r
        p[k.path,j,k]<-summary(be_mcmcpos)$statistics[1,1]
        r[k.path,j,k]<-summary(be_mcmcpos)$statistics[2,1]
      }
    }
    nrow=n.path
    ncol=n.age*n.neighb
    par(mfrow=c(nrow,ncol))
    par(mar=c(2,3,5.5,2))
    e<-array(NA,c(9,n.neighb,1000)) 
    f<-array(NA,c(9,n.neighb,2,1000)) 
    risk<-array(NA,c(9,n.neighb,2,1000))
    n<-array(NA,c(9,n.neighb,2))
    dose<-array(NA,c(9,n.neighb,2))
    intake<-array(c(0.0006,1,10.43,0.0154,0.037,0.0006,0.034,NA,0.0499,
                    0.01,0.5,4.14,0.2042,0.2042,0.01,0.034,NA,0.09975),c(9,2)) #need to input this information!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    for (j in 1:n.neighb){
      for (k in 1:n.age){
        for (m in 1:1000){
          e[k.path,j,m]<-rnorm(1,mu[k.path,j],sigma[k.path,j])
          f[k.path,j,k,m]<-rnbinom(1,size=r[k.path,j,k],prob=p[k.path,j,k])
          risk[k.path,j,k,m]<-f[k.path,j,k,m]*(10^e[k.path,j,m])*intake[k.path,k]
        }
        
        non0 <- function(mc){
          tmp <- mc; tmp[!(tmp>0)] <- NA; return(tmp);
        }
        
        n[k.path,j,k]<-(1-length(which(f[k.path,j,k,]==0))/1000)*100;
        dose[k.path,j,k]<-log10(mean(non0(risk[k.path,j,k,]),na.rm=TRUE))
        
        PS_Plot(paste("Neighborhood: ",k.neighb[j],"\n",label_path[k.path],label_age[k.age[k]]),n[k.path,j,k],dose[k.path,j,k])
      }
    }
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