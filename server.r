library(shiny)
library(rjags)
#setwd("~/stat/RAT-shiny/model")
source("./model/PS_Plot.r")
source('api_helpers.R')
options(shiny.maxRequestSize = 9*1024^2)

url <- 'http://54.210.2.87/api/v1/data' # this is the main access point to use for data
forms <- getAPI_forms(url, api_token)

shinyServer(function(input, output, session) {
  
  # will stop shiny when the window closes
  session$onSessionEnded(function() {
    stopApp()
  })
  # Update the form options ---------------------------------------------------------
  observe({
    # URL and API token are currently defined in the API Helpers script.
    # This returns a list of available forms based on the user token
    # provided.

    # update the options
    updateSelectizeInput(session, 'col_file', choices=names(filterAPI_forms('collection', forms)))
    updateSelectizeInput(session, 'lab_file', choices=names(filterAPI_forms('lab', forms)))
    updateSelectizeInput(session, 'hh_file', choices=names(filterAPI_forms('household', forms)))
    updateSelectizeInput(session, 'sch_file', choices=names(filterAPI_forms('school', forms)))
    updateSelectizeInput(session, 'com_file', choices=names(filterAPI_forms('community', forms)))

  })
  
  # Download the data ----------------------------------------------------------------
  school_data <- reactive({
    formhubGET_csv(baseURL, u, p, input$sch_file)
  })

  community_data <- reactive({
    formhubGET_csv(baseURL, u, p, input$com_file)
  })
  output$community <- renderText({input$com_file})
  
  
  be_data <- reactive({ # household data, keeping name for consistency
    formhubGET_csv(baseURL, u, p, input$hh_file)
    
  })
  output$hh <- renderText({input$hh_file})
  
  collection_data <- reactive({
    collection <- formhubGET_csv(baseURL, u, p, input$col_file)
  
#     # it seems like these are used for analysis and they need to be numeric. 
#     columns <- c('sampleid', 'free_cl', 'containr', 'total_cl', 'source_ty', 'covered', 'dis_lat', 
#                  'latrine_user_num', 'vis_fec', 'source_type', 'hw_stat', 'n_stall', 
#                  'particle_sample_type', 'sample_weight', 'source_dist')
#     collection[,columns] <- apply(collection[,columns], 2, as.numeric)
    updateSelectInput(session, "neighb", choices = c("All"=0,unique(collection$neighbor)))
    updateSelectInput(session, "samtype", choices = c("All"=0,c("Drain Water"=1, "Produce"=2, "Piped Water"=3, 
                                                                "Ocean Water"=4, "Surface Water"=5, "Flood Water"=6,
                                                                "Public Latrine Surfaces"=7, "Particulate"=8, "Bathing"=9)[unique(collection$sample_type)]))
   
    collection 
  })

  lab_data <- reactive({
    formhubGET_csv(baseURL, u, p, input$lab_file)
  })
  
  
  
  output$sch_table <- renderTable({
    formhubGET_csv(baseURL, u, p, input$sch_file)
  })
  
  # Update analysis options ----------------------------------------------------------
#   observe({
#     #   })

  # Analysis -------------------------------------------------------------------------
  ec_data <- reactive({
    # merge the data
    ec_data<-merge(collection_data(),lab_data(),by=c("sample_type","sampleid"))
    
    ec_data$ec_denom=100
    ec_data$ec_denom[which(ec_data$sample_type==2)]=500
    ec_data$ec_denom[which(ec_data$sample_type==2)]=14
    ec_data$ec_denom[which(ec_data$sample_type==8)]=2
    ec_data$ec_denom[is.na(ec_data$sample_type)]=NA
    
    ec_data$count1[ec_data$ec_dil1>=ec_data$ec_dil2]<-ec_data$ec_ecnt1[ec_data$ec_dil1>=ec_data$ec_dil2]
    ec_data$count2[ec_data$ec_dil1>=ec_data$ec_dil2]<-ec_data$ec_ecnt2[ec_data$ec_dil1>=ec_data$ec_dil2]
    ec_data$dil1[ec_data$ec_dil1>=ec_data$ec_dil2]<-ec_data$ec_dil1[ec_data$ec_dil1>=ec_data$ec_dil2]
    ec_data$dil2[ec_data$ec_dil1>=ec_data$ec_dil2]<-ec_data$ec_dil2[ec_data$ec_dil1>=ec_data$ec_dil2]
    
    ec_data$count2[ec_data$ec_dil1<ec_data$ec_dil2]<-ec_data$ec_ecnt1[ec_data$ec_dil1<ec_data$ec_dil2]
    ec_data$count1[ec_data$ec_dil1<ec_data$ec_dil2]<-ec_data$ec_ecnt2[ec_data$ec_dil1<ec_data$ec_dil2]
    ec_data$dil2[ec_data$ec_dil1<ec_data$ec_dil2]<-ec_data$ec_dil1[ec_data$ec_dil1<ec_data$ec_dil2]
    ec_data$dil1[ec_data$ec_dil1<ec_data$ec_dil2]<-ec_data$ec_dil2[ec_data$ec_dil1<ec_data$ec_dil2]
    
    
    condition1=which((ec_data$count1==999 | ec_data$count1==998) & (ec_data$count2==999 | ec_data$count2==998))
    condition2=which((ec_data$count1==999 | ec_data$count1==998) & ec_data$count2>=10 & ec_data$count2<=200)
    condition3=which((ec_data$count1==999 | ec_data$count1==998) & ec_data$count2>=1 & ec_data$count2<=9)
    condition4=which(ec_data$count1>=10 & ec_data$count1<=200 & ec_data$count2>=10 & ec_data$count2<=200)
    condition5=which(ec_data$count1>=10 & ec_data$count1<=200 & ec_data$count2>=1 & ec_data$count2<=9)
    condition6=which(ec_data$count1>=10 & ec_data$count1<=200 & ec_data$count2==0)
    condition7=which(ec_data$count1>=1 & ec_data$count1<=9 & ec_data$count2>=1 & ec_data$count2<=9)
    condition8=which(ec_data$count1>=1 & ec_data$count1<=9 & ec_data$count2==0)
    condition9=which(ec_data$count1==0 & ec_data$count2==0)
    
    ec_con<-c()
    ec_con[condition1]=200/ec_data$dil2[condition1]*ec_data$ec_denom[condition1]
    ec_con[condition2]=ec_data$count2[condition2]/ec_data$dil2[condition2]*ec_data$ec_denom[condition2]
    ec_con[condition3]=ec_data$count2[condition3]/ec_data$dil2[condition3]*ec_data$ec_denom[condition3]
    ec_con[condition4]=(ec_data$count1[condition4]+ec_data$count2[condition4])/(ec_data$dil1[condition4]+ec_data$dil2[condition4])*ec_data$ec_denom[condition4]
    ec_con[condition5]=ec_data$count1[condition5]/ec_data$dil1[condition5]*ec_data$ec_denom[condition5]
    ec_con[condition6]=ec_data$count1[condition6]/ec_data$dil1[condition6]*ec_data$ec_denom[condition6]
    ec_con[condition7]=(ec_data$count1[condition7]+ec_data$count2[condition7])/(ec_data$dil1[condition7]+ec_data$dil2[condition7])*ec_data$ec_denom[condition7]
    ec_con[condition8]=ec_data$count1[condition8]/ec_data$dil1[condition8]*ec_data$ec_denom[condition8]
    ec_con[condition9]=0.5/(ec_data$dil1[condition9]+ec_data$dil2[condition9])*ec_data$ec_denom[condition9]
    ec_data$ec_conc<-ec_con
    ec_data
  })
  
  conc <- reactive({
    ec_data<-ec_data() # this really isn't necessary.  can call ec_data() directly
    conc<-list()
    for (i in 1:length(unique(as.numeric(ec_data$neighbor)))){
      # sample type 1=drain water, 2=produce, 3=piped water, 4=ocean water, 5=surface water, 6=flood water, 7=Public Latrine Surfaces, 8=particulate, 9=bathing
      for (j in 1:9){
        conc[[9*(sort(unique(as.numeric(ec_data$neighbor)))[i]-1)+j]]=ec_data$ec_conc[which(ec_data$neighbor==sort(unique(ec_data$neighbor))[i] 
                                                                                            & ec_data$sample_type==j)]
      }
    }
    conc
  })

  frq <- reactive({
    freq<-list()
    for (i in 1:length(unique(be_data()$neighbor))){
      # sample type 1|2=drain, 3|4=produce, 5|6=piped water, 7|8=ocean water, 9|10=surface water, 11|12=flood water, 13|14=Public Latrine Surfaces  
      freq[[14*(sort(unique(be_data()$neighbor))[i]-1)+1]]=as.numeric(be_data()$hh_q6[which(be_data()$hh_q6!="n/a" & be_data()$neighbor==i)]);
      freq[[14*(sort(unique(be_data()$neighbor))[i]-1)+2]]=as.numeric(be_data()$hh_q7[which(be_data()$hh_q7!="n/a" & be_data()$neighbor==i)]);
      freq[[14*(sort(unique(be_data()$neighbor))[i]-1)+3]]=as.numeric(be_data()$hh_q13[which(be_data()$hh_q13!="n/a" & be_data()$neighbor==i)]);
      freq[[14*(sort(unique(be_data()$neighbor))[i]-1)+4]]=as.numeric(be_data()$hh_q14[which(be_data()$hh_q14!="n/a" & be_data()$neighbor==i)]);
      freq[[14*(sort(unique(be_data()$neighbor))[i]-1)+5]]=as.numeric(be_data()$hh_q10[which(be_data()$hh_q10!="n/a" & be_data()$neighbor==i)]);
      freq[[14*(sort(unique(be_data()$neighbor))[i]-1)+6]]=as.numeric(be_data()$hh_q11[which(be_data()$hh_q11!="n/a" & be_data()$neighbor==i)]);
      freq[[14*(sort(unique(be_data()$neighbor))[i]-1)+7]]=as.numeric(be_data()$hh_q2[which(be_data()$hh_q2!="n/a" & be_data()$neighbor==i)]);
      freq[[14*(sort(unique(be_data()$neighbor))[i]-1)+8]]=as.numeric(be_data()$hh_q3[which(be_data()$hh_q3!="n/a" & be_data()$neighbor==i)]);
      freq[[14*(sort(unique(be_data()$neighbor))[i]-1)+9]]=as.numeric(be_data()$hh_q4[which(be_data()$hh_q4!="n/a" & be_data()$neighbor==i)]);
      freq[[14*(sort(unique(be_data()$neighbor))[i]-1)+10]]=as.numeric(be_data()$hh_q5[which(be_data()$hh_q5!="n/a" & be_data()$neighbor==i)]);
      freq[[14*(sort(unique(be_data()$neighbor))[i]-1)+11]]=as.numeric(be_data()$hh_q8[which(be_data()$hh_q8!="n/a" & be_data()$neighbor==i)]);
      freq[[14*(sort(unique(be_data()$neighbor))[i]-1)+12]]=as.numeric(be_data()$hh_q9[which(be_data()$hh_q9!="n/a" & be_data()$neighbor==i)]);
      freq[[14*(sort(unique(be_data()$neighbor))[i]-1)+13]]=as.numeric(be_data()$hh_q15[which(be_data()$hh_q15!="n/a" & be_data()$neighbor==i)]);
      freq[[14*(sort(unique(be_data()$neighbor))[i]-1)+14]]=as.numeric(be_data()$hh_q16[which(be_data()$hh_q16!="n/a" & be_data()$neighbor==i)]);
    }
    freq
  })

  ps.frq <- reactive({
    freq<-list()
    
    for (i in 1:length(unique(be_data()$neighbor))){
      # sample type 1|2=drain, 3|4=produce, 5|6=piped water, 7|8=ocean water, 9|10=surface water, 11|12=flood water, 13|14=Public Latrine Surfaces  
      freq[[14*(sort(unique(be_data()$neighbor))[i]-1)+1]]=4-as.numeric(be_data()$hh_q6[which(be_data()$hh_q6!="n/a" & be_data()$neighbor==i)]);
      freq[[14*(sort(unique(be_data()$neighbor))[i]-1)+2]]=4-as.numeric(be_data()$hh_q7[which(be_data()$hh_q7!="n/a" & be_data()$neighbor==i)]);
      freq[[14*(sort(unique(be_data()$neighbor))[i]-1)+3]]=4-as.numeric(be_data()$hh_q13[which(be_data()$hh_q13!="n/a" & be_data()$neighbor==i)]);
      freq[[14*(sort(unique(be_data()$neighbor))[i]-1)+4]]=4-as.numeric(be_data()$hh_q14[which(be_data()$hh_q14!="n/a" & be_data()$neighbor==i)]);
      freq[[14*(sort(unique(be_data()$neighbor))[i]-1)+5]]=4-as.numeric(be_data()$hh_q10[which(be_data()$hh_q10!="n/a" & be_data()$neighbor==i)]);
      freq[[14*(sort(unique(be_data()$neighbor))[i]-1)+6]]=4-as.numeric(be_data()$hh_q11[which(be_data()$hh_q11!="n/a" & be_data()$neighbor==i)]);
      freq[[14*(sort(unique(be_data()$neighbor))[i]-1)+7]]=4-as.numeric(be_data()$hh_q2[which(be_data()$hh_q2!="n/a" & be_data()$neighbor==i)]);
      freq[[14*(sort(unique(be_data()$neighbor))[i]-1)+8]]=4-as.numeric(be_data()$hh_q3[which(be_data()$hh_q3!="n/a" & be_data()$neighbor==i)]);
      freq[[14*(sort(unique(be_data()$neighbor))[i]-1)+9]]=4-as.numeric(be_data()$hh_q4[which(be_data()$hh_q4!="n/a" & be_data()$neighbor==i)]);
      freq[[14*(sort(unique(be_data()$neighbor))[i]-1)+10]]=4-as.numeric(be_data()$hh_q5[which(be_data()$hh_q5!="n/a" & be_data()$neighbor==i)]);
      freq[[14*(sort(unique(be_data()$neighbor))[i]-1)+11]]=4-as.numeric(be_data()$hh_q8[which(be_data()$hh_q8!="n/a" & be_data()$neighbor==i)]);
      freq[[14*(sort(unique(be_data()$neighbor))[i]-1)+12]]=4-as.numeric(be_data()$hh_q9[which(be_data()$hh_q9!="n/a" & be_data()$neighbor==i)]);
      freq[[14*(sort(unique(be_data()$neighbor))[i]-1)+13]]=4-as.numeric(be_data()$hh_q15[which(be_data()$hh_q15!="n/a" & be_data()$neighbor==i)]);
      freq[[14*(sort(unique(be_data()$neighbor))[i]-1)+14]]=4-as.numeric(be_data()$hh_q16[which(be_data()$hh_q16!="n/a" & be_data()$neighbor==i)]);
    }
    freq
  })
  

  output$ec_table <- renderTable({
    ec_data()
  })
  output$hh_table <- renderTable({
    be_data()
  })

  output$com_table <- renderTable({
    community_data()
  }) 
  
#This table is for test only#
output$table1 <- renderPrint({
  be_data<-be_data()
  freq<-frq()
  n.neighb=ifelse(input$neighb==0, length(unique(as.numeric(be_data$neighbor))),1)
  n.path=ifelse(input$samtype==0, 7, 1)
  n.age=ifelse(input$ad_ch==0, 2, 1)
  if (n.neighb==1 & input$neighb!=0) {k.neighb=as.numeric(input$neighb)} else {k.neighb=sort(unique(as.numeric(be_data$neighbor)))}
  if (n.path==1 & input$samtype!=0) {k.path=as.numeric(input$samtype)} else {k.path=c(2,3,4,5,6,8,9)}
  if (n.age==1 & input$ad_ch!=0) {k.age=as.numeric(input$ad_ch)} else {k.age=c(1,2)}
  nrow=n.path
  ncol=n.age*n.neighb
  list(n.neighb, 
              n.path,
              n.age,
              k.neighb, 
              k.path,
              k.age,
       nrow,ncol)
})

output$pie_chart1 <- renderPlot({
  be_data<-be_data()
  freq<-frq()
  n.neighb=ifelse(input$neighb==0, length(unique(as.numeric(be_data$neighbor))),1)
  n.path=1
  n.age=ifelse(input$ad_ch==0, 2, 1)
  if (n.neighb==1 & input$neighb!=0) {k.neighb=as.numeric(input$neighb)} else {k.neighb=sort(unique(as.numeric(be_data$neighbor)))}
  if (input$samtype!=0) {k.path=as.numeric(input$samtype)} else {k.path=1}
  if (n.age==1 & input$ad_ch!=0) {k.age=as.numeric(input$ad_ch)} else {k.age=c(1,2)}
  nrow=n.path
  ncol=n.age*n.neighb
  par(mfrow=c(nrow,ncol))
  par(mar=c(0.5,6.5,4.5,6.5))
  label1<-c(">10 times/month","6-10 times/month","1-5 times/month","never","don't know")
  label_path<-c("Drain Water", "Produce", "Piped Water", "Ocean Water", "Surface Water", "Flood Water", "Public Latrine Surfaces", "Particulate", "Bathing")
  label_age<-c("Adults","Children")
  if (input$samtype!=8 & input$samtype!=9){
    for (i in 1:n.path){
      for (j in 1:n.neighb){
        for (k in 1:n.age){
          slices <- c(table(as.numeric(freq[[14*(k.neighb[j]-1)+2*(k.path-1)+k.age[k]]]))) 
          label1<-c(">10 times/month","6-10 times/month","1-5 times/month","never","don't know")
          pct <- c(0,0,0,0,0)
          pct[sort(unique(as.numeric(freq[[14*(k.neighb[j]-1)+2*(k.path-1)+k.age[k]]])))] <- round(slices/sum(slices)*100)
          label1 <- paste(label1, "\n", pct, sep="") # add percents to labels 
          label1 <- paste(label1,"%",sep="") # add % to labels 
          pie(slices,
              labels = label1[sort(unique(as.numeric(freq[[14*(k.neighb[j]-1)+2*(k.path-1)+k.age[k]]])))], 
              col=rainbow(5)[sort(unique(as.numeric(freq[[14*(k.neighb[j]-1)+2*(k.path-1)+k.age[k]]])))],
              main=paste("Neighborhood ",k.neighb[j],", ",label_path[k.path],", ",label_age[k.age[k]]),
              cex=1.3,cex.main=1.5,init.angle = 90)
        }
      }
    }
  }
})

output$pie_chart2 <- renderPlot({
  if (input$samtype!=0) return(NULL)
  be_data<-be_data()
  freq<-frq()
  n.neighb=ifelse(input$neighb==0, length(unique(as.numeric(be_data$neighbor))),1)
  n.path=1
  n.age=ifelse(input$ad_ch==0, 2, 1)
  if (n.neighb==1 & input$neighb!=0) {k.neighb=as.numeric(input$neighb)} else {k.neighb=sort(unique(as.numeric(be_data$neighbor)))}
  k.path=2
  if (n.age==1 & input$ad_ch!=0) {k.age=as.numeric(input$ad_ch)} else {k.age=c(1,2)}
  nrow=n.path
  ncol=n.age*n.neighb
  par(mfrow=c(nrow,ncol))
  par(mar=c(0.5,6.5,4.5,6.5))
  label1<-c(">10 times/month","6-10 times/month","1-5 times/month","never","don't know")
  label_path<-c("Drain Water", "Produce", "Piped Water", "Ocean Water", "Surface Water", "Flood Water", "Public Latrine Surfaces", "Particulate", "Bathing")
  label_age<-c("Adults","Children")
  if (input$samtype!=8 & input$samtype!=9){
    for (i in 1:n.path){
      for (j in 1:n.neighb){
        for (k in 1:n.age){
          slices <- c(table(as.numeric(freq[[14*(k.neighb[j]-1)+2*(k.path-1)+k.age[k]]]))) 
          label1<-c(">10 times/month","6-10 times/month","1-5 times/month","never","don't know")
          pct <- c(0,0,0,0,0)
          pct[sort(unique(as.numeric(freq[[14*(k.neighb[j]-1)+2*(k.path-1)+k.age[k]]])))] <- round(slices/sum(slices)*100)
          label1 <- paste(label1, "\n", pct, sep="") # add percents to labels 
          label1 <- paste(label1,"%",sep="") # add % to labels 
          pie(slices,
              labels = label1[sort(unique(as.numeric(freq[[14*(k.neighb[j]-1)+2*(k.path-1)+k.age[k]]])))], 
              col=rainbow(5)[sort(unique(as.numeric(freq[[14*(k.neighb[j]-1)+2*(k.path-1)+k.age[k]]])))],
              main=paste("Neighborhood ",k.neighb[j],", ",label_path[k.path],", ",label_age[k.age[k]]),
              cex=1.3,cex.main=1.5,init.angle = 90)
        }
      }
    }
  }
})

output$pie_chart3 <- renderPlot({
  if (input$samtype!=0) return(NULL)
  be_data<-be_data()
  freq<-frq()
  n.neighb=ifelse(input$neighb==0, length(unique(as.numeric(be_data$neighbor))),1)
  n.path=1
  n.age=ifelse(input$ad_ch==0, 2, 1)
  if (n.neighb==1 & input$neighb!=0) {k.neighb=as.numeric(input$neighb)} else {k.neighb=sort(unique(as.numeric(be_data$neighbor)))}
  k.path=3
  if (n.age==1 & input$ad_ch!=0) {k.age=as.numeric(input$ad_ch)} else {k.age=c(1,2)}
  nrow=n.path
  ncol=n.age*n.neighb
  par(mfrow=c(nrow,ncol))
  par(mar=c(0.5,6.5,4.5,6.5))
  label2<-c("everyday","4-6 days/week","1-3 days/month","never","don't know")
  label_path<-c("Drain Water", "Produce", "Piped Water", "Ocean Water", "Surface Water", "Flood Water", "Public Latrine Surfaces", "Particulate", "Bathing")
  label_age<-c("Adults","Children")
  if (input$samtype!=8 & input$samtype!=9){
    for (i in 1:n.path){
      for (j in 1:n.neighb){
        for (k in 1:n.age){
          slices <- c(table(as.numeric(freq[[14*(k.neighb[j]-1)+2*(k.path-1)+k.age[k]]]))) 
          label2<-c("everyday","4-6 days/week","1-3 days/month","never","don't know")
          pct <- c(0,0,0,0,0)
          pct[sort(unique(as.numeric(freq[[14*(k.neighb[j]-1)+2*(k.path-1)+k.age[k]]])))] <- round(slices/sum(slices)*100)
          label2 <- paste(label2, "\n", pct, sep="") # add percents to labels 
          label2 <- paste(label2,"%",sep="") # add % to labels 
          pie(slices,
              labels = label2[sort(unique(as.numeric(freq[[14*(k.neighb[j]-1)+2*(k.path-1)+k.age[k]]])))], 
              col=rainbow(5)[sort(unique(as.numeric(freq[[14*(k.neighb[j]-1)+2*(k.path-1)+k.age[k]]])))],
              main=paste("Neighborhood ",k.neighb[j],", ",label_path[k.path],", ",label_age[k.age[k]]),
              cex=1.3,cex.main=1.5,init.angle = 90)
        }
      }
    }
  }
})

output$pie_chart4 <- renderPlot({
  if (input$samtype!=0) return(NULL)
  be_data<-be_data()
  freq<-frq()
  n.neighb=ifelse(input$neighb==0, length(unique(as.numeric(be_data$neighbor))),1)
  n.path=1
  n.age=ifelse(input$ad_ch==0, 2, 1)
  if (n.neighb==1 & input$neighb!=0) {k.neighb=as.numeric(input$neighb)} else {k.neighb=sort(unique(as.numeric(be_data$neighbor)))}
  k.path=4
  if (n.age==1 & input$ad_ch!=0) {k.age=as.numeric(input$ad_ch)} else {k.age=c(1,2)}
  nrow=n.path
  ncol=n.age*n.neighb
  par(mfrow=c(nrow,ncol))
  par(mar=c(0.5,6.5,4.5,6.5))
  label1<-c(">10 times/month","6-10 times/month","1-5 times/month","never","don't know")
  label_path<-c("Drain Water", "Produce", "Piped Water", "Ocean Water", "Surface Water", "Flood Water", "Public Latrine Surfaces", "Particulate", "Bathing")
  label_age<-c("Adults","Children")
  if (input$samtype!=8 & input$samtype!=9){
    for (i in 1:n.path){
      for (j in 1:n.neighb){
        for (k in 1:n.age){
          slices <- c(table(as.numeric(freq[[14*(k.neighb[j]-1)+2*(k.path-1)+k.age[k]]]))) 
          label1<-c(">10 times/month","6-10 times/month","1-5 times/month","never","don't know")
          pct <- c(0,0,0,0,0)
          pct[sort(unique(as.numeric(freq[[14*(k.neighb[j]-1)+2*(k.path-1)+k.age[k]]])))] <- round(slices/sum(slices)*100)
          label1 <- paste(label1, "\n", pct, sep="") # add percents to labels 
          label1 <- paste(label1,"%",sep="") # add % to labels 
          pie(slices,
              labels = label1[sort(unique(as.numeric(freq[[14*(k.neighb[j]-1)+2*(k.path-1)+k.age[k]]])))], 
              col=rainbow(5)[sort(unique(as.numeric(freq[[14*(k.neighb[j]-1)+2*(k.path-1)+k.age[k]]])))],
              main=paste("Neighborhood ",k.neighb[j],", ",label_path[k.path],", ",label_age[k.age[k]]),
              cex=1.3,cex.main=1.5,init.angle = 90)
        }
      }
    }
  }
})

output$pie_chart5 <- renderPlot({
  if (input$samtype!=0) return(NULL)
  be_data<-be_data()
  freq<-frq()
  n.neighb=ifelse(input$neighb==0, length(unique(as.numeric(be_data$neighbor))),1)
  n.path=1
  n.age=ifelse(input$ad_ch==0, 2, 1)
  if (n.neighb==1 & input$neighb!=0) {k.neighb=as.numeric(input$neighb)} else {k.neighb=sort(unique(as.numeric(be_data$neighbor)))}
  k.path=5
  if (n.age==1 & input$ad_ch!=0) {k.age=as.numeric(input$ad_ch)} else {k.age=c(1,2)}
  nrow=n.path
  ncol=n.age*n.neighb
  par(mfrow=c(nrow,ncol))
  par(mar=c(0.5,6.5,4.5,6.5))
  label1<-c(">10 times/month","6-10 times/month","1-5 times/month","never","don't know")
  label_path<-c("Drain Water", "Produce", "Piped Water", "Ocean Water", "Surface Water", "Flood Water", "Public Latrine Surfaces", "Particulate", "Bathing")
  label_age<-c("Adults","Children")
  if (input$samtype!=8 & input$samtype!=9){
    for (i in 1:n.path){
      for (j in 1:n.neighb){
        for (k in 1:n.age){
          slices <- c(table(as.numeric(freq[[14*(k.neighb[j]-1)+2*(k.path-1)+k.age[k]]]))) 
          label1<-c(">10 times/month","6-10 times/month","1-5 times/month","never","don't know")
          pct <- c(0,0,0,0,0)
          pct[sort(unique(as.numeric(freq[[14*(k.neighb[j]-1)+2*(k.path-1)+k.age[k]]])))] <- round(slices/sum(slices)*100)
          label1 <- paste(label1, "\n", pct, sep="") # add percents to labels 
          label1 <- paste(label1,"%",sep="") # add % to labels 
          pie(slices,
              labels = label1[sort(unique(as.numeric(freq[[14*(k.neighb[j]-1)+2*(k.path-1)+k.age[k]]])))], 
              col=rainbow(5)[sort(unique(as.numeric(freq[[14*(k.neighb[j]-1)+2*(k.path-1)+k.age[k]]])))],
              main=paste("Neighborhood ",k.neighb[j],", ",label_path[k.path],", ",label_age[k.age[k]]),
              cex=1.3,cex.main=1.5,init.angle = 90)
        }
      }
    }
  }
})

output$pie_chart6 <- renderPlot({
  if (input$samtype!=0) return(NULL)
  be_data<-be_data()
  freq<-frq()
  n.neighb=ifelse(input$neighb==0, length(unique(as.numeric(be_data$neighbor))),1)
  n.path=1
  n.age=ifelse(input$ad_ch==0, 2, 1)
  if (n.neighb==1 & input$neighb!=0) {k.neighb=as.numeric(input$neighb)} else {k.neighb=sort(unique(as.numeric(be_data$neighbor)))}
  k.path=6
  if (n.age==1 & input$ad_ch!=0) {k.age=as.numeric(input$ad_ch)} else {k.age=c(1,2)}
  nrow=n.path
  ncol=n.age*n.neighb
  par(mfrow=c(nrow,ncol))
  par(mar=c(0.5,6.5,4.5,6.5))
  label1<-c(">10 times/month","6-10 times/month","1-5 times/month","never","don't know")
  label_path<-c("Drain Water", "Produce", "Piped Water", "Ocean Water", "Surface Water", "Flood Water", "Public Latrine Surfaces", "Particulate", "Bathing")
  label_age<-c("Adults","Children")
  if (input$samtype!=8 & input$samtype!=9){
    for (i in 1:n.path){
      for (j in 1:n.neighb){
        for (k in 1:n.age){
          slices <- c(table(as.numeric(freq[[14*(k.neighb[j]-1)+2*(k.path-1)+k.age[k]]]))) 
          label1<-c(">10 times/month","6-10 times/month","1-5 times/month","never","don't know")
          pct <- c(0,0,0,0,0)
          pct[sort(unique(as.numeric(freq[[14*(k.neighb[j]-1)+2*(k.path-1)+k.age[k]]])))] <- round(slices/sum(slices)*100)
          label1 <- paste(label1, "\n", pct, sep="") # add percents to labels 
          label1 <- paste(label1,"%",sep="") # add % to labels 
          pie(slices,
              labels = label1[sort(unique(as.numeric(freq[[14*(k.neighb[j]-1)+2*(k.path-1)+k.age[k]]])))], 
              col=rainbow(5)[sort(unique(as.numeric(freq[[14*(k.neighb[j]-1)+2*(k.path-1)+k.age[k]]])))],
              main=paste("Neighborhood ",k.neighb[j],", ",label_path[k.path],", ",label_age[k.age[k]]),
              cex=1.3,cex.main=1.5,init.angle = 90)
        }
      }
    }
  }
})

output$pie_chart7 <- renderPlot({
  if (input$samtype!=0) return(NULL)
  be_data<-be_data()
  freq<-frq()
  n.neighb=ifelse(input$neighb==0, length(unique(as.numeric(be_data$neighbor))),1)
  n.path=1
  n.age=ifelse(input$ad_ch==0, 2, 1)
  if (n.neighb==1 & input$neighb!=0) {k.neighb=as.numeric(input$neighb)} else {k.neighb=sort(unique(as.numeric(be_data$neighbor)))}
  k.path=7
  if (n.age==1 & input$ad_ch!=0) {k.age=as.numeric(input$ad_ch)} else {k.age=c(1,2)}
  nrow=n.path
  ncol=n.age*n.neighb
  par(mfrow=c(nrow,ncol))
  par(mar=c(0.5,6.5,4.5,6.5))
  label1<-c(">10 times/month","6-10 times/month","1-5 times/month","never","don't know")
  label_path<-c("Drain Water", "Produce", "Piped Water", "Ocean Water", "Surface Water", "Flood Water", "Public Latrine Surfaces", "Particulate", "Bathing")
  label_age<-c("Adults","Children")
  if (input$samtype!=8 & input$samtype!=9){
    for (i in 1:n.path){
      for (j in 1:n.neighb){
        for (k in 1:n.age){
          slices <- c(table(as.numeric(freq[[14*(k.neighb[j]-1)+2*(k.path-1)+k.age[k]]]))) 
          label1<-c(">10 times/month","6-10 times/month","1-5 times/month","never","don't know")
          pct <- c(0,0,0,0,0)
          pct[sort(unique(as.numeric(freq[[14*(k.neighb[j]-1)+2*(k.path-1)+k.age[k]]])))] <- round(slices/sum(slices)*100)
          label1 <- paste(label1, "\n", pct, sep="") # add percents to labels 
          label1 <- paste(label1,"%",sep="") # add % to labels 
          pie(slices,
              labels = label1[sort(unique(as.numeric(freq[[14*(k.neighb[j]-1)+2*(k.path-1)+k.age[k]]])))], 
              col=rainbow(5)[sort(unique(as.numeric(freq[[14*(k.neighb[j]-1)+2*(k.path-1)+k.age[k]]])))],
              main=paste("Neighborhood ",k.neighb[j],", ",label_path[k.path],", ",label_age[k.age[k]]),
              cex=1.3,cex.main=1.5,init.angle = 90)
        }
      }
    }
  }
})


output$hist1 <- renderPlot({
  ec_data<-ec_data()
  conc<-conc()
  n.neighb=ifelse(input$neighb==0, length(unique(as.numeric(ec_data$neighbor))),1)
  n.path=1
  if (n.neighb==1 & input$neighb!=0) {k.neighb=as.numeric(input$neighb)} else {k.neighb=sort(unique(as.numeric(ec_data$neighbor)))}
  if (input$samtype!=0) {k.path=as.numeric(input$samtype)} else {k.path=1}
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
  if (n.neighb==1 & input$neighb!=0) {k.neighb=as.numeric(input$neighb)} else {k.neighb=sort(unique(as.numeric(ec_data$neighbor)))}
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
  if (n.neighb==1 & input$neighb!=0) {k.neighb=as.numeric(input$neighb)} else {k.neighb=sort(unique(as.numeric(ec_data$neighbor)))}
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
  if (n.neighb==1 & input$neighb!=0) {k.neighb=as.numeric(input$neighb)} else {k.neighb=sort(unique(as.numeric(ec_data$neighbor)))}
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
  if (n.neighb==1 & input$neighb!=0) {k.neighb=as.numeric(input$neighb)} else {k.neighb=sort(unique(as.numeric(ec_data$neighbor)))}
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
  if (n.neighb==1 & input$neighb!=0) {k.neighb=as.numeric(input$neighb)} else {k.neighb=sort(unique(as.numeric(ec_data$neighbor)))}
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
  if (n.neighb==1 & input$neighb!=0) {k.neighb=as.numeric(input$neighb)} else {k.neighb=sort(unique(as.numeric(ec_data$neighbor)))}
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
  if (n.neighb==1 & input$neighb!=0) {k.neighb=as.numeric(input$neighb)} else {k.neighb=sort(unique(as.numeric(ec_data$neighbor)))}
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
  if (n.neighb==1 & input$neighb!=0) {k.neighb=as.numeric(input$neighb)} else {k.neighb=sort(unique(as.numeric(ec_data$neighbor)))}
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
  ec_data<-ec_data()
  conc<-conc()
  be_data<-be_data()
  freq<-ps.frq()
  label_path<-c("Drain Water", "Produce", "Piped Water", "Ocean Water", "Surface Water", "Flood Water", "Public Latrine Surfaces", "Particulate", "Bathing")
  label_age<-c("Adults","Children")
  
  n.neighb=ifelse(input$neighb==0, length(unique(as.numeric(be_data$neighbor))),1)
  n.path=1
  n.age=ifelse(input$ad_ch==0, 2, 1)
  if (n.neighb==1 & input$neighb!=0) {k.neighb=as.numeric(input$neighb)} else {k.neighb=sort(unique(as.numeric(be_data$neighbor)))}
  if (input$samtype!=0) {k.path=as.numeric(input$samtype)} else {k.path=1}
  if (n.age==1 & input$ad_ch!=0) {k.age=as.numeric(input$ad_ch)} else {k.age=c(1,2)}
  
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
  ec_data<-ec_data()
  conc<-conc()
  be_data<-be_data()
  freq<-ps.frq()
  label_path<-c("Drain Water", "Produce", "Piped Water", "Ocean Water", "Surface Water", "Flood Water", "Public Latrine Surfaces", "Particulate", "Bathing")
  label_age<-c("Adults","Children")
  
  n.neighb=ifelse(input$neighb==0, length(unique(as.numeric(be_data$neighbor))),1)
  n.path=1
  n.age=ifelse(input$ad_ch==0, 2, 1)
  if (n.neighb==1 & input$neighb!=0) {k.neighb=as.numeric(input$neighb)} else {k.neighb=sort(unique(as.numeric(be_data$neighbor)))}
  k.path=2
  if (n.age==1 & input$ad_ch!=0) {k.age=as.numeric(input$ad_ch)} else {k.age=c(1,2)}
  
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
  ec_data<-ec_data()
  conc<-conc()
  be_data<-be_data()
  freq<-ps.frq()
  label_path<-c("Drain Water", "Produce", "Piped Water", "Ocean Water", "Surface Water", "Flood Water", "Public Latrine Surfaces", "Particulate", "Bathing")
  label_age<-c("Adults","Children")
  
  n.neighb=ifelse(input$neighb==0, length(unique(as.numeric(be_data$neighbor))),1)
  n.path=1
  n.age=ifelse(input$ad_ch==0, 2, 1)
  if (n.neighb==1 & input$neighb!=0) {k.neighb=as.numeric(input$neighb)} else {k.neighb=sort(unique(as.numeric(be_data$neighbor)))}
  k.path=3
  if (n.age==1 & input$ad_ch!=0) {k.age=as.numeric(input$ad_ch)} else {k.age=c(1,2)}
  
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
  ec_data<-ec_data()
  conc<-conc()
  be_data<-be_data()
  freq<-ps.frq()
  label_path<-c("Drain Water", "Produce", "Piped Water", "Ocean Water", "Surface Water", "Flood Water", "Public Latrine Surfaces", "Particulate", "Bathing")
  label_age<-c("Adults","Children")
  
  n.neighb=ifelse(input$neighb==0, length(unique(as.numeric(be_data$neighbor))),1)
  n.path=1
  n.age=ifelse(input$ad_ch==0, 2, 1)
  if (n.neighb==1 & input$neighb!=0) {k.neighb=as.numeric(input$neighb)} else {k.neighb=sort(unique(as.numeric(be_data$neighbor)))}
  k.path=4
  if (n.age==1 & input$ad_ch!=0) {k.age=as.numeric(input$ad_ch)} else {k.age=c(1,2)}
  
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
  ec_data<-ec_data()
  conc<-conc()
  be_data<-be_data()
  freq<-ps.frq()
  label_path<-c("Drain Water", "Produce", "Piped Water", "Ocean Water", "Surface Water", "Flood Water", "Public Latrine Surfaces", "Particulate", "Bathing")
  label_age<-c("Adults","Children")
  
  n.neighb=ifelse(input$neighb==0, length(unique(as.numeric(be_data$neighbor))),1)
  n.path=1
  n.age=ifelse(input$ad_ch==0, 2, 1)
  if (n.neighb==1 & input$neighb!=0) {k.neighb=as.numeric(input$neighb)} else {k.neighb=sort(unique(as.numeric(be_data$neighbor)))}
  k.path=5
  if (n.age==1 & input$ad_ch!=0) {k.age=as.numeric(input$ad_ch)} else {k.age=c(1,2)}
  
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
  ec_data<-ec_data()
  conc<-conc()
  be_data<-be_data()
  freq<-ps.frq()
  label_path<-c("Drain Water", "Produce", "Piped Water", "Ocean Water", "Surface Water", "Flood Water", "Public Latrine Surfaces", "Particulate", "Bathing")
  label_age<-c("Adults","Children")
  
  n.neighb=ifelse(input$neighb==0, length(unique(as.numeric(be_data$neighbor))),1)
  n.path=1
  n.age=ifelse(input$ad_ch==0, 2, 1)
  if (n.neighb==1 & input$neighb!=0) {k.neighb=as.numeric(input$neighb)} else {k.neighb=sort(unique(as.numeric(be_data$neighbor)))}
  k.path=6
  if (n.age==1 & input$ad_ch!=0) {k.age=as.numeric(input$ad_ch)} else {k.age=c(1,2)}
  
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
  ec_data<-ec_data()
  conc<-conc()
  be_data<-be_data()
  freq<-ps.frq()
  label_path<-c("Drain Water", "Produce", "Piped Water", "Ocean Water", "Surface Water", "Flood Water", "Public Latrine Surfaces", "Particulate", "Bathing")
  label_age<-c("Adults","Children")
  
  n.neighb=ifelse(input$neighb==0, length(unique(as.numeric(be_data$neighbor))),1)
  n.path=1
  n.age=ifelse(input$ad_ch==0, 2, 1)
  if (n.neighb==1 & input$neighb!=0) {k.neighb=as.numeric(input$neighb)} else {k.neighb=sort(unique(as.numeric(be_data$neighbor)))}
  k.path=7
  if (n.age==1 & input$ad_ch!=0) {k.age=as.numeric(input$ad_ch)} else {k.age=c(1,2)}
  
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

piechart1<-reactive({
  be_data<-be_data()
  freq<-frq()
  n.neighb=ifelse(input$neighb==0, length(unique(as.numeric(be_data$neighbor))),1)
  n.path=1
  n.age=ifelse(input$ad_ch==0, 2, 1)
  if (n.neighb==1 & input$neighb!=0) {k.neighb=as.numeric(input$neighb)} else {k.neighb=sort(unique(as.numeric(be_data$neighbor)))}
  if (input$samtype!=0) {k.path=as.numeric(input$samtype)} else {k.path=1}
  if (n.age==1 & input$ad_ch!=0) {k.age=as.numeric(input$ad_ch)} else {k.age=c(1,2)}
  nrow=n.path
  ncol=n.age*n.neighb
  par(mfrow=c(nrow,ncol))
  par(mar=c(0.5,6.5,4.5,6.5))
  label1<-c(">10 times/month","6-10 times/month","1-5 times/month","never","don't know")
  label_path<-c("Drain Water", "Produce", "Piped Water", "Ocean Water", "Surface Water", "Flood Water", "Public Latrine Surfaces", "Particulate", "Bathing")
  label_age<-c("Adults","Children")
  if (input$samtype!=8 & input$samtype!=9){
    for (i in 1:n.path){
      for (j in 1:n.neighb){
        for (k in 1:n.age){
          slices <- c(table(as.numeric(freq[[14*(k.neighb[j]-1)+2*(k.path-1)+k.age[k]]]))) 
          label1<-c(">10 times/month","6-10 times/month","1-5 times/month","never","don't know")
          pct <- c(0,0,0,0,0)
          pct[sort(unique(as.numeric(freq[[14*(k.neighb[j]-1)+2*(k.path-1)+k.age[k]]])))] <- round(slices/sum(slices)*100)
          label1 <- paste(label1, "\n", pct, sep="") # add percents to labels 
          label1 <- paste(label1,"%",sep="") # add % to labels 
          print(pie(slices,
              labels = label1[sort(unique(as.numeric(freq[[14*(k.neighb[j]-1)+2*(k.path-1)+k.age[k]]])))], 
              col=rainbow(5)[sort(unique(as.numeric(freq[[14*(k.neighb[j]-1)+2*(k.path-1)+k.age[k]]])))],
              main=paste("Neighborhood ",k.neighb[j],", ",label_path[k.path],", ",label_age[k.age[k]]),
              cex=1.3,cex.main=1.5,init.angle = 90))
        }
      }
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