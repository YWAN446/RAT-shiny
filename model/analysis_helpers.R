## Analysis helpers for the SaniPath Analysis tool
## these have been derived from the original server.R 
## file.
library(ggplot2)
library(rlist)
library(plyr)
library(reshape2)

# MERGING --------------------------------------------------------------------
create_ecData <- function(collection_data, lab_data) {
  # merge and calculate e. coli data?
  
  ec_data<-merge(collection_data,lab_data,by=c("sample_type","sampleid"))
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
  
  ec_data$neighbor <- as.factor(ec_data$neighbor)
  
  return(ec_data)
}

create_concData <- function(ec_data) {
  # Calculate concentration amounts?
  conc_names <- c("Drain Water", "Produce", "Municipal and Piped Water",'Ocean Water', 'Surface Water', "Flood Water", 
                  "Public Latrine Surfaces", "Particulate", "Bathing")
  conc<-list()
  for (i in 1:length(unique(as.numeric(ec_data$neighbor)))){
    # sample type 1=drain water, 2=produce, 3=piped water, 4=ocean water, 5=surface water, 6=flood water, 7=Public Latrine Surfaces, 8=particulate, 9=bathing
    for (j in 1:9){
      conc <- append(conc, 
                        list(conc=list(sample = conc_names[j],
                                       neighborhood = paste("Neighborhood", i),
                                       data = ec_data$ec_conc[which(ec_data$neighbor==sort(unique(ec_data$neighbor))[i] 
                                                                                          & ec_data$sample_type==j)])
                        )
      )
      
    }
  }
  return(conc)
}

# FREQUENCIES ----------------------------------------------------------------
calculate_freq <- function(..., type='pie chart', survey_type=NULL) {
  # calculate the appropriate factors for plotting pie charts
  # and people plots.  This can handle all of the different survey types
  # household, community, and school.  The function returns a long list.
  # Each object in the list contains 4 elements:
  # sample, neighborhood, age, and data
  # The first three offer the identifying information of the path
  # in question.  The final, data, has the frequency counts. Specify
  # the type of freqencies necessary by specifying type = 'ppl plot' 
  # or 'pie chart'.  Default is pie chart. 
  #
  # Ex.
  # > calculate_freq(household_data, type= 'pie chart')
  #  $path
  #  $path$sample
  #  [1] "Public Latrine"
  #  $path$age
  #  [1] "Children"
  #  $path$neighborhood
  #  [1] "Neighborhood 2"
  #  $path$data
  #  [1] 1 1 1 1 1 1 2 2 3 3 3 3 4 4 5 5
  #
  # Or Ex.
  # > calculate_freq(hh, sch, comm)

  
  # this allows us to pass multiple data objects without having to explictly 
  #say what they are. since the surveys always follow a pattern for the question
  # headers, we can figure out what data we have using that. 
  dat <- list(...)
  data_map <- c('household_data' = 'hh_', 'community_data' = 'com_', 'school_data' = 'sch_')
  
  # let's figure out what we have
  surveys_matched <- character()
  for (x in dat) { # look at each object that we passed in
    # check if any of the column headers match what we expect
    match <- sapply(data_map, function(dn) any(grepl(dn, names(x))))
    if (any(match)) {
      # if it matches, make an object with that name
      assign(names(data_map[match]), x)
      surveys_matched <- c(surveys_matched, names(data_map[match]))
    }
   }
  
  # some error handling
  if (!(length(surveys_matched) == 1 | length(surveys_matched) == 3)) {
    stop(paste0('Something is wrong with the data. Either pass 1 or 3 data objects.\n',
                'Matched objects: ', paste(surveys_matched, collapse=', ')))
  }   
  if (!any(surveys_matched %in% names(data_map))) {
    stop(paste('Unable to determine survey type. Do the column headers have hh, sch, or com in the names?\n',
               'Matched objects:', paste(surveys_matched, collapse=', ')))
    
  }
  
  if (is.null(survey_type)) {
    survey_type <- ifelse(length(surveys_matched) == 3, 'combined', gsub('_data', '', surveys_matched))
  }
  

  freq <- list()
  # For each pathway, we're going to look at the neighborhoods and ages-------
  # drain 
  for (i in 1:length(unique(household_data$neighbor))) {
    sub = list(path=
                 list(sample = 'Drain Water',
                      age = 'Adults',
                      neighborhood = paste('Neighborhood',i),
                      data = switch(survey_type,
                                    'combined' = c(as.numeric(household_data$hh_q6[which(household_data$hh_q6!="n/a" & household_data$neighbor==i)]),
                                                   as.numeric(c(rep(1,sum(school_data$sch_q9a[which(school_data$neighbor==i)])),rep(2,sum(school_data$sch_q9c[which(school_data$neighbor==i)])),
                                                                           rep(3,sum(school_data$sch_q9e[which(school_data$neighbor==i)])),rep(4,sum(school_data$sch_q9g[which(school_data$neighbor==i)])),
                                                                           rep(5,sum(school_data$sch_q9i[which(school_data$neighbor==i)])))),
                                                   as.numeric(c(rep(1,sum(community_data$com_q6a[which(community_data$neighbor==i)])),rep(2,sum(community_data$com_q6c[which(community_data$neighbor==i)])),
                                                                              rep(3,sum(community_data$com_q6e[which(community_data$neighbor==i)])),rep(4,sum(community_data$com_q6g[which(community_data$neighbor==i)]))))),
                                    'household' = as.numeric(household_data$hh_q6[which(household_data$hh_q6!="n/a" & household_data$neighbor==i)]),
                                    'school' = as.numeric(c(rep(1,sum(school_data$sch_q9a[which(school_data$neighbor==i)])),rep(2,sum(school_data$sch_q9c[which(school_data$neighbor==i)])),
                                                            rep(3,sum(school_data$sch_q9e[which(school_data$neighbor==i)])),rep(4,sum(school_data$sch_q9g[which(school_data$neighbor==i)])),
                                                            rep(5,sum(school_data$sch_q9i[which(school_data$neighbor==i)])))),
                                    'community' = as.numeric(c(rep(1,sum(community_data$com_q6a[which(community_data$neighbor==i)])),rep(2,sum(community_data$com_q6c[which(community_data$neighbor==i)])),
                                                               rep(3,sum(community_data$com_q6e[which(community_data$neighbor==i)])),rep(4,sum(community_data$com_q6g[which(community_data$neighbor==i)]))))
                      ) # end of switch
                                    
                      ),
               path=
                 list(sample = 'Drain Water',
                      age = 'Children',
                      neighborhood = paste('Neighborhood',i),
                      data = switch(survey_type, 
                                    'combined' = c(as.numeric(household_data$hh_q7[which(household_data$hh_q7!="n/a" & household_data$neighbor==i)]),
                                                   as.numeric(c(rep(1,sum(school_data$sch_q8a[which(school_data$neighbor==i)])),rep(2,sum(school_data$sch_q8c[which(school_data$neighbor==i)])),
                                                                           rep(3,sum(school_data$sch_q8e[which(school_data$neighbor==i)])),rep(4,sum(school_data$sch_q8g[which(school_data$neighbor==i)])))),
                                                   as.numeric(c(rep(1,sum(community_data$com_q21a[which(community_data$neighbor==i)])),rep(2,sum(community_data$com_q21c[which(community_data$neighbor==i)])),
                                                                               rep(3,sum(community_data$com_q21e[which(community_data$neighbor==i)])),rep(4,sum(community_data$com_q21g[which(community_data$neighbor==i)])),
                                                                               rep(5,sum(community_data$com_q21i[which(community_data$neighbor==i)]))))),
                                    'household' = as.numeric(household_data$hh_q7[which(household_data$hh_q7!="n/a" & household_data$neighbor==i)]),
                                    'school' = as.numeric(c(rep(1,sum(school_data$sch_q8a[which(school_data$neighbor==i)])),rep(2,sum(school_data$sch_q8c[which(school_data$neighbor==i)])),
                                                            rep(3,sum(school_data$sch_q8e[which(school_data$neighbor==i)])),rep(4,sum(school_data$sch_q8g[which(school_data$neighbor==i)])))),
                                    'community' =  as.numeric(c(rep(1,sum(community_data$com_q21a[which(community_data$neighbor==i)])),rep(2,sum(community_data$com_q21c[which(community_data$neighbor==i)])),
                                                                             rep(3,sum(community_data$com_q21e[which(community_data$neighbor==i)])),rep(4,sum(community_data$com_q21g[which(community_data$neighbor==i)])),
                                                                             rep(5,sum(community_data$com_q21i[which(community_data$neighbor==i)]))))
                      ) # end of switch
                      ),
               path=
                 list(sample = 'Produce',
                      age = 'Adults',
                      neighborhood = paste('Neighborhood',i),
                      data = switch(survey_type, 
                                    'combined' = c(as.numeric(household_data$hh_q13[which(household_data$hh_q13!="n/a" & household_data$neighbor==i)]),
                                                  as.numeric(c(rep(1,sum(school_data$sch_q15a[which(school_data$neighbor==i)])),rep(2,sum(school_data$sch_q15c[which(school_data$neighbor==i)])),
                                                                           rep(3,sum(school_data$sch_q15e[which(school_data$neighbor==i)])),rep(4,sum(school_data$sch_q15g[which(school_data$neighbor==i)])),
                                                                           rep(5,sum(school_data$sch_q15i[which(school_data$neighbor==i)])))),
                                                  as.numeric(c(rep(1,sum(community_data$com_q9a[which(community_data$neighbor==i)])),rep(2,sum(community_data$com_q9c[which(community_data$neighbor==i)])),
                                                                              rep(3,sum(community_data$com_q9e[which(community_data$neighbor==i)])),rep(4,sum(community_data$com_q9g[which(community_data$neighbor==i)]))))),
                                    'household' = as.numeric(household_data$hh_q13[which(household_data$hh_q13!="n/a" & household_data$neighbor==i)]),
                                    'school' = as.numeric(c(rep(1,sum(school_data$sch_q15a[which(school_data$neighbor==i)])),rep(2,sum(school_data$sch_q15c[which(school_data$neighbor==i)])),
                                                            rep(3,sum(school_data$sch_q15e[which(school_data$neighbor==i)])),rep(4,sum(school_data$sch_q15g[which(school_data$neighbor==i)])),
                                                            rep(5,sum(school_data$sch_q15i[which(school_data$neighbor==i)])))),
                                    'community' = as.numeric(c(rep(1,sum(community_data$com_q9a[which(community_data$neighbor==i)])),rep(2,sum(community_data$com_q9c[which(community_data$neighbor==i)])),
                                                               rep(3,sum(community_data$com_q9e[which(community_data$neighbor==i)])),rep(4,sum(community_data$com_q9g[which(community_data$neighbor==i)]))))
                      ) # End of switch
                      ),
               path=
                 list(sample = 'Produce',
                      age = 'Children',
                      neighborhood = paste('Neighborhood',i),
                      data = switch(survey_type, 
                                    'combined' = c(as.numeric(household_data$hh_q14[which(household_data$hh_q14!="n/a" & household_data$neighbor==i)]),
                                                   as.numeric(c(rep(1,sum(school_data$sch_q14a[which(school_data$neighbor==i)])),rep(2,sum(school_data$sch_q14c[which(school_data$neighbor==i)])),
                                                                           rep(3,sum(school_data$sch_q14e[which(school_data$neighbor==i)])),rep(4,sum(school_data$sch_q14g[which(school_data$neighbor==i)])))),
                                                   as.numeric(c(rep(1,sum(community_data$com_q24a[which(community_data$neighbor==i)])),rep(2,sum(community_data$com_q24c[which(community_data$neighbor==i)])),
                                                                              rep(3,sum(community_data$com_q24e[which(community_data$neighbor==i)])),rep(4,sum(community_data$com_q24g[which(community_data$neighbor==i)])),
                                                                              rep(5,sum(community_data$com_q24i[which(community_data$neighbor==i)]))))),
                                    'household' = as.numeric(household_data$hh_q14[which(household_data$hh_q14!="n/a" & household_data$neighbor==i)]),
                                    'school' = as.numeric(c(rep(1,sum(school_data$sch_q14a[which(school_data$neighbor==i)])),rep(2,sum(school_data$sch_q14c[which(school_data$neighbor==i)])),
                                                 rep(3,sum(school_data$sch_q14e[which(school_data$neighbor==i)])),rep(4,sum(school_data$sch_q14g[which(school_data$neighbor==i)])))),
                                    'community' = as.numeric(c(rep(1,sum(community_data$com_q24a[which(community_data$neighbor==i)])),rep(2,sum(community_data$com_q24c[which(community_data$neighbor==i)])),
                                                               rep(3,sum(community_data$com_q24e[which(community_data$neighbor==i)])),rep(4,sum(community_data$com_q24g[which(community_data$neighbor==i)])),
                                                               rep(5,sum(community_data$com_q24i[which(community_data$neighbor==i)]))))
                      ) # end of switch
                      ),
               path=
                 list(sample = 'Municipal and Piped Water',
                      age = 'Adults',
                      neighborhood = paste('Neighborhood',i),
                      data = switch(survey_type, 
                                    'combined' = c(as.numeric(household_data$hh_q10[which(household_data$hh_q10!="n/a" & household_data$neighbor==i)]),
                                                   as.numeric(c(rep(1,sum(school_data$sch_q13a[which(school_data$neighbor==i)])),rep(2,sum(school_data$sch_q13c[which(school_data$neighbor==i)])),
                                                                           rep(3,sum(school_data$sch_q13e[which(school_data$neighbor==i)])),rep(4,sum(school_data$sch_q13g[which(school_data$neighbor==i)])),
                                                                           rep(5,sum(school_data$sch_q13i[which(school_data$neighbor==i)])))),
                                                   as.numeric(c(rep(1,sum(community_data$com_q8a[which(community_data$neighbor==i)])),rep(2,sum(community_data$com_q8c[which(community_data$neighbor==i)])),
                                                                              rep(3,sum(community_data$com_q8e[which(community_data$neighbor==i)])),rep(4,sum(community_data$com_q8g[which(community_data$neighbor==i)]))))),
                                    'household' = as.numeric(household_data$hh_q10[which(household_data$hh_q10!="n/a" & household_data$neighbor==i)]),
                                    'school' = as.numeric(c(rep(1,sum(school_data$sch_q13a[which(school_data$neighbor==i)])),rep(2,sum(school_data$sch_q13c[which(school_data$neighbor==i)])),
                                                            rep(3,sum(school_data$sch_q13e[which(school_data$neighbor==i)])),rep(4,sum(school_data$sch_q13g[which(school_data$neighbor==i)])),
                                                            rep(5,sum(school_data$sch_q13i[which(school_data$neighbor==i)])))),
                                    'community' = as.numeric(c(rep(1,sum(community_data$com_q8a[which(community_data$neighbor==i)])),rep(2,sum(community_data$com_q8c[which(community_data$neighbor==i)])),
                                                               rep(3,sum(community_data$com_q8e[which(community_data$neighbor==i)])),rep(4,sum(community_data$com_q8g[which(community_data$neighbor==i)]))))
                      )# end of switch
                      ),
               path=
                 list(sample = 'Municipal and Piped Water',
                    age = 'Children',
                      neighborhood = paste('Neighborhood',i),
                      data = switch(survey_type, 
                                    'combined' = c(as.numeric(household_data$hh_q11[which(household_data$hh_q11!="n/a" & household_data$neighbor==i)]),
                                                   as.numeric(c(rep(1,sum(school_data$sch_q12a[which(school_data$neighbor==i)])),rep(2,sum(school_data$sch_q12c[which(school_data$neighbor==i)])),
                                                                           rep(3,sum(school_data$sch_q12e[which(school_data$neighbor==i)])),rep(4,sum(school_data$sch_q12g[which(school_data$neighbor==i)])))),
                                                   as.numeric(c(rep(1,sum(community_data$com_q23a[which(community_data$neighbor==i)])),rep(2,sum(community_data$com_q23c[which(community_data$neighbor==i)])),
                                                                              rep(3,sum(community_data$com_q23e[which(community_data$neighbor==i)])),rep(4,sum(community_data$com_q23g[which(community_data$neighbor==i)])),
                                                                              rep(5,sum(community_data$com_q23i[which(community_data$neighbor==i)]))))),
                                    'household' = as.numeric(household_data$hh_q11[which(household_data$hh_q11!="n/a" & household_data$neighbor==i)]),
                                    'school' = as.numeric(c(rep(1,sum(school_data$sch_q12a[which(school_data$neighbor==i)])),rep(2,sum(school_data$sch_q12c[which(school_data$neighbor==i)])),
                                                            rep(3,sum(school_data$sch_q12e[which(school_data$neighbor==i)])),rep(4,sum(school_data$sch_q12g[which(school_data$neighbor==i)])))),
                                    'community' = as.numeric(c(rep(1,sum(community_data$com_q23a[which(community_data$neighbor==i)])),rep(2,sum(community_data$com_q23c[which(community_data$neighbor==i)])),
                                                               rep(3,sum(community_data$com_q23e[which(community_data$neighbor==i)])),rep(4,sum(community_data$com_q23g[which(community_data$neighbor==i)])),
                                                               rep(5,sum(community_data$com_q23i[which(community_data$neighbor==i)]))))
                      ) # end of switch
                 ),
               path=
                 list(sample = 'Ocean Water',
                      age = 'Adults',
                      neighborhood = paste('Neighborhood',i),
                      data = switch(survey_type, 
                                    'combined' = c(as.numeric(household_data$hh_q2[which(household_data$hh_q2!="n/a" & household_data$neighbor==i)]),
                                                   as.numeric(c(rep(1,sum(school_data$sch_q5a[which(school_data$neighbor==i)])),rep(2,sum(school_data$sch_q5c[which(school_data$neighbor==i)])),
                                                                           rep(3,sum(school_data$sch_q5e[which(school_data$neighbor==i)])),rep(4,sum(school_data$sch_q5g[which(school_data$neighbor==i)])),
                                                                           rep(5,sum(school_data$sch_q5i[which(school_data$neighbor==i)])))),
                                                   as.numeric(c(rep(1,sum(community_data$com_q4a[which(community_data$neighbor==i)])),rep(2,sum(community_data$com_q4c[which(community_data$neighbor==i)])),
                                                                              rep(3,sum(community_data$com_q4e[which(community_data$neighbor==i)])),rep(4,sum(community_data$com_q4g[which(community_data$neighbor==i)]))))),
                                    'household' = as.numeric(household_data$hh_q2[which(household_data$hh_q2!="n/a" & household_data$neighbor==i)]),
                                    'school' = as.numeric(c(rep(1,sum(school_data$sch_q5a[which(school_data$neighbor==i)])),rep(2,sum(school_data$sch_q5c[which(school_data$neighbor==i)])),
                                                 rep(3,sum(school_data$sch_q5e[which(school_data$neighbor==i)])),rep(4,sum(school_data$sch_q5g[which(school_data$neighbor==i)])),
                                                 rep(5,sum(school_data$sch_q5i[which(school_data$neighbor==i)])))),
                                    'community' = as.numeric(c(rep(1,sum(community_data$com_q4a[which(community_data$neighbor==i)])),rep(2,sum(community_data$com_q4c[which(community_data$neighbor==i)])),
                                                               rep(3,sum(community_data$com_q4e[which(community_data$neighbor==i)])),rep(4,sum(community_data$com_q4g[which(community_data$neighbor==i)]))))
                      ) # end of switch
                                    
                 ),
               path=
                 list(sample = 'Ocean Water',
                      age = 'Children',
                      neighborhood = paste('Neighborhood',i),
                      data = switch(survey_type, 
                                    'combined' = c(as.numeric(household_data$hh_q3[which(household_data$hh_q3!="n/a" & household_data$neighbor==i)]),
                                                   as.numeric(c(rep(1,sum(school_data$sch_q4a[which(school_data$neighbor==i)])),rep(2,sum(school_data$sch_q4c[which(school_data$neighbor==i)])),
                                                                           rep(3,sum(school_data$sch_q4e[which(school_data$neighbor==i)])),rep(4,sum(school_data$sch_q4g[which(school_data$neighbor==i)])))),
                                                   as.numeric(c(rep(1,sum(community_data$com_q19a[which(community_data$neighbor==i)])),rep(2,sum(community_data$com_q19c[which(community_data$neighbor==i)])),
                                                                              rep(3,sum(community_data$com_q19e[which(community_data$neighbor==i)])),rep(4,sum(community_data$com_q19g[which(community_data$neighbor==i)])),
                                                                              rep(5,sum(community_data$com_q19i[which(community_data$neighbor==i)]))))),
                                    'household' = as.numeric(household_data$hh_q3[which(household_data$hh_q3!="n/a" & household_data$neighbor==i)]),
                                    'school' = as.numeric(c(rep(1,sum(school_data$sch_q4a[which(school_data$neighbor==i)])),rep(2,sum(school_data$sch_q4c[which(school_data$neighbor==i)])),
                                                            rep(3,sum(school_data$sch_q4e[which(school_data$neighbor==i)])),rep(4,sum(school_data$sch_q4g[which(school_data$neighbor==i)])))),
                                    'community' = as.numeric(c(rep(1,sum(community_data$com_q19a[which(community_data$neighbor==i)])),rep(2,sum(community_data$com_q19c[which(community_data$neighbor==i)])),
                                                               rep(3,sum(community_data$com_q19e[which(community_data$neighbor==i)])),rep(4,sum(community_data$com_q19g[which(community_data$neighbor==i)])),
                                                               rep(5,sum(community_data$com_q19i[which(community_data$neighbor==i)]))))
                      ) # end of switch
                 ),
               path=
                 list(sample = 'Surface Water',
                      age = 'Adults',
                      neighborhood = paste('Neighborhood',i),
                      data = switch(survey_type, 
                                    'combined' = c(as.numeric(household_data$hh_q4[which(household_data$hh_q4!="n/a" & household_data$neighbor==i)]),
                                                  as.numeric(c(rep(1,sum(school_data$sch_q7a[which(school_data$neighbor==i)])),rep(2,sum(school_data$sch_q7c[which(school_data$neighbor==i)])),
                                                                           rep(3,sum(school_data$sch_q7e[which(school_data$neighbor==i)])),rep(4,sum(school_data$sch_q7g[which(school_data$neighbor==i)])),
                                                                           rep(5,sum(school_data$sch_q7i[which(school_data$neighbor==i)])))),
                                                   as.numeric(c(rep(1,sum(community_data$com_q5a[which(community_data$neighbor==i)])),rep(2,sum(community_data$com_q5c[which(community_data$neighbor==i)])),
                                                                              rep(3,sum(community_data$com_q5e[which(community_data$neighbor==i)])),rep(4,sum(community_data$com_q5g[which(community_data$neighbor==i)]))))),
                                    'household' = as.numeric(household_data$hh_q4[which(household_data$hh_q4!="n/a" & household_data$neighbor==i)]),
                                    'school' = as.numeric(c(rep(1,sum(school_data$sch_q7a[which(school_data$neighbor==i)])),rep(2,sum(school_data$sch_q7c[which(school_data$neighbor==i)])),
                                                            rep(3,sum(school_data$sch_q7e[which(school_data$neighbor==i)])),rep(4,sum(school_data$sch_q7g[which(school_data$neighbor==i)])),
                                                            rep(5,sum(school_data$sch_q7i[which(school_data$neighbor==i)])))),
                                    'community' = as.numeric(c(rep(1,sum(community_data$com_q5a[which(community_data$neighbor==i)])),rep(2,sum(community_data$com_q5c[which(community_data$neighbor==i)])),
                                                               rep(3,sum(community_data$com_q5e[which(community_data$neighbor==i)])),rep(4,sum(community_data$com_q5g[which(community_data$neighbor==i)]))))
                      ) # end of switch
                 ),
               path=
                 list(sample = 'Surface Water',
                      age = 'Children',
                      neighborhood = paste('Neighborhood',i),
                      data = switch(survey_type, 
                                    'combined' = c(as.numeric(household_data$hh_q5[which(household_data$hh_q5!="n/a" & household_data$neighbor==i)]),
                                                   as.numeric(c(rep(1,sum(school_data$sch_q6a[which(school_data$neighbor==i)])),rep(2,sum(school_data$sch_q6c[which(school_data$neighbor==i)])),
                                                                           rep(3,sum(school_data$sch_q6e[which(school_data$neighbor==i)])),rep(4,sum(school_data$sch_q6g[which(school_data$neighbor==i)])))),
                                                   as.numeric(c(rep(1,sum(community_data$com_q20a[which(community_data$neighbor==i)])),rep(2,sum(community_data$com_q20c[which(community_data$neighbor==i)])),
                                                                              rep(3,sum(community_data$com_q20e[which(community_data$neighbor==i)])),rep(4,sum(community_data$com_q20g[which(community_data$neighbor==i)])),
                                                                              rep(5,sum(community_data$com_q20i[which(community_data$neighbor==i)]))))),
                                    'household' = as.numeric(household_data$hh_q5[which(household_data$hh_q5!="n/a" & household_data$neighbor==i)]),
                                    'school' = as.numeric(c(rep(1,sum(school_data$sch_q6a[which(school_data$neighbor==i)])),rep(2,sum(school_data$sch_q6c[which(school_data$neighbor==i)])),
                                                            rep(3,sum(school_data$sch_q6e[which(school_data$neighbor==i)])),rep(4,sum(school_data$sch_q6g[which(school_data$neighbor==i)])))),
                                    'community' = as.numeric(c(rep(1,sum(community_data$com_q20a[which(community_data$neighbor==i)])),rep(2,sum(community_data$com_q20c[which(community_data$neighbor==i)])),
                                                               rep(3,sum(community_data$com_q20e[which(community_data$neighbor==i)])),rep(4,sum(community_data$com_q20g[which(community_data$neighbor==i)])),
                                                               rep(5,sum(community_data$com_q20i[which(community_data$neighbor==i)]))))
                      ) # end of switch
                 ),
               path=
                 list(sample = 'Flood Water',
                      age = 'Adults',
                      neighborhood = paste('Neighborhood',i),
                      data = switch(survey_type, 
                                    'combined' = c(as.numeric(household_data$hh_q8[which(household_data$hh_q8!="n/a" & household_data$neighbor==i)]),
                                                   as.numeric(c(rep(1,sum(school_data$sch_q11a[which(school_data$neighbor==i)])),rep(2,sum(school_data$sch_q11c[which(school_data$neighbor==i)])),
                                                                           rep(3,sum(school_data$sch_q11e[which(school_data$neighbor==i)])),rep(4,sum(school_data$sch_q11g[which(school_data$neighbor==i)])),
                                                                           rep(5,sum(school_data$sch_q11i[which(school_data$neighbor==i)])))),
                                                   as.numeric(c(rep(1,sum(community_data$com_q7a[which(community_data$neighbor==i)])),rep(2,sum(community_data$com_q7c[which(community_data$neighbor==i)])),
                                                                              rep(3,sum(community_data$com_q7e[which(community_data$neighbor==i)])),rep(4,sum(community_data$com_q7g[which(community_data$neighbor==i)]))))),
                                    'household' = as.numeric(household_data$hh_q8[which(household_data$hh_q8!="n/a" & household_data$neighbor==i)]),
                                    'school' = as.numeric(c(rep(1,sum(school_data$sch_q11a[which(school_data$neighbor==i)])),rep(2,sum(school_data$sch_q11c[which(school_data$neighbor==i)])),
                                                            rep(3,sum(school_data$sch_q11e[which(school_data$neighbor==i)])),rep(4,sum(school_data$sch_q11g[which(school_data$neighbor==i)])),
                                                            rep(5,sum(school_data$sch_q11i[which(school_data$neighbor==i)])))),
                                    'community' = as.numeric(c(rep(1,sum(community_data$com_q7a[which(community_data$neighbor==i)])),rep(2,sum(community_data$com_q7c[which(community_data$neighbor==i)])),
                                                               rep(3,sum(community_data$com_q7e[which(community_data$neighbor==i)])),rep(4,sum(community_data$com_q7g[which(community_data$neighbor==i)]))))
                      ) # end of switch
                 ),
               path=
                 list(sample = 'Flood Water',
                      age = 'Children',
                      neighborhood = paste('Neighborhood',i),
                      data = switch(survey_type, 
                                    'combined' = c(as.numeric(household_data$hh_q9[which(household_data$hh_q9!="n/a" & household_data$neighbor==i)]),
                                                   as.numeric(c(rep(1,sum(school_data$sch_q10a[which(school_data$neighbor==i)])),rep(2,sum(school_data$sch_q10c[which(school_data$neighbor==i)])),
                                                                           rep(3,sum(school_data$sch_q10e[which(school_data$neighbor==i)])),rep(4,sum(school_data$sch_q10g[which(school_data$neighbor==i)])))),
                                                   as.numeric(c(rep(1,sum(community_data$com_q22a[which(community_data$neighbor==i)])),rep(2,sum(community_data$com_q22c[which(community_data$neighbor==i)])),
                                                                              rep(3,sum(community_data$com_q22e[which(community_data$neighbor==i)])),rep(4,sum(community_data$com_q22g[which(community_data$neighbor==i)])),
                                                                              rep(5,sum(community_data$com_q22i[which(community_data$neighbor==i)]))))),
                                    'household' = as.numeric(household_data$hh_q9[which(household_data$hh_q9!="n/a" & household_data$neighbor==i)]),
                                    'school' = as.numeric(c(rep(1,sum(school_data$sch_q10a[which(school_data$neighbor==i)])),rep(2,sum(school_data$sch_q10c[which(school_data$neighbor==i)])),
                                                            rep(3,sum(school_data$sch_q10e[which(school_data$neighbor==i)])),rep(4,sum(school_data$sch_q10g[which(school_data$neighbor==i)])))),
                                    'community' = as.numeric(c(rep(1,sum(community_data$com_q22a[which(community_data$neighbor==i)])),rep(2,sum(community_data$com_q22c[which(community_data$neighbor==i)])),
                                                               rep(3,sum(community_data$com_q22e[which(community_data$neighbor==i)])),rep(4,sum(community_data$com_q22g[which(community_data$neighbor==i)])),
                                                               rep(5,sum(community_data$com_q22i[which(community_data$neighbor==i)]))))
                      ) # end of switch
                 ),
               path=
                 list(sample = 'Public Latrine',
                      age = 'Adults',
                      neighborhood = paste('Neighborhood',i),
                      data = switch(survey_type, 
                                    'combined' = c(as.numeric(household_data$hh_q15[which(household_data$hh_q15!="n/a" & household_data$neighbor==i)]),
                                                   as.numeric(c(rep(1,sum(school_data$sch_q17a[which(school_data$neighbor==i)])),rep(2,sum(school_data$sch_q17c[which(school_data$neighbor==i)])),
                                                                           rep(3,sum(school_data$sch_q17e[which(school_data$neighbor==i)])),rep(4,sum(school_data$sch_q17g[which(school_data$neighbor==i)])),
                                                                           rep(5,sum(school_data$sch_q17i[which(school_data$neighbor==i)])))),
                                                   as.numeric(c(rep(1,sum(community_data$com_q10a[which(community_data$neighbor==i)])),rep(2,sum(community_data$com_q10c[which(community_data$neighbor==i)])),
                                                                              rep(3,sum(community_data$com_q10e[which(community_data$neighbor==i)])),rep(4,sum(community_data$com_q10g[which(community_data$neighbor==i)]))))),
                                    'household' = as.numeric(household_data$hh_q15[which(household_data$hh_q15!="n/a" & household_data$neighbor==i)]),
                                    'school' = as.numeric(c(rep(1,sum(school_data$sch_q17a[which(school_data$neighbor==i)])),rep(2,sum(school_data$sch_q17c[which(school_data$neighbor==i)])),
                                                            rep(3,sum(school_data$sch_q17e[which(school_data$neighbor==i)])),rep(4,sum(school_data$sch_q17g[which(school_data$neighbor==i)])),
                                                            rep(5,sum(school_data$sch_q17i[which(school_data$neighbor==i)])))),
                                    'community' = as.numeric(c(rep(1,sum(community_data$com_q10a[which(community_data$neighbor==i)])),rep(2,sum(community_data$com_q10c[which(community_data$neighbor==i)])),
                                                               rep(3,sum(community_data$com_q10e[which(community_data$neighbor==i)])),rep(4,sum(community_data$com_q10g[which(community_data$neighbor==i)]))))
                      ) # end of switch
                 ),
               path=
                 list(sample = 'Public Latrine',
                      age = 'Children',
                      neighborhood = paste('Neighborhood',i),
                      data = switch(survey_type, 
                                    'combined' = c(as.numeric(household_data$hh_q16[which(household_data$hh_q16!="n/a" & household_data$neighbor==i)]),
                                                   as.numeric(c(rep(1,sum(school_data$sch_q16a[which(school_data$neighbor==i)])),rep(2,sum(school_data$sch_q16c[which(school_data$neighbor==i)])),
                                                                           rep(3,sum(school_data$sch_q16e[which(school_data$neighbor==i)])),rep(4,sum(school_data$sch_q16g[which(school_data$neighbor==i)])))),
                                                   as.numeric(c(rep(1,sum(community_data$com_q25a[which(community_data$neighbor==i)])),rep(2,sum(community_data$com_q25c[which(community_data$neighbor==i)])),
                                                                               rep(3,sum(community_data$com_q25e[which(community_data$neighbor==i)])),rep(4,sum(community_data$com_q25g[which(community_data$neighbor==i)])),
                                                                               rep(5,sum(community_data$com_q25i[which(community_data$neighbor==i)]))))),
                                    'household' = as.numeric(household_data$hh_q16[which(household_data$hh_q16!="n/a" & household_data$neighbor==i)]),
                                    'school' = as.numeric(c(rep(1,sum(school_data$sch_q16a[which(school_data$neighbor==i)])),rep(2,sum(school_data$sch_q16c[which(school_data$neighbor==i)])),
                                                            rep(3,sum(school_data$sch_q16e[which(school_data$neighbor==i)])),rep(4,sum(school_data$sch_q16g[which(school_data$neighbor==i)])))),
                                    'community' =  as.numeric(c(rep(1,sum(community_data$com_q25a[which(community_data$neighbor==i)])),rep(2,sum(community_data$com_q25c[which(community_data$neighbor==i)])),
                                                                rep(3,sum(community_data$com_q25e[which(community_data$neighbor==i)])),rep(4,sum(community_data$com_q25g[which(community_data$neighbor==i)])),
                                                                rep(5,sum(community_data$com_q25i[which(community_data$neighbor==i)]))))
                      ) # end of switch
                 )
               
    )

    freq <- append(freq, sub)

  }
  
  # lastly, make sure it's the right numbers. 
  if (type == 'pie chart') {
    return(freq)
  }
  # frequencies for pie charts
  else if (type == 'ppl plot') {
    # if we want data for a people plot, calculate 4 - the value per vector object in the list
    for (i in 1:length(freq)) {
      freq[[i]]$data <- 4 - freq[[i]]$data
    }
    return(freq)
  }
  else {
    warning('Unknown type.  Options are "pie chart" or "ppl plot"\n')
  }
}



create_freqTbl <- function(freq_vector, sample_type) {
  # convert the answers from the frequency calculation funcitons into 
  # a table for plotting
  labels <- unlist(ifelse(sample_type=='Municipal and Piped Water', 
                          list(c("everyday","4-6/wk","1-3/mo","never","don't know")),
                          list(c(">10/mo","6-10/mo","1-5/mo","never","don't know"))
  )
  )
  colors <- c('#00FF00', '#99FF00', '#FF6600', '#FF0000', '#333333')
  
  tbl <- as.data.frame(table('answer'= freq_vector))
  tbl$color <- factor(colors[tbl$answer], levels=colors[tbl$answer])
  tbl$answer <- labels[tbl$answer]
  tbl$breaks <- cumsum(tbl$Freq) - tbl$Freq / 2
  tbl$labels = paste(tbl$answer, "\n", paste0(round(tbl$Freq / sum(tbl$Freq) * 100, 1),"%")) 
  return(tbl)
}

# People Plotting
calculate_pplPlotData <- function(freq, conc, nburn=1000, niter=10000, thin=1, cutpoint=c(0, 5, 10), shinySession=NULL) {
  # function to caclulate the percent of population 
  # exposed for all pathways given.  performs Bayesian 
  # analysis on behavior and environmental data first
  # then calculates the final statistics for plotting
  
  # run the Bayesian analyses
  freq <- bayesian_behavior_estimates(freq, nburn, niter, thin, cutpoint, shinySession=shinySession)
  conc <- bayesian_environmental_estimates(conc, nburn, niter, thin, shinySession=shinySession)
  
  
  # based on the original ps_plot section of the shiny server,
  # it seems they are based on the behavoir data
  # need to find the number of neighborhoods
  # and samples
  neighborhoods <- unique(names(list.names(freq, neighborhood))) # unique neighborhood values
  samples <- unique(names(list.names(freq, sample))) # unique sample values
  age <- unique(names(list.names(freq, age)))
  
  for (smp in samples) {
    for (nb in neighborhoods) {
      # filter the concentration data to just this neighborhood and sample
      sub.conc <- conc[[list.which(conc, neighborhood == nb && sample == smp)]]
      
      # calculate exposure for adults and children using the behavior data
      for (a in age) {
        # filter frequency to just the age we want 
        sub.freq <- freq[[list.which(freq, sample == smp && neighborhood == nb && age == a)]]
        
        # calculate the exposure
        exposed <- calculate_exposure(sub.freq, sub.conc)
        
        # update the object at this position
        freq[[list.which(freq, sample == smp && neighborhood == nb && age == a)]] <- exposed     
      }
    }
  }
  
  # give back the updated behavior data object
  return(freq)
}

bayesian_environmental_estimates <- function(conc, nburn=1000, niter=10000, thin=1, shinySession=NULL) {
  # Run bayesian model on the environmental data collected.  this will be run for each 
  # neighborhood, age, and sample combination.  Warning: Could take quite a while. 
  # Future development: Way of backgrounding this?
  calcul <- paste(nburn,niter,thin,sep="|")
  
  # environmental samples
  tomonitor <- c("mu","sigma")
  if (!is.null(shinySession)) {
    withProgress({
      for (k in 1:length(conc)){
        log_ec<-log10(as.numeric(conc[[k]]$data))
        env_data<-list(lnconc=log_ec,N=length(log_ec))
        
        modelpos <- jags.model(file="./model/env_model.jags",data=env_data,n.chains=3);
        update(modelpos,n.burn=nburn);
        env_mcmcpos <- coda.samples(modelpos,tomonitor,n.iter=niter,thin=thin);
        #Bayesian estimators of mu and sigma
        mu <-summary(env_mcmcpos)$statistics[1,1]
        sigma <-summary(env_mcmcpos)$statistics[2,1]
        
        conc[[k]] <- append(conc[[k]], list('mu' = mu, 'sigma' = sigma))
        incProgress(k/length(conc), session=shinySession)
      }
    }, message='Bayesian Environmental Analysis', session=shinySession, value=0)
  }
  else {
    for (k in 1:length(conc)){
      log_ec<-log10(as.numeric(conc[[k]]$data))
      env_data<-list(lnconc=log_ec,N=length(log_ec))
      
      modelpos <- jags.model(file="./model/env_model.jags",data=env_data,n.chains=3);
      update(modelpos,n.burn=nburn);
      env_mcmcpos <- coda.samples(modelpos,tomonitor,n.iter=niter,thin=thin);
      #Bayesian estimators of mu and sigma
      mu <-summary(env_mcmcpos)$statistics[1,1]
      sigma <-summary(env_mcmcpos)$statistics[2,1]
      
      conc[[k]] <- append(conc[[k]], list('mu' = mu, 'sigma' = sigma))
      
    }
  }
  

  return(conc)
}

bayesian_behavior_estimates <- function(freq, nburn=1000, niter=10000, thin=1, cutpoint=c(0, 5, 10), shinySession=NULL) {
  # Run bayesian model on the behavior data collected.  this will be run for each 
  # neighborhood, age, and sample combination.  Warning: Could take quite a while. 
  # Future development: Way of backgrounding this?
  
  bemonitor <- c("p","r")
  calcul <- paste(nburn,niter,thin,sep="|")
  if (!is.null(shinySession)) {
    withProgress({
      # Look at each sample combination taken
      for (k in 1:length(freq)) {
        print(k)
        # set up the data for analysis to be passed to jags
        freq_be0=freq[[k]]$data
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
        
        # Jags model runs
        modelpos <- jags.model(file="./model/be_model.jags",data=be_data,n.chains=3,inits=init)
        update(modelpos,n.burn=nburn)
        cat('Coda Samples\n\n')
        be_mcmcpos <- coda.samples(modelpos, bemonitor, n.iter=niter, thin=thin)
        
        # Extract results
        # Bayesian estimators of p and r
        p <-summary(be_mcmcpos)$statistics[1,1]
        r <-summary(be_mcmcpos)$statistics[2,1]
        
        freq[[k]] <- append(freq[[k]], list('p' = p, 'r' = r))
        
        incProgress(k/length(freq), session=shinySession)
      }
      
    }, message='Bayesian Behavior Analysis', session=shinySession, value=0)
  }
  else {
    for (k in 1:length(freq)) {
      print(k)
      # set up the data for analysis to be passed to jags
      freq_be0=freq[[k]]$data
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
      
      # Jags model runs
      modelpos <- jags.model(file="./model/be_model.jags",data=be_data,n.chains=3,inits=init)
      update(modelpos,n.burn=nburn)
      cat('Coda Samples\n\n')
      be_mcmcpos <- coda.samples(modelpos, bemonitor, n.iter=niter, thin=thin)
      
      # Extract results
      # Bayesian estimators of p and r
      p <-summary(be_mcmcpos)$statistics[1,1]
      r <-summary(be_mcmcpos)$statistics[2,1]
      
      freq[[k]] <- append(freq[[k]], list('p' = p, 'r' = r))
      
    }
  }
  return(freq)
}

calculate_exposure <- function(behavior_data, concentration_data) {
  # used for people plot generation.  this is for a single pathway
  # and assumes the data are already subset to the appropriate level
  e <- rep(NA, 1000)
  f <- rep(NA, 1000)
  risk <- rep(NA, 1000)
  
  # values applied based on sample and age
  intake<-array(c(0.0006,1,10.43,0.0154,0.037,0.0006,0.034,NA,0.0499,
                  0.01,0.5,4.14,0.2042,0.2042,0.01,0.034,NA,0.09975),c(9,2)) #need to input this information!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  rownames(intake) <- c('Drain Water', 'Produce', 'Municipal and Piped Water', 'Ocean Water', 'Surface Water', 
                        'Flood Water', 'Public Latrine Surfaces', 'Particulate', 'Bathing')
  colnames(intake) <- c("Adults", "Children")
  
  # simulate some numbers
  for (m in 1:1000){
    # is it necessary for this to be in a loop?
    e[m] <- rnorm(1, concentration_data$mu, concentration_data$sigma)
    f[m] <- rnbinom(1, size= behavior_data$r, prob= behavior_data$p)
    risk[m] <- f[m]* (10^e[m]) * intake[behavior_data$sample, behavior_data$age]
  }
  
  non0 <- function(mc){
    tmp <- mc; tmp[!(tmp>0)] <- NA
    return(tmp)
  }
  
  n <-(1-length(which(f==0))/1000)*100;
  dose <-log10(mean(non0(risk),na.rm=TRUE))
  # add the percent exposure and dose information to the behavior data
  behavior_data <- append(behavior_data, list('n' = n, 'dose' = dose))
  
  # give the updated object back
  return(behavior_data)
  
}


