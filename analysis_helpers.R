## Analysis helpers for the SaniPath Analysis tool
## these have been derived from the original server.R 
## file.
library(ggplot2)

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
  
  conc<-list()
  for (i in 1:length(unique(as.numeric(ec_data$neighbor)))){
    # sample type 1=drain water, 2=produce, 3=piped water, 4=ocean water, 5=surface water, 6=flood water, 7=Public Latrine Surfaces, 8=particulate, 9=bathing
    for (j in 1:9){
      conc[[9*(sort(unique(as.numeric(ec_data$neighbor)))[i]-1)+j]]=ec_data$ec_conc[which(ec_data$neighbor==sort(unique(ec_data$neighbor))[i] 
                                                                                          & ec_data$sample_type==j)]
    }
  }
  return(conc)
}

# FREQUENCIES ----------------------------------------------------------------
freq_to_ppl_plot <- function(freq) {
  # take an object of freq values and convert them for ps plotting
  for (p in 1:length(freq)) { # each path
    for (n in 1:length(freq[[p]])) { # each neighborhood
      for (a in 1:length(freq[[p]][[n]])) { # each age
        if (length(freq[[p]][[n]][[a]]) > 0) {
          freq[[p]][[n]][[a]] <- (4 - freq[[p]][[n]][[a]])
        }
      }
    }
  }
  return(freq)
}

calculate_householdFreq <- function(household_data, type='pie chart') {
  # calculate the appropriate factors for plotting pie charts
  # and people plots.  This is specific to the household 
  # form. returns a list with two objects. type can equal
  # 'pie chart' or 'ppl plot'
  #
  # Ex.
  # > calculate_householdFreq(household_data, 'pie chart')
  # $drain$neighborhood1$adults
  # [1] 4 4 4 4 4 4 ...
  

  freq <- list('drain' = list(), 'produce' = list(), 'piped_water' = list(), 'ocean_water' = list(),
               'surface_water' = list(), 'flood_water' = list(), 'public_latrine' = list()
  )
  
  # For each pathway, we're going to look at the neighborhoods and ages-------
  # drain 
  for (i in 1:length(unique(household_data$neighbor))) {
    sub <- list(
      list('adults' = as.numeric(household_data$hh_q6[which(household_data$hh_q6!="n/a" & household_data$neighbor==i)]),
               'children' = as.numeric(household_data$hh_q7[which(household_data$hh_q7!="n/a" & household_data$neighbor==i)])
          )
    )
    names(sub) <- paste0('neighborhood',i)
    freq$drain <- append(freq$drain, sub)
            
  }
  
  # produce
  for (i in 1:length(unique(household_data$neighbor))) {
    sub <- list(
      list('adults' = as.numeric(household_data$hh_q13[which(household_data$hh_q13!="n/a" & household_data$neighbor==i)]),
           'children' = as.numeric(household_data$hh_q14[which(household_data$hh_q14!="n/a" & household_data$neighbor==i)])
      )
    )
    names(sub) <- paste0('neighborhood',i)
    freq$produce <- append(freq$produce, sub)
    
  }
  
  # municipal/piped water
  for (i in 1:length(unique(household_data$neighbor))) {
    sub <- list(
      list('adults' = as.numeric(household_data$hh_q10[which(household_data$hh_q10!="n/a" & household_data$neighbor==i)]),
           'children' = as.numeric(household_data$hh_q11[which(household_data$hh_q11!="n/a" & household_data$neighbor==i)])
      )
    )
    names(sub) <- paste0('neighborhood',i)
    freq$piped_water <- append(freq$piped_water, sub)
    
  }
  
  # ocean water
  for (i in 1:length(unique(household_data$neighbor))) {
    sub <- list(
      list('adults' = as.numeric(household_data$hh_q2[which(household_data$hh_q2!="n/a" & household_data$neighbor==i)]),
           'children' = as.numeric(household_data$hh_q3[which(household_data$hh_q3!="n/a" & household_data$neighbor==i)])
      )
    )
    names(sub) <- paste0('neighborhood',i)
    freq$ocean_water <- append(freq$ocean_water, sub)
    
  }

  # surface water
  for (i in 1:length(unique(household_data$neighbor))) {
    sub <- list(
      list('adults' = as.numeric(household_data$hh_q4[which(household_data$hh_q4!="n/a" & household_data$neighbor==i)]),
           'children' = as.numeric(household_data$hh_q5[which(household_data$hh_q5!="n/a" & household_data$neighbor==i)])
      )
    )
    names(sub) <- paste0('neighborhood',i)
    freq$surface_water <- append(freq$surface_water, sub)
    
  }
  
  # flood water
  for (i in 1:length(unique(household_data$neighbor))) {
    sub <- list(
      list('adults' = as.numeric(household_data$hh_q8[which(household_data$hh_q8!="n/a" & household_data$neighbor==i)]),
           'children' = as.numeric(household_data$hh_q9[which(household_data$hh_q9!="n/a" & household_data$neighbor==i)])
      )
    )
    names(sub) <- paste0('neighborhood',i)
    freq$flood_water <- append(freq$flood_water, sub)
    
  }
  
  # public latrine
  for (i in 1:length(unique(household_data$neighbor))) {
    sub <- list(
      list('adults' = as.numeric(household_data$hh_q15[which(household_data$hh_q15!="n/a" & household_data$neighbor==i)]),
           'children' = as.numeric(household_data$hh_q16[which(household_data$hh_q16!="n/a" & household_data$neighbor==i)])
      )
    )
    names(sub) <- paste0('neighborhood',i)
    freq$public_latrine <- append(freq$public_latrine, sub)
    
  }

  if (type == 'pie chart') {
    # freq <- lapply(freq, function(x) round(table(x)/sum(table(x)),3))
    return(freq)
  }
  # frequencies for pie charts
  else if (type == 'ppl plot') {
    # if we want data for a people plot, calculate 4 - the value per vector object in the list
    freq <- freq_to_ppl_plot(freq)
    return(freq)
  }
  else {
    warning('Unknown type.  Options are "pie chart" or "ppl plot"\n')
  }
}


calculate_schoolFreq <- function(school_data, type='pie chart') {
  # calculate the appropriate factors for plotting pie charts
  # and people plots.  This is specific to the school 
  # form. returns a list with two objects. type can equal
  # 'pie chart' or 'ppl plot'
  #
  # Ex.
  # > calculate_schoolFreq(school_data, 'pie chart')
  # $[[1]]
  # [1] 3 4 2 1
  # ...
  
  # it's the same calculations for a pie chart or a people plot, just shifted. 
  freq <- list('drain' = list(), 'produce' = list(), 'piped_water' = list(), 'ocean_water' = list(),
               'surface_water' = list(), 'flood_water' = list(), 'public_latrine' = list()
  )
  
  for (i in 1:length(unique(school_data$neighbor))) {
    sub <- list(
      list('adults' = as.numeric(c(rep(1,sum(school_data$sch_q9a[which(school_data$neighbor==i)])),rep(2,sum(school_data$sch_q9c[which(school_data$neighbor==i)])),
                                   rep(3,sum(school_data$sch_q9e[which(school_data$neighbor==i)])),rep(4,sum(school_data$sch_q9g[which(school_data$neighbor==i)])),
                                   rep(5,sum(school_data$sch_q9i[which(school_data$neighbor==i)])))),
           'children' = as.numeric(c(rep(1,sum(school_data$sch_q8a[which(school_data$neighbor==i)])),rep(2,sum(school_data$sch_q8c[which(school_data$neighbor==i)])),
                                     rep(3,sum(school_data$sch_q8e[which(school_data$neighbor==i)])),rep(4,sum(school_data$sch_q8g[which(school_data$neighbor==i)]))))
      )
    )
    names(sub) <- paste0('neighborhood',i)
    freq$drain <- append(freq$drain, sub)
    
  }
  for (i in 1:length(unique(school_data$neighbor))) {
    sub <- list(
      list('adults' = as.numeric(c(rep(1,sum(school_data$sch_q15a[which(school_data$neighbor==i)])),rep(2,sum(school_data$sch_q15c[which(school_data$neighbor==i)])),
                                   rep(3,sum(school_data$sch_q15e[which(school_data$neighbor==i)])),rep(4,sum(school_data$sch_q15g[which(school_data$neighbor==i)])),
                                   rep(5,sum(school_data$sch_q15i[which(school_data$neighbor==i)])))),
           'children' = as.numeric(c(rep(1,sum(school_data$sch_q14a[which(school_data$neighbor==i)])),rep(2,sum(school_data$sch_q14c[which(school_data$neighbor==i)])),
                                     rep(3,sum(school_data$sch_q14e[which(school_data$neighbor==i)])),rep(4,sum(school_data$sch_q14g[which(school_data$neighbor==i)]))))
      )
    )
    names(sub) <- paste0('neighborhood',i)
    freq$produce <- append(freq$produce, sub)
    
  }
  for (i in 1:length(unique(school_data$neighbor))) {
    sub <- list(
      list('adults' = as.numeric(c(rep(1,sum(school_data$sch_q13a[which(school_data$neighbor==i)])),rep(2,sum(school_data$sch_q13c[which(school_data$neighbor==i)])),
                                   rep(3,sum(school_data$sch_q13e[which(school_data$neighbor==i)])),rep(4,sum(school_data$sch_q13g[which(school_data$neighbor==i)])),
                                   rep(5,sum(school_data$sch_q13i[which(school_data$neighbor==i)])))),
           'children' = as.numeric(c(rep(1,sum(school_data$sch_q12a[which(school_data$neighbor==i)])),rep(2,sum(school_data$sch_q12c[which(school_data$neighbor==i)])),
                                     rep(3,sum(school_data$sch_q12e[which(school_data$neighbor==i)])),rep(4,sum(school_data$sch_q12g[which(school_data$neighbor==i)]))))
      )
    )
    names(sub) <- paste0('neighborhood',i)
    freq$piped_water <- append(freq$piped_water, sub)
    
  }
  for (i in 1:length(unique(school_data$neighbor))) {
    sub <- list(
      list('adults' = as.numeric(c(rep(1,sum(school_data$sch_q5a[which(school_data$neighbor==i)])),rep(2,sum(school_data$sch_q5c[which(school_data$neighbor==i)])),
                                   rep(3,sum(school_data$sch_q5e[which(school_data$neighbor==i)])),rep(4,sum(school_data$sch_q5g[which(school_data$neighbor==i)])),
                                   rep(5,sum(school_data$sch_q5i[which(school_data$neighbor==i)])))),
           'children' = as.numeric(c(rep(1,sum(school_data$sch_q4a[which(school_data$neighbor==i)])),rep(2,sum(school_data$sch_q4c[which(school_data$neighbor==i)])),
                                     rep(3,sum(school_data$sch_q4e[which(school_data$neighbor==i)])),rep(4,sum(school_data$sch_q4g[which(school_data$neighbor==i)]))))
      )
    )
    names(sub) <- paste0('neighborhood',i)
    freq$ocean_water <- append(freq$ocean_water, sub)
    
  }
  
  for (i in 1:length(unique(school_data$neighbor))) {
    sub <- list(
      list('adults' = as.numeric(c(rep(1,sum(school_data$sch_q7a[which(school_data$neighbor==i)])),rep(2,sum(school_data$sch_q7c[which(school_data$neighbor==i)])),
                                   rep(3,sum(school_data$sch_q7e[which(school_data$neighbor==i)])),rep(4,sum(school_data$sch_q7g[which(school_data$neighbor==i)])),
                                   rep(5,sum(school_data$sch_q7i[which(school_data$neighbor==i)])))),
           'children' = as.numeric(c(rep(1,sum(school_data$sch_q6a[which(school_data$neighbor==i)])),rep(2,sum(school_data$sch_q6c[which(school_data$neighbor==i)])),
                                     rep(3,sum(school_data$sch_q6e[which(school_data$neighbor==i)])),rep(4,sum(school_data$sch_q6g[which(school_data$neighbor==i)]))))
      )
    )
    names(sub) <- paste0('neighborhood',i)
    freq$surface_water <- append(freq$surface_water, sub)
    
  }
  
  for (i in 1:length(unique(school_data$neighbor))) {
    sub <- list(
      list('adults' = as.numeric(c(rep(1,sum(school_data$sch_q11a[which(school_data$neighbor==i)])),rep(2,sum(school_data$sch_q11c[which(school_data$neighbor==i)])),
                                   rep(3,sum(school_data$sch_q11e[which(school_data$neighbor==i)])),rep(4,sum(school_data$sch_q11g[which(school_data$neighbor==i)])),
                                   rep(5,sum(school_data$sch_q11i[which(school_data$neighbor==i)])))),
           'children' = as.numeric(c(rep(1,sum(school_data$sch_q10a[which(school_data$neighbor==i)])),rep(2,sum(school_data$sch_q10c[which(school_data$neighbor==i)])),
                                     rep(3,sum(school_data$sch_q10e[which(school_data$neighbor==i)])),rep(4,sum(school_data$sch_q10g[which(school_data$neighbor==i)]))))
      )
    )
    names(sub) <- paste0('neighborhood',i)
    freq$flood_water <- append(freq$flood_water, sub)
    
  }
  
  for (i in 1:length(unique(school_data$neighbor))) {
    sub <- list(
      list('adults' = as.numeric(c(rep(1,sum(school_data$sch_q17a[which(school_data$neighbor==i)])),rep(2,sum(school_data$sch_q17c[which(school_data$neighbor==i)])),
                                   rep(3,sum(school_data$sch_q17e[which(school_data$neighbor==i)])),rep(4,sum(school_data$sch_q17g[which(school_data$neighbor==i)])),
                                   rep(5,sum(school_data$sch_q17i[which(school_data$neighbor==i)])))),
           'children' = as.numeric(c(rep(1,sum(school_data$sch_q16a[which(school_data$neighbor==i)])),rep(2,sum(school_data$sch_q16c[which(school_data$neighbor==i)])),
                                     rep(3,sum(school_data$sch_q16e[which(school_data$neighbor==i)])),rep(4,sum(school_data$sch_q16g[which(school_data$neighbor==i)]))))
      )
    )
    names(sub) <- paste0('neighborhood',i)
    freq$public_latrine <- append(freq$public_latrine, sub)
    
  }
 
  # it's the same calculations for a pie chart or a people plot, just shifted. 
  if (type == 'pie chart') {
    return(freq)
  }
  # frequencies for pie charts
  else if (type == 'ppl plot') {
    # if we want data for a people plot, calculate 4 - the value per vector object in the list to shift the values
    freq <- freq_to_ppl_plot(freq)
    return(freq)
  }
  else {
    cat('Unknown type.  Options are "pie chart" or "ppl plot"\n')
  }
  
}

calculate_communityFreq <- function(community_data, type='pie chart') {
  # calculate the appropriate factors for plotting pie charts
  # and people plots.  This is specific to the community 
  # form. returns a list with two objects. type can equal
  # 'pie chart' or 'ppl plot'
  #
  # Ex.
  # > calculate_communityFreq(community_data, 'pie chart')
  # $[[1]]
  # [1] 3 4 2 1
  # ...
  freq <- list('drain' = list(), 'produce' = list(), 'piped_water' = list(), 'ocean_water' = list(),
               'surface_water' = list(), 'flood_water' = list(), 'public_latrine' = list()
  )
  
  for (i in 1:length(unique(community_data$neighbor))) {
    sub <- list(
      list('adults' = as.numeric(c(rep(1,sum(community_data$com_q6a[which(community_data$neighbor==i)])),rep(2,sum(community_data$com_q6c[which(community_data$neighbor==i)])),
                                   rep(3,sum(community_data$com_q6e[which(community_data$neighbor==i)])),rep(4,sum(community_data$com_q6g[which(community_data$neighbor==i)])))),
           'children' = as.numeric(c(rep(1,sum(community_data$com_q21a[which(community_data$neighbor==i)])),rep(2,sum(community_data$com_q21c[which(community_data$neighbor==i)])),
                                     rep(3,sum(community_data$com_q21e[which(community_data$neighbor==i)])),rep(4,sum(community_data$com_q21g[which(community_data$neighbor==i)])),
                                     rep(5,sum(community_data$com_q21i[which(community_data$neighbor==i)]))))
      )
    )
    names(sub) <- paste0('neighborhood',i)
    freq$drain <- append(freq$drain, sub)
    
  }
  
  for (i in 1:length(unique(community_data$neighbor))) {
    sub <- list(
      list('adults' = as.numeric(c(rep(1,sum(community_data$com_q9a[which(community_data$neighbor==i)])),rep(2,sum(community_data$com_q9c[which(community_data$neighbor==i)])),
                                   rep(3,sum(community_data$com_q9e[which(community_data$neighbor==i)])),rep(4,sum(community_data$com_q9g[which(community_data$neighbor==i)])))),
           'children' = as.numeric(c(rep(1,sum(community_data$com_q24a[which(community_data$neighbor==i)])),rep(2,sum(community_data$com_q24c[which(community_data$neighbor==i)])),
                                     rep(3,sum(community_data$com_q24e[which(community_data$neighbor==i)])),rep(4,sum(community_data$com_q24g[which(community_data$neighbor==i)])),
                                     rep(5,sum(community_data$com_q24i[which(community_data$neighbor==i)]))))
      )
    )
    names(sub) <- paste0('neighborhood',i)
    freq$produce <- append(freq$produce, sub)
    
  }

  for (i in 1:length(unique(community_data$neighbor))) {
    sub <- list(
      list('adults' = as.numeric(c(rep(1,sum(community_data$com_q8a[which(community_data$neighbor==i)])),rep(2,sum(community_data$com_q8c[which(community_data$neighbor==i)])),
                                   rep(3,sum(community_data$com_q8e[which(community_data$neighbor==i)])),rep(4,sum(community_data$com_q8g[which(community_data$neighbor==i)])))),
           'children' = as.numeric(c(rep(1,sum(community_data$com_q23a[which(community_data$neighbor==i)])),rep(2,sum(community_data$com_q23c[which(community_data$neighbor==i)])),
                                     rep(3,sum(community_data$com_q23e[which(community_data$neighbor==i)])),rep(4,sum(community_data$com_q23g[which(community_data$neighbor==i)])),
                                     rep(5,sum(community_data$com_q23i[which(community_data$neighbor==i)]))))
      )
    )
    names(sub) <- paste0('neighborhood',i)
    freq$piped_water <- append(freq$piped_water, sub)
    
  }
  
  for (i in 1:length(unique(community_data$neighbor))) {
    sub <- list(
      list('adults' = as.numeric(c(rep(1,sum(community_data$com_q4a[which(community_data$neighbor==i)])),rep(2,sum(community_data$com_q4c[which(community_data$neighbor==i)])),
                                   rep(3,sum(community_data$com_q4e[which(community_data$neighbor==i)])),rep(4,sum(community_data$com_q4g[which(community_data$neighbor==i)])))),
           'children' = as.numeric(c(rep(1,sum(community_data$com_q19a[which(community_data$neighbor==i)])),rep(2,sum(community_data$com_q19c[which(community_data$neighbor==i)])),
                                     rep(3,sum(community_data$com_q19e[which(community_data$neighbor==i)])),rep(4,sum(community_data$com_q19g[which(community_data$neighbor==i)])),
                                     rep(5,sum(community_data$com_q19i[which(community_data$neighbor==i)]))))
      )
    )
    names(sub) <- paste0('neighborhood',i)
    freq$ocean_water <- append(freq$ocean_water, sub)
    
  }
  
  for (i in 1:length(unique(community_data$neighbor))) {
    sub <- list(
      list('adults' = as.numeric(c(rep(1,sum(community_data$com_q5a[which(community_data$neighbor==i)])),rep(2,sum(community_data$com_q5c[which(community_data$neighbor==i)])),
                                   rep(3,sum(community_data$com_q5e[which(community_data$neighbor==i)])),rep(4,sum(community_data$com_q5g[which(community_data$neighbor==i)])))),
           'children' = as.numeric(c(rep(1,sum(community_data$com_q20a[which(community_data$neighbor==i)])),rep(2,sum(community_data$com_q20c[which(community_data$neighbor==i)])),
                                     rep(3,sum(community_data$com_q20e[which(community_data$neighbor==i)])),rep(4,sum(community_data$com_q20g[which(community_data$neighbor==i)])),
                                     rep(5,sum(community_data$com_q20i[which(community_data$neighbor==i)]))))
      )
    )
    names(sub) <- paste0('neighborhood',i)
    freq$surface_water <- append(freq$surface_water, sub)
    
  }
  
  for (i in 1:length(unique(community_data$neighbor))) {
    sub <- list(
      list('adults' = as.numeric(c(rep(1,sum(community_data$com_q7a[which(community_data$neighbor==i)])),rep(2,sum(community_data$com_q7c[which(community_data$neighbor==i)])),
                                   rep(3,sum(community_data$com_q7e[which(community_data$neighbor==i)])),rep(4,sum(community_data$com_q7g[which(community_data$neighbor==i)])))),
           'children' = as.numeric(c(rep(1,sum(community_data$com_q22a[which(community_data$neighbor==i)])),rep(2,sum(community_data$com_q22c[which(community_data$neighbor==i)])),
                                     rep(3,sum(community_data$com_q22e[which(community_data$neighbor==i)])),rep(4,sum(community_data$com_q22g[which(community_data$neighbor==i)])),
                                     rep(5,sum(community_data$com_q22i[which(community_data$neighbor==i)]))))
      )
    )
    names(sub) <- paste0('neighborhood',i)
    freq$flood_water <- append(freq$flood_water, sub)
    
  }
  
  for (i in 1:length(unique(community_data$neighbor))) {
    sub <- list(
      list('adults' = as.numeric(c(rep(1,sum(community_data$com_q10a[which(community_data$neighbor==i)])),rep(2,sum(community_data$com_q10c[which(community_data$neighbor==i)])),
                                   rep(3,sum(community_data$com_q10e[which(community_data$neighbor==i)])),rep(4,sum(community_data$com_q10g[which(community_data$neighbor==i)])))),
           'children' = as.numeric(c(rep(1,sum(community_data$com_q25a[which(community_data$neighbor==i)])),rep(2,sum(community_data$com_q25c[which(community_data$neighbor==i)])),
                                     rep(3,sum(community_data$com_q25e[which(community_data$neighbor==i)])),rep(4,sum(community_data$com_q25g[which(community_data$neighbor==i)])),
                                     rep(5,sum(community_data$com_q25i[which(community_data$neighbor==i)]))))
      )
    )
    names(sub) <- paste0('neighborhood',i)
    freq$public_latrine <- append(freq$public_latrine, sub)
    
  }
    
  # it's the same calculations for a pie chart or a people plot, just shifted. 
  if (type == 'pie chart') {
    return(freq)
  }
  # frequencies for pie charts
  else if (type == 'ppl plot') {
    # if we want data for a people plot, calculate 4 - the value per vector object in the list to shift the values
    freq <- freq_to_ppl_plot(freq)
    return(freq)
  }
  else {
    cat('Unknown type.  Options are "pie chart" or "ppl plot"\n')
  }
}

calculate_combinedFreq <- function(household_data, school_data, community_data, type='pie chart') {
  # calculate the appropriate factors for plotting pie charts
  # and people plots.  This is specific to the combined 
  # form. returns a list with two objects. type can equal
  # 'pie chart' or 'ppl plot'.  this will combine the other
  # results from the individual calculations
  #
  # Ex.
  # > calculate_combinedFreq(household_data, school_data, community_data, 'pie chart')
  # $drain
  # $drain$neighborhood1
  # $drain$neighborhood1$adults
  # [1] 3 4 2 1
  # ...
  num.neighb<-sort(unique(c(as.numeric(household_data$neighbor),as.numeric(school_data$neighbor),as.numeric(community_data$neighbor))))
  
  
  freq <- list('drain' = list(), 'produce' = list(), 'piped_water' = list(), 'ocean_water' = list(),
               'surface_water' = list(), 'flood_water' = list(), 'public_latrine' = list()
  )
  household_freq <- calculate_householdFreq(household_data, type)
  school_freq <- calculate_schoolFreq(school_data, type)
  community_freq <- calculate_communityFreq(community_data, type)
  
  for (p in 1:length(freq)) {
    for (n in num.neighb) {
      sub_a <- list('adults' = c(household_freq[[p]][[n]]$adults,
                                 school_freq[[p]][[n]]$adults,
                                 community_freq[[p]][[n]]$adults), 
                    'children' = c(household_freq[[p]][[n]]$children,
                                        school_freq[[p]][[n]]$children,
                                        community_freq[[p]][[n]]$children)
                    )

    }
    freq[[p]][[paste0('neighborhood',n)]] <- sub_a
    
  }

  # it's the same calculations for a pie chart or a people plot, just shifted. 
  if (type == 'pie chart' | type == 'ppl plot') {
    return(freq)
  }
  else {
    cat('Unknown type.  Options are "pie chart" or "ppl plot"\n')
  }
  
}

convert_to_old_freq <- function(freq) {
  # this is in order to not refactor all of the people plot code for the moment
  # this is not intented to be used long term.
  # this assumes the nested list is path -> neighborhood -> age
  # result will be path * age
  out <- list()
  for (n in 1:length(freq[[1]])) {
    for (p in 1:length(freq)) {
      out <- append(out, list(freq[[p]][[n]]$adults))
      out <- append(out, list(freq[[p]][[n]]$children))
    }
  }

  return(out)
}

# PLOTTING ====================================================================
## PIE CHARTS 
create_pieCharts <- function(freq) {
  # this will make a grid of pie charts based on neighborhood rows
  # and age columns
  
  # first let's regroup the data into a table that can be used
  # for plotting.  it will have 4 columns, neighborhood, age, answer, Freq


  
  for (p in 1:length(freq)) { # for each path
    path <- freq[[p]]
    labels <- unlist(ifelse(p==3, 
                            list(c("everyday","4-6/wk","1-3/mo","never","don't know")),
                            list(c(">10/mo","6-10/mo","1-5/mo","never","don't know"))
    ))
    for (n in 1:length(path)) { # freq is now subset to just the neighborhoods for a specific path
      neighborhood <- path[[n]]
      for (a in 1:length(neighborhood)) {
        if (length(neighborhood[[a]]) > 0) {
          # check if there's actually data first, then plot, otherwise the list elements are left
          # untouched
          tbl <- create_freqTbl(neighborhood[[a]], labels)
          freq[[p]][[n]][[a]] <- ggpie(tbl, 'answer', 'Freq', ifelse(a == 1, 'Adults', 'Children'))
        }

        
      }
      
    }
  }
  
  
  
  return(freq)
}

create_freqTbl <- function(freq_vector, labels) {
  # convert the answers from the frequency calculation funcitons into 
  # a table for plotting
  tbl <- as.data.frame(table('answer'= freq_vector))
  tbl$answer <- labels[tbl$answer]
  tbl$breaks <- cumsum(tbl$Freq) - tbl$Freq / 2
  tbl$labels = paste0(round(tbl$Freq / sum(tbl$Freq) *
                              100, 1), "%")
  return(tbl)
}

make_histogram <- function(samtype, ec_data, conc) {
  if (samtype!=0) return(NULL)
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
}

## Graphing support -----------------------------------------------------
plotElements <- # these are global settings that are applied to all plots generated
  # make changes here to apply across the board (most of the time)
  theme(
    plot.title = element_text(face = "bold", size = 26),
    panel.background = element_blank(),
    # X axis settings
    axis.text.x = element_text(size = 14),
    axis.title.x = element_text(size = 16, face='bold'),
    # Y axis settings
    axis.text.y = element_text(size = 16),
    axis.title.y = element_text(size = 16, face='bold')
  ) + theme_bw() + theme(legend.text = element_text(size=16),
                         legend.title = element_text(size=16))


ggpie <- function (dat, group_by, value_column, title) {
  # found this function online to create pie charts using ggplot
  # pass the melted data set, group column (group_by) and value column (value_column)
  
  plot <-
    ggplot(dat, aes_string(
      x = factor(1), y = value_column, fill = group_by, colour = group_by
    )) +
    geom_bar(stat = 'identity', size = 1, alpha = .6) +
    guides(fill = guide_legend(override.aes = list(colour = NA))) + # removes black borders from legend
    coord_polar(theta = 'y') + theme_bw()  +
    theme(
      axis.ticks = element_blank(),
      axis.text.y = element_blank(),
      axis.text.x = element_text(
        colour = 'black', size = 12, angle = 0, hjust = 1, vjust=0
      ),
      axis.title = element_blank(),
      panel.border = element_blank(),
      legend.position = 'none'
    ) +
    scale_y_continuous(breaks = cumsum(dat[[value_column]]) - dat[[value_column]] / 2,
                       labels = paste0(round(dat[[value_column]] / sum(dat[[value_column]]) *
                                               100, 1), "%","\n",dat$answer ))  +
    ggtitle(title)
  
  
  return(plot)
}