## Analysis helpers for the SaniPath Analysis tool
## these have been derived from the original server.R
## file.
library(rlist)
library(tidyverse)
#library(reshape2)
library(magrittr)
library(doParallel)
library(rjags)


# CONCENTRATIONS ---------------------------------------------  
compute_concentrations <- function(collection_data, lab_data,
                            config = config,
                            pathway_codes = config$pathway_codes,
                            pathway_labels = config$pathway_labels) {
  
  lab_analysis_method <- unique(lab_data$lab_analysis)
  if (lab_analysis_method == 1) {
    reading = config$idexx_reading
    value = config$idexx_value
    MF = F
  }
  else {
    reading = config$membrane_reading
    value = config$membrane_value
    MF = T
  }
  denoms = config$denoms
  
  # Calculate the e coli combined dataframe
  ec_data <- create_ecData(collection_data = collection_data, 
                           lab_data = lab_data,
                           mpn_tbl = config$mpn_tbl,
                           reading = reading,
                           value = value,
                           denoms = denoms,
                           MF = MF) 

  # now build our output of concentration values
  pathway_selected_vector <- suppressWarnings(as.integer(pathway_codes) %>% .[!is.na(.)])
  
  found_pathways <- unique(ec_data$sample_type)
  conc<-list()
  for (i in unique(factor_to_numeric(ec_data$neighbor))) {
    # sample type 1=drain water, 2=produce, 3=piped water, 4=ocean water, 5=surface water, 6=flood water, 7=Public Latrine Surfaces, 8=particulate, 9=bathing
    for (j in 1:length(pathway_selected_vector)) {
      if (pathway_selected_vector[j] %in% found_pathways) {
        s = names(pathway_codes[j])
        x <-list(s = s,
                 neighb = i,
                 sample = pathway_labels[[s]],
                 neighborhood = paste("Neighborhood", i),# The neighborhood information should change based on the configuration before deployment.
                 data = ec_data$ec_conc[which(ec_data$neighbor == i
                                              & ec_data$sample_type == pathway_selected_vector[j])])
        
        x$plot_name <- paste0(x$neighborhood,", ", x$sample, '\n(N=',length(x$data),")")
        x$fn <- sprintf('%s_%s.png', i, x$s)
        conc <- append(conc, list(x))
      }
    }
  }
  return(conc)
}


# master create_ecData
create_ecData <- function(collection_data, lab_data, mpn_tbl,
                          reading = config$idexx_reading, value = config$idexx_value,
                          denoms = config$denoms,
                          MF = F # defaults to IDEXX method
                          ) {
  #logic to decide whether the function recieves IDEXX data or MF data;
  #This is assuming all the samples will be tested in one of the method: either IDEXX or MF.
  #This field will be filled based on configuration of the project.

  if (!MF) {
    # idexx specific value manipulation
    lab_data %<>% ec_prepare_idexx(reading, mpn_tbl)
  }
  else {
    ec_data %<>% ec_prepare_mf(reading)
  }

  # These steps are the same for both methods
  ec_data <- ec_merge(collection_data, lab_data)

  # add denominators
  ec_data %<>% ec_add_denoms(denoms)

  # calculate the swaps
  ec_data %<>% ec_calc_swaps()

  # calculate the conditions
  cond_func <- if (MF) ec_mf_conditions else ec_idexx_conditions

  ec_data %<>% cond_func(value)

  ec_data$neighbor <- as.factor(ec_data$col_neighborhood)

  return(ec_data)


}

# FREQUENCIES ----------------------------------------------------------------
compute_frequencies <- function(..., type='pie', analysis_type=NULL, 
                                config=NULL,  pathway_labels = config$pathway_labels) {
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
  
  if (is.null(config)) stop('Missing config object!')
  
  # this allows us to pass multiple data objects without having to explictly
  #say what they are. since the surveys always follow a pattern for the question
  # headers, we can figure out what data we have using that.
  dat <- list(...)
  # this should be based on the columns within each export
  # the ^ is a special regex command meaning starts with
  data_map <- c('household_data' = '^h_', 'community_data' = '^c_', 'school_data' = '^s_')

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
#   if (!(length(surveys_matched) == 1 | length(surveys_matched) == 3)) {
#     stop(paste0('Something is wrong with the data. Either pass 1 or 3 data objects.\n',
#                 'Matched objects: ', paste(surveys_matched, collapse=', ')))
#   }
  if (!any(surveys_matched %in% names(data_map))) {
    stop(paste('Unable to determine survey type. Do the column headers have h, s, or c in the names?\n',
               'Matched objects:', paste(surveys_matched, collapse=', ')))

  }

  if (is.null(analysis_type)) {
    # update the survey type
    analysis_type <- ifelse(length(surveys_matched) == 3, 'combined', gsub('_data', '', surveys_matched))
  }

  if (analysis_type == 'combined') {
    df_for_analysis <- bind_rows(household_data, community_data, school_data)
  }
  else {
    df_for_analysis <- eval(parse(text=paste0(analysis_type, '_data')))
  }

  freq <- find_pathways(df_for_analysis, analysis_type, pathway_labels = pathway_labels)

  # lastly, make sure it's the right numbers.
  if (type == 'pie') {
    return(freq)
  }
  # frequencies for pie charts
  else if (type == 'ppl') {

    # if we want data for a people plot, calculate 4 - the value per vector object in the list
    freq <- lapply(freq, function(x) {
      x$data %<>% subtract(4, .) 
      x
    })
    return(freq)
  }
  else {
    warning('Unknown type.  Options are "pie" or "ppl"\n')
  }
}

find_pathways <- function(df, analysis_type, pathway_labels=config$pathway_labels) {
  neighborhoods <- unique(df[,grep('neighborhood$', names(df))])
  # this is ugly, but it works
  # pattern match columns, split by underscore, convert to rows,
  # look at the first three columns and find unique combinations,
  # these are our full pathways.
  pathways <- grep('[a-z]{1}_[a-z]{1,2}_[a-z]{1}', names(df), value=T, perl = T) %>%
    strsplit("_") %>%
    lapply(function(j) as.data.frame(t(j), stringsAsFactors=F)) %>%
    bind_rows() %>%
    .[,c(1:3)] %>%
    .[!duplicated(.),] %>%
    .[!apply(. == 'metadata' | . == 'neighborhood', 1, any),] # neighborhood is making it through for some reason
  
  pathways <- pathways[-grep("\\d{1,}$", pathways$V3),]
  # iterate through neighborhoods and find pathways for each
  freq <- lapply(neighborhoods, function(n) {
    apply(pathways, 1, function(pathway_combo) {
      x <- find_pathway(df, n, analysis_type, pathway_combo[2], pathway_combo[3])
      x$sample <- pathway_labels[[pathway_combo[2]]]
      x$plot_name <- sprintf("%s, %s\n%s (N= %s)", x$neighborhood, x$sample, x$age, length(x$data))
      x$s <- unname(pathway_combo[2])
      x$neighb <- n
      x$pop <- unname(pathway_combo[3])
      x$analysis_type <- analysis_type
      x$fn <- sprintf("%s_%s_%s_%s.png", x$neighb, x$s, x$pop, x$analysis_type)
      list(x)
      }) %>% unname() %>% unlist(recursive=F)
  }) %>% unlist(recursive=F)
  
  freq <- freq[sapply(freq, function(x) !is.null(x$data) & !is.null(x$sample))]
  
  return(freq)
}

find_pathway <- function(df, neighb, analysis_type, pathway_type, population_type) {
  return(list(sample = unname(pathway_type),
              age = switch(population_type, 'a' = 'Adults', 'c' = 'Children'),
              neighborhood = paste('Neighborhood',neighb),
              data = switch(analysis_type,
                            # if analysis type is "combined" we'll loop through all survey types and stick the results together
                            'combined' = sapply(c('h', 's', 'c'), function(x) {
                              find_freq(df[df[,paste0(x, '_neighborhood')] == neighb,], x, pathway_type, population_type)}
                              ) %>% unlist(),
                            # otherwise, we'll just look at specific columns
                            'household' = find_freq(df[df$h_neighborhood == neighb,], 'h', pathway_type, population_type),
                            'school' = find_freq(df[df$s_neighborhood == neighb,], 's', pathway_type, population_type),
                            'community' = find_freq(df[df$c_neighborhood == neighb,], 'c', pathway_type, population_type)
              )
          )
  )
}

find_freq <- function(df, survey_type, pathway_type, population_type) {
  # For a given survey dataframe, find the answers for that pathway and population type
  # assumes the pattern survey_pathway_population naming convention and expects
  # _3, _2, etc for school and community surveys, household only pulls one column.
  value_map = list('_3' = 1,
                   '_2' = 2,
                   '_1' = 3,
                   '_0' = 4,
                   '_na' = 5)
  # find the column names that match the pattern
  cols <- grep(paste0(c(survey_type, pathway_type, population_type), collapse='_'), names(df), value = T)
  if (survey_type != 'h') {
    results <- sapply(names(value_map), function(x) {
      n <- df[,grep(paste0(x,"$"), cols, value=T)] %>% as.numeric() %>% sum()
      if (length(n) > 0) {
        rep(value_map[x], n)
      }
    }) %>% unlist() %>% unname()
  }
  else {
    results <- df[,grep(paste0('*_', pathway_type, '_', population_type, "$"), names(df), value=T)]
  }

  return(results)

}


create_freqTbl <- function(freq_vector, sample_type) {
  # convert the answers from the frequency calculation funcitons into
  # a table for plotting
  labels <- unlist(ifelse(sample_type=='Municipal and Piped Water', list(c("everyday","4-6/wk","1-3/wk","never","don't know")),
                          ifelse(sample_type=="Produce" | sample_type=="Public Latrine Surfaces" | sample_type=="Flood Water" | sample_type=="Street Food" | sample_type=="Bathing Water",list(c(">10/wk","6-10/wk","1-5/wk","never","don't know")),
                          list(c(">10/mo","6-10/mo","1-5/mo","never","don't know"))))
  )
  #colors <- c('#00FF00', '#99FF00', '#FF6600', '#FF0000', '#333333')
  colors <- c("#F8766D", "#A3A500", "#00BF7D", "#00B0F6", "#E76BF3")

  tbl <- as.data.frame(table('answer'= freq_vector))
  tbl$color <- colors[factor_to_numeric(tbl$answer)]
  tbl$answer <- labels[factor_to_numeric(tbl$answer)]
  tbl$Freq <- factor_to_numeric(tbl$Freq)


  tbl$breaks <- cumsum(tbl$Freq) - tbl$Freq / 2
  tbl$labels = paste(tbl$answer, "\n", paste0(round(tbl$Freq / sum(tbl$Freq) * 100, 1),"%"))
  return(tbl)
}

# EXPOSURE -----------------------------------------------------
compute_exposure <- function(freq, 
                             conc, 
                             config = config,
                             parallel=T, nc=detectCores()) {
  # function to caclulate the percent of population
  # exposed for all pathways given.  performs Bayesian
  # analysis on behavior and environmental data first
  # then calculates the final statistics for plotting
  # nburn=1000, niter=10000, thin=1, cutpoint=c(0, 5, 10)
  # run the Bayesian analyses
  jags_par_env = config$jags_par_env
  jags_par_freq = config$jags_par_freq
  cut_point = config$cut_point
  init_freq = config$init_freq
  nsim = config$nsim 
  intake = config$intake
  pathway_codes = config$pathway_codes
  
  freq <- bayesian_behavior_estimates(freq, 
                                      nburn = get("nburn",jags_par_freq), 
                                      niter = get("niter",jags_par_freq), 
                                      thin = get("thin",jags_par_freq), 
                                      cut_point = cut_point, 
                                      init_freq = init_freq, 
                                      parallel=parallel, nc=nc)
  conc <- bayesian_environmental_estimates(conc, 
                                           nburn = get("nburn",jags_par_env), 
                                           niter = get("niter",jags_par_env), 
                                           thin = get("thin",jags_par_env), 
                                           parallel=parallel, nc=nc)


  # based on the original ps_plot section of the shiny server,
  # it seems they are based on the behavoir data
  # need to find the number of neighborhoods
  # and samples

  neighborhoods <- c(unique(names(list.names(conc, neighborhood))), unique(names(list.names(freq, neighborhood)))) # unique neighborhood values
  neighborhoods <- unique(neighborhoods[duplicated(neighborhoods)]) # if it's duplicated, then it will show up in both freq and conc

  # Bathing water can be assumed as 30, all others should be present to calculate
  samples <- c(unique(names(list.names(conc, sample))), unique(names(list.names(freq, sample)))) # unique sample values
  samples <- unique(samples[duplicated(samples)])

  age <- unique(names(list.names(freq, age)))

  ps.freq <- list()
  for (smp in samples) {
    for (nb in neighborhoods) {
      sub.conc <- conc[[list.which(conc, neighborhood == nb && sample == smp)]]
      # calculate exposure for adults and children using the behavior data
      for (a in age) {
        # filter frequency to just the age we want
        sub.freq <- freq[[list.which(freq, sample == smp && neighborhood == nb && age == a)]]
        # calculate the exposure. Requires concentration, freq is optional
        exposed <- calculate_exposure(sub.freq, sub.conc, smp, nsim = nsim, intake = intake, pathway_codes = pathway_codes)
        # update the object at this position
        ps.freq <- append(ps.freq, list(exposed))
      }
      # filter the concentration data to just this neighborhood and sample
    }
  }

  # give back the updated behavior data object
  return(ps.freq)
}

bayesian_environmental_estimates <- function(conc, nburn=1000, niter=10000, thin=1, parallel = T, nc=detectCores()) {
  # Run bayesian model on the environmental data collected.  this will be run for each
  # neighborhood, age, and sample combination.  Warning: Could take quite a while.
  # Future development: Way of backgrounding this?
  calcul <- paste(nburn,niter,thin,sep="|")

  # environmental samples
  tomonitor <- c("mu","sigma")
  if (parallel) {
    func <- partial(mclapply, mc.cores=nc)
  }
  else {
    func <- lapply
  }
  
  conc <- func(1:length(conc), function(k) {
    log_ec<-log10(as.numeric(conc[[k]]$data))
    env_data<-list(lnconc=log_ec,N=length(log_ec))

    modelpos <- jags.model(file="./model/env_model.jags",data=env_data,n.chains=3);
    update(modelpos,n.burn=nburn);
    env_mcmcpos <- coda.samples(modelpos,tomonitor,n.iter=niter,thin=thin);
    #Bayesian estimators of mu and sigma
    mu <-summary(env_mcmcpos)$statistics[1,1]
    sigma <-summary(env_mcmcpos)$statistics[2,1]

    append(conc[[k]], list('mu' = mu, 'sigma' = sigma))

  })
  
  return(conc)
}

bayesian_behavior_estimates <- function(freq, nburn=1000, niter=10000, thin=1, 
                                        cut_point = list('times' = c(0, 5, 10), 'days' = c(0, 3, 6)), 
                                        init_freq = list('times' = c(NA, 2, 7, 12), "days" = c(NA, 2, 5, 7), 'r' = 1, 'p' = 0.2),
                                        parallel=T, 
                                        nc= detectCores()) {
  # Run bayesian model on the behavior data collected.  this will be run for each
  # neighborhood, age, and sample combination.  Warning: Could take quite a while.
  # Future development: Way of backgrounding this?

  bemonitor <- c("p","r")
  calcul <- paste(nburn,niter,thin,sep="|")
  
  if (parallel) func <- partial(mclapply, mc.cores=nc)
  else func <- lapply
  
  freq <- func(1:length(freq), function(k) {
    print(k)
    # set up the data for analysis to be passed to jags
    freq_be0=freq[[k]]$data
    freq_be<-freq_be0[which(freq_be0>=0)]
    #initial values
    init_be<-as.numeric(rep(0,length(freq_be)))
    init_be[which(freq_be==1)]<-1
    init_be[which(freq_be==2)]<-2
    init_be[which(freq_be==3)]<-3

    if (freq[[k]]$sample=='Municipal and Piped Water'){
      cutpoint<- cut_point$days
      init_freq_be<-as.numeric(rep( init_freq$days[1],length(freq_be)))
      init_freq_be[which(freq_be==1)]<- init_freq$days[2]
      init_freq_be[which(freq_be==2)]<- init_freq$days[3]
      init_freq_be[which(freq_be==3)]<- init_freq$days[4]
    } else {
      cutpoint<- cut_point$times
      init_freq_be<-as.numeric(rep( init_freq$times[1],length(freq_be)))
      init_freq_be[which(freq_be==1)]<- init_freq$times[2]
      init_freq_be[which(freq_be==2)]<- init_freq$times[3]
      init_freq_be[which(freq_be==3)]<- init_freq$times[4]
    }

    be_data<-list(select=freq_be,N=length(freq_be),cut=cutpoint)
    init<-list(freq=init_freq_be,r= init_freq$r,p= init_freq$p)

    # Jags model runs
    modelpos <- jags.model(file="./model/be_model.jags",data=be_data,n.chains=3,inits=init)
    update(modelpos,n.burn=nburn)
    cat('Coda Samples\n\n')
    be_mcmcpos <- coda.samples(modelpos, bemonitor, n.iter=niter, thin=thin)

    # Extract results
    # Bayesian estimators of p and r
    p <-summary(be_mcmcpos)$statistics[1,1]
    r <-summary(be_mcmcpos)$statistics[2,1]

    append(freq[[k]], list('p' = p, 'r' = r))

  })
  return(freq)
}

calculate_exposure <- function(behavior_data, concentration_data, smp, nsim = 1000, intake = config$intake, pathway_codes = config$pathway_codes) {
  # used for people plot generation.  this is for a single pathway
  # and assumes the data are already subset to the appropriate level
  e <- rep(NA, nsim)
  f <- rep(NA, nsim)
  risk <- rep(NA, nsim)

  # values applied based on sample and age
  #intake<-array(c(0.0006,1,10.43,0.0154,0.037,0.0006,0.034,NA,0.0499,1,
  #                0.01,0.5,4.14,0.2042,0.2042,0.01,0.034,NA,0.09975,0.5),c(10,2)) #need to input this information!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  rownames(intake) <- c('Drain Water', 'Produce', 'Municipal and Piped Water', 'Ocean Water', 'Surface Water',
                        'Flood Water', 'Public Latrine', 'Particulate', 'Bathing Water','Street Food')
  colnames(intake) <- c("Adults", "Children")

  # simulate some numbers
  for (m in 1:nsim){
    # is it necessary for this to be in a loop?
    e[m] <- rnorm(1, concentration_data$mu, concentration_data$sigma)
    if (smp %in% unlist(pathway_codes[c('p','f','l','bw','sf')])) {
      f[m] <- round(rnbinom(1, size= behavior_data$r, prob= behavior_data$p)/7*30)
    } else if (smp==3) {
      f[m] <- min(round(rnbinom(1, size= behavior_data$r, prob= behavior_data$p)/7*30),30)
    } else {
      f[m] <- rnbinom(1, size= behavior_data$r, prob= behavior_data$p)
    }
    risk[m] <- f[m]* (10^e[m]) * intake[behavior_data$sample, behavior_data$age]
  }

  non0 <- function(mc){
    tmp <- mc; tmp[!(tmp>0)] <- NA
    return(tmp)
  }

  n <-(1-length(which(f==0))/nsim)*100;
  dose <-log10(mean(non0(risk),na.rm=TRUE))
  # add the percent exposure and dose information to the behavior data
  behavior_data <- append(behavior_data, list('n' = n, 'dose' = dose))

  # give the updated object back
  return(behavior_data)

}

