

ordered_shinyCharts <- function(dat, columns=2, level1_type=NULL, level2_type=NULL,  
                                sample_filter=NULL, neighborhood_filter=NULL, age_filter=NULL, non_shiny=F) {
  
  
  # double check that the level types stated are correct and do not repeat
  correct_types <- c('sample', 'neighborhood', 'age')
  stated_types <- list(level1_type, level2_type)
  stated_types <- append(stated_types, correct_types[!(correct_types %in% stated_types)])
  
  filters <- list('sample' = sample_filter, 'neighborhood' = neighborhood_filter,'age' = age_filter)
  assign('level1_filter', filters[[stated_types[[1]]]])
  assign('level2_filter', filters[[stated_types[[2]]]])
  assign('level3_filter', filters[[stated_types[[3]]]])
  
  
  
  # check if there are any dupes
  dupe_check <- unlist(stated_types)
  dupe_types <- duplicated(dupe_check)
  if (any(dupe_types)) {
    stop(paste('Duplicate level type:', stated_types[dupe_check][dupe_types]))
  }
  
  
  # Check if all of the types stated exist
  # we return TRUE if the value is NULL because we check for NULL vals after
  correct_test <- sapply(stated_types, function(x) {if(!is.null(x)) x %in% correct_types else T}) 
  if(all(correct_test) != T) {
    stop(paste('Unrecognized level type:', stated_types[correct_test != T],
               '\nLevel type must be one of the following:', paste(correct_types, collapse=', ')))
  }
  
  # check for NULL values
  null_vals <- sapply(stated_types, is.null)
  if (any(null_vals)) {
    for (n in 1:length(stated_types[null_vals])) {
      # loop through the null parameters and fill in the first correct type available
      stated_types[null_vals][n] <- correct_types[!(correct_types %in% stated_types)][1]
    } 
  }
  
  
  
  if (is.null(level1_filter)) level1_filter <- unique(names(list.names(dat, eval(parse(text=stated_types[1])))))
  if (is.null(level2_filter)) level2_filter <- unique(names(list.names(dat, eval(parse(text=stated_types[2])))))
  if (is.null(level3_filter)) level3_filter <- unique(names(list.names(dat, eval(parse(text=stated_types[3])))))
  
  ordered_list <- list()
  if (non_shiny == F) {
    # Shiny Generation
    for (l1 in level1_filter) {
      # add the first level header
      ordered_list <- append(ordered_list, list(fluidRow(h2(toupper(l1)))))
      ordered_list <- append(ordered_list, list(hr()))
      for (l2 in level2_filter) {
        # add the second level header 
        ordered_list <- append(ordered_list, list(fluidRow(h4(toupper(l2)))))
        # subset the data down to l1 + l2.  this will be the third
        # level set of data.  Ex. if set to filter by sample then 
        # neighborhood, this subset will contain adults and children
        # per combination of sample and neighborhood.
        l2_sub <- dat[list.which(dat, eval(parse(text=stated_types[1])) == l1 && # level 1 filter
                                   eval(parse(text=stated_types[2])) == l2 && # level 2 filter
                                   eval(parse(text=stated_types[3])) %in% level3_filter)] # match anything in the level 3 filter
        
        # make plot names for the options that matched by combining sample + neighborhood + age
        plot_names <- sapply(1:length(l2_sub), function(x) paste0(l2_sub[x]$path$sample, '-', l2_sub[x]$path$neighborhood, "-", l2_sub[x]$path$age))
        # there are spaces in the names, since we're using the for labels too, 
        # let's remove those for the actual plot names
        plot_names <- gsub(" ", "", plot_names)
        # count the plots we need to make
        num_plots <- length(l2_sub)
        # count the number of rows specified.
        num_rows <- ceiling(num_plots/columns)
        col_size <- 12/columns # this is how wide each column will be. 
        

        for (i in seq(1, num_plots, by=columns)) {
          # each value of i indicates a new row in the output
          # make the columns populated with the plots we want
          
          # grab the plots
          row <- plot_names[(i + 0:(columns - 1))] 
          row <- row[!is.na(row)]
          # make each row element a column
          row <- sapply(1:length(row), function(a) paste0('column(',col_size,",align='center',h5('",l2_sub[[a]][stated_types[[3]]],"', align='center'), plotOutput('",row[a],"', height=350, width=300))"))
          # make that into one long string appropriately separated by commas
          row <- paste(row, collapse = ", ")
          # add a row marker 
          ordered_list <- append(ordered_list, list(eval(parse(text=paste0('fluidRow(',row,')')))))
        }
      }
    }
  }
  # #   
  
  else {
    # Non-Shiny Example of how it works
    for (l1 in level1_filter) {
      ordered_list <- append(ordered_list, paste("Level 1 Heading:",l1))
      for (l2 in level2_filter) {
        ordered_list <- append(ordered_list, paste('Level 2 heading:',l2))
        l2_sub <- dat[list.which(dat, eval(parse(text=stated_types[1])) == l1 && eval(parse(text=stated_types[2])) == l2)]
        plot_names <- sapply(1:length(l2_sub), function(x) paste0(l2_sub[x]$path$sample, '-', l2_sub[x]$path$neighborhood, "-", l2_sub[x]$path$age))
        plot_names <- gsub(" ", "", plot_names)
        num_plots <- length(l2_sub)
        num_rows <- ceiling(num_plots/columns)
        col_size <- 12/columns # this is how wide each column will be. 
        
        
        for (i in seq(1, num_plots, by=columns)) {
          row <- plot_names[(i + 0:(columns - 1))]
          row <- row[!is.na(row)]
          row <- sapply(1:length(row), function(a) paste0("column(",col_size,",h5('",l2_sub[[a]][stated_types[[3]]], "'), plotOutput('",row[a],"'))"))
          row <- paste(row, collapse = ", ")
          ordered_list <- append(ordered_list, list(row))
        }
      }
    }
  }
  return(ordered_list)
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

# PLOTTING ====================================================================
## PIE CHARTS 
create_shinyPieCharts <- function(dat, label_level, sample_filter=NULL, neighborhood_filter=NULL, age_filter=NULL, output) {

  if (is.null(sample_filter)) sample_filter <- unique(names(list.names(dat, sample)))
  if (is.null(neighborhood_filter)) neighborhood_filter <- unique(names(list.names(dat, neighborhood)))
  if (is.null(age_filter)) age_filter <- unique(names(list.names(dat, age)))
  
  dat <- dat[list.which(dat, sample %in% sample_filter && 
                          neighborhood %in% neighborhood_filter &&
                          age %in% age_filter)]
  for (i in dat) {
    local({
      my_i <- i
      p_name <- gsub(' ', '', paste0(my_i$path$sample,"-",my_i$neighborhood, '-', my_i$age))
      output[[p_name]] <- create_pieChart(my_i$data, my_i$sample, my_i[label_level])
    })
  }


  
  return(output)
}

create_pieChart <- function(freq, sample_type, title_label) {
  # this will make a pie charts based on the input freq values
  
  # first let's regroup the data into a table that can be used
  # for plotting.  it will have 4 columns, neighborhood, age, answer, Freq
  labels <- unlist(ifelse(sample_type=='Municipal and Piped Water', 
                          list(c("everyday","4-6/wk","1-3/mo","never","don't know")),
                          list(c(">10/mo","6-10/mo","1-5/mo","never","don't know"))
  )
  )
  
  tbl <- create_freqTbl(freq, labels)
  p <- ggpie(tbl, 'answer', 'Freq', title_label)
  
  
  return(p)
}
