library(ggplot2)
library(ggrepel)
library(RgoogleMaps)

ordered_shinyCharts <- function(dat, columns=2, level1_type=NULL, level2_type=NULL,  
                                sample_filter=NULL, neighborhood_filter=NULL, 
                                age_filter=NULL, width=450, height=400, chart_prefix='pie-', non_shiny=F, 
                                shinySession=NULL) {
  
  # this will order the pie charts and people plots according to the levels specified
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
  
  
  # if the filters are null, do everything. 
  if (is.null(level1_filter)) level1_filter <- unique(names(list.names(dat, eval(parse(text=stated_types[1])))))
  if (is.null(level2_filter)) level2_filter <- unique(names(list.names(dat, eval(parse(text=stated_types[2])))))
  if (is.null(level3_filter)) level3_filter <- unique(names(list.names(dat, eval(parse(text=stated_types[3])))))
  
  ordered_list <- list()
  if (non_shiny == F) {
    # Shiny Generation
    withProgress({
      count <- 1
      nplot <- length(dat)
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
          plot_names <- sapply(1:length(l2_sub), function(x) paste0(chart_prefix, l2_sub[x]$path$sample, '-', l2_sub[x]$path$neighborhood, "-", l2_sub[x]$path$age))
          # there are spaces in the names, since we're using the for labels too, 
          # let's remove those for the actual plot names
          plot_names <- gsub(" ", "", plot_names)
          # count the plots we need to make
          num_plots <- length(l2_sub)
          # count the number of rows specified.
          num_rows <- ceiling(num_plots/columns)
          col_size <- 12/columns # this is how wide each column will be. 
          
          if (num_rows > 0) {
            for (i in seq(1, num_plots, by=columns)) {
              # each value of i indicates a new row in the output
              # make the columns populated with the plots we want
              
              # grab the plots
              row <- plot_names[(i + 0:(columns - 1))] 
              row <- row[!is.na(row)]
              row_names <- unlist(sapply(l2_sub[i + 0:(columns - 1)], function(x) x[stated_types[[3]]]))
              row_names <- row_names[!is.na(row_names) & !is.null(row_names)]
              # make each row element a column
              row <- sapply(1:length(row), function(a) paste0('column(',col_size,",align='center',h5('",row_names[a],"', align='center'), plotOutput('",row[a],"', height=", height,", width=", width,"))"))
              # make that into one long string appropriately separated by commas
              row <- paste(row, collapse = ", ")
              # add a row marker 
              ordered_list <- append(ordered_list, list(eval(parse(text=paste0('fluidRow(',row,')')))))
            }
          }
          incProgress(count/nplot)
          count <- count + 1
        }
      }
    }, message='Arranging Charts')
  }
  # #   
  
  else {
    # Non-Shiny Example of how it works
    for (l1 in level1_filter) {
      ordered_list <- append(ordered_list, paste("Level 1 Heading:",l1))
      for (l2 in level2_filter) {
        ordered_list <- append(ordered_list, paste('Level 2 heading:',l2))
        l2_sub <- dat[list.which(dat, eval(parse(text=stated_types[1])) == l1 && eval(parse(text=stated_types[2])) == l2)]
        plot_names <- sapply(1:length(l2_sub), function(x) paste0(chart_prefix, l2_sub[x]$path$sample, '-', l2_sub[x]$path$neighborhood, "-", l2_sub[x]$path$age))
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

ordered_shinyHists <- function(dat, columns=2, level1_type=NULL, 
                                sample_filter=NULL, neighborhood_filter=NULL) {
  
  # this is the same as ordered_shinyCharts, but for histograms.  it will organize by
  # the first level stated.  this only has one level since the histograms do not
  # go down to age of respondent. 
  # double check that the level types stated are correct and do not repeat
  correct_types <- c('sample', 'neighborhood')
  stated_types <- list(level1_type)
  stated_types <- append(stated_types, correct_types[!(correct_types %in% stated_types)])
  
  filters <- list('sample' = sample_filter, 'neighborhood' = neighborhood_filter)
  assign('level1_filter', filters[[stated_types[[1]]]])
  assign('level2_filter', filters[[stated_types[[2]]]])

  
  
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
  
  
  # if the filters are empty, we have to do something, so 
  if (is.null(level1_filter)) level1_filter <- unique(names(list.names(dat, eval(parse(text=stated_types[1])))))
  if (is.null(level2_filter)) level2_filter <- unique(names(list.names(dat, eval(parse(text=stated_types[2])))))

  ordered_list <- list()
    # Shiny Generation
    for (l1 in level1_filter) {
      # add the first level header
      ordered_list <- append(ordered_list, list(fluidRow(h2(toupper(l1)))))
      ordered_list <- append(ordered_list, list(hr()))
        # subset the data down to l1 + l2.  this will be the third
        # level set of data.  Ex. if set to filter by sample then 
        # neighborhood, this subset will contain adults and children
        # per combination of sample and neighborhood.
        l2_sub <- dat[list.which(dat, eval(parse(text=stated_types[1])) == l1 && # level 1 filter
                                   eval(parse(text=stated_types[2])) %in% level2_filter)] # match anything in the level 2 filter
        
        # make plot names for the options that matched by combining sample + neighborhood + age
        plot_names <- sapply(1:length(l2_sub), function(x) paste0('hist-',l2_sub[x]$conc$sample, '-', l2_sub[x]$conc$neighborhood))
        # there are spaces in the names, since we're using the for labels too, 
        # let's remove those for the actual plot names
        plot_names <- gsub(" ", "", plot_names)
        # count the plots we need to make
        num_plots <- length(l2_sub)
        # count the number of rows specified.
        num_rows <- ceiling(num_plots/columns)
        col_size <- 12/columns # this is how wide each column will be. 
        if(num_plots > 0) {
          
          for (i in seq(1, num_plots, by=columns)) {
            # each value of i indicates a new row in the output
            # make the columns populated with the plots we want
            
            # grab the plots
            row <- plot_names[(i + 0:(columns - 1))] 
            row <- row[!is.na(row)]

            # make each row element a column
            row <- sapply(1:length(row), function(a) paste0('column(',col_size,",align='center', plotOutput('",row[a],"', height=350, width=300))"))
            # make that into one long string appropriately separated by commas
            row <- paste(row, collapse = ", ")
            # add a row marker 
            ordered_list <- append(ordered_list, list(eval(parse(text=paste0('fluidRow(',row,')')))))
          }
          
        }
        
    }
  
 
  return(ordered_list)
}
make_histogram <- function(conc, title) {
  # make a histogram with a specific path data
    hist(log10(as.numeric(conc)),breaks=seq(0,10,by=1),col="skyblue",ylim=c(0,1),freq=FALSE,yaxt="n",ylab="percent",
         main=title,
         cex.main=1.3,xlab=expression(paste("log10 ", italic("E. coli"), "concentration (CFU/100mL)")))
   # returns NULL
}

## Graphing support -----------------------------------------------------
ggpie <- function (dat, group_by, value_column, title, nudgex=.5) {
  # found this function online to create pie charts using ggplot
  # pass the melted data set, group column (group_by) and value column (value_column)
  # to give credit where credit is due:
  # http://mathematicalcoffee.blogspot.com/2014/06/ggpie-pie-graphs-in-ggplot2.html
  # label repelling courtesy of ggrepel
  # https://cran.r-project.org/web/packages/ggrepel/vignettes/ggrepel.html
  
  plot <-
    ggplot(dat, aes_string(
      x = factor(1), y = value_column)) +
    geom_bar(stat = 'identity', size = 1, alpha = .5, aes(fill= color, colour=color)) +
    guides(fill = guide_legend(override.aes = list(colour = NA))) + # removes black borders from legend
    coord_polar(theta = 'y') + theme_bw()  +
    theme(
      plot.title=element_text(face='bold'),
      axis.ticks = element_blank(),
      axis.text.y = element_blank(),
      axis.text.x = element_blank(),
      axis.title = element_blank(),
      panel.border = element_blank(),
      legend.position = 'none'
    ) +
    scale_y_continuous(breaks = cumsum(dat[[value_column]]) - dat[[value_column]] / 2)  +
    ggtitle(title) + geom_text_repel(aes(y=breaks, label= labels), 
                                     color='black', fontface='bold',
                                      box.padding = unit(.1, 'lines'),
                                      point.padding = unit(.1, 'lines'),nudge_y = 0, nudge_x= nudgex # nudge sets how far out the labels show
                                      ,alpha=.7) 
#    
#     scale_fill_manual(values=dat$color) + scale_color_manual(values=dat$color)
#     geom_text_repel(aes(y=breaks, label= labels), color='black', 
#                      box.padding = unit(.18, 'lines'), fontface='bold',
#                      point.padding = unit(.12, 'lines'),nudge_y = 0, nudge_x=nudgex # nudge sets how far out the labels show
#                      ,alpha=.60)
  
  
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
  
  tbl <- create_freqTbl(freq, sample_type)
  p <- suppressWarnings(ggpie(tbl, 'answer', 'Freq', title_label, nudgex=.22))
  
  
  return(p)
}

create_surveyMap <- function(household_data, school_data, community_data, collection_data) {

  lat = as.numeric(c(household_data$X_hh_gps_latitude, school_data$X_ch_gps_latitude, 
          community_data$X_com_gps_latitude, collection_data$X_ev_lat_lon_latitude))
  lon = as.numeric(c(household_data$X_hh_gps_longitude, school_data$X_ch_gps_longitude, 
          community_data$X_com_gps_longitude, collection_data$X_ev_lat_lon_longitude))
  neighbs = c(household_data$neighbor, school_data$neighbor, 
              community_data$neighbor, collection_data$neighbor)
  colors = 'blue'
  
  points <- cbind.data.frame('lat'=lat, 'long'=lon, 'neigh'=neighbs, 'col'=colors)
  points$label <- c(rep('HH',nrow(household_data)), rep('SCH', nrow(school_data)),
                    rep('COM', nrow(community_data)), rep("COL", nrow(collection_data)))
  
  points <- points[!(is.na(points$lat) | is.na(points$long)),]
 
  
  points$marker <- apply(points, 1, function(a) paste0("&markers=color:",a['col'],"|label:",a['label'],"|",a['lat'],",",a['long']))
  
  points <- points[!duplicated(points$marker),]
  
  center = c(mean(points$lat), mean(points$long))
  zoom <- min(MaxZoom(range(points$lat), range(points$long))) + 3
  
  
  Map <- GetMap(center=center, zoom=zoom,destfile = "temp/Map.png");
  # png(filename='temp/Map.png', width=640, height= 480)
  PlotOnStaticMap(Map, lat = points$lat, 
                         lon = points$long, 
                         destfile = "temp/Map.png", cex=3,pch=20,                       
                         col=points$col, add=F)
  # dev.off()
  
}











