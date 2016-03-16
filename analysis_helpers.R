## Analysis helpers for the SaniPath Analysis tool
## these have been derived from the original server.R 
## file.

calculate_householdFreq <- function(household_data, type='pie chart') {
  # calculate the appropriate factors for plotting pie charts
  # and people plots.  This is specific to the household 
  # form. returns a list with two objects. type can equal
  # 'pie chart' or 'ppl plot'
  #
  # Ex.
  # > calculate_householdFreq(household_data, 'pie chart')
  # $[[1]]
  # [1] 3 4 2 1
  # ...

  
  if (type == 'pie chart') {
    freq<-list()
    
    for (i in 1:length(unique(household_data$neighbor))){
      # sample type 1|2=drain, 3|4=produce, 5|6=piped water, 7|8=ocean water, 9|10=surface water, 11|12=flood water, 13|14=Public Latrine Surfaces  
      freq[[14*(sort(unique(household_data$neighbor))[i]-1)+1]]=as.numeric(household_data$hh_q6[which(household_data$hh_q6!="n/a" & household_data$neighbor==i)]);
      freq[[14*(sort(unique(household_data$neighbor))[i]-1)+2]]=as.numeric(household_data$hh_q7[which(household_data$hh_q7!="n/a" & household_data$neighbor==i)]);
      freq[[14*(sort(unique(household_data$neighbor))[i]-1)+3]]=as.numeric(household_data$hh_q13[which(household_data$hh_q13!="n/a" & household_data$neighbor==i)]);
      freq[[14*(sort(unique(household_data$neighbor))[i]-1)+4]]=as.numeric(household_data$hh_q14[which(household_data$hh_q14!="n/a" & household_data$neighbor==i)]);
      freq[[14*(sort(unique(household_data$neighbor))[i]-1)+5]]=as.numeric(household_data$hh_q10[which(household_data$hh_q10!="n/a" & household_data$neighbor==i)]);
      freq[[14*(sort(unique(household_data$neighbor))[i]-1)+6]]=as.numeric(household_data$hh_q11[which(household_data$hh_q11!="n/a" & household_data$neighbor==i)]);
      freq[[14*(sort(unique(household_data$neighbor))[i]-1)+7]]=as.numeric(household_data$hh_q2[which(household_data$hh_q2!="n/a" & household_data$neighbor==i)]);
      freq[[14*(sort(unique(household_data$neighbor))[i]-1)+8]]=as.numeric(household_data$hh_q3[which(household_data$hh_q3!="n/a" & household_data$neighbor==i)]);
      freq[[14*(sort(unique(household_data$neighbor))[i]-1)+9]]=as.numeric(household_data$hh_q4[which(household_data$hh_q4!="n/a" & household_data$neighbor==i)]);
      freq[[14*(sort(unique(household_data$neighbor))[i]-1)+10]]=as.numeric(household_data$hh_q5[which(household_data$hh_q5!="n/a" & household_data$neighbor==i)]);
      freq[[14*(sort(unique(household_data$neighbor))[i]-1)+11]]=as.numeric(household_data$hh_q8[which(household_data$hh_q8!="n/a" & household_data$neighbor==i)]);
      freq[[14*(sort(unique(household_data$neighbor))[i]-1)+12]]=as.numeric(household_data$hh_q9[which(household_data$hh_q9!="n/a" & household_data$neighbor==i)]);
      freq[[14*(sort(unique(household_data$neighbor))[i]-1)+13]]=as.numeric(household_data$hh_q15[which(household_data$hh_q15!="n/a" & household_data$neighbor==i)]);
      freq[[14*(sort(unique(household_data$neighbor))[i]-1)+14]]=as.numeric(household_data$hh_q16[which(household_data$hh_q16!="n/a" & household_data$neighbor==i)]);
    }
  }
  # frequencies for pie charts
  else if (type == 'ppl plot') {
    freq<-list()
    
    for (i in 1:length(unique(household_data$neighbor))){
      # sample type 1|2=drain, 3|4=produce, 5|6=piped water, 7|8=ocean water, 9|10=surface water, 11|12=flood water, 13|14=Public Latrine Surfaces  
      freq[[14*(sort(unique(household_data$neighbor))[i]-1)+1]]=4-as.numeric(household_data$hh_q6[which(household_data$hh_q6!="n/a" & household_data$neighbor==i)]);
      freq[[14*(sort(unique(household_data$neighbor))[i]-1)+2]]=4-as.numeric(household_data$hh_q7[which(household_data$hh_q7!="n/a" & household_data$neighbor==i)]);
      freq[[14*(sort(unique(household_data$neighbor))[i]-1)+3]]=4-as.numeric(household_data$hh_q13[which(household_data$hh_q13!="n/a" & household_data$neighbor==i)]);
      freq[[14*(sort(unique(household_data$neighbor))[i]-1)+4]]=4-as.numeric(household_data$hh_q14[which(household_data$hh_q14!="n/a" & household_data$neighbor==i)]);
      freq[[14*(sort(unique(household_data$neighbor))[i]-1)+5]]=4-as.numeric(household_data$hh_q10[which(household_data$hh_q10!="n/a" & household_data$neighbor==i)]);
      freq[[14*(sort(unique(household_data$neighbor))[i]-1)+6]]=4-as.numeric(household_data$hh_q11[which(household_data$hh_q11!="n/a" & household_data$neighbor==i)]);
      freq[[14*(sort(unique(household_data$neighbor))[i]-1)+7]]=4-as.numeric(household_data$hh_q2[which(household_data$hh_q2!="n/a" & household_data$neighbor==i)]);
      freq[[14*(sort(unique(household_data$neighbor))[i]-1)+8]]=4-as.numeric(household_data$hh_q3[which(household_data$hh_q3!="n/a" & household_data$neighbor==i)]);
      freq[[14*(sort(unique(household_data$neighbor))[i]-1)+9]]=4-as.numeric(household_data$hh_q4[which(household_data$hh_q4!="n/a" & household_data$neighbor==i)]);
      freq[[14*(sort(unique(household_data$neighbor))[i]-1)+10]]=4-as.numeric(household_data$hh_q5[which(household_data$hh_q5!="n/a" & household_data$neighbor==i)]);
      freq[[14*(sort(unique(household_data$neighbor))[i]-1)+11]]=4-as.numeric(household_data$hh_q8[which(household_data$hh_q8!="n/a" & household_data$neighbor==i)]);
      freq[[14*(sort(unique(household_data$neighbor))[i]-1)+12]]=4-as.numeric(household_data$hh_q9[which(household_data$hh_q9!="n/a" & household_data$neighbor==i)]);
      freq[[14*(sort(unique(household_data$neighbor))[i]-1)+13]]=4-as.numeric(household_data$hh_q15[which(household_data$hh_q15!="n/a" & household_data$neighbor==i)]);
      freq[[14*(sort(unique(household_data$neighbor))[i]-1)+14]]=4-as.numeric(household_data$hh_q16[which(household_data$hh_q16!="n/a" & household_data$neighbor==i)]);
    }
  }
  else {
    freq <- NULL
  }

  return(freq)
  
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
  freq<-list()
  if (type == 'pie chart') {
    for (i in 1:length(unique(school_data$neighbor))){
        # sample type 1|2=drain, 3|4=produce, 5|6=piped water, 7|8=ocean water, 9|10=surface water, 11|12=flood water, 13|14=Public Latrine Surfaces  
        freq[[14*(sort(unique(school_data$neighbor))[i]-1)+1]]=c(rep(1,sum(school_data$sch_q9a[which(school_data$neighbor==i)])),rep(2,sum(school_data$sch_q9c[which(school_data$neighbor==i)])),
                                                             rep(3,sum(school_data$sch_q9e[which(school_data$neighbor==i)])),rep(4,sum(school_data$sch_q9g[which(school_data$neighbor==i)])),
                                                             rep(5,sum(school_data$sch_q9i[which(school_data$neighbor==i)])));
        freq[[14*(sort(unique(school_data$neighbor))[i]-1)+2]]=c(rep(1,sum(school_data$sch_q8a[which(school_data$neighbor==i)])),rep(2,sum(school_data$sch_q8c[which(school_data$neighbor==i)])),
                                                             rep(3,sum(school_data$sch_q8e[which(school_data$neighbor==i)])),rep(4,sum(school_data$sch_q8g[which(school_data$neighbor==i)])));
        freq[[14*(sort(unique(school_data$neighbor))[i]-1)+3]]=c(rep(1,sum(school_data$sch_q15a[which(school_data$neighbor==i)])),rep(2,sum(school_data$sch_q15c[which(school_data$neighbor==i)])),
                                                             rep(3,sum(school_data$sch_q15e[which(school_data$neighbor==i)])),rep(4,sum(school_data$sch_q15g[which(school_data$neighbor==i)])),
                                                             rep(5,sum(school_data$sch_q15i[which(school_data$neighbor==i)])));
        freq[[14*(sort(unique(school_data$neighbor))[i]-1)+4]]=c(rep(1,sum(school_data$sch_q14a[which(school_data$neighbor==i)])),rep(2,sum(school_data$sch_q14c[which(school_data$neighbor==i)])),
                                                             rep(3,sum(school_data$sch_q14e[which(school_data$neighbor==i)])),rep(4,sum(school_data$sch_q14g[which(school_data$neighbor==i)])));
        freq[[14*(sort(unique(school_data$neighbor))[i]-1)+5]]=c(rep(1,sum(school_data$sch_q13a[which(school_data$neighbor==i)])),rep(2,sum(school_data$sch_q13c[which(school_data$neighbor==i)])),
                                                             rep(3,sum(school_data$sch_q13e[which(school_data$neighbor==i)])),rep(4,sum(school_data$sch_q13g[which(school_data$neighbor==i)])),
                                                             rep(5,sum(school_data$sch_q13i[which(school_data$neighbor==i)])));
        freq[[14*(sort(unique(school_data$neighbor))[i]-1)+6]]=c(rep(1,sum(school_data$sch_q12a[which(school_data$neighbor==i)])),rep(2,sum(school_data$sch_q12c[which(school_data$neighbor==i)])),
                                                             rep(3,sum(school_data$sch_q12e[which(school_data$neighbor==i)])),rep(4,sum(school_data$sch_q12g[which(school_data$neighbor==i)])));
        freq[[14*(sort(unique(school_data$neighbor))[i]-1)+7]]=c(rep(1,sum(school_data$sch_q5a[which(school_data$neighbor==i)])),rep(2,sum(school_data$sch_q5c[which(school_data$neighbor==i)])),
                                                             rep(3,sum(school_data$sch_q5e[which(school_data$neighbor==i)])),rep(4,sum(school_data$sch_q5g[which(school_data$neighbor==i)])),
                                                             rep(5,sum(school_data$sch_q5i[which(school_data$neighbor==i)])));
        freq[[14*(sort(unique(school_data$neighbor))[i]-1)+8]]=c(rep(1,sum(school_data$sch_q4a[which(school_data$neighbor==i)])),rep(2,sum(school_data$sch_q4c[which(school_data$neighbor==i)])),
                                                             rep(3,sum(school_data$sch_q4e[which(school_data$neighbor==i)])),rep(4,sum(school_data$sch_q4g[which(school_data$neighbor==i)])));
        freq[[14*(sort(unique(school_data$neighbor))[i]-1)+9]]=c(rep(1,sum(school_data$sch_q7a[which(school_data$neighbor==i)])),rep(2,sum(school_data$sch_q7c[which(school_data$neighbor==i)])),
                                                             rep(3,sum(school_data$sch_q7e[which(school_data$neighbor==i)])),rep(4,sum(school_data$sch_q7g[which(school_data$neighbor==i)])),
                                                             rep(5,sum(school_data$sch_q7i[which(school_data$neighbor==i)])));
        freq[[14*(sort(unique(school_data$neighbor))[i]-1)+10]]=c(rep(1,sum(school_data$sch_q6a[which(school_data$neighbor==i)])),rep(2,sum(school_data$sch_q6c[which(school_data$neighbor==i)])),
                                                              rep(3,sum(school_data$sch_q6e[which(school_data$neighbor==i)])),rep(4,sum(school_data$sch_q6g[which(school_data$neighbor==i)])));
        freq[[14*(sort(unique(school_data$neighbor))[i]-1)+11]]=c(rep(1,sum(school_data$sch_q11a[which(school_data$neighbor==i)])),rep(2,sum(school_data$sch_q11c[which(school_data$neighbor==i)])),
                                                              rep(3,sum(school_data$sch_q11e[which(school_data$neighbor==i)])),rep(4,sum(school_data$sch_q11g[which(school_data$neighbor==i)])),
                                                              rep(5,sum(school_data$sch_q11i[which(school_data$neighbor==i)])));
        freq[[14*(sort(unique(school_data$neighbor))[i]-1)+12]]=c(rep(1,sum(school_data$sch_q10a[which(school_data$neighbor==i)])),rep(2,sum(school_data$sch_q10c[which(school_data$neighbor==i)])),
                                                              rep(3,sum(school_data$sch_q10e[which(school_data$neighbor==i)])),rep(4,sum(school_data$sch_q10g[which(school_data$neighbor==i)])));
        freq[[14*(sort(unique(school_data$neighbor))[i]-1)+13]]=c(rep(1,sum(school_data$sch_q17a[which(school_data$neighbor==i)])),rep(2,sum(school_data$sch_q17c[which(school_data$neighbor==i)])),
                                                              rep(3,sum(school_data$sch_q17e[which(school_data$neighbor==i)])),rep(4,sum(school_data$sch_q17g[which(school_data$neighbor==i)])),
                                                              rep(5,sum(school_data$sch_q17i[which(school_data$neighbor==i)])));
        freq[[14*(sort(unique(school_data$neighbor))[i]-1)+14]]=c(rep(1,sum(school_data$sch_q16a[which(school_data$neighbor==i)])),rep(2,sum(school_data$sch_q16c[which(school_data$neighbor==i)])),
                                                              rep(3,sum(school_data$sch_q16e[which(school_data$neighbor==i)])),rep(4,sum(school_data$sch_q16g[which(school_data$neighbor==i)])));
    }
  }
  
  # people plot frequencies
  else if (type == 'ppl plot') {
    for (i in 1:length(unique(school_data$neighbor))){
        # sample type 1|2=drain, 3|4=produce, 5|6=piped water, 7|8=ocean water, 9|10=surface water, 11|12=flood water, 13|14=Public Latrine Surfaces  
        freq[[14*(sort(unique(school_data$neighbor))[i]-1)+1]]=4-c(rep(1,sum(school_data$sch_q9a[which(school_data$neighbor==i)])),rep(2,sum(school_data$sch_q9c[which(school_data$neighbor==i)])),
                                                               rep(3,sum(school_data$sch_q9e[which(school_data$neighbor==i)])),rep(4,sum(school_data$sch_q9g[which(school_data$neighbor==i)])),
                                                               rep(5,sum(school_data$sch_q9i[which(school_data$neighbor==i)])));
        freq[[14*(sort(unique(school_data$neighbor))[i]-1)+2]]=4-c(rep(1,sum(school_data$sch_q8a[which(school_data$neighbor==i)])),rep(2,sum(school_data$sch_q8c[which(school_data$neighbor==i)])),
                                                               rep(3,sum(school_data$sch_q8e[which(school_data$neighbor==i)])),rep(4,sum(school_data$sch_q8g[which(school_data$neighbor==i)])));
        freq[[14*(sort(unique(school_data$neighbor))[i]-1)+3]]=4-c(rep(1,sum(school_data$sch_q15a[which(school_data$neighbor==i)])),rep(2,sum(school_data$sch_q15c[which(school_data$neighbor==i)])),
                                                               rep(3,sum(school_data$sch_q15e[which(school_data$neighbor==i)])),rep(4,sum(school_data$sch_q15g[which(school_data$neighbor==i)])),
                                                               rep(5,sum(school_data$sch_q15i[which(school_data$neighbor==i)])));
        freq[[14*(sort(unique(school_data$neighbor))[i]-1)+4]]=4-c(rep(1,sum(school_data$sch_q14a[which(school_data$neighbor==i)])),rep(2,sum(school_data$sch_q14c[which(school_data$neighbor==i)])),
                                                               rep(3,sum(school_data$sch_q14e[which(school_data$neighbor==i)])),rep(4,sum(school_data$sch_q14g[which(school_data$neighbor==i)])));
        freq[[14*(sort(unique(school_data$neighbor))[i]-1)+5]]=4-c(rep(1,sum(school_data$sch_q13a[which(school_data$neighbor==i)])),rep(2,sum(school_data$sch_q13c[which(school_data$neighbor==i)])),
                                                               rep(3,sum(school_data$sch_q13e[which(school_data$neighbor==i)])),rep(4,sum(school_data$sch_q13g[which(school_data$neighbor==i)])),
                                                               rep(5,sum(school_data$sch_q13i[which(school_data$neighbor==i)])));
        freq[[14*(sort(unique(school_data$neighbor))[i]-1)+6]]=4-c(rep(1,sum(school_data$sch_q12a[which(school_data$neighbor==i)])),rep(2,sum(school_data$sch_q12c[which(school_data$neighbor==i)])),
                                                               rep(3,sum(school_data$sch_q12e[which(school_data$neighbor==i)])),rep(4,sum(school_data$sch_q12g[which(school_data$neighbor==i)])));
        freq[[14*(sort(unique(school_data$neighbor))[i]-1)+7]]=4-c(rep(1,sum(school_data$sch_q5a[which(school_data$neighbor==i)])),rep(2,sum(school_data$sch_q5c[which(school_data$neighbor==i)])),
                                                               rep(3,sum(school_data$sch_q5e[which(school_data$neighbor==i)])),rep(4,sum(school_data$sch_q5g[which(school_data$neighbor==i)])),
                                                               rep(5,sum(school_data$sch_q5i[which(school_data$neighbor==i)])));
        freq[[14*(sort(unique(school_data$neighbor))[i]-1)+8]]=4-c(rep(1,sum(school_data$sch_q4a[which(school_data$neighbor==i)])),rep(2,sum(school_data$sch_q4c[which(school_data$neighbor==i)])),
                                                               rep(3,sum(school_data$sch_q4e[which(school_data$neighbor==i)])),rep(4,sum(school_data$sch_q4g[which(school_data$neighbor==i)])));
        freq[[14*(sort(unique(school_data$neighbor))[i]-1)+9]]=4-c(rep(1,sum(school_data$sch_q7a[which(school_data$neighbor==i)])),rep(2,sum(school_data$sch_q7c[which(school_data$neighbor==i)])),
                                                               rep(3,sum(school_data$sch_q7e[which(school_data$neighbor==i)])),rep(4,sum(school_data$sch_q7g[which(school_data$neighbor==i)])),
                                                               rep(5,sum(school_data$sch_q7i[which(school_data$neighbor==i)])));
        freq[[14*(sort(unique(school_data$neighbor))[i]-1)+10]]=4-c(rep(1,sum(school_data$sch_q6a[which(school_data$neighbor==i)])),rep(2,sum(school_data$sch_q6c[which(school_data$neighbor==i)])),
                                                                rep(3,sum(school_data$sch_q6e[which(school_data$neighbor==i)])),rep(4,sum(school_data$sch_q6g[which(school_data$neighbor==i)])));
        freq[[14*(sort(unique(school_data$neighbor))[i]-1)+11]]=4-c(rep(1,sum(school_data$sch_q11a[which(school_data$neighbor==i)])),rep(2,sum(school_data$sch_q11c[which(school_data$neighbor==i)])),
                                                                rep(3,sum(school_data$sch_q11e[which(school_data$neighbor==i)])),rep(4,sum(school_data$sch_q11g[which(school_data$neighbor==i)])),
                                                                rep(5,sum(school_data$sch_q11i[which(school_data$neighbor==i)])));
        freq[[14*(sort(unique(school_data$neighbor))[i]-1)+12]]=4-c(rep(1,sum(school_data$sch_q10a[which(school_data$neighbor==i)])),rep(2,sum(school_data$sch_q10c[which(school_data$neighbor==i)])),
                                                                rep(3,sum(school_data$sch_q10e[which(school_data$neighbor==i)])),rep(4,sum(school_data$sch_q10g[which(school_data$neighbor==i)])));
        freq[[14*(sort(unique(school_data$neighbor))[i]-1)+13]]=4-c(rep(1,sum(school_data$sch_q17a[which(school_data$neighbor==i)])),rep(2,sum(school_data$sch_q17c[which(school_data$neighbor==i)])),
                                                                rep(3,sum(school_data$sch_q17e[which(school_data$neighbor==i)])),rep(4,sum(school_data$sch_q17g[which(school_data$neighbor==i)])),
                                                                rep(5,sum(school_data$sch_q17i[which(school_data$neighbor==i)])));
        freq[[14*(sort(unique(school_data$neighbor))[i]-1)+14]]=4-c(rep(1,sum(school_data$sch_q16a[which(school_data$neighbor==i)])),rep(2,sum(school_data$sch_q16c[which(school_data$neighbor==i)])),
                                                                rep(3,sum(school_data$sch_q16e[which(school_data$neighbor==i)])),rep(4,sum(school_data$sch_q16g[which(school_data$neighbor==i)])));
    }
  }
  else {
    freq <- NULL
  }
  
  return(freq)
  
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
  freq<-list()
  
  
  if (type == 'pie chart') {
    for (i in 1:length(unique(community_data$neighbor))){
      # sample type 1|2=drain, 3|4=produce, 5|6=piped water, 7|8=ocean water, 9|10=surface water, 11|12=flood water, 13|14=Public Latrine Surfaces  
      freq[[14*(sort(unique(community_data$neighbor))[i]-1)+1]]=c(rep(1,sum(community_data$com_q6a[which(community_data$neighbor==i)])),rep(2,sum(community_data$com_q6c[which(community_data$neighbor==i)])),
                                                           rep(3,sum(community_data$com_q6e[which(community_data$neighbor==i)])),rep(4,sum(community_data$com_q6g[which(community_data$neighbor==i)])));
      freq[[14*(sort(unique(community_data$neighbor))[i]-1)+2]]=c(rep(1,sum(community_data$com_q21a[which(community_data$neighbor==i)])),rep(2,sum(community_data$com_q21c[which(community_data$neighbor==i)])),
                                                           rep(3,sum(community_data$com_q21e[which(community_data$neighbor==i)])),rep(4,sum(community_data$com_q21g[which(community_data$neighbor==i)])),
                                                           rep(5,sum(community_data$com_q21i[which(community_data$neighbor==i)])));
      freq[[14*(sort(unique(community_data$neighbor))[i]-1)+3]]=c(rep(1,sum(community_data$com_q9a[which(community_data$neighbor==i)])),rep(2,sum(community_data$com_q9c[which(community_data$neighbor==i)])),
                                                           rep(3,sum(community_data$com_q9e[which(community_data$neighbor==i)])),rep(4,sum(community_data$com_q9g[which(community_data$neighbor==i)])));
      freq[[14*(sort(unique(community_data$neighbor))[i]-1)+4]]=c(rep(1,sum(community_data$com_q24a[which(community_data$neighbor==i)])),rep(2,sum(community_data$com_q24c[which(community_data$neighbor==i)])),
                                                           rep(3,sum(community_data$com_q24e[which(community_data$neighbor==i)])),rep(4,sum(community_data$com_q24g[which(community_data$neighbor==i)])),
                                                           rep(5,sum(community_data$com_q24i[which(community_data$neighbor==i)])));
      freq[[14*(sort(unique(community_data$neighbor))[i]-1)+5]]=c(rep(1,sum(community_data$com_q8a[which(community_data$neighbor==i)])),rep(2,sum(community_data$com_q8c[which(community_data$neighbor==i)])),
                                                           rep(3,sum(community_data$com_q8e[which(community_data$neighbor==i)])),rep(4,sum(community_data$com_q8g[which(community_data$neighbor==i)])));
      freq[[14*(sort(unique(community_data$neighbor))[i]-1)+6]]=c(rep(1,sum(community_data$com_q23a[which(community_data$neighbor==i)])),rep(2,sum(community_data$com_q23c[which(community_data$neighbor==i)])),
                                                           rep(3,sum(community_data$com_q23e[which(community_data$neighbor==i)])),rep(4,sum(community_data$com_q23g[which(community_data$neighbor==i)])),
                                                           rep(5,sum(community_data$com_q23i[which(community_data$neighbor==i)])));
      freq[[14*(sort(unique(community_data$neighbor))[i]-1)+7]]=c(rep(1,sum(community_data$com_q4a[which(community_data$neighbor==i)])),rep(2,sum(community_data$com_q4c[which(community_data$neighbor==i)])),
                                                           rep(3,sum(community_data$com_q4e[which(community_data$neighbor==i)])),rep(4,sum(community_data$com_q4g[which(community_data$neighbor==i)])));
      freq[[14*(sort(unique(community_data$neighbor))[i]-1)+8]]=c(rep(1,sum(community_data$com_q19a[which(community_data$neighbor==i)])),rep(2,sum(community_data$com_q19c[which(community_data$neighbor==i)])),
                                                           rep(3,sum(community_data$com_q19e[which(community_data$neighbor==i)])),rep(4,sum(community_data$com_q19g[which(community_data$neighbor==i)])),
                                                           rep(5,sum(community_data$com_q19i[which(community_data$neighbor==i)])));
      freq[[14*(sort(unique(community_data$neighbor))[i]-1)+9]]=c(rep(1,sum(community_data$com_q5a[which(community_data$neighbor==i)])),rep(2,sum(community_data$com_q5c[which(community_data$neighbor==i)])),
                                                           rep(3,sum(community_data$com_q5e[which(community_data$neighbor==i)])),rep(4,sum(community_data$com_q5g[which(community_data$neighbor==i)])));
      freq[[14*(sort(unique(community_data$neighbor))[i]-1)+10]]=c(rep(1,sum(community_data$com_q20a[which(community_data$neighbor==i)])),rep(2,sum(community_data$com_q20c[which(community_data$neighbor==i)])),
                                                            rep(3,sum(community_data$com_q20e[which(community_data$neighbor==i)])),rep(4,sum(community_data$com_q20g[which(community_data$neighbor==i)])),
                                                            rep(5,sum(community_data$com_q20i[which(community_data$neighbor==i)])));
      freq[[14*(sort(unique(community_data$neighbor))[i]-1)+11]]=c(rep(1,sum(community_data$com_q7a[which(community_data$neighbor==i)])),rep(2,sum(community_data$com_q7c[which(community_data$neighbor==i)])),
                                                            rep(3,sum(community_data$com_q7e[which(community_data$neighbor==i)])),rep(4,sum(community_data$com_q7g[which(community_data$neighbor==i)])));
      freq[[14*(sort(unique(community_data$neighbor))[i]-1)+12]]=c(rep(1,sum(community_data$com_q22a[which(community_data$neighbor==i)])),rep(2,sum(community_data$com_q22c[which(community_data$neighbor==i)])),
                                                            rep(3,sum(community_data$com_q22e[which(community_data$neighbor==i)])),rep(4,sum(community_data$com_q22g[which(community_data$neighbor==i)])),
                                                            rep(5,sum(community_data$com_q22i[which(community_data$neighbor==i)])));
      freq[[14*(sort(unique(community_data$neighbor))[i]-1)+13]]=c(rep(1,sum(community_data$com_q10a[which(community_data$neighbor==i)])),rep(2,sum(community_data$com_q10c[which(community_data$neighbor==i)])),
                                                            rep(3,sum(community_data$com_q10e[which(community_data$neighbor==i)])),rep(4,sum(community_data$com_q10g[which(community_data$neighbor==i)])));
      freq[[14*(sort(unique(community_data$neighbor))[i]-1)+14]]=c(rep(1,sum(community_data$com_q25a[which(community_data$neighbor==i)])),rep(2,sum(community_data$com_q25c[which(community_data$neighbor==i)])),
                                                            rep(3,sum(community_data$com_q25e[which(community_data$neighbor==i)])),rep(4,sum(community_data$com_q25g[which(community_data$neighbor==i)])),
                                                            rep(5,sum(community_data$com_q25i[which(community_data$neighbor==i)])));
    }
    
  }
  # frequencies for pie charts
  else if (type == 'ppl plot') {
    for (i in 1:length(unique(community_data$neighbor))){
      # sample type 1|2=drain, 3|4=produce, 5|6=piped water, 7|8=ocean water, 9|10=surface water, 11|12=flood water, 13|14=Public Latrine Surfaces  
      freq[[14*(sort(unique(community_data$neighbor))[i]-1)+1]]=4-c(rep(1,sum(community_data$com_q6a[which(community_data$neighbor==i)])),rep(2,sum(community_data$com_q6c[which(community_data$neighbor==i)])),
                                                             rep(3,sum(community_data$com_q6e[which(community_data$neighbor==i)])),rep(4,sum(community_data$com_q6g[which(community_data$neighbor==i)])));
      freq[[14*(sort(unique(community_data$neighbor))[i]-1)+2]]=4-c(rep(1,sum(community_data$com_q21a[which(community_data$neighbor==i)])),rep(2,sum(community_data$com_q21c[which(community_data$neighbor==i)])),
                                                             rep(3,sum(community_data$com_q21e[which(community_data$neighbor==i)])),rep(4,sum(community_data$com_q21g[which(community_data$neighbor==i)])),
                                                             rep(5,sum(community_data$com_q21i[which(community_data$neighbor==i)])));
      freq[[14*(sort(unique(community_data$neighbor))[i]-1)+3]]=4-c(rep(1,sum(community_data$com_q9a[which(community_data$neighbor==i)])),rep(2,sum(community_data$com_q9c[which(community_data$neighbor==i)])),
                                                             rep(3,sum(community_data$com_q9e[which(community_data$neighbor==i)])),rep(4,sum(community_data$com_q9g[which(community_data$neighbor==i)])));
      freq[[14*(sort(unique(community_data$neighbor))[i]-1)+4]]=4-c(rep(1,sum(community_data$com_q24a[which(community_data$neighbor==i)])),rep(2,sum(community_data$com_q24c[which(community_data$neighbor==i)])),
                                                             rep(3,sum(community_data$com_q24e[which(community_data$neighbor==i)])),rep(4,sum(community_data$com_q24g[which(community_data$neighbor==i)])),
                                                             rep(5,sum(community_data$com_q24i[which(community_data$neighbor==i)])));
      freq[[14*(sort(unique(community_data$neighbor))[i]-1)+5]]=4-c(rep(1,sum(community_data$com_q8a[which(community_data$neighbor==i)])),rep(2,sum(community_data$com_q8c[which(community_data$neighbor==i)])),
                                                             rep(3,sum(community_data$com_q8e[which(community_data$neighbor==i)])),rep(4,sum(community_data$com_q8g[which(community_data$neighbor==i)])));
      freq[[14*(sort(unique(community_data$neighbor))[i]-1)+6]]=4-c(rep(1,sum(community_data$com_q23a[which(community_data$neighbor==i)])),rep(2,sum(community_data$com_q23c[which(community_data$neighbor==i)])),
                                                             rep(3,sum(community_data$com_q23e[which(community_data$neighbor==i)])),rep(4,sum(community_data$com_q23g[which(community_data$neighbor==i)])),
                                                             rep(5,sum(community_data$com_q23i[which(community_data$neighbor==i)])));
      freq[[14*(sort(unique(community_data$neighbor))[i]-1)+7]]=4-c(rep(1,sum(community_data$com_q4a[which(community_data$neighbor==i)])),rep(2,sum(community_data$com_q4c[which(community_data$neighbor==i)])),
                                                             rep(3,sum(community_data$com_q4e[which(community_data$neighbor==i)])),rep(4,sum(community_data$com_q4g[which(community_data$neighbor==i)])));
      freq[[14*(sort(unique(community_data$neighbor))[i]-1)+8]]=4-c(rep(1,sum(community_data$com_q19a[which(community_data$neighbor==i)])),rep(2,sum(community_data$com_q19c[which(community_data$neighbor==i)])),
                                                             rep(3,sum(community_data$com_q19e[which(community_data$neighbor==i)])),rep(4,sum(community_data$com_q19g[which(community_data$neighbor==i)])),
                                                             rep(5,sum(community_data$com_q19i[which(community_data$neighbor==i)])));
      freq[[14*(sort(unique(community_data$neighbor))[i]-1)+9]]=4-c(rep(1,sum(community_data$com_q5a[which(community_data$neighbor==i)])),rep(2,sum(community_data$com_q5c[which(community_data$neighbor==i)])),
                                                             rep(3,sum(community_data$com_q5e[which(community_data$neighbor==i)])),rep(4,sum(community_data$com_q5g[which(community_data$neighbor==i)])));
      freq[[14*(sort(unique(community_data$neighbor))[i]-1)+10]]=4-c(rep(1,sum(community_data$com_q20a[which(community_data$neighbor==i)])),rep(2,sum(community_data$com_q20c[which(community_data$neighbor==i)])),
                                                              rep(3,sum(community_data$com_q20e[which(community_data$neighbor==i)])),rep(4,sum(community_data$com_q20g[which(community_data$neighbor==i)])),
                                                              rep(5,sum(community_data$com_q20i[which(community_data$neighbor==i)])));
      freq[[14*(sort(unique(community_data$neighbor))[i]-1)+11]]=4-c(rep(1,sum(community_data$com_q7a[which(community_data$neighbor==i)])),rep(2,sum(community_data$com_q7c[which(community_data$neighbor==i)])),
                                                              rep(3,sum(community_data$com_q7e[which(community_data$neighbor==i)])),rep(4,sum(community_data$com_q7g[which(community_data$neighbor==i)])));
      freq[[14*(sort(unique(community_data$neighbor))[i]-1)+12]]=4-c(rep(1,sum(community_data$com_q22a[which(community_data$neighbor==i)])),rep(2,sum(community_data$com_q22c[which(community_data$neighbor==i)])),
                                                              rep(3,sum(community_data$com_q22e[which(community_data$neighbor==i)])),rep(4,sum(community_data$com_q22g[which(community_data$neighbor==i)])),
                                                              rep(5,sum(community_data$com_q22i[which(community_data$neighbor==i)])));
      freq[[14*(sort(unique(community_data$neighbor))[i]-1)+13]]=4-c(rep(1,sum(community_data$com_q10a[which(community_data$neighbor==i)])),rep(2,sum(community_data$com_q10c[which(community_data$neighbor==i)])),
                                                              rep(3,sum(community_data$com_q10e[which(community_data$neighbor==i)])),rep(4,sum(community_data$com_q10g[which(community_data$neighbor==i)])));
      freq[[14*(sort(unique(community_data$neighbor))[i]-1)+14]]=4-c(rep(1,sum(community_data$com_q25a[which(community_data$neighbor==i)])),rep(2,sum(community_data$com_q25c[which(community_data$neighbor==i)])),
                                                              rep(3,sum(community_data$com_q25e[which(community_data$neighbor==i)])),rep(4,sum(community_data$com_q25g[which(community_data$neighbor==i)])),
                                                              rep(5,sum(community_data$com_q25i[which(community_data$neighbor==i)])));
    }
    
  }
  else {
    freq <- NULL
  }
  
  return(freq)
  
}

calculate_combinedFreq <- function(household_data, school_data, community_data, type='pie chart') {
  # calculate the appropriate factors for plotting pie charts
  # and people plots.  This is specific to the combined 
  # form. returns a list with two objects. type can equal
  # 'pie chart' or 'ppl plot'
  #
  # Ex.
  # > calculate_combinedFreq(household_data, school_data, community_data, 'pie chart')
  # $[[1]]
  # [1] 3 4 2 1
  # ...
  freq<-list()
  num.neighb<-sort(unique(c(as.numeric(household_data$neighbor),as.numeric(school_data$neighbor),as.numeric(community_data$neighbor))))
  
  if (type == 'pie chart') {
    for (i in 1:length(num.neighb)){
      # sample type 1|2=drain, 3|4=produce, 5|6=piped water, 7|8=ocean water, 9|10=surface water, 11|12=flood water, 13|14=Public Latrine Surfaces  
      freq[[14*(num.neighb[i]-1)+1]]=c(as.numeric(household_data$hh_q6[which(household_data$hh_q6!="n/a" & household_data$neighbor==i)]),
                                       c(rep(1,sum(school_data$sch_q9a[which(school_data$neighbor==i)])),rep(2,sum(school_data$sch_q9c[which(school_data$neighbor==i)])),
                                         rep(3,sum(school_data$sch_q9e[which(school_data$neighbor==i)])),rep(4,sum(school_data$sch_q9g[which(school_data$neighbor==i)])),
                                         rep(5,sum(school_data$sch_q9i[which(school_data$neighbor==i)]))),
                                       c(rep(1,sum(community_data$com_q6a[which(community_data$neighbor==i)])),rep(2,sum(community_data$com_q6c[which(community_data$neighbor==i)])),
                                         rep(3,sum(community_data$com_q6e[which(community_data$neighbor==i)])),rep(4,sum(community_data$com_q6g[which(community_data$neighbor==i)]))));
      freq[[14*(num.neighb[i]-1)+2]]=c(as.numeric(household_data$hh_q7[which(household_data$hh_q7!="n/a" & household_data$neighbor==i)]),
                                       c(rep(1,sum(school_data$sch_q8a[which(school_data$neighbor==i)])),rep(2,sum(school_data$sch_q8c[which(school_data$neighbor==i)])),
                                         rep(3,sum(school_data$sch_q8e[which(school_data$neighbor==i)])),rep(4,sum(school_data$sch_q8g[which(school_data$neighbor==i)]))),
                                       c(rep(1,sum(community_data$com_q21a[which(community_data$neighbor==i)])),rep(2,sum(community_data$com_q21c[which(community_data$neighbor==i)])),
                                         rep(3,sum(community_data$com_q21e[which(community_data$neighbor==i)])),rep(4,sum(community_data$com_q21g[which(community_data$neighbor==i)])),
                                         rep(5,sum(community_data$com_q21i[which(community_data$neighbor==i)]))));                                       
      freq[[14*(num.neighb[i]-1)+3]]=c(as.numeric(household_data$hh_q13[which(household_data$hh_q13!="n/a" & household_data$neighbor==i)]),
                                       c(rep(1,sum(school_data$sch_q15a[which(school_data$neighbor==i)])),rep(2,sum(school_data$sch_q15c[which(school_data$neighbor==i)])),
                                         rep(3,sum(school_data$sch_q15e[which(school_data$neighbor==i)])),rep(4,sum(school_data$sch_q15g[which(school_data$neighbor==i)])),
                                         rep(5,sum(school_data$sch_q15i[which(school_data$neighbor==i)]))),
                                       c(rep(1,sum(community_data$com_q9a[which(community_data$neighbor==i)])),rep(2,sum(community_data$com_q9c[which(community_data$neighbor==i)])),
                                         rep(3,sum(community_data$com_q9e[which(community_data$neighbor==i)])),rep(4,sum(community_data$com_q9g[which(community_data$neighbor==i)]))));
      freq[[14*(num.neighb[i]-1)+4]]=c(as.numeric(household_data$hh_q14[which(household_data$hh_q14!="n/a" & household_data$neighbor==i)]),
                                       c(rep(1,sum(school_data$sch_q14a[which(school_data$neighbor==i)])),rep(2,sum(school_data$sch_q14c[which(school_data$neighbor==i)])),
                                         rep(3,sum(school_data$sch_q14e[which(school_data$neighbor==i)])),rep(4,sum(school_data$sch_q14g[which(school_data$neighbor==i)]))),
                                       c(rep(1,sum(community_data$com_q24a[which(community_data$neighbor==i)])),rep(2,sum(community_data$com_q24c[which(community_data$neighbor==i)])),
                                         rep(3,sum(community_data$com_q24e[which(community_data$neighbor==i)])),rep(4,sum(community_data$com_q24g[which(community_data$neighbor==i)])),
                                         rep(5,sum(community_data$com_q24i[which(community_data$neighbor==i)]))));
      freq[[14*(num.neighb[i]-1)+5]]=c(as.numeric(household_data$hh_q10[which(household_data$hh_q10!="n/a" & household_data$neighbor==i)]),
                                       c(rep(1,sum(school_data$sch_q13a[which(school_data$neighbor==i)])),rep(2,sum(school_data$sch_q13c[which(school_data$neighbor==i)])),
                                         rep(3,sum(school_data$sch_q13e[which(school_data$neighbor==i)])),rep(4,sum(school_data$sch_q13g[which(school_data$neighbor==i)])),
                                         rep(5,sum(school_data$sch_q13i[which(school_data$neighbor==i)]))),
                                       c(rep(1,sum(community_data$com_q8a[which(community_data$neighbor==i)])),rep(2,sum(community_data$com_q8c[which(community_data$neighbor==i)])),
                                         rep(3,sum(community_data$com_q8e[which(community_data$neighbor==i)])),rep(4,sum(community_data$com_q8g[which(community_data$neighbor==i)]))));
      freq[[14*(num.neighb[i]-1)+6]]=c(as.numeric(household_data$hh_q11[which(household_data$hh_q11!="n/a" & household_data$neighbor==i)]),
                                       c(rep(1,sum(school_data$sch_q12a[which(school_data$neighbor==i)])),rep(2,sum(school_data$sch_q12c[which(school_data$neighbor==i)])),
                                         rep(3,sum(school_data$sch_q12e[which(school_data$neighbor==i)])),rep(4,sum(school_data$sch_q12g[which(school_data$neighbor==i)]))),
                                       c(rep(1,sum(community_data$com_q23a[which(community_data$neighbor==i)])),rep(2,sum(community_data$com_q23c[which(community_data$neighbor==i)])),
                                         rep(3,sum(community_data$com_q23e[which(community_data$neighbor==i)])),rep(4,sum(community_data$com_q23g[which(community_data$neighbor==i)])),
                                         rep(5,sum(community_data$com_q23i[which(community_data$neighbor==i)]))));
      freq[[14*(num.neighb[i]-1)+7]]=c(as.numeric(household_data$hh_q2[which(household_data$hh_q2!="n/a" & household_data$neighbor==i)]),
                                       c(rep(1,sum(school_data$sch_q5a[which(school_data$neighbor==i)])),rep(2,sum(school_data$sch_q5c[which(school_data$neighbor==i)])),
                                         rep(3,sum(school_data$sch_q5e[which(school_data$neighbor==i)])),rep(4,sum(school_data$sch_q5g[which(school_data$neighbor==i)])),
                                         rep(5,sum(school_data$sch_q5i[which(school_data$neighbor==i)]))),
                                       c(rep(1,sum(community_data$com_q4a[which(community_data$neighbor==i)])),rep(2,sum(community_data$com_q4c[which(community_data$neighbor==i)])),
                                         rep(3,sum(community_data$com_q4e[which(community_data$neighbor==i)])),rep(4,sum(community_data$com_q4g[which(community_data$neighbor==i)]))));
      freq[[14*(num.neighb[i]-1)+8]]=c(as.numeric(household_data$hh_q3[which(household_data$hh_q3!="n/a" & household_data$neighbor==i)]),
                                       c(rep(1,sum(school_data$sch_q4a[which(school_data$neighbor==i)])),rep(2,sum(school_data$sch_q4c[which(school_data$neighbor==i)])),
                                         rep(3,sum(school_data$sch_q4e[which(school_data$neighbor==i)])),rep(4,sum(school_data$sch_q4g[which(school_data$neighbor==i)]))),
                                       c(rep(1,sum(community_data$com_q19a[which(community_data$neighbor==i)])),rep(2,sum(community_data$com_q19c[which(community_data$neighbor==i)])),
                                         rep(3,sum(community_data$com_q19e[which(community_data$neighbor==i)])),rep(4,sum(community_data$com_q19g[which(community_data$neighbor==i)])),
                                         rep(5,sum(community_data$com_q19i[which(community_data$neighbor==i)]))));
      freq[[14*(num.neighb[i]-1)+9]]=c(as.numeric(household_data$hh_q4[which(household_data$hh_q4!="n/a" & household_data$neighbor==i)]),
                                       c(rep(1,sum(school_data$sch_q7a[which(school_data$neighbor==i)])),rep(2,sum(school_data$sch_q7c[which(school_data$neighbor==i)])),
                                         rep(3,sum(school_data$sch_q7e[which(school_data$neighbor==i)])),rep(4,sum(school_data$sch_q7g[which(school_data$neighbor==i)])),
                                         rep(5,sum(school_data$sch_q7i[which(school_data$neighbor==i)]))),
                                       c(rep(1,sum(community_data$com_q5a[which(community_data$neighbor==i)])),rep(2,sum(community_data$com_q5c[which(community_data$neighbor==i)])),
                                         rep(3,sum(community_data$com_q5e[which(community_data$neighbor==i)])),rep(4,sum(community_data$com_q5g[which(community_data$neighbor==i)]))));
      freq[[14*(num.neighb[i]-1)+10]]=c(as.numeric(household_data$hh_q5[which(household_data$hh_q5!="n/a" & household_data$neighbor==i)]),
                                        c(rep(1,sum(school_data$sch_q6a[which(school_data$neighbor==i)])),rep(2,sum(school_data$sch_q6c[which(school_data$neighbor==i)])),
                                          rep(3,sum(school_data$sch_q6e[which(school_data$neighbor==i)])),rep(4,sum(school_data$sch_q6g[which(school_data$neighbor==i)]))),
                                        c(rep(1,sum(community_data$com_q20a[which(community_data$neighbor==i)])),rep(2,sum(community_data$com_q20c[which(community_data$neighbor==i)])),
                                          rep(3,sum(community_data$com_q20e[which(community_data$neighbor==i)])),rep(4,sum(community_data$com_q20g[which(community_data$neighbor==i)])),
                                          rep(5,sum(community_data$com_q20i[which(community_data$neighbor==i)]))));
      freq[[14*(num.neighb[i]-1)+11]]=c(as.numeric(household_data$hh_q8[which(household_data$hh_q8!="n/a" & household_data$neighbor==i)]),
                                        c(rep(1,sum(school_data$sch_q11a[which(school_data$neighbor==i)])),rep(2,sum(school_data$sch_q11c[which(school_data$neighbor==i)])),
                                          rep(3,sum(school_data$sch_q11e[which(school_data$neighbor==i)])),rep(4,sum(school_data$sch_q11g[which(school_data$neighbor==i)])),
                                          rep(5,sum(school_data$sch_q11i[which(school_data$neighbor==i)]))),
                                        c(rep(1,sum(community_data$com_q7a[which(community_data$neighbor==i)])),rep(2,sum(community_data$com_q7c[which(community_data$neighbor==i)])),
                                          rep(3,sum(community_data$com_q7e[which(community_data$neighbor==i)])),rep(4,sum(community_data$com_q7g[which(community_data$neighbor==i)]))));
      freq[[14*(num.neighb[i]-1)+12]]=c(as.numeric(household_data$hh_q9[which(household_data$hh_q9!="n/a" & household_data$neighbor==i)]),
                                        c(rep(1,sum(school_data$sch_q10a[which(school_data$neighbor==i)])),rep(2,sum(school_data$sch_q10c[which(school_data$neighbor==i)])),
                                          rep(3,sum(school_data$sch_q10e[which(school_data$neighbor==i)])),rep(4,sum(school_data$sch_q10g[which(school_data$neighbor==i)]))),
                                        c(rep(1,sum(community_data$com_q22a[which(community_data$neighbor==i)])),rep(2,sum(community_data$com_q22c[which(community_data$neighbor==i)])),
                                          rep(3,sum(community_data$com_q22e[which(community_data$neighbor==i)])),rep(4,sum(community_data$com_q22g[which(community_data$neighbor==i)])),
                                          rep(5,sum(community_data$com_q22i[which(community_data$neighbor==i)]))));
      freq[[14*(num.neighb[i]-1)+13]]=c(as.numeric(household_data$hh_q15[which(household_data$hh_q15!="n/a" & household_data$neighbor==i)]),
                                        c(rep(1,sum(school_data$sch_q17a[which(school_data$neighbor==i)])),rep(2,sum(school_data$sch_q17c[which(school_data$neighbor==i)])),
                                          rep(3,sum(school_data$sch_q17e[which(school_data$neighbor==i)])),rep(4,sum(school_data$sch_q17g[which(school_data$neighbor==i)])),
                                          rep(5,sum(school_data$sch_q17i[which(school_data$neighbor==i)]))),
                                        c(rep(1,sum(community_data$com_q10a[which(community_data$neighbor==i)])),rep(2,sum(community_data$com_q10c[which(community_data$neighbor==i)])),
                                          rep(3,sum(community_data$com_q10e[which(community_data$neighbor==i)])),rep(4,sum(community_data$com_q10g[which(community_data$neighbor==i)]))));
      freq[[14*(num.neighb[i]-1)+14]]=c(as.numeric(household_data$hh_q16[which(household_data$hh_q16!="n/a" & household_data$neighbor==i)]),
                                        c(rep(1,sum(school_data$sch_q16a[which(school_data$neighbor==i)])),rep(2,sum(school_data$sch_q16c[which(school_data$neighbor==i)])),
                                          rep(3,sum(school_data$sch_q16e[which(school_data$neighbor==i)])),rep(4,sum(school_data$sch_q16g[which(school_data$neighbor==i)]))),
                                        c(rep(1,sum(community_data$com_q25a[which(community_data$neighbor==i)])),rep(2,sum(community_data$com_q25c[which(community_data$neighbor==i)])),
                                          rep(3,sum(community_data$com_q25e[which(community_data$neighbor==i)])),rep(4,sum(community_data$com_q25g[which(community_data$neighbor==i)])),
                                          rep(5,sum(community_data$com_q25i[which(community_data$neighbor==i)]))));
    }
  }
  # frequencies for pie charts
  else if (type == 'ppl plot') {
    for (i in 1:length(num.neighb)){
      # sample type 1|2=drain, 3|4=produce, 5|6=piped water, 7|8=ocean water, 9|10=surface water, 11|12=flood water, 13|14=Public Latrine Surfaces  
      freq[[14*(num.neighb[i]-1)+1]]=4-c(as.numeric(household_data$hh_q6[which(household_data$hh_q6!="n/a" & household_data$neighbor==i)]),
                                         c(rep(1,sum(school_data$sch_q9a[which(school_data$neighbor==i)])),rep(2,sum(school_data$sch_q9c[which(school_data$neighbor==i)])),
                                           rep(3,sum(school_data$sch_q9e[which(school_data$neighbor==i)])),rep(4,sum(school_data$sch_q9g[which(school_data$neighbor==i)])),
                                           rep(5,sum(school_data$sch_q9i[which(school_data$neighbor==i)]))),
                                         c(rep(1,sum(community_data$com_q6a[which(community_data$neighbor==i)])),rep(2,sum(community_data$com_q6c[which(community_data$neighbor==i)])),
                                           rep(3,sum(community_data$com_q6e[which(community_data$neighbor==i)])),rep(4,sum(community_data$com_q6g[which(community_data$neighbor==i)]))));
      freq[[14*(num.neighb[i]-1)+2]]=4-c(as.numeric(household_data$hh_q7[which(household_data$hh_q7!="n/a" & household_data$neighbor==i)]),
                                         c(rep(1,sum(school_data$sch_q8a[which(school_data$neighbor==i)])),rep(2,sum(school_data$sch_q8c[which(school_data$neighbor==i)])),
                                           rep(3,sum(school_data$sch_q8e[which(school_data$neighbor==i)])),rep(4,sum(school_data$sch_q8g[which(school_data$neighbor==i)]))),
                                         c(rep(1,sum(community_data$com_q21a[which(community_data$neighbor==i)])),rep(2,sum(community_data$com_q21c[which(community_data$neighbor==i)])),
                                           rep(3,sum(community_data$com_q21e[which(community_data$neighbor==i)])),rep(4,sum(community_data$com_q21g[which(community_data$neighbor==i)])),
                                           rep(5,sum(community_data$com_q21i[which(community_data$neighbor==i)]))));                                       
      freq[[14*(num.neighb[i]-1)+3]]=4-c(as.numeric(household_data$hh_q13[which(household_data$hh_q13!="n/a" & household_data$neighbor==i)]),
                                         c(rep(1,sum(school_data$sch_q15a[which(school_data$neighbor==i)])),rep(2,sum(school_data$sch_q15c[which(school_data$neighbor==i)])),
                                           rep(3,sum(school_data$sch_q15e[which(school_data$neighbor==i)])),rep(4,sum(school_data$sch_q15g[which(school_data$neighbor==i)])),
                                           rep(5,sum(school_data$sch_q15i[which(school_data$neighbor==i)]))),
                                         c(rep(1,sum(community_data$com_q9a[which(community_data$neighbor==i)])),rep(2,sum(community_data$com_q9c[which(community_data$neighbor==i)])),
                                           rep(3,sum(community_data$com_q9e[which(community_data$neighbor==i)])),rep(4,sum(community_data$com_q9g[which(community_data$neighbor==i)]))));
      freq[[14*(num.neighb[i]-1)+4]]=4-c(as.numeric(household_data$hh_q14[which(household_data$hh_q14!="n/a" & household_data$neighbor==i)]),
                                         c(rep(1,sum(school_data$sch_q14a[which(school_data$neighbor==i)])),rep(2,sum(school_data$sch_q14c[which(school_data$neighbor==i)])),
                                           rep(3,sum(school_data$sch_q14e[which(school_data$neighbor==i)])),rep(4,sum(school_data$sch_q14g[which(school_data$neighbor==i)]))),
                                         c(rep(1,sum(community_data$com_q24a[which(community_data$neighbor==i)])),rep(2,sum(community_data$com_q24c[which(community_data$neighbor==i)])),
                                           rep(3,sum(community_data$com_q24e[which(community_data$neighbor==i)])),rep(4,sum(community_data$com_q24g[which(community_data$neighbor==i)])),
                                           rep(5,sum(community_data$com_q24i[which(community_data$neighbor==i)]))));
      freq[[14*(num.neighb[i]-1)+5]]=4-c(as.numeric(household_data$hh_q10[which(household_data$hh_q10!="n/a" & household_data$neighbor==i)]),
                                         c(rep(1,sum(school_data$sch_q13a[which(school_data$neighbor==i)])),rep(2,sum(school_data$sch_q13c[which(school_data$neighbor==i)])),
                                           rep(3,sum(school_data$sch_q13e[which(school_data$neighbor==i)])),rep(4,sum(school_data$sch_q13g[which(school_data$neighbor==i)])),
                                           rep(5,sum(school_data$sch_q13i[which(school_data$neighbor==i)]))),
                                         c(rep(1,sum(community_data$com_q8a[which(community_data$neighbor==i)])),rep(2,sum(community_data$com_q8c[which(community_data$neighbor==i)])),
                                           rep(3,sum(community_data$com_q8e[which(community_data$neighbor==i)])),rep(4,sum(community_data$com_q8g[which(community_data$neighbor==i)]))));
      freq[[14*(num.neighb[i]-1)+6]]=4-c(as.numeric(household_data$hh_q11[which(household_data$hh_q11!="n/a" & household_data$neighbor==i)]),
                                         c(rep(1,sum(school_data$sch_q12a[which(school_data$neighbor==i)])),rep(2,sum(school_data$sch_q12c[which(school_data$neighbor==i)])),
                                           rep(3,sum(school_data$sch_q12e[which(school_data$neighbor==i)])),rep(4,sum(school_data$sch_q12g[which(school_data$neighbor==i)]))),
                                         c(rep(1,sum(community_data$com_q23a[which(community_data$neighbor==i)])),rep(2,sum(community_data$com_q23c[which(community_data$neighbor==i)])),
                                           rep(3,sum(community_data$com_q23e[which(community_data$neighbor==i)])),rep(4,sum(community_data$com_q23g[which(community_data$neighbor==i)])),
                                           rep(5,sum(community_data$com_q23i[which(community_data$neighbor==i)]))));
      freq[[14*(num.neighb[i]-1)+7]]=4-c(as.numeric(household_data$hh_q2[which(household_data$hh_q2!="n/a" & household_data$neighbor==i)]),
                                         c(rep(1,sum(school_data$sch_q5a[which(school_data$neighbor==i)])),rep(2,sum(school_data$sch_q5c[which(school_data$neighbor==i)])),
                                           rep(3,sum(school_data$sch_q5e[which(school_data$neighbor==i)])),rep(4,sum(school_data$sch_q5g[which(school_data$neighbor==i)])),
                                           rep(5,sum(school_data$sch_q5i[which(school_data$neighbor==i)]))),
                                         c(rep(1,sum(community_data$com_q4a[which(community_data$neighbor==i)])),rep(2,sum(community_data$com_q4c[which(community_data$neighbor==i)])),
                                           rep(3,sum(community_data$com_q4e[which(community_data$neighbor==i)])),rep(4,sum(community_data$com_q4g[which(community_data$neighbor==i)]))));
      freq[[14*(num.neighb[i]-1)+8]]=4-c(as.numeric(household_data$hh_q3[which(household_data$hh_q3!="n/a" & household_data$neighbor==i)]),
                                         c(rep(1,sum(school_data$sch_q4a[which(school_data$neighbor==i)])),rep(2,sum(school_data$sch_q4c[which(school_data$neighbor==i)])),
                                           rep(3,sum(school_data$sch_q4e[which(school_data$neighbor==i)])),rep(4,sum(school_data$sch_q4g[which(school_data$neighbor==i)]))),
                                         c(rep(1,sum(community_data$com_q19a[which(community_data$neighbor==i)])),rep(2,sum(community_data$com_q19c[which(community_data$neighbor==i)])),
                                           rep(3,sum(community_data$com_q19e[which(community_data$neighbor==i)])),rep(4,sum(community_data$com_q19g[which(community_data$neighbor==i)])),
                                           rep(5,sum(community_data$com_q19i[which(community_data$neighbor==i)]))));
      freq[[14*(num.neighb[i]-1)+9]]=4-c(as.numeric(household_data$hh_q4[which(household_data$hh_q4!="n/a" & household_data$neighbor==i)]),
                                         c(rep(1,sum(school_data$sch_q7a[which(school_data$neighbor==i)])),rep(2,sum(school_data$sch_q7c[which(school_data$neighbor==i)])),
                                           rep(3,sum(school_data$sch_q7e[which(school_data$neighbor==i)])),rep(4,sum(school_data$sch_q7g[which(school_data$neighbor==i)])),
                                           rep(5,sum(school_data$sch_q7i[which(school_data$neighbor==i)]))),
                                         c(rep(1,sum(community_data$com_q5a[which(community_data$neighbor==i)])),rep(2,sum(community_data$com_q5c[which(community_data$neighbor==i)])),
                                           rep(3,sum(community_data$com_q5e[which(community_data$neighbor==i)])),rep(4,sum(community_data$com_q5g[which(community_data$neighbor==i)]))));
      freq[[14*(num.neighb[i]-1)+10]]=4-c(as.numeric(household_data$hh_q5[which(household_data$hh_q5!="n/a" & household_data$neighbor==i)]),
                                          c(rep(1,sum(school_data$sch_q6a[which(school_data$neighbor==i)])),rep(2,sum(school_data$sch_q6c[which(school_data$neighbor==i)])),
                                            rep(3,sum(school_data$sch_q6e[which(school_data$neighbor==i)])),rep(4,sum(school_data$sch_q6g[which(school_data$neighbor==i)]))),
                                          c(rep(1,sum(community_data$com_q20a[which(community_data$neighbor==i)])),rep(2,sum(community_data$com_q20c[which(community_data$neighbor==i)])),
                                            rep(3,sum(community_data$com_q20e[which(community_data$neighbor==i)])),rep(4,sum(community_data$com_q20g[which(community_data$neighbor==i)])),
                                            rep(5,sum(community_data$com_q20i[which(community_data$neighbor==i)]))));
      freq[[14*(num.neighb[i]-1)+11]]=4-c(as.numeric(household_data$hh_q8[which(household_data$hh_q8!="n/a" & household_data$neighbor==i)]),
                                          c(rep(1,sum(school_data$sch_q11a[which(school_data$neighbor==i)])),rep(2,sum(school_data$sch_q11c[which(school_data$neighbor==i)])),
                                            rep(3,sum(school_data$sch_q11e[which(school_data$neighbor==i)])),rep(4,sum(school_data$sch_q11g[which(school_data$neighbor==i)])),
                                            rep(5,sum(school_data$sch_q11i[which(school_data$neighbor==i)]))),
                                          c(rep(1,sum(community_data$com_q7a[which(community_data$neighbor==i)])),rep(2,sum(community_data$com_q7c[which(community_data$neighbor==i)])),
                                            rep(3,sum(community_data$com_q7e[which(community_data$neighbor==i)])),rep(4,sum(community_data$com_q7g[which(community_data$neighbor==i)]))));
      freq[[14*(num.neighb[i]-1)+12]]=4-c(as.numeric(household_data$hh_q9[which(household_data$hh_q9!="n/a" & household_data$neighbor==i)]),
                                          c(rep(1,sum(school_data$sch_q10a[which(school_data$neighbor==i)])),rep(2,sum(school_data$sch_q10c[which(school_data$neighbor==i)])),
                                            rep(3,sum(school_data$sch_q10e[which(school_data$neighbor==i)])),rep(4,sum(school_data$sch_q10g[which(school_data$neighbor==i)]))),
                                          c(rep(1,sum(community_data$com_q22a[which(community_data$neighbor==i)])),rep(2,sum(community_data$com_q22c[which(community_data$neighbor==i)])),
                                            rep(3,sum(community_data$com_q22e[which(community_data$neighbor==i)])),rep(4,sum(community_data$com_q22g[which(community_data$neighbor==i)])),
                                            rep(5,sum(community_data$com_q22i[which(community_data$neighbor==i)]))));
      freq[[14*(num.neighb[i]-1)+13]]=4-c(as.numeric(household_data$hh_q15[which(household_data$hh_q15!="n/a" & household_data$neighbor==i)]),
                                          c(rep(1,sum(school_data$sch_q17a[which(school_data$neighbor==i)])),rep(2,sum(school_data$sch_q17c[which(school_data$neighbor==i)])),
                                            rep(3,sum(school_data$sch_q17e[which(school_data$neighbor==i)])),rep(4,sum(school_data$sch_q17g[which(school_data$neighbor==i)])),
                                            rep(5,sum(school_data$sch_q17i[which(school_data$neighbor==i)]))),
                                          c(rep(1,sum(community_data$com_q10a[which(community_data$neighbor==i)])),rep(2,sum(community_data$com_q10c[which(community_data$neighbor==i)])),
                                            rep(3,sum(community_data$com_q10e[which(community_data$neighbor==i)])),rep(4,sum(community_data$com_q10g[which(community_data$neighbor==i)]))));
      freq[[14*(num.neighb[i]-1)+14]]=4-c(as.numeric(household_data$hh_q16[which(household_data$hh_q16!="n/a" & household_data$neighbor==i)]),
                                          c(rep(1,sum(school_data$sch_q16a[which(school_data$neighbor==i)])),rep(2,sum(school_data$sch_q16c[which(school_data$neighbor==i)])),
                                            rep(3,sum(school_data$sch_q16e[which(school_data$neighbor==i)])),rep(4,sum(school_data$sch_q16g[which(school_data$neighbor==i)]))),
                                          c(rep(1,sum(community_data$com_q25a[which(community_data$neighbor==i)])),rep(2,sum(community_data$com_q25c[which(community_data$neighbor==i)])),
                                            rep(3,sum(community_data$com_q25e[which(community_data$neighbor==i)])),rep(4,sum(community_data$com_q25g[which(community_data$neighbor==i)])),
                                            rep(5,sum(community_data$com_q25i[which(community_data$neighbor==i)]))));
    }
  }
  else {
    freq <- NULL
  }
  
  return(freq)
  
}