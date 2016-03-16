## Analysis helpers for the SaniPath Analysis tool
## these have been derived from the original server.R 
## file.

calculate_householdFreq <- function(household_data) {
  # calculate the appropriate factors for plotting pie charts
  # and people plots.  This is specific to the household 
  # form. returns a list with two objects
  #
  # Ex.
  # > calculate_householdFreq(household_data)
  # $pie_chart
  # $pie_chart[[1]]
  # [1] 3 4 2 1
  # ...
  # $ppl_plot
  # $ppl_plot[[1]]
  # [1] 1 1 0 1
  # ...
  
  # frequencies for pie charts
  pie_chart<-list()
  
  for (i in 1:length(unique(household_data$neighbor))){
    # sample type 1|2=drain, 3|4=produce, 5|6=piped water, 7|8=ocean water, 9|10=surface water, 11|12=flood water, 13|14=Public Latrine Surfaces  
    pie_chart[[14*(sort(unique(household_data$neighbor))[i]-1)+1]]=as.numeric(household_data$hh_q6[which(household_data$hh_q6!="n/a" & household_data$neighbor==i)]);
    pie_chart[[14*(sort(unique(household_data$neighbor))[i]-1)+2]]=as.numeric(household_data$hh_q7[which(household_data$hh_q7!="n/a" & household_data$neighbor==i)]);
    pie_chart[[14*(sort(unique(household_data$neighbor))[i]-1)+3]]=as.numeric(household_data$hh_q13[which(household_data$hh_q13!="n/a" & household_data$neighbor==i)]);
    pie_chart[[14*(sort(unique(household_data$neighbor))[i]-1)+4]]=as.numeric(household_data$hh_q14[which(household_data$hh_q14!="n/a" & household_data$neighbor==i)]);
    pie_chart[[14*(sort(unique(household_data$neighbor))[i]-1)+5]]=as.numeric(household_data$hh_q10[which(household_data$hh_q10!="n/a" & household_data$neighbor==i)]);
    pie_chart[[14*(sort(unique(household_data$neighbor))[i]-1)+6]]=as.numeric(household_data$hh_q11[which(household_data$hh_q11!="n/a" & household_data$neighbor==i)]);
    pie_chart[[14*(sort(unique(household_data$neighbor))[i]-1)+7]]=as.numeric(household_data$hh_q2[which(household_data$hh_q2!="n/a" & household_data$neighbor==i)]);
    pie_chart[[14*(sort(unique(household_data$neighbor))[i]-1)+8]]=as.numeric(household_data$hh_q3[which(household_data$hh_q3!="n/a" & household_data$neighbor==i)]);
    pie_chart[[14*(sort(unique(household_data$neighbor))[i]-1)+9]]=as.numeric(household_data$hh_q4[which(household_data$hh_q4!="n/a" & household_data$neighbor==i)]);
    pie_chart[[14*(sort(unique(household_data$neighbor))[i]-1)+10]]=as.numeric(household_data$hh_q5[which(household_data$hh_q5!="n/a" & household_data$neighbor==i)]);
    pie_chart[[14*(sort(unique(household_data$neighbor))[i]-1)+11]]=as.numeric(household_data$hh_q8[which(household_data$hh_q8!="n/a" & household_data$neighbor==i)]);
    pie_chart[[14*(sort(unique(household_data$neighbor))[i]-1)+12]]=as.numeric(household_data$hh_q9[which(household_data$hh_q9!="n/a" & household_data$neighbor==i)]);
    pie_chart[[14*(sort(unique(household_data$neighbor))[i]-1)+13]]=as.numeric(household_data$hh_q15[which(household_data$hh_q15!="n/a" & household_data$neighbor==i)]);
    pie_chart[[14*(sort(unique(household_data$neighbor))[i]-1)+14]]=as.numeric(household_data$hh_q16[which(household_data$hh_q16!="n/a" & household_data$neighbor==i)]);
  }

  ppl_plot<-list()
  
  for (i in 1:length(unique(household_data$neighbor))){
    # sample type 1|2=drain, 3|4=produce, 5|6=piped water, 7|8=ocean water, 9|10=surface water, 11|12=flood water, 13|14=Public Latrine Surfaces  
    ppl_plot[[14*(sort(unique(household_data$neighbor))[i]-1)+1]]=4-as.numeric(household_data$hh_q6[which(household_data$hh_q6!="n/a" & household_data$neighbor==i)]);
    ppl_plot[[14*(sort(unique(household_data$neighbor))[i]-1)+2]]=4-as.numeric(household_data$hh_q7[which(household_data$hh_q7!="n/a" & household_data$neighbor==i)]);
    ppl_plot[[14*(sort(unique(household_data$neighbor))[i]-1)+3]]=4-as.numeric(household_data$hh_q13[which(household_data$hh_q13!="n/a" & household_data$neighbor==i)]);
    ppl_plot[[14*(sort(unique(household_data$neighbor))[i]-1)+4]]=4-as.numeric(household_data$hh_q14[which(household_data$hh_q14!="n/a" & household_data$neighbor==i)]);
    ppl_plot[[14*(sort(unique(household_data$neighbor))[i]-1)+5]]=4-as.numeric(household_data$hh_q10[which(household_data$hh_q10!="n/a" & household_data$neighbor==i)]);
    ppl_plot[[14*(sort(unique(household_data$neighbor))[i]-1)+6]]=4-as.numeric(household_data$hh_q11[which(household_data$hh_q11!="n/a" & household_data$neighbor==i)]);
    ppl_plot[[14*(sort(unique(household_data$neighbor))[i]-1)+7]]=4-as.numeric(household_data$hh_q2[which(household_data$hh_q2!="n/a" & household_data$neighbor==i)]);
    ppl_plot[[14*(sort(unique(household_data$neighbor))[i]-1)+8]]=4-as.numeric(household_data$hh_q3[which(household_data$hh_q3!="n/a" & household_data$neighbor==i)]);
    ppl_plot[[14*(sort(unique(household_data$neighbor))[i]-1)+9]]=4-as.numeric(household_data$hh_q4[which(household_data$hh_q4!="n/a" & household_data$neighbor==i)]);
    ppl_plot[[14*(sort(unique(household_data$neighbor))[i]-1)+10]]=4-as.numeric(household_data$hh_q5[which(household_data$hh_q5!="n/a" & household_data$neighbor==i)]);
    ppl_plot[[14*(sort(unique(household_data$neighbor))[i]-1)+11]]=4-as.numeric(household_data$hh_q8[which(household_data$hh_q8!="n/a" & household_data$neighbor==i)]);
    ppl_plot[[14*(sort(unique(household_data$neighbor))[i]-1)+12]]=4-as.numeric(household_data$hh_q9[which(household_data$hh_q9!="n/a" & household_data$neighbor==i)]);
    ppl_plot[[14*(sort(unique(household_data$neighbor))[i]-1)+13]]=4-as.numeric(household_data$hh_q15[which(household_data$hh_q15!="n/a" & household_data$neighbor==i)]);
    ppl_plot[[14*(sort(unique(household_data$neighbor))[i]-1)+14]]=4-as.numeric(household_data$hh_q16[which(household_data$hh_q16!="n/a" & household_data$neighbor==i)]);
  }
  
  return(list('pie_chart' = pie_chart, 'ppl_plot' = ppl_plot))
  
}