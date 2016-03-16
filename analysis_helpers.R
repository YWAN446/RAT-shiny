## Analysis helpers for the SaniPath Analysis tool
## these have been derived from the original server.R 
## file.


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

# PLOTTING ====================================================================
## PIE CHARTS 
make_pieChart <- function(freq, num.neighb, surtype) {
  if (surtype==0){
    be_data1<-household_data()
    be_data2<-school_data()
    be_data3<-community_data()
    freq<-frq0()
    num.neighb<-sort(unique(c(as.numeric(be_data1$neighbor),as.numeric(be_data2$neighbor),as.numeric(be_data3$neighbor))))
  }
  if (input$surtype==1){
    be_data1<-household_data()
    freq<-frq1()
    num.neighb<-sort(unique(as.numeric(be_data1$neighbor)))
  }
  if (input$surtype==2){
    be_data2<-school_data()
    freq<-frq2()
    num.neighb<-sort(unique(as.numeric(be_data2$neighbor)))
  }
  if (input$surtype==3){
    be_data3<-community_data()
    freq<-frq3()
    num.neighb<-sort(unique(as.numeric(be_data3$neighbor)))
  }
  n.neighb=ifelse(input$neighb==0, length(num.neighb),1)
  n.path=1
  n.age=ifelse(input$ad_ch==0, 2, 1)
  if (n.neighb==1 & input$neighb!=0) {k.neighb=as.numeric(input$neighb)}
  else {k.neighb=sort(num.neighb)}
  if (input$samtype!=0){k.path=as.numeric(input$samtype)} 
  else{k.path=1}
  if (n.age==1 & input$ad_ch!=0) {k.age=as.numeric(input$ad_ch)}
  else {k.age=c(1,2)}
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