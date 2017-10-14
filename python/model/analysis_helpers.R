## Analysis helpers for the SaniPath Analysis tool
## these have been derived from the original server.R
## file.
# library(ggplot2)
library(rlist)
library(plyr)
library(reshape2)
library(magrittr)
# library(rjags)

create_concData <- function(collection_data, lab_data) {
  ec_data <- create_ecData(collection_data, lab_data)
  # Calculate concentration amounts?
  conc_names <- c("Drain Water", "Produce", "Municipal and Piped Water",'Ocean Water', 'Surface Water', "Flood Water",
                  "Public Latrine", "Particulate", "Bathing")
  conc<-list()
  for (i in 1:length(unique(factor_to_numeric(ec_data$neighbor)))){
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


# master create_ecData
create_ecData <- function(collection_data, lab_data) {
  #logic to decide whether the function recieves IDEXX data or MF data;
  #This is assuming all the samples will be tested in one of the method: either IDEXX or MF.
  #This field will be filled based on configuration of the project.
  if (lab_data$lab_analysis[1]==1){
    return(create_ecData_Idexx(collection_data, lab_data))
  } else if (lab_data$lab_analysis[1]==2){
    return(create_ecData_MF(collection_data, lab_data))
  }
}

#load the IDEXX table;
# load("./data/mpn_tbl.rda")

# IDEXX method to calculate the concentration-----------------------------
create_ecData_Idexx <- function(collection_data, lab_data, mpn_loc, default_denom = 100, alternate_denoms = list('p' = 500, 'l' = 14)){
  #function to prepare the ec_data for IDEXX;
  for (i in 1:length(lab_data$lab_id)){
    lab_data$lab_1_ecoli[i]<-ifelse(lab_data$lab_1_ecoli_reading_idexx==2,mpn_tbl[lab_data$lab_1_ecoli_big_idexx[i]+1,lab_data$lab_1_ecoli_small_idexx[i]+1],9999)
    lab_data$lab_2_ecoli[i]<-ifelse(lab_data$lab_2_ecoli_reading_idexx==2,mpn_tbl[lab_data$lab_2_ecoli_big_idexx[i]+1,lab_data$lab_2_ecoli_small_idexx[i]+1],9999)
    lab_data$lab_3_ecoli[i]<-ifelse(lab_data$lab_3_ecoli_reading_idexx==2,mpn_tbl[lab_data$lab_3_ecoli_big_idexx[i]+1,lab_data$lab_3_ecoli_small_idexx[i]+1],9999)
  }

  #Capitalized and remove missings in ID and sample types for merge purpose;
  collection_data[,c("col_sample_type","col_id")] <- apply(collection_data[,c("col_sample_type","col_id")], 2, function(x) toupper(x))
  lab_data[,c("lab_sample_type","lab_id")] <- apply(lab_data[,c("lab_sample_type","lab_id")], 2, function(x) toupper(x))
  collection_data$col_id<-gsub(" ","",collection_data$col_id)
  lab_data$lab_id<-gsub(" ","",lab_data$lab_id)

  ec_data<-merge(collection_data,lab_data,by.x=c("col_sample_type","col_id"),by.y=c("lab_sample_type","lab_id"))
  names(ec_data)[which(names(ec_data)=="col_sample_type")]<-"sample_type"
  names(ec_data)[which(names(ec_data)=="col_id")]<-"sampleid"
  #back calculation factor
  ec_data$ec_denom=100
  ec_data$ec_denom[which(ec_data$sample_type==2)]=500
  ec_data$ec_denom[which(ec_data$sample_type==7)]=14
  ec_data$ec_denom[which(ec_data$sample_type==8)]=2
  #street food used WASH benefit protocol 10 grams into 100 mL, serving size was defined using weight of street food sampled.
  ec_data$ec_denom[which(ec_data$sample_type==10)]=10*ec_data$lab_sf_weight[which(ec_data$sample_type==10)]
  ec_data$ec_denom[is.na(ec_data$sample_type)]=NA

  #ec_data$neighbor<-factor_to_numeric(ec_data$neighbor)
  ec_data$sample_type<-as.numeric(ec_data$sample_type)
  ec_data$ec_dil1<-factor_to_numeric(ec_data$lab_1_dil_tested)
  ec_data$ec_dil2<-factor_to_numeric(ec_data$lab_2_dil_tested)
  ec_data$ec_dil3<-factor_to_numeric(ec_data$lab_3_dil_tested)
  ec_data$lab_1_volume<-factor_to_numeric(ec_data$lab_1_volume)
  ec_data$lab_2_volume<-factor_to_numeric(ec_data$lab_2_volume)
  ec_data$lab_3_volume<-factor_to_numeric(ec_data$lab_3_volume)
  ec_data$ec_dil1<-(10^ec_data$ec_dil1)/(10^7)*ec_data$lab_1_volume
  ec_data$ec_dil2<-(10^ec_data$ec_dil2)/(10^7)*ec_data$lab_2_volume
  ec_data$ec_dil3<-(10^ec_data$ec_dil3)/(10^7)*ec_data$lab_3_volume
  ec_data$ec_ecnt1<-factor_to_numeric(ec_data$lab_1_ecoli)
  ec_data$ec_ecnt2<-factor_to_numeric(ec_data$lab_2_ecoli)
  ec_data$ec_ecnt3<-factor_to_numeric(ec_data$lab_3_ecoli)

  swap1<-(ec_data$ec_dil1>=ec_data$ec_dil2 & is.na(ec_data$ec_dil3))
  swap2<-(ec_data$ec_dil1<ec_data$ec_dil2 & is.na(ec_data$ec_dil3))
  swap3<-(ec_data$ec_dil1>=ec_data$ec_dil2 & !is.na(ec_data$ec_dil3))
  swap4<-(ec_data$ec_dil1<ec_data$ec_dil2 & !is.na(ec_data$ec_dil3))
  swap5<-(ec_data$ec_dil2>=ec_data$ec_dil3 & !is.na(ec_data$ec_dil3))
  swap6<-(ec_data$ec_dil2<ec_data$ec_dil3 & !is.na(ec_data$ec_dil3))
  swap7<-(ec_data$ec_dil3>=ec_data$ec_dil1 & !is.na(ec_data$ec_dil3))
  swap8<-(ec_data$ec_dil3<ec_data$ec_dil1 & !is.na(ec_data$ec_dil3))

  dilution3<-!is.na(ec_data$ec_dil3)
  no_dilution3<-is.na(ec_data$ec_dil3)

  ec_data$count1[swap1]<-ec_data$ec_ecnt1[swap1]
  ec_data$count2[swap1]<-ec_data$ec_ecnt2[swap1]
  ec_data$dil1[swap1]<-ec_data$ec_dil1[swap1]
  ec_data$dil2[swap1]<-ec_data$ec_dil2[swap1]

  ec_data$count2[swap2]<-ec_data$ec_ecnt1[swap2]
  ec_data$count1[swap2]<-ec_data$ec_ecnt2[swap2]
  ec_data$dil2[swap2]<-ec_data$ec_dil1[swap2]
  ec_data$dil1[swap2]<-ec_data$ec_dil2[swap2]

  ec_data$count1[swap3 & swap5 & swap8]<-ec_data$ec_ecnt1[swap3 & swap5 & swap8]
  ec_data$count2[swap3 & swap5 & swap8]<-ec_data$ec_ecnt2[swap3 & swap5 & swap8]
  ec_data$count3[swap3 & swap5 & swap8]<-ec_data$ec_ecnt3[swap3 & swap5 & swap8]
  ec_data$dil1[swap3 & swap5 & swap8]<-ec_data$ec_dil1[swap3 & swap5 & swap8]
  ec_data$dil2[swap3 & swap5 & swap8]<-ec_data$ec_dil2[swap3 & swap5 & swap8]
  ec_data$dil3[swap3 & swap5 & swap8]<-ec_data$ec_dil3[swap3 & swap5 & swap8]

  ec_data$count1[swap3 & swap6 & swap7]<-ec_data$ec_ecnt3[swap3 & swap6 & swap7]
  ec_data$count2[swap3 & swap6 & swap7]<-ec_data$ec_ecnt1[swap3 & swap6 & swap7]
  ec_data$count3[swap3 & swap6 & swap7]<-ec_data$ec_ecnt2[swap3 & swap6 & swap7]
  ec_data$dil1[swap3 & swap6 & swap7]<-ec_data$ec_dil3[swap3 & swap6 & swap7]
  ec_data$dil2[swap3 & swap6 & swap7]<-ec_data$ec_dil1[swap3 & swap6 & swap7]
  ec_data$dil3[swap3 & swap6 & swap7]<-ec_data$ec_dil2[swap3 & swap6 & swap7]

  ec_data$count1[swap4 & swap5 & swap7]<-ec_data$ec_ecnt2[swap4 & swap5 & swap7]
  ec_data$count2[swap4 & swap5 & swap7]<-ec_data$ec_ecnt3[swap4 & swap5 & swap7]
  ec_data$count3[swap4 & swap5 & swap7]<-ec_data$ec_ecnt1[swap4 & swap5 & swap7]
  ec_data$dil1[swap4 & swap5 & swap7]<-ec_data$ec_dil2[swap4 & swap5 & swap7]
  ec_data$dil2[swap4 & swap5 & swap7]<-ec_data$ec_dil3[swap4 & swap5 & swap7]
  ec_data$dil3[swap4 & swap5 & swap7]<-ec_data$ec_dil1[swap4 & swap5 & swap7]

  ec_data$count1[swap3 & swap6 & swap8]<-ec_data$ec_ecnt1[swap3 & swap6 & swap8]
  ec_data$count2[swap3 & swap6 & swap8]<-ec_data$ec_ecnt3[swap3 & swap6 & swap8]
  ec_data$count3[swap3 & swap6 & swap8]<-ec_data$ec_ecnt2[swap3 & swap6 & swap8]
  ec_data$dil1[swap3 & swap6 & swap8]<-ec_data$ec_dil1[swap3 & swap6 & swap8]
  ec_data$dil2[swap3 & swap6 & swap8]<-ec_data$ec_dil3[swap3 & swap6 & swap8]
  ec_data$dil3[swap3 & swap6 & swap8]<-ec_data$ec_dil2[swap3 & swap6 & swap8]

  ec_data$count1[swap4 & swap5 & swap8]<-ec_data$ec_ecnt2[swap4 & swap5 & swap8]
  ec_data$count2[swap4 & swap5 & swap8]<-ec_data$ec_ecnt1[swap4 & swap5 & swap8]
  ec_data$count3[swap4 & swap5 & swap8]<-ec_data$ec_ecnt3[swap4 & swap5 & swap8]
  ec_data$dil1[swap4 & swap5 & swap8]<-ec_data$ec_dil2[swap4 & swap5 & swap8]
  ec_data$dil2[swap4 & swap5 & swap8]<-ec_data$ec_dil1[swap4 & swap5 & swap8]
  ec_data$dil3[swap4 & swap5 & swap8]<-ec_data$ec_dil3[swap4 & swap5 & swap8]

  ec_data$count1[swap4 & swap6 & swap7]<-ec_data$ec_ecnt3[swap4 & swap6 & swap7]
  ec_data$count2[swap4 & swap6 & swap7]<-ec_data$ec_ecnt2[swap4 & swap6 & swap7]
  ec_data$count3[swap4 & swap6 & swap7]<-ec_data$ec_ecnt1[swap4 & swap6 & swap7]
  ec_data$dil1[swap4 & swap6 & swap7]<-ec_data$ec_dil3[swap4 & swap6 & swap7]
  ec_data$dil2[swap4 & swap6 & swap7]<-ec_data$ec_dil2[swap4 & swap6 & swap7]
  ec_data$dil3[swap4 & swap6 & swap7]<-ec_data$ec_dil1[swap4 & swap6 & swap7]

  #check whether threre is a dilution jumping.
  dil_jump1_1<-abs(ec_data$dil1/ec_data$dil2-10)<0.0001
  dil_jump1_2<-abs(ec_data$dil1/ec_data$dil2-100)<0.0001
  dil_jump2_1<-abs(ec_data$dil2/ec_data$dil3-10)<0.0001
  dil_jump2_2<-abs(ec_data$dil2/ec_data$dil3-100)<0.0001

  #two dilution cases (1:10 dilution jump and 1:100 dilution jump)
  condition1=which(ec_data$count1==9999 & ec_data$count2==9999 & no_dilution3)
  condition2=which(ec_data$count1==9999 & ec_data$count2>=200 & ec_data$count2<=2419.6 & no_dilution3)
  condition3=which(ec_data$count1==9999 & ec_data$count2>=1 & ec_data$count2<200 & no_dilution3)
  condition4=which(ec_data$count1>=200 & ec_data$count1<=2419.6 & ec_data$count2>=200 & ec_data$count2<2419.6 & no_dilution3 & dil_jump1_1)
  condition5=which(ec_data$count1>=200 & ec_data$count1<=2419.6 & ec_data$count2>=1 & ec_data$count2<200 & no_dilution3)
  condition6=which(ec_data$count1>=200 & ec_data$count1<=2419.6 & ec_data$count2<1 & no_dilution3)
  condition7=which(ec_data$count1>=1 & ec_data$count1<200 & ec_data$count2>=1 & ec_data$count2<200 & no_dilution3 & dil_jump1_1)
  condition8=which(ec_data$count1>=1 & ec_data$count1<200 & ec_data$count2>=1 & ec_data$count2<200 & no_dilution3 & dil_jump1_2)
  condition9=which(ec_data$count1>=1 & ec_data$count1<200 & ec_data$count2<1 & no_dilution3)
  condition10=which(ec_data$count1<1 & ec_data$count2<1 & no_dilution3)

  #three dilution cases (1:10 dilution jump and 1:100 dilution jump)
  condition11=which(ec_data$count1==9999 & ec_data$count2==9999 & ec_data$count3==9999 & dilution3)
  condition12=which(ec_data$count1==9999 & ec_data$count2==9999 & ec_data$count3>=200 & ec_data$count3<=2419.6 & dilution3)
  condition13=which(ec_data$count1==9999 & ec_data$count2==9999 & ec_data$count3>=1 & ec_data$count3<200 & dilution3)
  condition15=which(ec_data$count1==9999 & ec_data$count2>=200 & ec_data$count2<=2419.6 & ec_data$count3>=200 & ec_data$count3<=2419.6 & dilution3 & dil_jump2_1)
  condition16=which(ec_data$count1==9999 & ec_data$count2>=200 & ec_data$count2<=2419.6 & ec_data$count3>=1 & ec_data$count3<200 & dilution3)
  condition17=which(ec_data$count1==9999 & ec_data$count2>=200 & ec_data$count2<=2419.6 & ec_data$count3<1 & dilution3)
  condition18=which(ec_data$count1==9999 & ec_data$count2>=1 & ec_data$count2<200 & ec_data$count3>=1 & ec_data$count3<200 & dilution3 & dil_jump2_1)
  condition19=which(ec_data$count1==9999 & ec_data$count2>=1 & ec_data$count2<200 & ec_data$count3>=1 & ec_data$count3<200 & dilution3 & dil_jump2_2)
  condition20=which(ec_data$count1==9999 & ec_data$count2>=1 & ec_data$count2<200 & ec_data$count3<1 & dilution3)

  condition23=which(ec_data$count1>=200 & ec_data$count1<=2419.6 & ec_data$count2>=200 & ec_data$count2<=2419.6 & ec_data$count3>=1 & ec_data$count3<200 & dilution3 & dil_jump1_1 & dil_jump2_1)
  condition25=which(ec_data$count1>=200 & ec_data$count1<=2419.6 & ec_data$count2>=1 & ec_data$count2<200 & ec_data$count3>=1 & ec_data$count3<200 & dilution3 & dil_jump1_1 & dil_jump2_1)
  condition26=which(ec_data$count1>=200 & ec_data$count1<=2419.6 & ec_data$count2>=1 & ec_data$count2<200 & ec_data$count3>=1 & ec_data$count3<200 & dilution3 & dil_jump1_2 & dil_jump2_2)
  condition27=which(ec_data$count1>=200 & ec_data$count1<=2419.6 & ec_data$count2>=1 & ec_data$count2<200 & ec_data$count3<1 & dilution3)
  condition29=which(ec_data$count1>=1 & ec_data$count1<200 & ec_data$count2>=1 & ec_data$count2<200 & ec_data$count3<1 & dilution3 & dil_jump1_1)
  condition30=which(ec_data$count1>=1 & ec_data$count1<200 & ec_data$count2>=1 & ec_data$count2<200 & ec_data$count3<1 & dilution3 & dil_jump1_2)
  condition31=which(ec_data$count1>=1 & ec_data$count1<200 & ec_data$count2<1 & ec_data$count3<1 & dilution3)
  condition32=which(ec_data$count1<1 & ec_data$count2<1 & ec_data$count3<1 & dilution3)

  ec_con<-rep(NA,length(ec_data$ec_ecnt1))
  ec_con[condition1]=2419.6/ec_data$dil2[condition1]*ec_data$ec_denom[condition1]
  ec_con[condition2]=ec_data$count2[condition2]/ec_data$dil2[condition2]*ec_data$ec_denom[condition2]
  ec_con[condition3]=ec_data$count2[condition3]/ec_data$dil2[condition3]*ec_data$ec_denom[condition3]
  ec_con[condition4]=(ec_data$count1[condition4]/ec_data$dil1[condition4]+ec_data$count2[condition4]/ec_data$dil2[condition4])/2*ec_data$ec_denom[condition4]
  ec_con[condition5]=(ec_data$count1[condition5]/ec_data$dil1[condition5]+ec_data$count2[condition5]/ec_data$dil2[condition5])/2*ec_data$ec_denom[condition5]
  ec_con[condition6]=ec_data$count1[condition6]/ec_data$dil1[condition6]*ec_data$ec_denom[condition6]
  ec_con[condition7]=(ec_data$count1[condition7]/ec_data$dil1[condition7]+ec_data$count2[condition7]/ec_data$dil2[condition7])/2*ec_data$ec_denom[condition7]
  ec_con[condition8]=ec_data$count1[condition8]/ec_data$dil1[condition8]*ec_data$ec_denom[condition8]
  ec_con[condition9]=ec_data$count1[condition9]/ec_data$dil1[condition9]*ec_data$ec_denom[condition9]
  ec_con[condition10]=0.5/ec_data$dil1[condition10]*ec_data$ec_denom[condition10]
  ec_con[condition11]=2419.6/ec_data$dil3[condition11]*ec_data$ec_denom[condition11]
  ec_con[condition12]=ec_data$count3[condition12]/ec_data$dil3[condition12]*ec_data$ec_denom[condition12]
  ec_con[condition13]=ec_data$count3[condition13]/ec_data$dil3[condition13]*ec_data$ec_denom[condition13]
  ec_con[condition15]=(ec_data$count2[condition15]/ec_data$dil2[condition15]+ec_data$count3[condition15]/ec_data$dil3[condition15])/2*ec_data$ec_denom[condition15]
  ec_con[condition16]=(ec_data$count2[condition16]/ec_data$dil2[condition16]+ec_data$count3[condition16]/ec_data$dil3[condition16])/2*ec_data$ec_denom[condition16]
  ec_con[condition17]=ec_data$count2[condition17]/ec_data$dil2[condition17]*ec_data$ec_denom[condition17]
  ec_con[condition18]=(ec_data$count2[condition18]/ec_data$dil2[condition18]+ec_data$count3[condition18]/ec_data$dil3[condition18])/2*ec_data$ec_denom[condition18]
  ec_con[condition19]=ec_data$count2[condition19]/ec_data$dil2[condition19]*ec_data$ec_denom[condition19]
  ec_con[condition20]=ec_data$count2[condition20]/ec_data$dil2[condition20]*ec_data$ec_denom[condition20]
  ec_con[condition23]=(ec_data$count1[condition23]/ec_data$dil1[condition23]+ec_data$count2[condition23]/ec_data$dil2[condition23]+ec_data$count3[condition23]/ec_data$dil3[condition23])/3*ec_data$ec_denom[condition23]
  ec_con[condition25]=(ec_data$count1[condition25]/ec_data$dil1[condition25]+ec_data$count2[condition25]/ec_data$dil2[condition25]+ec_data$count3[condition25]/ec_data$dil3[condition25])/3*ec_data$ec_denom[condition25]
  ec_con[condition26]=(ec_data$count1[condition26]/ec_data$dil1[condition26]+ec_data$count2[condition26]/ec_data$dil2[condition26])/2*ec_data$ec_denom[condition26]
  ec_con[condition27]=(ec_data$count1[condition27]/ec_data$dil1[condition27]+ec_data$count2[condition27]/ec_data$dil2[condition27])/2*ec_data$ec_denom[condition27]
  ec_con[condition29]=(ec_data$count1[condition29]/ec_data$dil1[condition29]+ec_data$count2[condition29]/ec_data$dil2[condition29])/2*ec_data$ec_denom[condition29]
  ec_con[condition30]=ec_data$count1[condition30]/ec_data$dil1[condition30]*ec_data$ec_denom[condition30]
  ec_con[condition31]=ec_data$count1[condition31]/ec_data$dil1[condition31]*ec_data$ec_denom[condition31]
  ec_con[condition32]=0.5/ec_data$dil1[condition32]*ec_data$ec_denom[condition32]
  ec_data$ec_conc<-ec_con
  ec_data$neighbor <- as.factor(ec_data$col_neighborhood)

  return(ec_data)
}

# MERGING and Membrane Filtration to calculate the concentration-----------------------------
create_ecData_MF <- function(collection_data, lab_data) {
  # merge and calculate e. coli data?
  collection_data[,c("col_sample_type","col_id")] <- apply(collection_data[,c("col_sample_type","col_id")], 2, function(x) toupper(x))
  lab_data[,c("lab_sample_type","lab_id")] <- apply(lab_data[,c("lab_sample_type","lab_id")], 2, function(x) toupper(x))
  collection_data$col_id<-gsub(" ","",collection_data$col_id)
  lab_data$lab_id<-gsub(" ","",lab_data$lab_id)
  #lab_data$lab_1_ecoli_reading

  ec_data<-merge(collection_data,lab_data,by.x=c("col_sample_type","col_id"),by.y=c("lab_sample_type","lab_id"))
  names(ec_data)[which(names(ec_data)=="col_sample_type")]<-"sample_type"
  names(ec_data)[which(names(ec_data)=="col_id")]<-"sampleid"
  ec_data$ec_denom=100
  ec_data$ec_denom[which(ec_data$sample_type==2)]=500
  ec_data$ec_denom[which(ec_data$sample_type==7)]=14
  ec_data$ec_denom[which(ec_data$sample_type==8)]=2
  #street food used WASH benefit protocol 10 grams into 100 mL, serving size was defined using weight of street food sampled.
  ec_data$ec_denom[which(ec_data$sample_type==10)]=10*ec_data$lab_sf_weight[which(ec_data$sample_type==10)]
  ec_data$ec_denom[is.na(ec_data$sample_type)]=NA

  #ec_data$neighbor<-factor_to_numeric(ec_data$neighbor)
  ec_data$sample_type<-as.numeric(ec_data$sample_type)
  ec_data$ec_dil1<-factor_to_numeric(ec_data$lab_1_dil_tested)
  ec_data$ec_dil2<-factor_to_numeric(ec_data$lab_2_dil_tested)
  ec_data$ec_dil3<-factor_to_numeric(ec_data$lab_3_dil_tested)
  ec_data$lab_1_volume<-factor_to_numeric(ec_data$lab_1_volume)
  ec_data$lab_2_volume<-factor_to_numeric(ec_data$lab_2_volume)
  ec_data$lab_3_volume<-factor_to_numeric(ec_data$lab_3_volume)
  ec_data$ec_dil1<-(10^ec_data$ec_dil1)/(10^7)*ec_data$lab_1_volume
  ec_data$ec_dil2<-(10^ec_data$ec_dil2)/(10^7)*ec_data$lab_2_volume
  ec_data$ec_dil3<-(10^ec_data$ec_dil3)/(10^7)*ec_data$lab_3_volume
  ec_data$ec_ecnt1<-factor_to_numeric(ec_data$lab_1_ecoli_membrane)
  ec_data$ec_ecnt2<-factor_to_numeric(ec_data$lab_2_ecoli_membrane)
  ec_data$ec_ecnt3<-factor_to_numeric(ec_data$lab_3_ecoli_membrane)

  ec_data$ec_ecnt1[which(ec_data$lab_1_ecoli_reading_membrane==1)]<-999
  ec_data$ec_ecnt1[which(ec_data$lab_1_ecoli_reading_membrane==2)]<-998
  ec_data$ec_ecnt2[which(ec_data$lab_2_ecoli_reading_membrane==1)]<-999
  ec_data$ec_ecnt2[which(ec_data$lab_2_ecoli_reading_membrane==2)]<-998
  ec_data$ec_ecnt3[which(ec_data$lab_3_ecoli_reading_membrane==1)]<-999
  ec_data$ec_ecnt3[which(ec_data$lab_3_ecoli_reading_membrane==2)]<-998

  swap1<-(ec_data$ec_dil1>=ec_data$ec_dil2 & is.na(ec_data$ec_dil3))
  swap2<-(ec_data$ec_dil1<ec_data$ec_dil2 & is.na(ec_data$ec_dil3))
  swap3<-(ec_data$ec_dil1>=ec_data$ec_dil2 & !is.na(ec_data$ec_dil3))
  swap4<-(ec_data$ec_dil1<ec_data$ec_dil2 & !is.na(ec_data$ec_dil3))
  swap5<-(ec_data$ec_dil2>=ec_data$ec_dil3 & !is.na(ec_data$ec_dil3))
  swap6<-(ec_data$ec_dil2<ec_data$ec_dil3 & !is.na(ec_data$ec_dil3))
  swap7<-(ec_data$ec_dil3>=ec_data$ec_dil1 & !is.na(ec_data$ec_dil3))
  swap8<-(ec_data$ec_dil3<ec_data$ec_dil1 & !is.na(ec_data$ec_dil3))

  dilution3<-!is.na(ec_data$ec_dil3)
  no_dilution3<-is.na(ec_data$ec_dil3)

  ec_data$count1[swap1]<-ec_data$ec_ecnt1[swap1]
  ec_data$count2[swap1]<-ec_data$ec_ecnt2[swap1]
  ec_data$dil1[swap1]<-ec_data$ec_dil1[swap1]
  ec_data$dil2[swap1]<-ec_data$ec_dil2[swap1]

  ec_data$count2[swap2]<-ec_data$ec_ecnt1[swap2]
  ec_data$count1[swap2]<-ec_data$ec_ecnt2[swap2]
  ec_data$dil2[swap2]<-ec_data$ec_dil1[swap2]
  ec_data$dil1[swap2]<-ec_data$ec_dil2[swap2]

  ec_data$count1[swap3 & swap5 & swap8]<-ec_data$ec_ecnt1[swap3 & swap5 & swap8]
  ec_data$count2[swap3 & swap5 & swap8]<-ec_data$ec_ecnt2[swap3 & swap5 & swap8]
  ec_data$count3[swap3 & swap5 & swap8]<-ec_data$ec_ecnt3[swap3 & swap5 & swap8]
  ec_data$dil1[swap3 & swap5 & swap8]<-ec_data$ec_dil1[swap3 & swap5 & swap8]
  ec_data$dil2[swap3 & swap5 & swap8]<-ec_data$ec_dil2[swap3 & swap5 & swap8]
  ec_data$dil3[swap3 & swap5 & swap8]<-ec_data$ec_dil3[swap3 & swap5 & swap8]

  ec_data$count1[swap3 & swap6 & swap7]<-ec_data$ec_ecnt3[swap3 & swap6 & swap7]
  ec_data$count2[swap3 & swap6 & swap7]<-ec_data$ec_ecnt1[swap3 & swap6 & swap7]
  ec_data$count3[swap3 & swap6 & swap7]<-ec_data$ec_ecnt2[swap3 & swap6 & swap7]
  ec_data$dil1[swap3 & swap6 & swap7]<-ec_data$ec_dil3[swap3 & swap6 & swap7]
  ec_data$dil2[swap3 & swap6 & swap7]<-ec_data$ec_dil1[swap3 & swap6 & swap7]
  ec_data$dil3[swap3 & swap6 & swap7]<-ec_data$ec_dil2[swap3 & swap6 & swap7]

  ec_data$count1[swap4 & swap5 & swap7]<-ec_data$ec_ecnt2[swap4 & swap5 & swap7]
  ec_data$count2[swap4 & swap5 & swap7]<-ec_data$ec_ecnt3[swap4 & swap5 & swap7]
  ec_data$count3[swap4 & swap5 & swap7]<-ec_data$ec_ecnt1[swap4 & swap5 & swap7]
  ec_data$dil1[swap4 & swap5 & swap7]<-ec_data$ec_dil2[swap4 & swap5 & swap7]
  ec_data$dil2[swap4 & swap5 & swap7]<-ec_data$ec_dil3[swap4 & swap5 & swap7]
  ec_data$dil3[swap4 & swap5 & swap7]<-ec_data$ec_dil1[swap4 & swap5 & swap7]

  ec_data$count1[swap3 & swap6 & swap8]<-ec_data$ec_ecnt1[swap3 & swap6 & swap8]
  ec_data$count2[swap3 & swap6 & swap8]<-ec_data$ec_ecnt3[swap3 & swap6 & swap8]
  ec_data$count3[swap3 & swap6 & swap8]<-ec_data$ec_ecnt2[swap3 & swap6 & swap8]
  ec_data$dil1[swap3 & swap6 & swap8]<-ec_data$ec_dil1[swap3 & swap6 & swap8]
  ec_data$dil2[swap3 & swap6 & swap8]<-ec_data$ec_dil3[swap3 & swap6 & swap8]
  ec_data$dil3[swap3 & swap6 & swap8]<-ec_data$ec_dil2[swap3 & swap6 & swap8]

  ec_data$count1[swap4 & swap5 & swap8]<-ec_data$ec_ecnt2[swap4 & swap5 & swap8]
  ec_data$count2[swap4 & swap5 & swap8]<-ec_data$ec_ecnt1[swap4 & swap5 & swap8]
  ec_data$count3[swap4 & swap5 & swap8]<-ec_data$ec_ecnt3[swap4 & swap5 & swap8]
  ec_data$dil1[swap4 & swap5 & swap8]<-ec_data$ec_dil2[swap4 & swap5 & swap8]
  ec_data$dil2[swap4 & swap5 & swap8]<-ec_data$ec_dil1[swap4 & swap5 & swap8]
  ec_data$dil3[swap4 & swap5 & swap8]<-ec_data$ec_dil3[swap4 & swap5 & swap8]

  ec_data$count1[swap4 & swap6 & swap7]<-ec_data$ec_ecnt3[swap4 & swap6 & swap7]
  ec_data$count2[swap4 & swap6 & swap7]<-ec_data$ec_ecnt2[swap4 & swap6 & swap7]
  ec_data$count3[swap4 & swap6 & swap7]<-ec_data$ec_ecnt1[swap4 & swap6 & swap7]
  ec_data$dil1[swap4 & swap6 & swap7]<-ec_data$ec_dil3[swap4 & swap6 & swap7]
  ec_data$dil2[swap4 & swap6 & swap7]<-ec_data$ec_dil2[swap4 & swap6 & swap7]
  ec_data$dil3[swap4 & swap6 & swap7]<-ec_data$ec_dil1[swap4 & swap6 & swap7]

  #check whether threre is a dilution jumping.
  dil_jump1_1<-abs(ec_data$dil1/ec_data$dil2-10)<0.0001
  dil_jump1_2<-abs(ec_data$dil1/ec_data$dil2-100)<0.0001
  dil_jump2_1<-abs(ec_data$dil2/ec_data$dil3-10)<0.0001
  dil_jump2_2<-abs(ec_data$dil2/ec_data$dil3-100)<0.0001

  #two dilution cases (1:10 dilution jump and 1:100 dilution jump)
  condition1=which((ec_data$count1==999 | ec_data$count1==998) & (ec_data$count2==999 | ec_data$count2==998) & no_dilution3)
  condition2=which((ec_data$count1==999 | ec_data$count1==998) & ec_data$count2>=10 & ec_data$count2<=200 & no_dilution3)
  condition3=which((ec_data$count1==999 | ec_data$count1==998) & ec_data$count2>=1 & ec_data$count2<=9 & no_dilution3)
  condition4=which(ec_data$count1>=10 & ec_data$count1<=200 & ec_data$count2>=10 & ec_data$count2<=200 & no_dilution3 & dil_jump1_1)
  condition5=which(ec_data$count1>=10 & ec_data$count1<=200 & ec_data$count2>=1 & ec_data$count2<=9 & no_dilution3)
  condition6=which(ec_data$count1>=10 & ec_data$count1<=200 & ec_data$count2==0 & no_dilution3)
  condition7=which(ec_data$count1>=1 & ec_data$count1<=9 & ec_data$count2>=1 & ec_data$count2<=9 & no_dilution3 & dil_jump1_1)
  condition8=which(ec_data$count1>=1 & ec_data$count1<=9 & ec_data$count2==0 & no_dilution3)
  condition9=which(ec_data$count1==0 & ec_data$count2==0 & no_dilution3)
  condition10=which((ec_data$count1==999 | ec_data$count1==998) & ec_data$count2==0 & no_dilution3 & dil_jump1_2)

  #three dilution cases (1:10 dilution jump and 1:100 dilution jump)
  condition11=which((ec_data$count1==999 | ec_data$count1==998) & (ec_data$count2==999 | ec_data$count2==998) & (ec_data$count3==999 | ec_data$count3==998) & dilution3)
  condition12=which((ec_data$count1==999 | ec_data$count1==998) & (ec_data$count2==999 | ec_data$count2==998) & ec_data$count3>=10 & ec_data$count3<=200 & dilution3)
  condition13=which((ec_data$count1==999 | ec_data$count1==998) & (ec_data$count2==999 | ec_data$count2==998) & ec_data$count3>=1 & ec_data$count3<=9 & dilution3)
  condition14=which((ec_data$count1==999 | ec_data$count1==998) & (ec_data$count2==999 | ec_data$count2==998) & ec_data$count3==0 & dilution3 & dil_jump2_2)
  condition15=which((ec_data$count1==999 | ec_data$count1==998) & ec_data$count2>=10 & ec_data$count2<=200 & ec_data$count3>=10 & ec_data$count3<=200 & dilution3)
  condition16=which((ec_data$count1==999 | ec_data$count1==998) & ec_data$count2>=10 & ec_data$count2<=200 & ec_data$count3>=1 & ec_data$count3<=9 & dilution3)
  condition17=which((ec_data$count1==999 | ec_data$count1==998) & ec_data$count2>=10 & ec_data$count2<=200 & ec_data$count3==0 & dilution3)
  condition18=which((ec_data$count1==999 | ec_data$count1==998) & ec_data$count2>=1 & ec_data$count2<=9 & ec_data$count3>=1 & ec_data$count3<=9 & dilution3 & dil_jump2_1)
  condition19=which((ec_data$count1==999 | ec_data$count1==998) & ec_data$count2>=1 & ec_data$count2<=9 & ec_data$count3>=1 & ec_data$count3<=9 & dilution3 & dil_jump2_2)
  condition20=which((ec_data$count1==999 | ec_data$count1==998) & ec_data$count2>=1 & ec_data$count2<=9 & ec_data$count3==0 & dilution3)
  condition21=which((ec_data$count1==999 | ec_data$count1==998) & ec_data$count2==0 & ec_data$count3==0 & dilution3 & dil_jump1_2)
  condition22=which(ec_data$count1>=10 & ec_data$count1<=200 & ec_data$count2>=10 & ec_data$count2<=200 & ec_data$count3>=10 & ec_data$count3<=200 & dilution3 & dil_jump1_1 & dil_jump2_1)
  condition23=which(ec_data$count1>=10 & ec_data$count1<=200 & ec_data$count2>=10 & ec_data$count2<=200 & ec_data$count3>=1 & ec_data$count3<=9 & dilution3 & dil_jump1_1 & dil_jump2_1)
  condition24=which(ec_data$count1>=10 & ec_data$count1<=200 & ec_data$count2>=10 & ec_data$count2<=200 & ec_data$count3>=1 & ec_data$count3<=9 & dilution3 & dil_jump1_2 & dil_jump2_2)
  condition25=which(ec_data$count1>=10 & ec_data$count1<=200 & ec_data$count2>=10 & ec_data$count2<=200 & ec_data$count3==0 & dilution3)
  condition26=which(ec_data$count1>=10 & ec_data$count1<=200 & ec_data$count2>=1 & ec_data$count2<=9 & ec_data$count3>=1 & ec_data$count3<=9 & dilution3)
  condition27=which(ec_data$count1>=10 & ec_data$count1<=200 & ec_data$count2>=1 & ec_data$count2<=9 & ec_data$count3==0 & dilution3)
  condition28=which(ec_data$count1>=10 & ec_data$count1<=200 & ec_data$count2==0 & ec_data$count3==0 & dilution3)
  condition29=which(ec_data$count1>=1 & ec_data$count1<=9 & ec_data$count2>=1 & ec_data$count2<=9 & ec_data$count3==0 & dilution3 & dil_jump1_1)
  condition30=which(ec_data$count1>=1 & ec_data$count1<=9 & ec_data$count2>=1 & ec_data$count2<=9 & ec_data$count3==0 & dilution3 & dil_jump1_2)
  condition31=which(ec_data$count1>=1 & ec_data$count1<=9 & ec_data$count2==0 & ec_data$count3==0 & dilution3)
  condition32=which(ec_data$count1==0 & ec_data$count2==0 & ec_data$count3==0 & dilution3)

  ec_con<-rep(NA,length(ec_data$ec_ecnt1))
  ec_con[condition1]=200/ec_data$dil2[condition1]*ec_data$ec_denom[condition1]
  ec_con[condition2]=ec_data$count2[condition2]/ec_data$dil2[condition2]*ec_data$ec_denom[condition2]
  ec_con[condition3]=ec_data$count2[condition3]/ec_data$dil2[condition3]*ec_data$ec_denom[condition3]
  ec_con[condition4]=(ec_data$count1[condition4]/ec_data$dil1[condition4]+ec_data$count2[condition4]/ec_data$dil2[condition4])/2*ec_data$ec_denom[condition4]
  ec_con[condition5]=ec_data$count1[condition5]/ec_data$dil1[condition5]*ec_data$ec_denom[condition5]
  ec_con[condition6]=ec_data$count1[condition6]/ec_data$dil1[condition6]*ec_data$ec_denom[condition6]
  ec_con[condition7]=(ec_data$count1[condition7]/ec_data$dil1[condition7]+ec_data$count2[condition7]/ec_data$dil2[condition7])/2*ec_data$ec_denom[condition7]
  ec_con[condition8]=ec_data$count1[condition8]/ec_data$dil1[condition8]*ec_data$ec_denom[condition8]
  #since jumping dilution
  #ec_con[condition9]=0.5/(ec_data$dil1[condition9]+ec_data$dil2[condition9])*ec_data$ec_denom[condition9]
  ec_con[condition9]=0.5/ec_data$dil1[condition9]*ec_data$ec_denom[condition9]
  ec_con[condition10]=200/ec_data$dil1[condition10]*ec_data$ec_denom[condition10]

  ec_con[condition11]=200/ec_data$dil3[condition11]*ec_data$ec_denom[condition11]
  ec_con[condition12]=ec_data$count3[condition12]/ec_data$dil3[condition12]*ec_data$ec_denom[condition12]
  ec_con[condition13]=ec_data$count3[condition13]/ec_data$dil3[condition13]*ec_data$ec_denom[condition13]
  ec_con[condition14]=200/ec_data$dil2[condition14]*ec_data$ec_denom[condition14]
  ec_con[condition15]=(ec_data$count2[condition15]/ec_data$dil2[condition15]+ec_data$count3[condition15]/ec_data$dil3[condition15])/2*ec_data$ec_denom[condition15]
  ec_con[condition16]=ec_data$count2[condition16]/ec_data$dil2[condition16]*ec_data$ec_denom[condition16]
  ec_con[condition17]=ec_data$count2[condition17]/ec_data$dil2[condition17]*ec_data$ec_denom[condition17]
  ec_con[condition18]=ec_data$count3[condition18]/ec_data$dil3[condition18]*ec_data$ec_denom[condition18]
  ec_con[condition19]=ec_data$count2[condition19]/ec_data$dil2[condition19]*ec_data$ec_denom[condition19]
  ec_con[condition20]=ec_data$count2[condition20]/ec_data$dil2[condition20]*ec_data$ec_denom[condition20]
  ec_con[condition21]=200/ec_data$dil1[condition21]*ec_data$ec_denom[condition21]
  ec_con[condition22]=(ec_data$count1[condition22]/ec_data$dil1[condition22]+ec_data$count2[condition22]/ec_data$dil2[condition22]+ec_data$count3[condition22]/ec_data$dil3[condition22])/3*ec_data$ec_denom[condition22]
  ec_con[condition23]=(ec_data$count1[condition23]/ec_data$dil1[condition23]+ec_data$count2[condition23]/ec_data$dil2[condition23])/2*ec_data$ec_denom[condition23]
  ec_con[condition24]=ec_data$count2[condition24]/ec_data$dil2[condition24]*ec_data$ec_denom[condition24]
  ec_con[condition25]=(ec_data$count1[condition25]/ec_data$dil1[condition25]+ec_data$count2[condition25]/ec_data$dil2[condition25])/2*ec_data$ec_denom[condition25]
  ec_con[condition26]=ec_data$count1[condition26]/ec_data$dil1[condition26]*ec_data$ec_denom[condition26]
  ec_con[condition27]=ec_data$count1[condition27]/ec_data$dil1[condition27]*ec_data$ec_denom[condition27]
  ec_con[condition28]=ec_data$count1[condition28]/ec_data$dil1[condition28]*ec_data$ec_denom[condition28]
  ec_con[condition29]=(ec_data$count1[condition29]/ec_data$dil1[condition29]+ec_data$count2[condition29]/ec_data$dil2[condition29])/2*ec_data$ec_denom[condition29]
  ec_con[condition30]=ec_data$count2[condition30]/ec_data$dil2[condition30]*ec_data$ec_denom[condition30]
  ec_con[condition31]=ec_data$count1[condition31]/ec_data$dil1[condition31]*ec_data$ec_denom[condition31]
  ec_con[condition32]=0.5/ec_data$dil1[condition32]*ec_data$ec_denom[condition32]
  ec_data$ec_conc<-ec_con
  ec_data$neighbor <- as.factor(ec_data$col_neighborhood)

  return(ec_data)
}


# FREQUENCIES ----------------------------------------------------------------
calculate_freq <- function(..., type='pie chart', analysis_type=NULL) {
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
    stop(paste('Unable to determine survey type. Do the column headers have hh, sch, or com in the names?\n',
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

  freq <- find_pathways(df_for_analysis, analysis_type)

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

find_pathways <- function(df, analysis_type) {
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
  # iterate through neighborhoods and find pathways for each
  freq <- lapply(neighborhoods, function(n) {
    apply(pathways, 1, function(pathway_combo) find_pathway(df, n, analysis_type, pathway_combo[2], pathway_combo[3])) %>% unname() %>% unlist(recursive=F)
  }) %>% unlist(recursive=F)
  return(freq)
}

find_pathway <- function(df, neighborhood, analysis_type, pathway_type, population_type) {
  return(list(path = 
                list(sample = unname(pathway_type),
                      age = switch(population_type, 'a' = 'Adults', 'c' = 'Children'),
                      neighborhood = paste('Neighborhood',neighborhood),
                      data = switch(analysis_type,
                                    # if analysis type is "combined" we'll loop through all survey types and stick the results together
                                    'combined' = sapply(c('h', 's', 'c'), function(x) {
                                      find_freq(df[df[,paste0(x, '_neighborhood')] == neighborhood,], x, pathway_type, population_type)}
                                      ) %>% unlist(),
                                    # otherwise, we'll just look at specific columns
                                    'household' = find_freq(df[df$h_neighborhood == neighborhood,], 'h', pathway_type, population_type),
                                    'school' = find_freq(df[df$s_neighborhood == neighborhood,], 's', pathway_type, population_type),
                                    'community' = find_freq(df[df$c_neighborhood == neighborhood,], 'c', pathway_type, population_type)
                      ) 
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

  neighborhoods <- c(unique(names(list.names(conc, neighborhood))), unique(names(list.names(freq, neighborhood)))) # unique neighborhood values
  neighborhoods <- unique(neighborhoods[duplicated(neighborhoods)]) # if it's duplicated, then it will show up in both freq and conc

  # Bathing water can be assumed as 30, all others should be present to calculate
  samples <- c(unique(names(list.names(conc, sample))), unique(names(list.names(freq, sample)))) # unique sample values
  samples <- unique(samples[duplicated(samples)])

 age <- unique(names(list.names(freq, age)))

  ps.freq <- list()
  for (smp in conc_samples) {
    print(smp)
    for (nb in conc_neighborhoods) {
      sub.conc <- conc[[list.which(conc, neighborhood == nb && sample == smp)]]
      print(nb)
      # calculate exposure for adults and children using the behavior data
      for (a in age) {
        print(a)
        # filter frequency to just the age we want
        sub.freq <- freq[[list.which(freq, sample == smp && neighborhood == nb && age == a)]]

        # calculate the exposure. Requires concentration, freq is optional
        exposed <- calculate_exposure(sub.freq, sub.conc, smp)

        # update the object at this position
        ps.freq <- append(ps.freq, list('path' = exposed))

      }
      # filter the concentration data to just this neighborhood and sample

    }
  }

  # give back the updated behavior data object
  return(ps.freq)
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
        if (freq[[k]]$sample=='Municipal and Piped Water'){
          cutpoint<-c(0,3,6)
          init_freq_be<-as.numeric(rep(NA,length(freq_be)))
          init_freq_be[which(freq_be==1)]<-2
          init_freq_be[which(freq_be==2)]<-5
          init_freq_be[which(freq_be==3)]<-7
        } else {
          cutpoint<-c(0,5,10)
          init_freq_be<-as.numeric(rep(NA,length(freq_be)))
          init_freq_be[which(freq_be==1)]<-2
          init_freq_be[which(freq_be==2)]<-7
          init_freq_be[which(freq_be==3)]<-12
        }

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

calculate_exposure <- function(behavior_data, concentration_data, smp) {
  # used for people plot generation.  this is for a single pathway
  # and assumes the data are already subset to the appropriate level
  e <- rep(NA, 1000)
  f <- rep(NA, 1000)
  risk <- rep(NA, 1000)

  # values applied based on sample and age
  intake<-array(c(0.0006,1,10.43,0.0154,0.037,0.0006,0.034,NA,0.0499,1,
                  0.01,0.5,4.14,0.2042,0.2042,0.01,0.034,NA,0.09975,0.5),c(10,2)) #need to input this information!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  rownames(intake) <- c('Drain Water', 'Produce', 'Municipal and Piped Water', 'Ocean Water', 'Surface Water',
                        'Flood Water', 'Public Latrine', 'Particulate', 'Bathing Water','Street Food')
  colnames(intake) <- c("Adults", "Children")

  # simulate some numbers
  for (m in 1:1000){
    # is it necessary for this to be in a loop?
    e[m] <- rnorm(1, concentration_data$mu, concentration_data$sigma)
    if (smp %in% c(2,6,7,9,10)) {
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

  n <-(1-length(which(f==0))/1000)*100;
  dose <-log10(mean(non0(risk),na.rm=TRUE))
  # add the percent exposure and dose information to the behavior data
  behavior_data <- append(behavior_data, list('n' = n, 'dose' = dose))

  # give the updated object back
  return(behavior_data)

}

factor_to_numeric <- function(x) {
  # convert factor or character data to numeric
  return(as.numeric(as.character(x)))
}
