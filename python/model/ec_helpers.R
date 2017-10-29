# Functions specifically for ec calculations

ec_prepare_idexx <- function(lab_data, reading, mpn_tbl) {
  #function to prepare the lab_data for IDEXX;
  for (i in 1:length(lab_data$lab_id)){
    print(i)
    lab_data$lab_1_ecoli[i]<-ifelse(lab_data$lab_1_ecoli_reading_idexx[i] == reading$valid,mpn_tbl[lab_data$lab_1_ecoli_big_idexx[i]+1,lab_data$lab_1_ecoli_small_idexx[i]+1],NA) #need to think about how this NA means to the analysis???
    lab_data$lab_2_ecoli[i]<-ifelse(lab_data$lab_2_ecoli_reading_idexx[i] == reading$valid,mpn_tbl[lab_data$lab_2_ecoli_big_idexx[i]+1,lab_data$lab_2_ecoli_small_idexx[i]+1],NA)
    lab_data$lab_3_ecoli[i]<-ifelse(lab_data$lab_3_ecoli_reading_idexx[i] == reading$valid,mpn_tbl[lab_data$lab_3_ecoli_big_idexx[i]+1,lab_data$lab_3_ecoli_small_idexx[i]+1],NA)
  }
  return(lab_data)
}

ec_prepare_mf <- function(lab_data, reading) {
  lab_data$ec_ecnt1[which(lab_data$lab_1_ecoli_reading_membrane== reading$TNTC)]<- value$TNTC #should we differentiate TNTC and TDTC?
  lab_data$ec_ecnt1[which(lab_data$lab_1_ecoli_reading_membrane== reading$TDTC)]<- value$TDTC
  lab_data$ec_ecnt2[which(lab_data$lab_2_ecoli_reading_membrane== reading$TNTC)]<- value$TNTC
  lab_data$ec_ecnt2[which(lab_data$lab_2_ecoli_reading_membrane== reading$TDTC)]<- value$TDTC
  lab_data$ec_ecnt3[which(lab_data$lab_3_ecoli_reading_membrane== reading$TNTC)]<- value$TNTC
  lab_data$ec_ecnt3[which(lab_data$lab_3_ecoli_reading_membrane== reading$TDTC)]<- value$TDTC
  
  return(lab_data)
}

ec_merge <- function(collection_data, lab_data) {
  #Capitalized and remove missings in ID and sample types for merge purpose;
  collection_data[,c("col_sample_type","col_id")] <- apply(collection_data[,c("col_sample_type","col_id")], 2, function(x) toupper(x))
  lab_data[,c("lab_sample_type","lab_id")] <- apply(lab_data[,c("lab_sample_type","lab_id")], 2, function(x) toupper(x))
  collection_data$col_id<-gsub(" ","",collection_data$col_id)
  lab_data$lab_id<-gsub(" ","",lab_data$lab_id)
  
  ec_data<-merge(collection_data,lab_data,by.x=c("col_sample_type","col_id"),by.y=c("lab_sample_type","lab_id"))
  names(ec_data)[which(names(ec_data)=="col_sample_type")]<-"sample_type"
  names(ec_data)[which(names(ec_data)=="col_id")]<-"sampleid"
  
  ec_data$sample_type<-as.numeric(ec_data$sample_type)
  names(ec_data) %<>% gsub('lab_\\d{1,}_group.', "", .)
  ec_data$ec_dil1<-factor_to_numeric(ec_data$lab_1_dil_tested)
  ec_data$ec_dil2<-factor_to_numeric(ec_data$lab_2_dil_tested)
  ec_data$ec_dil3<-factor_to_numeric(ec_data$lab_3_dil_tested)
  ec_data$lab_1_volume<-factor_to_numeric(ec_data$lab_1_volume)
  ec_data$lab_2_volume<-factor_to_numeric(ec_data$lab_2_volume)
  ec_data$lab_3_volume<-factor_to_numeric(ec_data$lab_3_volume)
  ec_data$ec_dil1<-(10^ec_data$ec_dil1)/(10^7)*ec_data$lab_1_volume #If lab protocol won't change, this won't change. We can ask Suraja about this part;
  ec_data$ec_dil2<-(10^ec_data$ec_dil2)/(10^7)*ec_data$lab_2_volume
  ec_data$ec_dil3<-(10^ec_data$ec_dil3)/(10^7)*ec_data$lab_3_volume
  ec_data$ec_ecnt1<-factor_to_numeric(ec_data$lab_1_ecoli)
  ec_data$ec_ecnt2<-factor_to_numeric(ec_data$lab_2_ecoli)
  ec_data$ec_ecnt3<-factor_to_numeric(ec_data$lab_3_ecoli)
  
  return(ec_data)
}


ec_add_denoms <- function(ec_data, denoms) {
  # set default denominator
  ec_data$ec_denom= denoms$default
  # set any other denominators
  for (n in names(denoms)) {
    ec_data$ec_denom[which(ec_data$sample_type == n)] <- denoms[n]
  }
  # street food has a sepcial calculation
  #street food used WASH benefit protocol 10 grams into 100 mL, serving size was defined using weight of street food sampled.
  
  if ('sf' %in% ec_data$sample_type) {
    ec_data$ec_denom[which(ec_data$sample_type== sample_type_code$sf)]= denoms$sf*ec_data$lab_sf_weight[which(ec_data$sample_type== sample_type_code$sf)]
  }
  # set any blanks to NA
  ec_data$ec_denom[is.na(ec_data$sample_type)]=NA
  
  return(ec_data)
}

ec_calc_swaps <- function(ec_data) {
  
  swap1 = (ec_data$ec_dil1 >= ec_data$ec_dil2 & is.na(ec_data$ec_dil3))
  swap2 = (ec_data$ec_dil1 < ec_data$ec_dil2 & is.na(ec_data$ec_dil3))
  swap3 = (ec_data$ec_dil1 >= ec_data$ec_dil2 & !is.na(ec_data$ec_dil3))
  swap4 = (ec_data$ec_dil1 < ec_data$ec_dil2 & !is.na(ec_data$ec_dil3))
  swap5 = (ec_data$ec_dil2 >= ec_data$ec_dil3 & !is.na(ec_data$ec_dil3))
  swap6 = (ec_data$ec_dil2 < ec_data$ec_dil3 & !is.na(ec_data$ec_dil3))
  swap7 = (ec_data$ec_dil3 >= ec_data$ec_dil1 & !is.na(ec_data$ec_dil3))
  swap8 = (ec_data$ec_dil3 < ec_data$ec_dil1 & !is.na(ec_data$ec_dil3))

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
  
  return(ec_data)
  
}

ec_check_dilutions <- function(ec_data) {
  dils <- list(
  dilution3=!is.na(ec_data$ec_dil3),
  no_dilution3=is.na(ec_data$ec_dil3),

  #check whether threre is a dilution jumping.
  dil_jump1_1=abs(ec_data$dil1/ec_data$dil2-10)<0.0001,
  dil_jump1_2=abs(ec_data$dil1/ec_data$dil2-100)<0.0001,
  dil_jump2_1=abs(ec_data$dil2/ec_data$dil3-10)<0.0001,
  dil_jump2_2=abs(ec_data$dil2/ec_data$dil3-100)<0.0001
  )
  
  return(dils)  
}

ec_idexx_conditions <- function(ec_data, value) {
  dils <- ec_check_dilutions(ec_data)
  
  condition1=which(ec_data$count1== value$censored & ec_data$count2== value$censored & dils$no_dilution3)
  condition2=which(ec_data$count1== value$censored & ec_data$count2>= value$cut_point & ec_data$count2<= value$upper_limit & dils$no_dilution3)
  condition3=which(ec_data$count1== value$censored & ec_data$count2>= value$lower_limit & ec_data$count2< value$cut_point & dils$no_dilution3)
  condition4=which(ec_data$count1>= value$cut_point & ec_data$count1<= value$upper_limit & ec_data$count2>= value$cut_point & ec_data$count2< value$upper_limit & dils$no_dilution3 & dils$dil_jump1_1)
  condition5=which(ec_data$count1>= value$cut_point & ec_data$count1<= value$upper_limit & ec_data$count2>= value$lower_limit & ec_data$count2< value$cut_point & dils$no_dilution3)
  condition6=which(ec_data$count1>= value$cut_point & ec_data$count1<= value$upper_limit & ec_data$count2< value$lower_limit & dils$no_dilution3)
  condition7=which(ec_data$count1>= value$lower_limit & ec_data$count1< value$cut_point & ec_data$count2>= value$lower_limit & ec_data$count2< value$cut_point & dils$no_dilution3 & dils$dil_jump1_1)
  condition8=which(ec_data$count1>= value$lower_limit & ec_data$count1< value$cut_point & ec_data$count2>= value$lower_limit & ec_data$count2< value$cut_point & dils$no_dilution3 & dils$dil_jump1_2)
  condition9=which(ec_data$count1>= value$lower_limit & ec_data$count1< value$cut_point & ec_data$count2< value$lower_limit & dils$no_dilution3)
  condition10=which(ec_data$count1< value$lower_limit & ec_data$count2< value$lower_limit & dils$no_dilution3)
  
  #three dilution cases (1:10 dilution jump and 1:100 dilution jump)
  condition11=which(ec_data$count1== value$censored & ec_data$count2== value$censored & ec_data$count3== value$censored &dils$dilution3)
  condition12=which(ec_data$count1== value$censored & ec_data$count2== value$censored & ec_data$count3>= value$cut_point & ec_data$count3<= value$upper_limit &dils$dilution3)
  condition13=which(ec_data$count1== value$censored & ec_data$count2== value$censored & ec_data$count3>= value$lower_limit & ec_data$count3< value$cut_point &dils$dilution3)
  condition15=which(ec_data$count1== value$censored & ec_data$count2>= value$cut_point & ec_data$count2<= value$upper_limit & ec_data$count3>= value$cut_point & ec_data$count3<= value$upper_limit &dils$dilution3 & dils$dil_jump2_1)
  condition16=which(ec_data$count1== value$censored & ec_data$count2>= value$cut_point & ec_data$count2<= value$upper_limit & ec_data$count3>= value$lower_limit & ec_data$count3< value$cut_point &dils$dilution3)
  condition17=which(ec_data$count1== value$censored & ec_data$count2>= value$cut_point & ec_data$count2<= value$upper_limit & ec_data$count3< value$lower_limit &dils$dilution3)
  condition18=which(ec_data$count1== value$censored & ec_data$count2>= value$lower_limit & ec_data$count2< value$cut_point & ec_data$count3>= value$lower_limit & ec_data$count3< value$cut_point &dils$dilution3 & dils$dil_jump2_1)
  condition19=which(ec_data$count1== value$censored & ec_data$count2>= value$lower_limit & ec_data$count2< value$cut_point & ec_data$count3>= value$lower_limit & ec_data$count3< value$cut_point &dils$dilution3 & dils$dil_jump2_2)
  condition20=which(ec_data$count1== value$censored & ec_data$count2>= value$lower_limit & ec_data$count2< value$cut_point & ec_data$count3< value$lower_limit &dils$dilution3)
  
  condition23=which(ec_data$count1>= value$cut_point & ec_data$count1<= value$upper_limit & ec_data$count2>= value$cut_point & ec_data$count2<= value$upper_limit & ec_data$count3>= value$lower_limit & ec_data$count3< value$cut_point &dils$dilution3 & dils$dil_jump1_1 & dils$dil_jump2_1)
  condition25=which(ec_data$count1>= value$cut_point & ec_data$count1<= value$upper_limit & ec_data$count2>= value$lower_limit & ec_data$count2< value$cut_point & ec_data$count3>= value$lower_limit & ec_data$count3< value$cut_point &dils$dilution3 & dils$dil_jump1_1 & dils$dil_jump2_1)
  condition26=which(ec_data$count1>= value$cut_point & ec_data$count1<= value$upper_limit & ec_data$count2>= value$lower_limit & ec_data$count2< value$cut_point & ec_data$count3>= value$lower_limit & ec_data$count3< value$cut_point &dils$dilution3 & dils$dil_jump1_2 & dils$dil_jump2_2)
  condition27=which(ec_data$count1>= value$cut_point & ec_data$count1<= value$upper_limit & ec_data$count2>= value$lower_limit & ec_data$count2< value$cut_point & ec_data$count3< value$lower_limit &dils$dilution3)
  condition29=which(ec_data$count1>= value$lower_limit & ec_data$count1< value$cut_point & ec_data$count2>= value$lower_limit & ec_data$count2< value$cut_point & ec_data$count3< value$lower_limit &dils$dilution3 & dils$dil_jump1_1)
  condition30=which(ec_data$count1>= value$lower_limit & ec_data$count1< value$cut_point & ec_data$count2>= value$lower_limit & ec_data$count2< value$cut_point & ec_data$count3< value$lower_limit &dils$dilution3 & dils$dil_jump1_2)
  condition31=which(ec_data$count1>= value$lower_limit & ec_data$count1< value$cut_point & ec_data$count2< value$lower_limit & ec_data$count3< value$lower_limit &dils$dilution3)
  condition32=which(ec_data$count1< value$lower_limit & ec_data$count2< value$lower_limit & ec_data$count3< value$lower_limit &dils$dilution3)
  # idexx specifc -----
  
  ec_con<-rep(NA,length(ec_data$ec_ecnt1))
  ec_con[condition1]= value$upper_limit/ec_data$dil2[condition1]*ec_data$ec_denom[condition1]
  ec_con[condition2]=ec_data$count2[condition2]/ec_data$dil2[condition2]*ec_data$ec_denom[condition2]
  ec_con[condition3]=ec_data$count2[condition3]/ec_data$dil2[condition3]*ec_data$ec_denom[condition3]
  ec_con[condition4]=(ec_data$count1[condition4]/ec_data$dil1[condition4]+ec_data$count2[condition4]/ec_data$dil2[condition4])/2*ec_data$ec_denom[condition4]
  ec_con[condition5]=(ec_data$count1[condition5]/ec_data$dil1[condition5]+ec_data$count2[condition5]/ec_data$dil2[condition5])/2*ec_data$ec_denom[condition5]
  ec_con[condition6]=ec_data$count1[condition6]/ec_data$dil1[condition6]*ec_data$ec_denom[condition6]
  ec_con[condition7]=(ec_data$count1[condition7]/ec_data$dil1[condition7]+ec_data$count2[condition7]/ec_data$dil2[condition7])/2*ec_data$ec_denom[condition7]
  ec_con[condition8]=ec_data$count1[condition8]/ec_data$dil1[condition8]*ec_data$ec_denom[condition8]
  ec_con[condition9]=ec_data$count1[condition9]/ec_data$dil1[condition9]*ec_data$ec_denom[condition9]
  ec_con[condition10]= value$negative/ec_data$dil1[condition10]*ec_data$ec_denom[condition10]
  ec_con[condition11]= value$upper_limit/ec_data$dil3[condition11]*ec_data$ec_denom[condition11]
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
  ec_con[condition32]= value$negative/ec_data$dil1[condition32]*ec_data$ec_denom[condition32]
  ec_data$ec_conc<-ec_con
 
  return(ec_data) 
}

ec_mf_conditions <- function(ec_data, value) {
  
  dils <- ec_check_dilutions(ec_data)
  
  lapply(names(dils), function(x) assign(x, dils[x]))
  
  #two dilution cases (1:10 dilution jump and 1:100 dilution jump)
  condition1=which((ec_data$count1== value$TNTC | ec_data$count1== value$TDTC) & (ec_data$count2== value$TNTC | ec_data$count2== value$TDTC) & dils$no_dilution3)
  condition2=which((ec_data$count1== value$TNTC | ec_data$count1== value$TDTC) & ec_data$count2>= value$cut_point & ec_data$count2<= value$upper_limit & dils$no_dilution3)
  condition3=which((ec_data$count1== value$TNTC | ec_data$count1== value$TDTC) & ec_data$count2> value$lower_limit & ec_data$count2< value$cut_point & dils$no_dilution3)
  condition4=which(ec_data$count1>= value$cut_point & ec_data$count1<= value$upper_limit & ec_data$count2>= value$cut_point & ec_data$count2<= value$upper_limit & dils$no_dilution3 & dils$dil_jump1_1)
  condition5=which(ec_data$count1>= value$cut_point & ec_data$count1<= value$upper_limit & ec_data$count2> value$lower_limit & ec_data$count2< value$cut_point & dils$no_dilution3)
  condition6=which(ec_data$count1>= value$cut_point & ec_data$count1<= value$upper_limit & ec_data$count2== value$lower_limit & dils$no_dilution3)
  condition7=which(ec_data$count1> value$lower_limit & ec_data$count1< value$cut_point & ec_data$count2> value$lower_limit & ec_data$count2< value$cut_point & dils$no_dilution3 & dils$dil_jump1_1)
  condition8=which(ec_data$count1> value$lower_limit & ec_data$count1< value$cut_point & ec_data$count2== value$lower_limit & dils$no_dilution3)
  condition9=which(ec_data$count1== value$lower_limit & ec_data$count2== value$lower_limit & dils$no_dilution3)
  condition10=which((ec_data$count1== value$TNTC | ec_data$count1== value$TDTC) & ec_data$count2== value$lower_limit & dils$no_dilution3 & dils$dil_jump1_2)
  
  #three dilution cases (1:10 dilution jump and 1:100 dilution jump)
  condition11=which((ec_data$count1== value$TNTC | ec_data$count1== value$TDTC) & (ec_data$count2== value$TNTC | ec_data$count2== value$TDTC) & (ec_data$count3== value$TNTC | ec_data$count3== value$TDTC) &dils$dilution3)
  condition12=which((ec_data$count1== value$TNTC | ec_data$count1== value$TDTC) & (ec_data$count2== value$TNTC | ec_data$count2== value$TDTC) & ec_data$count3>= value$cut_point & ec_data$count3<= value$upper_limit &dils$dilution3)
  condition13=which((ec_data$count1== value$TNTC | ec_data$count1== value$TDTC) & (ec_data$count2== value$TNTC | ec_data$count2== value$TDTC) & ec_data$count3> value$lower_limit & ec_data$count3< value$cut_point &dils$dilution3)
  condition14=which((ec_data$count1== value$TNTC | ec_data$count1== value$TDTC) & (ec_data$count2== value$TNTC | ec_data$count2== value$TDTC) & ec_data$count3== value$lower_limit &dils$dilution3 & dils$dil_jump2_2)
  condition15=which((ec_data$count1== value$TNTC | ec_data$count1== value$TDTC) & ec_data$count2>= value$cut_point & ec_data$count2<= value$upper_limit & ec_data$count3>= value$cut_point & ec_data$count3<= value$upper_limit &dils$dilution3)
  condition16=which((ec_data$count1== value$TNTC | ec_data$count1== value$TDTC) & ec_data$count2>= value$cut_point & ec_data$count2<= value$upper_limit & ec_data$count3> value$lower_limit & ec_data$count3< value$cut_point &dils$dilution3)
  condition17=which((ec_data$count1== value$TNTC | ec_data$count1== value$TDTC) & ec_data$count2>= value$cut_point & ec_data$count2<= value$upper_limit & ec_data$count3== value$lower_limit &dils$dilution3)
  condition18=which((ec_data$count1== value$TNTC | ec_data$count1== value$TDTC) & ec_data$count2> value$lower_limit & ec_data$count2< value$cut_point & ec_data$count3> value$lower_limit & ec_data$count3< value$cut_point &dils$dilution3 & dils$dil_jump2_1)
  condition19=which((ec_data$count1== value$TNTC | ec_data$count1== value$TDTC) & ec_data$count2> value$lower_limit & ec_data$count2< value$cut_point & ec_data$count3> value$lower_limit & ec_data$count3< value$cut_point &dils$dilution3 & dils$dil_jump2_2)
  condition20=which((ec_data$count1== value$TNTC | ec_data$count1== value$TDTC) & ec_data$count2> value$lower_limit & ec_data$count2< value$cut_point & ec_data$count3== value$lower_limit &dils$dilution3)
  condition21=which((ec_data$count1== value$TNTC | ec_data$count1== value$TDTC) & ec_data$count2== value$lower_limit & ec_data$count3== value$lower_limit &dils$dilution3 & dils$dil_jump1_2)
  condition22=which(ec_data$count1>= value$cut_point & ec_data$count1<= value$upper_limit & ec_data$count2>= value$cut_point & ec_data$count2<= value$upper_limit & ec_data$count3>= value$cut_point & ec_data$count3<= value$upper_limit &dils$dilution3 & dils$dil_jump1_1 & dils$dil_jump2_1)
  condition23=which(ec_data$count1>= value$cut_point & ec_data$count1<= value$upper_limit & ec_data$count2>= value$cut_point & ec_data$count2<= value$upper_limit & ec_data$count3> value$lower_limit & ec_data$count3< value$cut_point &dils$dilution3 & dils$dil_jump1_1 & dils$dil_jump2_1)
  condition24=which(ec_data$count1>= value$cut_point & ec_data$count1<= value$upper_limit & ec_data$count2>= value$cut_point & ec_data$count2<= value$upper_limit & ec_data$count3> value$lower_limit & ec_data$count3< value$cut_point &dils$dilution3 & dils$dil_jump1_2 & dils$dil_jump2_2)
  condition25=which(ec_data$count1>= value$cut_point & ec_data$count1<= value$upper_limit & ec_data$count2>= value$cut_point & ec_data$count2<= value$upper_limit & ec_data$count3== value$lower_limit &dils$dilution3)
  condition26=which(ec_data$count1>= value$cut_point & ec_data$count1<= value$upper_limit & ec_data$count2> value$lower_limit & ec_data$count2< value$cut_point & ec_data$count3> value$lower_limit & ec_data$count3< value$cut_point &dils$dilution3)
  condition27=which(ec_data$count1>= value$cut_point & ec_data$count1<= value$upper_limit & ec_data$count2> value$lower_limit & ec_data$count2< value$cut_point & ec_data$count3== value$lower_limit &dils$dilution3)
  condition28=which(ec_data$count1>= value$cut_point & ec_data$count1<= value$upper_limit & ec_data$count2== value$lower_limit & ec_data$count3== value$lower_limit &dils$dilution3)
  condition29=which(ec_data$count1> value$lower_limit & ec_data$count1< value$cut_point & ec_data$count2> value$lower_limit & ec_data$count2< value$cut_point & ec_data$count3== value$lower_limit &dils$dilution3 & dils$dil_jump1_1)
  condition30=which(ec_data$count1> value$lower_limit & ec_data$count1< value$cut_point & ec_data$count2> value$lower_limit & ec_data$count2< value$cut_point & ec_data$count3== value$lower_limit &dils$dilution3 & dils$dil_jump1_2)
  condition31=which(ec_data$count1> value$lower_limit & ec_data$count1< value$cut_point & ec_data$count2== value$lower_limit & ec_data$count3== value$lower_limit &dils$dilution3)
  condition32=which(ec_data$count1== value$lower_limit & ec_data$count2== value$lower_limit & ec_data$count3== value$lower_limit &dils$dilution3)
  # membrane specific ----
  
  ec_con<-rep(NA,length(ec_data$ec_ecnt1))
  ec_con[condition1]= value$upper_limit/ec_data$dil2[condition1]*ec_data$ec_denom[condition1]
  ec_con[condition2]=ec_data$count2[condition2]/ec_data$dil2[condition2]*ec_data$ec_denom[condition2]
  ec_con[condition3]=ec_data$count2[condition3]/ec_data$dil2[condition3]*ec_data$ec_denom[condition3]
  ec_con[condition4]=(ec_data$count1[condition4]/ec_data$dil1[condition4]+ec_data$count2[condition4]/ec_data$dil2[condition4])/2*ec_data$ec_denom[condition4]
  ec_con[condition5]=ec_data$count1[condition5]/ec_data$dil1[condition5]*ec_data$ec_denom[condition5]
  ec_con[condition6]=ec_data$count1[condition6]/ec_data$dil1[condition6]*ec_data$ec_denom[condition6]
  ec_con[condition7]=(ec_data$count1[condition7]/ec_data$dil1[condition7]+ec_data$count2[condition7]/ec_data$dil2[condition7])/2*ec_data$ec_denom[condition7]
  ec_con[condition8]=ec_data$count1[condition8]/ec_data$dil1[condition8]*ec_data$ec_denom[condition8]
  #since jumping dilution
  #ec_con[condition9]=0.5/(ec_data$dil1[condition9]+ec_data$dil2[condition9])*ec_data$ec_denom[condition9]
  ec_con[condition9]= value$negative/ec_data$dil1[condition9]*ec_data$ec_denom[condition9]
  ec_con[condition10]= value$upper_limit/ec_data$dil1[condition10]*ec_data$ec_denom[condition10]
  
  ec_con[condition11]= value$upper_limit/ec_data$dil3[condition11]*ec_data$ec_denom[condition11]
  ec_con[condition12]=ec_data$count3[condition12]/ec_data$dil3[condition12]*ec_data$ec_denom[condition12]
  ec_con[condition13]=ec_data$count3[condition13]/ec_data$dil3[condition13]*ec_data$ec_denom[condition13]
  ec_con[condition14]= value$upper_limit/ec_data$dil2[condition14]*ec_data$ec_denom[condition14]
  ec_con[condition15]=(ec_data$count2[condition15]/ec_data$dil2[condition15]+ec_data$count3[condition15]/ec_data$dil3[condition15])/2*ec_data$ec_denom[condition15]
  ec_con[condition16]=ec_data$count2[condition16]/ec_data$dil2[condition16]*ec_data$ec_denom[condition16]
  ec_con[condition17]=ec_data$count2[condition17]/ec_data$dil2[condition17]*ec_data$ec_denom[condition17]
  ec_con[condition18]=ec_data$count3[condition18]/ec_data$dil3[condition18]*ec_data$ec_denom[condition18]
  ec_con[condition19]=ec_data$count2[condition19]/ec_data$dil2[condition19]*ec_data$ec_denom[condition19]
  ec_con[condition20]=ec_data$count2[condition20]/ec_data$dil2[condition20]*ec_data$ec_denom[condition20]
  ec_con[condition21]= value$upper_limit/ec_data$dil1[condition21]*ec_data$ec_denom[condition21]
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
  ec_con[condition32]= value$negative/ec_data$dil1[condition32]*ec_data$ec_denom[condition32]
  ec_data$ec_conc<-ec_con
  
  return(ec_data)
}