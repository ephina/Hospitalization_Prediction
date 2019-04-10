library(dplyr)
library(magrittr)


#excluding the data with EXCLUDE=1
fn_exclude <- function(df_org){
  l_df_exclude <- df_org %>% filter(EXCLUDE!=1 & outcome_enc!=1 & det_pt_eligible=="in" & combined_category!="icu" & combined_category!="OR")
  return (l_df_exclude)
}

#select columns without NA values
fn_wo_na <- function(){
  l_na_cnt_vec <- sapply(g_df_ex,function(x) sum(is.na(x)))
  l_ind <- 0
  l_coln_na <- c()
  for(x in l_na_cnt_vec){
    l_ind <- l_ind+1
    l_na_score <- x/g_tot_rec
    if(l_na_score<0.1){
      l_coln_na <- c(l_coln_na,g_column_names[l_ind])
    }
  }
  l_df_wo_na <- g_df_ex[,l_coln_na]
  return(l_df_wo_na)
}

fn_pro_fact <- function(){
  
  
  l_df_m <- g_df_wo_na[,1:3]
#  l_df_m$pt_episode_id <- g_df_wo_na$pt_episode_id
#  l_df_m$patient_id <- g_df_wo_na$patient_id
#  l_df_m$encounter_id <- g_df_wo_na$encounter_id 
  
  l_df_m$dx1 <- sapply(g_df_wo_na$dx1,as.numeric)
  l_df_m$dx2 <- sapply(g_df_wo_na$dx2,as.numeric)
  l_df_m$dx3 <- sapply(g_df_wo_na$dx3,as.numeric)  
  l_df_m$dx4 <- sapply(g_df_wo_na$dx4,as.numeric)
  l_df_m$dx5 <- sapply(g_df_wo_na$dx5,as.numeric)
  l_df_m$dx6 <- sapply(g_df_wo_na$dx6,as.numeric)
  l_df_m$dx7 <- sapply(g_df_wo_na$dx7,as.numeric)
  l_df_m$dx8 <- sapply(g_df_wo_na$dx8,as.numeric)
  l_df_m$dx9 <- sapply(g_df_wo_na$dx9,as.numeric)
  l_df_m$dx10 <- sapply(g_df_wo_na$dx10,as.numeric)
  l_df_m$dx11<- sapply(g_df_wo_na$dx11,as.numeric)
  l_df_m$dx12 <- sapply(g_df_wo_na$dx12,as.numeric)
  l_df_m$dx13 <- sapply(g_df_wo_na$dx13,as.numeric)
  l_df_m$dx14 <- sapply(g_df_wo_na$dx14,as.numeric)
  l_df_m$dx15 <- sapply(g_df_wo_na$dx15,as.numeric)
  l_df_m$dx16 <- sapply(g_df_wo_na$dx16,as.numeric)
  l_df_m$dx17 <- sapply(g_df_wo_na$dx17,as.numeric)
  l_df_m$los <- sapply(g_df_wo_na$los,as.numeric)
  l_df_m$los_hours <- sapply(g_df_wo_na$los_hours,as.numeric)
  l_df_m$married <- sapply(g_df_wo_na$married,as.numeric)
  l_df_m$male <- sapply(g_df_wo_na$male,as.numeric)
  l_df_m$episode_cnt <- sapply(g_df_wo_na$episode_cnt,as.numeric)
  #25
  #dt_admit- month of admit
  l_admit_mon <- format(as.Date(g_df_wo_na[,4]),"%m")
  l_df_m$admit_mon <- sapply(l_admit_mon,as.numeric) 
  
  #duration of admission
  l_admit_dt <- as.Date(g_df_wo_na[,4])
  l_dischg_dt <- as.Date(g_df_wo_na[,5])
  l_dur <- difftime(l_dischg_dt,l_admit_dt, units="days")
  l_df_m$admit_dur <- sapply(l_dur,as.numeric) 
  
  #dt_record- month of record
  l_rec_mon <- format(as.Date(g_df_wo_na$dt_record),"%m")
  l_df_m$rec_mon <- sapply(l_rec_mon,as.numeric) 
  
  #code_status
  l_cs <-c()
  for( x in g_df_wo_na$code_status){
    if(x=="full code") l_cs <- c(l_cs,1)  
    else if((x=="dnrdni")) l_cs <- c(l_cs,2) 
    else l_cs <- c(l_cs,3)
  }  
  l_df_m$code_status <- sapply(l_cs,as.numeric)   
    
  #combined_category  
  l_cc <-c()
  for( x in g_df_wo_na$combined_category){
    if(x=="general") l_cc <- c(l_cc,1)  
    else if((x=="peds")) l_cc <- c(l_cc,2) 
    else if((x=="telemetry")) l_cc <- c(l_cc,3)
    else if((x=="transplant")) l_cc <- c(l_cc,4)
    else if((x=="procedure")) l_cc <- c(l_cc,5)
    else if((x=="psychiatry")) l_cc <- c(l_cc,6)
    else if((x=="dialysis")) l_cc <- c(l_cc,7)
    else l_cc <- c(l_cc,8)  
  }  
  l_df_m$combined_category <- sapply(l_cc,as.numeric)  
  #30
  #race 
  l_race <-c()
  for( x in g_df_wo_na$race){
    if(x=="A") l_race <- c(l_race,1)  
    else if((x=="B")) l_race <- c(l_race,2) 
    else if((x=="C")) l_race <- c(l_race,3)
    else if((x=="N")) l_race <- c(l_race,4)
    else if((x=="O")) l_race <- c(l_race,5)
    else if((x=="P")) l_race <- c(l_race,6)
    else if((x=="U")) l_race <- c(l_race,7)
    else if((x=="X")) l_race <- c(l_race,8)
    else l_race <- c(l_race,9)  
  }
  l_df_m$race <- sapply(l_race,as.numeric) 
  
  #ethncity 
  l_ethn <-c()
  for( x in g_df_wo_na$ethncity){
    if(x=="H") l_ethn <- c(l_ethn,1)  
    else if((x=="HA")) l_ethn <- c(l_ethn,2) 
    else if((x=="HC")) l_ethn <- c(l_ethn,3)
    else if((x=="HM")) l_ethn <- c(l_ethn,4)
    else if((x=="HO")) l_ethn <- c(l_ethn,5)
    else if((x=="HP")) l_ethn <- c(l_ethn,6)
    else if((x=="HS")) l_ethn <- c(l_ethn,7)
    else if((x=="N")) l_ethn <- c(l_ethn,8)
    else if((x=="U")) l_ethn <- c(l_ethn,8)
    else if((x=="X")) l_ethn <- c(l_ethn,10)
    else l_ethn <- c(l_ethn,11)  
  }
  l_df_m$ethncity<- sapply(l_ethn,as.numeric) 
  
  #dschgdsp
  l_dschg <- c()
  for( x in g_df_wo_na$dschgdsp){
    if(x=="2") l_dschg <- c(l_dschg,1)  
    else if(x=="21") l_dschg <- c(l_dschg,2)  
    else if(x=="3") l_dschg <- c(l_dschg,3)  
    else if(x=="43") l_dschg <- c(l_dschg,4)  
    else if(x=="61") l_dschg <- c(l_dschg,5)  
    else if(x=="65") l_dschg <- c(l_dschg,6)  
    else if(x=="66") l_dschg <- c(l_dschg,7)
    else if(x=="70") l_dschg <- c(l_dschg,8)  
    else if(x=="A") l_dschg <- c(l_dschg,9)  
    else if(x=="B") l_dschg <- c(l_dschg,10)  
    else if(x=="D") l_dschg <- c(l_dschg,11)  
    else if(x=="E") l_dschg <- c(l_dschg,12)  
    else if(x=="FTR") l_dschg <- c(l_dschg,13)  
    else if(x=="G") l_dschg <- c(l_dschg,14)  
    else if(x=="H") l_dschg <- c(l_dschg,15)  
    else if(x=="J") l_dschg <- c(l_dschg,16)
    else if(x=="K") l_dschg <- c(l_dschg,17)
    else if(x=="L") l_dschg <- c(l_dschg,18)
    else if(x=="M") l_dschg <- c(l_dschg,19)
    else if(x=="O") l_dschg <- c(l_dschg,20)
    else if(x=="T1") l_dschg <- c(l_dschg,21)
    else if(x=="T2") l_dschg <- c(l_dschg,22)
    else if(x=="T3") l_dschg <- c(l_dschg,23)
    else if(x=="T4") l_dschg <- c(l_dschg,24)
    else if(x=="T5") l_dschg <- c(l_dschg,25)
    else if(x=="T6") l_dschg <- c(l_dschg,26)
    else if(x=="T7") l_dschg <- c(l_dschg,27)
    else if(x=="TB") l_dschg <- c(l_dschg,28)
    else if(x=="TA") l_dschg <- c(l_dschg,29)
    else l_dschg <- c(l_dschg,30)  
  }
  l_df_m$dschgdsp<- sapply(l_dschg,as.numeric)
  
  #admsour
  l_admsour <- c()
  for( x in g_df_wo_na$admsour){
    if(x=="1") l_admsour <- c(l_admsour,1)  
    else if(x=="2") l_admsour <- c(l_admsour,2)  
    else if(x=="3") l_admsour <- c(l_admsour,3)  
    else if(x=="4") l_admsour <- c(l_admsour,4) 
    else if(x=="8") l_admsour <- c(l_admsour,5) 
    else if(x=="9") l_admsour <- c(l_admsour,6) 
    else if(x=="A") l_admsour <- c(l_admsour,7) 
    else if(x=="B") l_admsour <- c(l_admsour,8) 
    else if(x=="D") l_admsour <- c(l_admsour,9) 
    else if(x=="E") l_admsour <- c(l_admsour,10) 
    else if(x=="F") l_admsour <- c(l_admsour,11) 
    else if(x=="G") l_admsour <- c(l_admsour,12)
    else if(x=="H") l_admsour <- c(l_admsour,13)
    else if(x=="M") l_admsour <- c(l_admsour,14)
    else if(x=="N") l_admsour <- c(l_admsour,15)
    else if(x=="O") l_admsour <- c(l_admsour,16)
    else l_admsour <- c(l_admsour,17)
  }
  l_df_m$admsour<- sapply(l_admsour,as.numeric)
  #surgery
  l_surgery <- c()
  for( x in g_df_wo_na$surgery){
    if(x=="Y") l_surgery <- c(l_surgery,1) 
    else l_surgery <- c(l_surgery,2)   
  }
  l_df_m$surgery <- sapply(l_surgery,as.numeric)
  #35
  #admtype
  l_admtype <- c()
  for( x in g_df_wo_na$admtype){
    if(x=="E") l_admtype <- c(l_admtype,1)  
    else if(x=="N") l_admtype <- c(l_admtype,2)  
    else if(x=="R") l_admtype <- c(l_admtype,3)  
    else if(x=="U") l_admtype <- c(l_admtype,4) 
    else if(x=="S") l_admtype <- c(l_admtype,5) 
    else l_admtype <- c(l_admtype,6)
  }
  l_df_m$admtype <- sapply(l_admtype,as.numeric)
  
  #marital status
  l_marstat <- c()
  for( x in g_df_wo_na$marstat){
    if(x=="D") l_marstat <- c(l_marstat,1)  
    else if(x=="M") l_marstat <- c(l_marstat,2)  
    else if(x=="P") l_marstat<- c(l_marstat,3)  
    else if(x=="U") l_marstat <- c(l_marstat,4) 
    else if(x=="S") l_marstat<- c(l_marstat,5) 
    else if(x=="W") l_marstat<- c(l_marstat,6) 
    else l_marstat<- c(l_marstat,7)
  }
  l_df_m$marstat <- sapply(l_marstat,as.numeric)
  
  #admit_source
  l_admit_source <- c()
  for( x in g_df_wo_na$admit_source){
    if(x=="ER") l_admit_source <- c(l_admit_source,1)  
    else if(x=="Exclude") l_admit_source <- c(l_admit_source,2)  
    else if(x=="Hospital") l_admit_source<- c(l_admit_source,3)  
    else if(x=="Internal") l_admit_source <- c(l_admit_source,4) 
    else if(x=="T") l_admit_source<- c(l_admit_source,5) 
    else if(x=="Outpatient") l_admit_source<- c(l_admit_source,6) 
    else if(x=="Skilled") l_admit_source<- c(l_admit_source,7) 
    else l_admit_source<- c(l_admit_source,8)
  }
  l_df_m$admit_source <- sapply(l_admit_source,as.numeric)
    
  #discharge
  l_discharge <- c()
  for( x in g_df_wo_na$discharge){
    if(x=="Death") l_discharge <- c(l_discharge,1)  
    else if(x=="Home") l_discharge <- c(l_discharge,2)  
    else if(x=="Home Health") l_discharge<- c(l_discharge,3)  
    else if(x=="Hospice") l_discharge <- c(l_discharge,4) 
    else if(x=="Psych") l_discharge<- c(l_discharge,5) 
    else if(x=="Nursing") l_discharge<- c(l_discharge,6) 
    else if(x=="Skilled") l_discharge<- c(l_discharge,7) 
    else l_discharge<- c(l_discharge,8)
  }
  l_df_m$discharge<- sapply(l_discharge,as.numeric)
  
  #duration of episode
  l_epst_dt <- as.Date(g_df_wo_na$dt_episode_start)
  l_eped_dt <- as.Date(g_df_wo_na$dt_episode_end)
  l_ep_dur <- difftime(l_eped_dt,l_epst_dt, units="days")
  l_df_m$episode_dur<- sapply(l_ep_dur,as.numeric)
  
  #age bracket
  l_agebracket <- c()
  for( x in g_df_wo_na$agebracket){
    if(x=="Between 16 and 25") l_agebracket <- c(l_agebracket,1)  
    else if(x=="Between 3 and 5") l_agebracket <- c(l_agebracket,2)  
    else if(x=="Between 26 and 40") l_agebracket<- c(l_agebracket,3)  
    else if(x=="Between 51 and 65") l_agebracket<- c(l_agebracket,4) 
    else if(x=="Between 41 and 50") l_agebracket<- c(l_agebracket,5) 
    else if(x=="Between 66 and 80") l_agebracket<- c(l_agebracket,6) 
    else if(x=="Greater than 80") l_agebracket<- c(l_agebracket,7) 
    else if(x=="Less than 2") l_agebracket<- c(l_agebracket,8) 
    else l_agebracket<- c(l_agebracket,9)
  }
  l_df_m$agebracket<- sapply(l_agebracket,as.numeric)
  
  l_df_m$dschstan <- g_df_wo_na$dschstan
  #42 
  l_df_m$outcome <- g_df_wo_na$outcome
  l_df_m$outcome_8hr <- g_df_wo_na$outcome_8hr
  l_df_m$outcome_4hr <- g_df_wo_na$outcome_4hr
  l_df_m$outcome_12hr <- g_df_wo_na$outcome_12hr
  l_df_m$outcome_24hr <- g_df_wo_na$outcome_24hr
  
  return (l_df_m)
}  
#main

g_column_names <- c("pt_episode_id","patient_id","encounter_id","dt_admit","dt_discharge","psych_flag","nofloor_flag","rehab_flag","ped_flag","EXCLUDE","dt_record","flowv_sbp","flowv_dbp","flowv_hr","flowv_resp_rate","flowv_spo2","flowv_temp","flowv_supp_oxygen","flowv_height","flowv_weight","flowv_modrass","chpl_sbp","chpl_dbp","chpl_hr","chpl_resp_rate","chpl_spo2","chpl_temp","chpl_supp_oxygen","outpt_weight","adt_fromtable","adt_room","adt_bed","rrt_code","frailty_braden_skin_score","frailty_fall_risk_score","frailty_patient_mobility","frailty_patient_nutrition","frailty_pt_activity_level","frailty_sensory_perception","frailty_getup","lab_device","lab_value_glucose","lab_value_bun","lab_value_creat","lab_value_hc03","lab_value_hemog","lab_value_leuko","lab_value_nphils","lab_value_platelet","lab_value_potas","lab_value_sodium","lab_value_troponin","lab_value_aniongap","lab_value_alk","lab_value_ast","lab_value_bilitot","lab_value_lipase","lab_value_aptt","lab_value_calcion","lab_value_inr","lab_value_lactate","lab_value_magnes","lab_value_ph","lab_value_amylase","lab_value_bilidir","lab_value_phos","lab_value_calcium","lab_value_alt","lab_value_ammonia","lab_value_base","lab_value_breaths","lab_value_o2flow","lab_value_pco2","lab_value_po2","lab_value_albumin","lab_value_sedrate","lab_value_crp","lab_value_egfr","urine_output","meds_drug_class","meds_drug_route","meds_dose_units","meds_drug_name","meds_drug_dose","dialysis_drug_start","code_status","combined_category","det_pt_eligible","unsched_xfer_to_icu","dx1","dx2","dx3","dx4","dx5","dx6","dx7","dx8","dx9","dx10","dx11","dx12","dx13","dx14","dx15","dx16","dx17","race","ethncity","time_since_hospital","dschgdsp","admsour","dschstan","surgery","admtype","marstat","ageyear","los","birthdt","v66_7","married","male","admit_source","discharge","los_hours","sbp","dbp","hr","resp_rate","spo2","temp","supp_oxygen","flowv_modrass_abs","episode_cnt","dt_episode_start","dt_episode_end","outcome","outcome_event","outcome_enc","outcome_enc_event","dt_outcome","minutes_to_outcome1","outcome_8hr","outcome_4hr","outcome_12hr","outcome_24hr","agebracket")

g_df_org <- df3


g_df_ex <- fn_exclude(g_df_org)

g_tot_rec <- nrow(g_df_ex)

g_df_ex[g_df_ex==""] <- NA

g_df_wo_na_w <- fn_wo_na()

#sampling
g_df_wo_na <- g_df_wo_na_w[sample(nrow(g_df_wo_na_w), 5000), ]

g_df_m <- fn_pro_fact()


