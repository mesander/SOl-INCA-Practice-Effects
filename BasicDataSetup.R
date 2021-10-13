#intro script. Not main analyses. 


#librarys to run
  require(psych)
#import main dataset. 
library(haven)
UneditedData_3_8_21 <- read_dta("F:/ext_collaborators/m_sanderson/data/Sets/hchs_sol_inca_reduced.dta")


> names(UneditedData_3_8_21)
[1] "ID"                        "strat_v1"                  "psu_id_v1"                 "pw_v1"                    
[5] "diet_score_jama_v1"        "age_v1"                    "agg_phys_v1"               "agg_ment_v1"              
[9] "race_v1"                   "us_born_v1"                "bmi_v1"                    "clindate_v1"              
[13] "income_v1"                 "income_c5_v1"              "lang_pref_v1"              "yrsus_v1"                 
[17] "hypertension2_v1"          "diabetes2_v1"              "prechd_angina_v1"          "cdcr_v1"                  
[21] "n_hc_v1"                   "nativity_subscore_mesa_v1" "language_subscore_mesa_v1" "bmigrp_c6_v1"             
[25] "bkgrd1_c7_v1"              "centernum_v1"              "gendernum_v1"              "digitsymbol_v1"           
[29] "sevlt_3trials_v1"          "sevlt_recall_v1"           "total_6item_v1"            "frame_cvd_risk_10yr_v1"   
[33] "education_c3_v1"           "cesd10_v1"                 "mets_ncep_v1"              "sash_lang_v1"             
[37] "occupation_long_v1"        "imgen_c4_v1"               "neea8_v1"                  "neea9_v1"                 
[41] "neea10_v1"                 "neea11_v1"                 "neea12_v1"                 "neea13_v1"                
[45] "DIABETES2_V2"              "INCIDENT_DM_V1V2"          "LANGUAGE_SUBSCORE_MESA_V2" "SEVLT_SUM_v2"             
[49] "SEVLT_RECALL_v2"           "NEE10_inca"                "NEE11_inca"                "NEE12_inca"               
[53] "NEE13_inca"                "NEE14_inca"                "NEE15_inca"                "pw_inca"                  
[57] "strat_inca"                "psu_id_inca"               "total_6item_v2"            "digitsymbol_v2"           
[61] "trail_a_t_v2"              "trail_b_t_v2"              "cesd10_v2"                 "pvt_score_age_adj_v2"     
[65] "center_v2"                 "gender_v2"                 "income_v2"                 "income_c5_v2"             
[69] "bmi_v2"                    "bmigrp_c6_v2"              "hypertension2_v2"          "clindate_v2"              
[73] "yrs_btwn_v1v2"             "age_v2"                    "incident_cvd_v1v2"         "education_c3_v2"          
[77] "n_hc_v2"                   "lang_pref_v2"              "sash_lang_v2"              "us_born_v2"               
[81] "yrsus_v2"                  "nativity_subscore_mesa_v2"


#Working dataset

MainData.V1=UneditedData_3_8_21

  #set up  variables, and rename
    #flulency; not certain have v1 and v2 worked out correctly. 
  MainData.V1$fluency.v1=MainData.V1$neea12_v1 +MainData.V1$neea13_v1
  MainData.V1$fluency.v2=MainData.V1$NEE14_inca + MainData.V1$NEE15_inca

    #recall at trial 1; just renaming
  MainData.V1$SEVLT_trial1.v1=MainData.V1$neea8_v1 
  MainData.V1$SEVLT_trial1.v2=MainData.V1$NEE10_inca 
  
  #basic comparisons
  MainData.V1$diff_SEVLT_learn=(MainData.V1$SEVLT_SUM_v2- MainData.V1$sevlt_3trials_v1 )
  MainData.V1$diff_SEVLT_RECALL=(MainData.V1$SEVLT_RECALL_v2- MainData.V1$sevlt_recall_v1 )
  MainData.V1$diff_digitsymbol=(MainData.V1$digitsymbol_v2- MainData.V1$digitsymbol_v1)
  MainData.V1$diff_total_6item=(MainData.V1$total_6item_v2- MainData.V1$total_6item_v1)
  MainData.V1$diff_fluency=(MainData.V1$fluency.v2-MainData.V1$fluency.v1)
  MainData.V1$diff_SEVLT_trial1=(MainData.V1$SEVLT_trial1.v2-MainData.V1$SEVLT_trial1.v1)

  #removes peopel who take test in different langauges
  
  MainData.V1.langdif<-subset(MainData.V1, lang_pref_v1==1 & lang_pref_v2==2 |
                                lang_pref_v1==2 & lang_pref_v2==1)
  
  MainData.V1.langdif$SEVLT_SUM_v2=NA
  MainData.V1.langdif$fluency.v2=NA
  MainData.V1.langdif$SEVLT_RECALL_v2=NA
  MainData.V1.langdif$digitsymbol_v2=NA
  MainData.V1.langdif$total_6item_v2=NA
  MainData.V1.langdif$SEVLT_trial1.v2=NA
  
  
  MainData.V1.b<-MainData.V1[-which(MainData.V1$ID %in% 
                                      MainData.V1.langdif$ID), ]
  
  MainData.V1.c=rbind(MainData.V1.b, MainData.V1.langdif)

  
#FINALIZE DATA
  MainData.SOLINCA=MainData.V1.c
  
  #descriptives
  temp=subset(MainData.V1.spanish, fluency.v1!='NA' & digitsymbol_v1!='NA' & total_6item_v1!='NA' &    sevlt_recall_v1!='NA' & age_v1>39.9)
  describe(temp$age_v1)
  describe(temp$age_v1)
  n=0
  for (n in 0:6) {
    print(((sum(temp$bkgrd1_c7_v1==n, na.rm=TRUE)/6030)*100))
    n<-n+1  
  }
  
  temp.a<-subset(MainData.V1.spanish, fluency.v2!='NA')
  describe(temp.a$age_v2)
  temp.b<-subset(MainData.V1.spanish,  digitsymbol_v2!='NA')
  describe(temp.b$age_v2)
  
  temp.c<-subset(MainData.V1.spanish, total_6item_v2!='NA')
  describe(temp.c$age_v2)
  
  temp.d<-subset(MainData.V1.spanish, SEVLT_RECALL_v2!='NA')
  describe(temp.d$age_v2)
  
  temp.a=subset(MainData.V1.spanish, fluency.v1!='NA' & digitsymbol_v1!='NA' & total_6item_v1!='NA' &    sevlt_recall_v1!='NA')
  
  temp.a=subset(MainData.V1.spanish, fluency.v1!='NA' & digitsymbol_v1!='NA' & total_6item_v1!='NA' &    sevlt_recall_v1!='NA')
  
  
  
  