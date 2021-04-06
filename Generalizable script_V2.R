#General PE script
NOTES
-should be working. 2/24/21





UPDATED MATCH CODE FOR chisq.test(). i HAVE DONE IT FOR MATCH1, AGE, AND EDY. NEED TO DO IT FOR REST OF MATCHES. tHEN ALSO FOR BASELINE MATCHING

as.factor(Matched.data.all$match)),simulate.p.value=TRUE) needs to be set for all chi square. 
cant figure out negative transformations. 


###### in progress ######

{#remove people with perfect scores at both time points, or with zero scores at both
  
  
  #Definition Section. Should be filled out for each test. 
  LowerBound=0         #minimum value for that test. Usually zero. 
  UpperBound=15        #Perfect score on that test. 
  
  #LowerBound should be defined at start of script
  baseline.data.removed.min=subset(Full.Baseline.data, COG_V1==LowerBound, select = c('ID','COG_V1'))
  FollowUp.data.removed.min=subset(Returnee.FollowUp.data, COGCOG==LowerBound, select = c('ID','COGCOG'))
  removedsublist=merge(FollowUp.data.removed.min, baseline.data.removed.min, by='ID')
  removedsublist=subset(removedsublist, select = 'ID')
  
  #'UpperBound' should be defined at start of script. 
  
  baseline.data.removed.max=subset(Full.Baseline.data, COG_V1==LowerBound, select = c('ID','COG_V1'))
  FollowUp.data.removed.max=subset(Returnee.FollowUp.data, COGCOG==LowerBound, select = c('ID','COGCOG'))
  removedsublistMAX=merge(baseline.data.removed.max, FollowUp.data.removed.max, by='ID')
  removedsublistMAX=subset(removedsublistMAX, select = 'ID')
  
  
  #remove people with perfect scores or zero scors at both timepoints. COULD HAVE ALSO JUST DONE THIS ONCE AT BASELINE. 
  #MEANS BASELINE, RETURNEES, AND REPLACEMNTS CANNOT BE SUBS WHO FAILED OUT OF TEST TWICE, OR WHO GOT PERFECT BOTH TIMES.
  if (nrow(removedsublist)!=0) {
    sum1=Returnee.FollowUp.data[which(Returnee.FollowUp.data$ID %in% removedsublist$ID),]
    if (sum1!=0) {
      Returnee.FollowUp.data.r.min=Returnee.FollowUp.data[-which(Returnee.FollowUp.data$ID %in% removedsublist$ID),]
    }
    
    
    sum3=Full.Baseline.data[which(Full.Baseline.data$ID %in% removedsublist$ID),]
    if (sum3!=0) {
      Full.Baseline.data.r.min=Full.Baseline.data[-which(Full.Baseline.data$ID %in% removedsublist$ID),]
    }
    
    sum4=Returnee.baseline.data[which(Returnee.baseline.data$ID %in% removedsublist$ID),]
    if (sum4!=0) {
      Returnee.baseline.data.r.min=Returnee.baseline.data[-which(Returnee.baseline.data$ID %in% removedsublist$ID),]
    }
    
    
    Returnee.baseline.data=Returnee.baseline.data.r.min
    Returnee.FollowUp.data=Returnee.FollowUp.data.r.min
    Full.Baseline.data=Full.Baseline.data.r.min
    
  }
  if (nrow(removedsublistMAX)!=0) {
    sum1=Returnee.FollowUp.data[which(Returnee.FollowUp.data$ID %in% removedsublistMAX$ID),]
    if (sum1!=0) {
      Returnee.FollowUp.data.r.max=Returnee.FollowUp.data[-which(Returnee.FollowUp.data$ID %in% removedsublistMAX$ID),]
    }
    sum3=Full.Baseline.data[which(Full.Baseline.data$ID %in% removedsublistMAX$ID),]
    if (sum3!=0) {
      Full.Baseline.data.r.max=Full.Baseline.data[-which(Full.Baseline.data$ID %in% removedsublistMAX$ID),]
    }
    sum4=Returnee.baseline.data[which(Returnee.baseline.data$ID %in% removedsublistMAX$ID),]
    if (sum4!=0) {
      Returnee.baseline.data.r.max=Returnee.baseline.data[-which(Returnee.baseline.data$ID %in% removedsublistMAX$ID),]
    }
    sum2=Returnee.FollowUp.data[which(Returnee.FollowUp.data$ID %in% removedsublistMAX$ID),]
    if (sum2!=0) {
      Returnee.FollowUp.data=Returnee.FollowUp.data[-which(Returnee.FollowUp.data$ID %in% removedsublistMAX$ID),]
      
      
      Returnee.baseline.data=Returnee.baseline.data.r.max
      Returnee.FollowUp.data=Returnee.FollowUp.data.r.max
      Full.Baseline.data=Full.Baseline.data.r.max
      
      
    }
    
    sum3=Full.Baseline.data[which(Full.Baseline.data$ID %in% removedsublist$ID),]
    if (sum3!=0) {
      baseline.data.v4=Full.Baseline.data[-which(Full.Baseline.data$ID %in% removedsublistMAX$ID),]
    }
    
    sum4=baseline.data.v4[which(baseline.data.v4$ID %in% removedsublist$ID),]
    if (sum4!=0) {
      FollowUp.data.v5=baseline.data.v4[-which(FollowUp.data.v4$ID %in% removedsublistMAX$ID),]
    }
  }
  
  
} #IN progress: Removes perfect scores at both time points.

{
  sender <- "pescriptemail@gmail.com"
  recipients <- c("mesandci@gmail.com")
  send.mail(from = sender,
            to = recipients,
            subject = "PE script complete",
            body = "yay",
            smtp = list(host.name = "smtp.gmail.com", port = 465, 
                        user.name = "pescriptemail@gmail.com",            
                        passwd = "TruckSpinach1", ssl = TRUE),
            authenticate = TRUE,
            send = TRUE)
} #in progress Email completion block. NOt working. 
#______________________________

###############################################################################♀
########################      Practice effect          ########################      
########################      Pseudo-replacements      ########################      
########################      4/1/21                   ########################      
###############################################################################

{
  #load libraries
  library(psych)
  library(MatchIt)
  library(readr)
  library(dplyr)
  library(haven)
  library(tidyverse)
  library(progress)
  library(svMisc)
  #Load main database
  MainData<- read_dta("F:/ext_collaborators/m_sanderson/data/Sets/hchs_sol_inca_reduced.dta")
  MainData$Base_impaired=ifelse(MainData$total_6item_v1<4,0,1)
 
  
  
  
} #SETUP: Pick database.


#--------------------------------------------------------------#
#               Block 1-3 set up databases. Block 1 and 2 require manual edits
{ 
  #read in data
    Data.A1 <- as_tibble(MainData)
  #rename  data variables to match what's in script
  
  Data.B2 <- Data.A1 %>% 
    rename(
      ID     = ID,
      AGE_V1 = age_v1,
      AGE_V2 = age_v2,
      V1DATE = clindate_v1,
      V2DATE = clindate_v2,
      BASEDX = Base_impaired,
      COG_V1 = sevlt_recall_v1,
      COG_V2 = SEVLT_RECALL_v2
    )
  
  
  {
    #EDu is a 3 part variable. 1 = No high school diploma or GED 2 = At most a High school diploma or GED
    #Gender: 0=female, 1=male
    #pvt_score_age_adj_v2:  Age Adjusted picture vocabulary test Score AT V2, NOT ADMIN AT V1
    #BASEDX needs to be binary variable, with 1 Being what you want to select for. 
    } #Notes about variables
  {          
    #SEVLT_trial1.v1
    #SEVLT_trial1.v2
    
    #sevlt_3trials_v1?
    #SEVLT_SUM_v2
    
    #sevlt_recall_v1
    #SEVLT_RECALL_v2
    
    #digitsymbol_v2
    #digitsymbol_v2
    
    #total_6item_v2
    #total_6item_v2
    
    #fluency.v1
    #fluency.v2
    
    
  }  #list of cog variables  
  
} #Block 1: Sets up initial databases, renames variabels

{
  
  #Unique matching variables for this dataset
  Match.Xe=subset(Data.B2, select = c('ID','education_c3_v1', 'gendernum_v1','centernum_v1',
                                      'bkgrd1_c7_v1','nativity_subscore_mesa_v1','income_c5_v1'))
  Names.Match.Xe=names(Match.Xe)
  Names.Match.Xe[1]="AGE"

  Match.Xe <- Match.Xe %>% 
    rename(
      EDU    = education_c3_v1,
      Match1 = gendernum_v1,
      Match2 = centernum_v1,
      Match3 = bkgrd1_c7_v1,
      Match4 = nativity_subscore_mesa_v1,
      Match5 = income_c5_v1
      
    )
  #If any Match XE need to be factors, convert them below with as.factor()
  Match.Xe$EDU=as.factor(Match.Xe$EDU)
  Match.Xe$Match1=as.factor(Match.Xe$Match1)
  Match.Xe$Match2=as.factor(Match.Xe$Match2)
  Match.Xe$Match3=as.factor(Match.Xe$Match3)
  Match.Xe$Match4=as.factor(Match.Xe$Match4)
  Match.Xe$Match5=as.factor(Match.Xe$Match5)
  
  
  Match.Xe.reduced=na.omit(Match.Xe) #removes anyonw with missing matching variables
  
  #  PREIQ  = pvt_score_age_adj_v2,
  #  Match2 = income_c5_v1,
  #  Match3 = bkgrd1_c7_v1,
  #  Match4 = us_born_v1,
  #  Match5 = nativity_subscore_mesa_v1,
  #  Match6 = agg_ment_v1,
  #  Match7 = agg_phys_v1,
  #  Match8 = language_subscore_mesa_v1
  
  #MATCHXE=subset(Data.V3, select = c('ID','Match1','Match2','Match3','Match4','Match5','Match6',
  #                                   'Match7','Match8','PREIQ','EDU','GENDER'))
  { 
    #Centernum= Numeric variable, city of origin: 1= Bronx, 2= Chicago, 3= Miami, 4= San Diego.
    #income_c5_v1 = income at V1. "1 = Less than $10,000
    #2 = $10,001-$20,000
    #3 = $20,001-$40,000
    #4 = $40,001-$75,000
    #5 = More than $75,000
    #2=$30,000 or more
    #3=Missing"
    #Bkgrd1_C7: 0 = Dominican 1 = Central American 2 = Cuban 3 = Mexican 4 = Puerto Rican
    #5 = South American 6 = More than one/Other heritage
    #US_Born: 0 = not born in US, 1 = Born in US
    #nativity_subscore_mesa_v1: Possible values are integers from 0 to 3 or missing. 0 indicates the lowest level of acculturation, 3 is the highest.
    #agg_ment_v1: aggregate mental health score; norm based, Z score, mean of 50 SD of 10
    #Agg_phys: aggregate physical health score; norm based, Z score, mean of 50 SD of 10
    #Language_Subscore_MESA:possible values from 0 to 2 in 0.5 increments or missing. 
    # 0 indicates the lowest level of acculturation, 2 ?? the highest.
  } #details on matching variables
  
} #Block 2: Identifies matching Xe. NOTE-listwise removal of anyone with NA value in matching Xe.

{
  #creates True V2 age
  Data.B2$Day_dif_visits=(as.Date(Data.B2$V2DATE) - as.Date(Data.B2$V1DATE)) #must be days different in visits. 
  Data.B2$True_Age_FU= as.numeric((Data.B2$AGE_V1+(Data.B2$Day_dif_visits/365)))
  #create baseline dataset with peopel who have that cog variable. And meet DX crieria.
  Baseline.Data.A1=subset(Data.B2, COG_V1!='NA' & BASEDX==1)
  #create follow-up dataset with peopel who have that cog variable at basleine and FU. Renames Cog variable [COGCOG]
  FollowUp.Data.A1=subset(Data.B2, COG_V2!='NA' & COG_V1!='NA', select='ID')
  FollowUp.Data.B2=merge(Baseline.Data.A1, FollowUp.Data.A1, by='ID')
  
  
  
  #rename variables for script
  Full.Baseline=subset(Baseline.Data.A1, select = c('ID','COG_V1','AGE_V1'))
  Full.Baseline <- Full.Baseline %>% 
    rename(
      COGCOG = COG_V1,
      AGE=AGE_V1
    )
  Returnee.Baseline=subset(FollowUp.Data.B2, select = c('ID','COG_V1','AGE_V1'))
  Returnee.Baseline <- Returnee.Baseline %>% 
    rename(
      COGCOG = COG_V1,
      AGE=AGE_V1
      
    )
  
  Returnee.FollowUp=subset(FollowUp.Data.B2, select = c('ID','COG_V2','AGE_V2'))
  Returnee.FollowUp <- Returnee.FollowUp %>% 
    rename(
      COGCOG = COG_V2,
      AGE=AGE_V2
    )
  
  #create Databases used by script. Reduces samples to those with Matching Xe.
  Returnee.baseline.data=merge(Returnee.Baseline, Match.Xe.reduced, by = 'ID')
  Returnee.FollowUp.data=merge(Returnee.FollowUp, Match.Xe.reduced, by = 'ID')
  Full.Baseline.data=merge(Full.Baseline, Match.Xe.reduced, by = 'ID')
  #create ID only lists.
  Full.Baseline.ID=Full.Baseline.data$ID
  Returnee.Baseline.ID=Returnee.baseline.data$ID
  Returnee.FollowUp.ID=Returnee.FollowUp.data$ID 
  #create attrition database [note this is not the PR sample. Though they can be in it]
  Attritors.baseline=Full.Baseline.data[-which(Full.Baseline.data$ID %in% Returnee.baseline.data$ID),]
  
} #Block 3: Selects for DX at baseline. creates Full Baseline sample. Creates FU sample with only returnees. Restricts sample to those with matching Xe

#--------CHECK DISTRIBUTIONS CHOSE T/F TO TRANSFORM DATA--------#
          # skew.sum provides "skew" 
          # from describe() for each dataset
{
  #skew for returnee baseline data
  MAX.BASE=max(Returnee.baseline.data$COGCOG)
  none.transform=describe(Returnee.baseline.data$COGCOG)
  skew.p.sqrt=describe(sqrt(Returnee.baseline.data$COGCOG))
  skew.p.log=describe(log(Returnee.baseline.data$COGCOG+3))
  skew.p.1_log=describe(1/log(Returnee.baseline.data$COGCOG+3))
  skew.n.sqrt=describe(sqrt(MAX.BASE+1-Returnee.baseline.data$COGCOG))
  skew.n.log=describe(log(MAX.BASE+1-Returnee.baseline.data$COGCOG))
  skew.n.1_log=describe(1/(MAX.BASE+1-Returnee.baseline.data$COGCOG))
  skew.sum=data.frame(No_transformation = none.transform$skew, 
                      pos_skew_sqrt = skew.p.sqrt$skew,
                      pos_skew_log  = skew.p.log$skew,
                      pos_skew_1_div_log  = skew.p.1_log$skew,
                      neg_skew_sqrt  = skew.n.sqrt$skew,
                      neg_skew_log  = skew.n.log$skew,
                      neg_skew_1_div_log  = skew.n.1_log$skew)
  #skew for returnee followup data
  MAX.RFU=max(Returnee.FollowUp.data$COGCOG)
  none.transform=describe(Returnee.FollowUp.data$COGCOG)
  skew.p.sqrt=describe(sqrt(Returnee.FollowUp.data$COGCOG))
  skew.p.log=describe(log(Returnee.FollowUp.data$COGCOG+3))
  skew.p.1_log=describe(1/log(Returnee.FollowUp.data$COGCOG+3))
  skew.n.sqrt=describe(sqrt(MAX.RFU+1-Returnee.FollowUp.data$COGCOG))
  skew.n.log=describe(log(MAX.RFU+1-Returnee.FollowUp.data$COGCOG))
  skew.n.1_log=describe(1/(MAX.RFU+1-Returnee.FollowUp.data$COGCOG))
  skew.sum.FU.v2=data.frame(No_transformation = none.transform$skew, 
                            pos_skew_sqrt = skew.p.sqrt$skew,
                            pos_skew_log  = skew.p.log$skew,
                            pos_skew_1_div_log  = skew.p.1_log$skew,
                            neg_skew_sqrt  = skew.n.sqrt$skew,
                            neg_skew_log  = skew.n.log$skew,
                            neg_skew_1_div_log  = skew.n.1_log$skew)
  #scew for whole sample
  MAX.AllB=max(Full.Baseline.data$COGCOG)
  none.transform=describe(Full.Baseline.data$COGCOG)
  skew.p.sqrt=describe(sqrt(Full.Baseline.data$COGCOG))
  skew.p.log=describe(log(Full.Baseline.data$COGCOG+3))
  skew.p.1_log=describe(1/log(Full.Baseline.data$COGCOG+3))
  skew.n.sqrt=describe(sqrt(MAX.AllB+1-Full.Baseline.data$COGCOG))
  skew.n.log=describe(log(MAX.AllB+1-Full.Baseline.data$COGCOG))
  skew.n.1_log=describe(1/(MAX.AllB+1-Full.Baseline.data$COGCOG))
  skew.sum.allbaseline=data.frame(No_transformation = none.transform$skew, 
                                  pos_skew_sqrt = skew.p.sqrt$skew,
                                  pos_skew_log  = skew.p.log$skew,
                                  pos_skew_1_div_log  = skew.p.1_log$skew,
                                  neg_skew_sqrt  = skew.n.sqrt$skew,
                                  neg_skew_log  = skew.n.log$skew,
                                  neg_skew_1_div_log  = skew.n.1_log$skew)
  
  
} #Defines skew.sum

      skew.sum
      skew.sum.allbaseline
      skew.sum.FU.v2
#histograms of non-transformed data. Look for outliers
    hist(Returnee.baseline.data$COGCOG)
    hist(Returnee.FollowUp.data$COGCOG)
    hist(Full.Baseline.data$COGCOG)
    hist(Attritors.baseline$COGCOG)
# use Block 4 to transform the data by changing appropriate variabel from FALSE to TRUE
    
{
  #skew options.
  #if do not want to transform data, leave all as false. 
  
  #POSITIVE SKEW OPTIONS
  hist(sqrt(Returnee.baseline.data$COGCOG))
  transform.sqrt=FALSE
  hist(log(Returnee.baseline.data$COGCOG+3))
  transform.log=FALSE   # if want do straight log transformation (+1), mark TRUE
  hist(1/log(Returnee.baseline.data$COGCOG+3))
  transform.1log=FALSE  # if want do 1/log transfomration (+1), mark TRUE. 
  
  #NEGATIVE SKEW OPTIONS
  MAX=max(Returnee.baseline.data$COGCOG)
  hist(sqrt(MAX+1)-Returnee.baseline.data$COGCOG)
  transform.sqrt.NEG=FALSE
  hist(log(MAX+1)-Returnee.baseline.data$COGCOG)
  transform.log.NEG=FALSE   # if want do straight log transformation (+1), mark TRUE
  hist(1/(MAX+1)-Returnee.baseline.data$COGCOG)
  transform.1log.NEG=FALSE  # if want do 1/log transfomration (+1), mark TRUE. 
  
  #if  positive skew
  if (transform.sqrt==TRUE) {
    Returnee.baseline.data$COGCOG=sqrt(Returnee.baseline.data$COGCOG+3)
    Returnee.FollowUp.data$COGCOG=sqrt(Returnee.FollowUp.data$COGCOG+3)
    Full.Baseline.data$COGCOG=sqrt(Full.Baseline.data$COGCOG+3)
    Attritors.baseline$COGCOG=sqrt(Attritors.baseline$COGCOG+3)
    hist(Returnee.baseline.data$COGCOG)
    hist(Returnee.FollowUp.data$COGCOG)
    hist(Full.Baseline.data$COGCOG)
    hist(Attritors.baseline$COGCOG)
  }
  #if moderate positive skew
  if (transform.log==TRUE) {
    Returnee.baseline.data$COGCOG=log(Returnee.baseline.data$COGCOG+3)
    Returnee.FollowUp.data$COGCOG=log(Returnee.FollowUp.data$COGCOG+3)
    Full.Baseline.data$COGCOG=log(Full.Baseline.data$COGCOG+3)
    Attritors.baseline$COGCOG=log(Attritors.baseline$COGCOG+3)
    hist(Returnee.baseline.data$COGCOG)
    hist(Returnee.FollowUp.data$COGCOG)
    hist(Full.Baseline.data$COGCOG)
    hist(Attritors.baseline$COGCOG)
    
    
  }
  #if heavy positive skew
  if (transform.1log==TRUE) {
    Returnee.baseline.data$COGCOG=1/log(Returnee.baseline.data$COGCOG+3)
    Returnee.FollowUp.data$COGCOG=1/log(Returnee.FollowUp.data$COGCOG+3)
    Full.Baseline.data$COGCOG=1/log(Full.Baseline.data$COGCOG+3)
    Attritors.baseline$COGCOG=1/log(Attritors.baseline$COGCOG+3)
    hist(Returnee.baseline.data$COGCOG)
    hist(Returnee.FollowUp.data$COGCOG)
    hist(Full.Baseline.data$COGCOG)
    hist(Attritors.baseline$COGCOG)
  }
  
  #if  NEGATIVE skew
  if (transform.sqrt.NEG==TRUE) {
    
    Returnee.baseline.data$COGCOG=sqrt(max(Returnee.baseline.data$COGCOG+1)-Returnee.baseline.data$COGCOG)
    Returnee.FollowUp.data$COGCOG=sqrt(max(Returnee.FollowUp.data$COGCOG+1)-Returnee.FollowUp.data$COGCOG)
    Full.Baseline.data$COGCOG=sqrt(max(Full.Baseline.data$COGCOG+1)-Full.Baseline.data$COGCOG)
    Attritors.baseline$COGCOG=sqrt(max(Attritors.baseline$COGCOG+1)-Attritors.baseline$COGCOG)
    hist(Returnee.baseline.data$COGCOG)
    hist(Returnee.FollowUp.data$COGCOG)
    hist(Full.Baseline.data$COGCOG)
    hist(Attritors.baseline$COGCOG)
    
    
  }  #not finished output code
  #if moderate negative skew
  if (transform.log.NEG==TRUE) {
    Returnee.baseline.data$COGCOG=log(max(Returnee.baseline.data$COGCOG+1)-Returnee.baseline.data$COGCOG)
    Returnee.FollowUp.data$COGCOG=log(max(Returnee.FollowUp.data$COGCOG+1)-Returnee.FollowUp.data$COGCOG)
    Full.Baseline.data$COGCOG=log(max(Full.Baseline.data$COGCOG+1)-Full.Baseline.data$COGCOG)
    Attritors.baseline$COGCOG=log(max(Attritors.baseline$COGCOG+1)-Attritors.baseline$COGCOG)
    hist(Returnee.baseline.data$COGCOG)
    hist(Returnee.FollowUp.data$COGCOG)
    hist(Full.Baseline.data$COGCOG)
    hist(Attritors.baseline$COGCOG)
    
    
  }  #not finished output code
  #if heavy negative skew
  if (transform.1log.NEG==TRUE) {
    Returnee.baseline.data$COGCOG=1/(max(Returnee.baseline.data$COGCOG+1)-Returnee.baseline.data$COGCOG)
    Returnee.FollowUp.data$COGCOG=1/(max(Returnee.FollowUp.data$COGCOG+1)-Returnee.FollowUp.data)
    Full.Baseline.data$COGCOG=1/(max(Full.Baseline.data$COGCOG+1)-Full.Baseline.data)
    Attritors.baseline$COGCOG=1/(max(Attritors.baseline$COGCOG+1)-Attritors.baseline)
    hist(Returnee.baseline.data$COGCOG)
    hist(Returnee.FollowUp.data$COGCOG)
    hist(Full.Baseline.data$COGCOG)
    hist(Attritors.baseline$COGCOG)
  } #not finished output code
  
  TRANSFORMED=data.frame(
    'transform.sqrt'=transform.sqrt,
    'transform.log'=transform.log,
    'transform.1log'=transform.1log,
    'transform.sqrt.NEG'=transform.sqrt.NEG,
    'transform.log.NEG'=transform.log.NEG,
    'transform.1log.NEG'=transform.1log.NEG
  )

  
  
}# Block 4: check normality, adjust tranformation 

#---------------------Block 5 records settings-----------------------------#  
{
  
  #select sample sizes
  N.returnee=100  #number of returnees in each iteration
  N.PR.Pool=400   #sets minimal number of subjects in potential PR pool. 
  N.PR.Start=1000  #number of PR to initially randomly sample. 
  N.base.pool=1000 #sets minimum number of subjects in potential baseline pool
  N.base.start=400    #number of base to initially randomly sample. 
  baseline.match.age.only=TRUE #this makes it so the baseline is age matched at each iteration only. Not matched to returnees on anythigne else. 
  
  #select number of iterations
  N.iteration=5001
  PE=1
  #select P value level for group testing to confirm Matchit correctly assigned groups
  value.match=.8
  
  #Loop counters
  PR.match.count=250 #number of attempts for matching PR to Returnees
  base.match.count=250 #number of attempts for matching baseline to returnees
  
  
  #rests saved databsaes
  Bootstrap.data=data.frame(Pract.effect=1000)
  Practice.Effect=data.frame(Pract.effect=1000)
  Attrition.Effect=data.frame(Pract.effect=1000)
  Mean.Bootstrap.data=data.frame('baseline'=1000,
                                 'baseline.SD'=1000,
                                 'returnee.baseline'=1000,
                                 'returnee.baseline.SD'=1000,
                                 'returnee.FU'=1000, 
                                 'returnee.FU.SD'=1000,
                                 'replcacement'=1000,
                                 'replcacement.SD'=1000)
  
  databaselist.returnee.followup=list()
  databaselist.replacement.baseline=list()
  databaselist.basleine=list()
  databaselist.returnee.baseline=list()
  
  Difference.Score.Iteration=NULL
  Attrition.Effect.Iteration=NULL
  Practice.Effect.Iteration=NULL
  Mean.Bootstrap.data.final=NULL
  Practice.Effect.final=NULL
  
  Pe.effect.table=NULL
  #coULD REDO THI WITH ROWNAMES OPTION IN DAT.FRAME THEN WOULD HAVE 1 COLUMN WITH LOTS ROWS, THAT'S MORE' READABLE
  list.settings=data.frame("N returnee selected"=N.returnee,
                           "N potential replacments"=N.PR.Pool,
                           "N replacments start match with"=N.PR.Start,
                           'Max attempts to match returrnee-replacments'=PR.match.count,
                           "N potential baseline subs"=N.base.pool,
                           'N potential baseline start with'=N.base.start,
                           'Max attempts to match returrnee-baseline'=base.match.count,
                           'Max iterations'=N.iteration,
                           "Iterations achieved"=PE,
                           "P value match"=value.match,
                           "baseline matched to only age"=baseline.match.age.only
  )
  list.settings$transformed=ifelse(transform.sqrt.NEG==TRUE, "sqrt(max(COGCOG+1)-COGCOG)",
                                   ifelse(transform.log.NEG==TRUE, "log(max(COGCOG+1)-COGCOG)",
                                          ifelse(transform.1log.NEG==TRUE, "1/(max(COGCOG+1)-COGCOG)",
                                                 ifelse(transform.sqrt==TRUE, "sqrt(COGCOG+3)",
                                                        ifelse(transform.log==TRUE, "log(COGCOG+3)",
                                                               ifelse(transform.1log==TRUE, "1/log(COGCOG+3)",
                                                                      ifelse(transform.sqrt.NEG==FALSE &
                                                                               transform.log.NEG==FALSE  &
                                                                               transform.1log.NEG==FALSE &
                                                                               transform.sqrt==FALSE     &
                                                                               transform.log==FALSE &
                                                                               transform.1log==FALSE, "Data was not transformed for analysis; assume normality", "ERROR IN TRANFORMATION"))) 
                                                 )  
                                          )
                                   ))
  
  
  #prints out settings

  
}# Block 5: define parameters, record values in "settings." Resets temporary databases used by main script

#displays settings; check prior to running main script#

print(TRANSFORMED)
list.settings

#--------------------------this section runs the program. ------------------------------------#  
ptm <- proc.time() #starts timer. 
while (PE<N.iteration) {
  {
  n=(ncol(Match.Xe)-2) #sets the number of match varaibles. See matchit() in main script. 
  base.print=NULL
  count<-0
  success.age=0
  success.match=0
  success.Match1=0
  success.Match2=0
  success.Match3=0
  success.Match4=0
  success.Match5=0
  success.Match6=0
  success.Match7=0
  success.Match8=0
  success.Match9=0
  success.Match10=0
  total.count=0
  baseline.count=0
  success <- 0
  baseline.success=0
  base.selection=TRUE
  restart=FALSE #sets up matching fail condition
  PR.selection=TRUE
  Matched.baseline.chosen=NULL
  Returnee.Baseline.Iteration=NULL
  Returnee.FollowUp.Iteration=NULL
  Pseudo.Replace.Iteration=NULL
  
  }#resets conditions for all loops
  
  while (success==0) {
    #selection a random pool of returnees
    r.sample.returnees <- Returnee.FollowUp.data[sample(nrow(Returnee.FollowUp.data), N.returnee), ] # starting sample size
    r.sample.returnees$match=1
    r.sample.returnees.ID=subset(r.sample.returnees, select = c('ID','match','AGE','COGCOG'))
    #Pseudo-replacments: end up with an a pool that is mostly random selected, except within age range of returnees
    #has loop in it that defines number of people necessaqry for PR pool
    if (PR.selection==TRUE) {
      Potential.PR.V1=Full.Baseline.data[-which(Full.Baseline.data$ID %in% r.sample.returnees$ID),]
      age_bounds=describe(r.sample.returnees$AGE)
      u_bound=age_bounds$max  
      l_bound=age_bounds$min
      Potential.PR.V1$keep=ifelse(Potential.PR.V1$AGE>u_bound,
                                  -1,
                                  ifelse(Potential.PR.V1$AGE<l_bound,
                                         -1,1))
      
      Potential.PR.V2=subset(Potential.PR.V1, keep==1) #removes anyone outside age range
      if (nrow(Potential.PR.V2)>N.PR.Pool) {
        PR.selection=FALSE
      }
    }
    
    Potential.PR.V3 <- Potential.PR.V2[sample(nrow(Potential.PR.V2), N.PR.Start), ] # starting sample size
    Potential.PR.V3$match=0
    Potential.PR.V3$keep=NULL
    #Potential.PR.ID=subset(Potential.PR.V3, select = c('ID','match','AGE','COGCOG'))
    #combine datasets so can use MatchIt package. 'match" is IDifying Xe [0=returnee, 1=PR]
    matching=rbind(Potential.PR.V3, r.sample.returnees)

    { 
      
      if (n==1) {
        m.out <- matchit(match ~ Match1+AGE+EDU, method='nearest',  data = matching, link='probit')
        
        Matched.data.all=match.data(m.out)
        Matched.data.PR=subset(Matched.data.all, match==0)
        Matched.data.Return=subset(Matched.data.all, match==1)
        
        #Tests to leave loop
        if (class(Matched.data.all$EDU)=="factor") {
          edu.test=chisq.test(table(Matched.data.all$EDU, as.factor(Matched.data.all$match)),simulate.p.value=TRUE)
          success.edu=ifelse(edu.test$p.value>value.match, 1, 0)
        }
        if (class(Matched.data.all$EDU)=="numeric") {
          edu.test=wilcox.test(Matched.data.PR$EDU, Matched.data.Return$EDU)
          success.edu=ifelse(edu.test$p.value>value.match, 1, 0)
        }
        
        if (class(Matched.data.all$AGE)=="factor") {
          age.test=chisq.test(table(Matched.data.all$AGE, as.factor(Matched.data.all$match)),simulate.p.value=TRUE)
          success.age=ifelse(age.test$p.value>value.match, 1, 0)
        }
        if (class(Matched.data.all$AGE)=="numeric") {
          age.test=wilcox.test(Matched.data.PR$AGE, Matched.data.Return$AGE)
          success.age=ifelse(age.test$p.value>value.match, 1, 0)
        }
        
        if (class(Matched.data.all$Match1)=='factor') {
         match1.test=chisq.test(table(Matched.data.all$Match1, as.factor(Matched.data.all$match)),simulate.p.value=TRUE)
           success.Match1=ifelse(match1.test$p.value>value.match, 1, 0)

        }
        if (class(Matched.data.all$Match1)=='numeric') {
          match1.test=wilcox.test(Matched.data.PR$Match1, Matched.data.Return$Match1) 
          success.Match1=ifelse(match1.test$p.value>value.match, 1, 0)
        }
        
        #Leave loop conditions
        success.match=ifelse(nrow(Matched.data.PR)>(.9*N.returnee), 1, 0)
        
        #exit code
        success=ifelse(success.match==1 & success.age==1 & success.edu==1 &   success.Match1==1, 1,0)
        
      } #updated
      if (n==2) {
        m.out <- matchit(match ~ Match1+AGE+EDU + Match2, method='nearest',  data = matching, link='probit')
        
        Matched.data.all=match.data(m.out)
        Matched.data.PR=subset(Matched.data.all, match==0)
        Matched.data.Return=subset(Matched.data.all, match==1)
        if (class(Matched.data.all$Match1)=='factor') {
          match1.test=chisq.test(table(Matched.data.all$Match1, as.factor(Matched.data.all$match)),simulate.p.value=TRUE)
          success.Match1=ifelse(match1.test$p.value>value.match, 1, 0)
          
        }
        if (class(Matched.data.all$Match1)=='numeric') {
          match1.test=wilcox.test(Matched.data.PR$Match1, Matched.data.Return$Match1) #Match 3 is always numerical
          success.Match1=ifelse(match1.test$p.value>value.match, 1, 0)
        }
        
        if (class(Matched.data.all$Match2)=='factor') {
          match2.test=chisq.test(table(Matched.data.all$Match2, as.factor(Matched.data.all$match)),simulate.p.value=TRUE)
          success.Match2=ifelse(match2.test$p.value>value.match, 1, 0)
          
        }
        if (class(Matched.data.all$Match2)=='numeric') {
          match2.test=wilcox.test(Matched.data.PR$Match2, Matched.data.Return$Match2) #Match 3 is always numerical
          success.Match2=ifelse(match2.test$p.value>value.match, 1, 0)
        }
        
        if (class(Matched.data.all$EDU)=="factor") {
          edu.test=chisq.test(table(Matched.data.all$EDU, as.factor(Matched.data.all$match)),simulate.p.value=TRUE)
          success.edu=ifelse(edu.test$p.value>value.match, 1, 0)
        }
        if (class(Matched.data.all$EDU)=="numeric") {
          edu.test=wilcox.test(Matched.data.PR$EDU, Matched.data.Return$EDU)
          success.edu=ifelse(edu.test$p.value>value.match, 1, 0)
        }
        
        if (class(Matched.data.all$AGE)=="factor") {
          age.test=chisq.test(table(Matched.data.all$AGE, as.factor(Matched.data.all$match)),simulate.p.value=TRUE)
          success.age=ifelse(age.test$p.value>value.match, 1, 0)
        }
        if (class(Matched.data.all$AGE)=="numeric") {
          age.test=wilcox.test(Matched.data.PR$AGE, Matched.data.Return$AGE)
          success.age=ifelse(age.test$p.value>value.match, 1, 0)
        } 
        #Leave loop conditions
        success.match=ifelse(nrow(Matched.data.PR)>(.9*N.returnee), 1, 0)
        #exit code
        success=ifelse(success.match==1 & success.age==1 & success.edu==1 &  success.Match1==1 & success.Match2==1, 1,0)
        
      } #updated
      if (n==3) {
        m.out <- matchit(match ~ Match1+AGE+EDU + Match2+ Match3, method='nearest',  data = matching, link='probit')
        
        Matched.data.all=match.data(m.out)
        Matched.data.PR=subset(Matched.data.all, match==0)
        Matched.data.Return=subset(Matched.data.all, match==1)
        
        if (class(Matched.data.all$EDU)=="factor") {
          edu.test=chisq.test(table(Matched.data.all$EDU, as.factor(Matched.data.all$match)),simulate.p.value=TRUE)
          success.edu=ifelse(edu.test$p.value>value.match, 1, 0)
        }
        if (class(Matched.data.all$EDU)=="numeric") {
          edu.test=wilcox.test(Matched.data.PR$EDU, Matched.data.Return$EDU)
          success.edu=ifelse(edu.test$p.value>value.match, 1, 0)
        }
        
        if (class(Matched.data.all$AGE)=="factor") {
          age.test=chisq.test(table(Matched.data.all$AGE, as.factor(Matched.data.all$match)),simulate.p.value=TRUE)
          success.age=ifelse(age.test$p.value>value.match, 1, 0)
        }
        if (class(Matched.data.all$AGE)=="numeric") {
          age.test=wilcox.test(Matched.data.PR$AGE, Matched.data.Return$AGE)
          success.age=ifelse(age.test$p.value>value.match, 1, 0)
        }
        
        if (class(Matched.data.all$Match1)=='factor') {
          match1.test=chisq.test(table(Matched.data.all$Match1, as.factor(Matched.data.all$match)),simulate.p.value=TRUE)
          success.Match1=ifelse(match1.test$p.value>value.match, 1, 0)
          
        }
        if (class(Matched.data.all$Match1)=='numeric') {
          match1.test=wilcox.test(Matched.data.PR$Match1, Matched.data.Return$Match1) #Match 3 is always numerical
          success.Match1=ifelse(match1.test$p.value>value.match, 1, 0)
        }
        
        if (class(Matched.data.all$Match2)=='factor') {
          match2.test=chisq.test(table(Matched.data.all$Match2, as.factor(Matched.data.all$match)),simulate.p.value=TRUE)
          success.Match2=ifelse(match2.test$p.value>value.match, 1, 0)
          
        }
        if (class(Matched.data.all$Match2)=='numeric') {
          match2.test=wilcox.test(Matched.data.PR$Match2, Matched.data.Return$Match2) #Match 3 is always numerical
          success.Match2=ifelse(match2.test$p.value>value.match, 1, 0)
        }
        
        if (class(Matched.data.all$Match3)=='factor') {
          match3.test=chisq.test(table(Matched.data.all$Match3, as.factor(Matched.data.all$match)),simulate.p.value=TRUE)
          success.Match1=ifelse(match3.test$p.value>value.match, 1, 0)
          
        }
        if (class(Matched.data.all$Match3)=='numeric') {
          match3.test=wilcox.test(Matched.data.PR$Match3, Matched.data.Return$Match3) #Match 3 is always numerical
          success.Match3=ifelse(match3.test$p.value>value.match, 1, 0)
        }  
        
        
        #Leave loop conditions
        success.match=ifelse(nrow(Matched.data.PR)==(nrow(Matched.data.Return)), 1, 0)         
        #exit code
        success=ifelse(success.match==1 & success.age==1 & success.edu==1 &  
                         success.Match1==1 & 
                         success.Match2==1 &
                         success.Match3==1, 1,0)
      } #updated
      if (n==4) {
        m.out <- matchit(match ~ Match1+AGE+EDU + Match2+ Match3+ Match4, method='nearest',  data = matching, link='probit')
        
        Matched.data.all=match.data(m.out)
        Matched.data.PR=subset(Matched.data.all, match==0)
        Matched.data.Return=subset(Matched.data.all, match==1)
        
        if (class(Matched.data.all$EDU)=="factor") {
          edu.test=chisq.test(table(Matched.data.all$EDU, as.factor(Matched.data.all$match)),simulate.p.value=TRUE)
          success.edu=ifelse(edu.test$p.value>value.match, 1, 0)
        }
        if (class(Matched.data.all$EDU)=="numeric") {
          edu.test=wilcox.test(Matched.data.PR$EDU, Matched.data.Return$EDU)
          success.edu=ifelse(edu.test$p.value>value.match, 1, 0)
        }
        
        if (class(Matched.data.all$AGE)=="factor") {
          age.test=chisq.test(table(Matched.data.all$AGE, as.factor(Matched.data.all$match)),simulate.p.value=TRUE)
          success.age=ifelse(age.test$p.value>value.match, 1, 0)
        }
        if (class(Matched.data.all$AGE)=="numeric") {
          age.test=wilcox.test(Matched.data.PR$AGE, Matched.data.Return$AGE)
          success.age=ifelse(age.test$p.value>value.match, 1, 0)
        }
        
        if (class(Matched.data.all$Match1)=='factor') {
          match1.test=chisq.test(table(Matched.data.all$Match1, as.factor(Matched.data.all$match)),simulate.p.value=TRUE)
          success.Match1=ifelse(match1.test$p.value>value.match, 1, 0)
          
        }
        if (class(Matched.data.all$Match1)=='numeric') {
          match1.test=wilcox.test(Matched.data.PR$Match1, Matched.data.Return$Match1) #Match 3 is always numerical
          success.Match1=ifelse(match1.test$p.value>value.match, 1, 0)
        }
        
        if (class(Matched.data.all$Match2)=='factor') {
          match2.test=chisq.test(table(Matched.data.all$Match2, as.factor(Matched.data.all$match)),simulate.p.value=TRUE)
          success.Match2=ifelse(match2.test$p.value>value.match, 1, 0)
          
        }
        if (class(Matched.data.all$Match2)=='numeric') {
          match2.test=wilcox.test(Matched.data.PR$Match2, Matched.data.Return$Match2) #Match 3 is always numerical
          success.Match2=ifelse(match2.test$p.value>value.match, 1, 0)
        }
        
        if (class(Matched.data.all$Match3)=='factor') {
          match3.test=anova(lm(match~Match3, data=Matched.data.all))
          success.Match3=ifelse(match3.test$`Pr(>F)`[1]>value.match, 1, 0)
          
        }
        if (class(Matched.data.all$Match3)=='numeric') {
          match3.test=wilcox.test(Matched.data.PR$Match3, Matched.data.Return$Match3) #Match 3 is always numerical
          success.Match3=ifelse(match3.test$p.value>value.match, 1, 0)
        }  
        
        if (class(Matched.data.all$Match4)=='factor') {
          match4.test=chisq.test(table(Matched.data.all$Match4, as.factor(Matched.data.all$match)),simulate.p.value=TRUE)
          success.Match4=ifelse(match4.test$p.value>value.match, 1, 0)
          
        }
        if (class(Matched.data.all$Match4)=='numeric') {
          match4.test=wilcox.test(Matched.data.PR$Match4, Matched.data.Return$Match4) #Match 3 is always numerical
          success.Match4=ifelse(match4.test$p.value>value.match, 1, 0)
        }  
        
        #Leave loop conditions
        success.match=ifelse(nrow(Matched.data.PR)>(.9*N.returnee), 1, 0)
        #exit code
        success=ifelse(success.match==1 & success.age==1 & success.edu==1 &  
                         success.Match1==1 & 
                         success.Match2==1 &
                         success.Match3==1 &
                         success.Match4==1, 1,0)
      } #updated
      if (n==5) {
        m.out <- matchit(match ~ Match1+AGE+EDU + Match2+ Match3+ Match4 +Match5, method='nearest',  data = matching, link='probit')
        
        Matched.data.all=match.data(m.out)
        Matched.data.PR=subset(Matched.data.all, match==0)
        Matched.data.Return=subset(Matched.data.all, match==1)
        
        if (class(Matched.data.all$EDU)=="factor") {
          edu.test=chisq.test(table(Matched.data.all$EDU, as.factor(Matched.data.all$match)),simulate.p.value=TRUE)
          success.edu=ifelse(edu.test$p.value>value.match, 1, 0)
        }
        if (class(Matched.data.all$EDU)=="numeric") {
          edu.test=wilcox.test(Matched.data.PR$EDU, Matched.data.Return$EDU)
          success.edu=ifelse(edu.test$p.value>value.match, 1, 0)
        }
        
        if (class(Matched.data.all$AGE)=="factor") {
          age.test=chisq.test(table(Matched.data.all$AGE, as.factor(Matched.data.all$match)),simulate.p.value=TRUE)
          success.age=ifelse(age.test$p.value>value.match, 1, 0)
        }
        if (class(Matched.data.all$AGE)=="numeric") {
          age.test=wilcox.test(Matched.data.PR$AGE, Matched.data.Return$AGE)
          success.age=ifelse(age.test$p.value>value.match, 1, 0)
        }
        
        if (class(Matched.data.all$Match1)=='factor') {
          match1.test=chisq.test(table(Matched.data.all$Match1, as.factor(Matched.data.all$match)),simulate.p.value=TRUE)
          success.Match1=ifelse(match1.test$p.value>value.match, 1, 0)
          
        }
        if (class(Matched.data.all$Match1)=='numeric') {
          match1.test=wilcox.test(Matched.data.PR$Match1, Matched.data.Return$Match1) #Match 3 is always numerical
          success.Match1=ifelse(match1.test$p.value>value.match, 1, 0)
        }
        
        if (class(Matched.data.all$Match2)=='factor') {
          match2.test=chisq.test(table(Matched.data.all$Match2, as.factor(Matched.data.all$match)),simulate.p.value=TRUE)
          success.Match2=ifelse(match2.test$p.value>value.match, 1, 0)
          
        }
        if (class(Matched.data.all$Match2)=='numeric') {
          match2.test=wilcox.test(Matched.data.PR$Match2, Matched.data.Return$Match2) #Match 3 is always numerical
          success.Match2=ifelse(match2.test$p.value>value.match, 1, 0)
        }
        
        if (class(Matched.data.all$Match3)=='factor') {
          match3.test=chisq.test(table(Matched.data.all$Match3, as.factor(Matched.data.all$match)),simulate.p.value=TRUE)
          success.Match3=ifelse(match3.test$p.value>value.match, 1, 0)
          
        }
        if (class(Matched.data.all$Match3)=='numeric') {
          match3.test=wilcox.test(Matched.data.PR$Match3, Matched.data.Return$Match3) #Match 3 is always numerical
          success.Match3=ifelse(match3.test$p.value>value.match, 1, 0)
        }  
        
        if (class(Matched.data.all$Match4)=='factor') {
          match4.test=chisq.test(table(Matched.data.all$Match4, as.factor(Matched.data.all$match)),simulate.p.value=TRUE)
          success.Match4=ifelse(match4.test$p.value>value.match, 1, 0)
          
        }
        if (class(Matched.data.all$Match4)=='numeric') {
          match4.test=wilcox.test(Matched.data.PR$Match4, Matched.data.Return$Match4) #Match 3 is always numerical
          success.Match4=ifelse(match4.test$p.value>value.match, 1, 0)
        }  
        
        if (class(Matched.data.all$Match5)=='factor') {
          match5.test=chisq.test(table(Matched.data.all$Match5, as.factor(Matched.data.all$match)),simulate.p.value=TRUE)
          success.Match5=ifelse(match5.test$p.value>value.match, 1, 0)
          
        }
        if (class(Matched.data.all$Match5)=='numeric') {
          match5.test=wilcox.test(Matched.data.PR$Match5, Matched.data.Return$Match5) #Match 3 is always numerical
          success.Match5=ifelse(match5.test$p.value>value.match, 1, 0)
        } 
        
        #Leave loop conditions
        success.match=ifelse(nrow(Matched.data.PR)>(.9*N.returnee), 1, 0)
        #exit code
        success=ifelse(success.match==1 & success.age==1 & success.edu==1 &  
                         success.Match1==1 & 
                         success.Match2==1 &
                         success.Match3==1 &
                         success.Match4==1 &
                         success.Match5==1, 1,0)
      } #updated
      if (n==6) {
        m.out <- matchit(match ~ Match1+AGE+EDU + Match2+ Match3+ Match4 +Match5 +Match6, method='nearest',  data = matching, link='probit')
        
        Matched.data.all=match.data(m.out)
        Matched.data.PR=subset(Matched.data.all, match==0)
        Matched.data.Return=subset(Matched.data.all, match==1)
        
        if (class(Matched.data.all$EDU)=="factor") {
          edu.test=chisq.test(table(Matched.data.all$EDU, as.factor(Matched.data.all$match)),simulate.p.value=TRUE)
          success.edu=ifelse(edu.test$p.value>value.match, 1, 0)
        }
        if (class(Matched.data.all$EDU)=="numeric") {
          edu.test=wilcox.test(Matched.data.PR$EDU, Matched.data.Return$EDU)
          success.edu=ifelse(edu.test$p.value>value.match, 1, 0)
        }
        
        if (class(Matched.data.all$AGE)=="factor") {
          age.test=chisq.test(table(Matched.data.all$AGE, as.factor(Matched.data.all$match)),simulate.p.value=TRUE)
          success.age=ifelse(age.test$p.value>value.match, 1, 0)
        }
        if (class(Matched.data.all$AGE)=="numeric") {
          age.test=wilcox.test(Matched.data.PR$AGE, Matched.data.Return$AGE)
          success.age=ifelse(age.test$p.value>value.match, 1, 0)
        }
        
        if (class(Matched.data.all$Match1)=='factor') {
          match1.test=chisq.test(table(Matched.data.all$Match1, as.factor(Matched.data.all$match)),simulate.p.value=TRUE)
          success.Match1=ifelse(match1.test$p.value>value.match, 1, 0)
          
        }
        if (class(Matched.data.all$Match1)=='numeric') {
          match1.test=wilcox.test(Matched.data.PR$Match1, Matched.data.Return$Match1) #Match 3 is always numerical
          success.Match1=ifelse(match1.test$p.value>value.match, 1, 0)
        }
        
        if (class(Matched.data.all$Match2)=='factor') {
          match2.test=chisq.test(table(Matched.data.all$Match2, as.factor(Matched.data.all$match)),simulate.p.value=TRUE)
          success.Match2=ifelse(match2.test$p.value>value.match, 1, 0)
          
        }
        if (class(Matched.data.all$Match2)=='numeric') {
          match2.test=wilcox.test(Matched.data.PR$Match2, Matched.data.Return$Match2) #Match 3 is always numerical
          success.Match2=ifelse(match2.test$p.value>value.match, 1, 0)
        }
        
        if (class(Matched.data.all$Match3)=='factor') {
          match3.test=chisq.test(table(Matched.data.all$Match3, as.factor(Matched.data.all$match)),simulate.p.value=TRUE)
          success.Match3=ifelse(match3.test$p.value>value.match, 1, 0)
          
        }
        if (class(Matched.data.all$Match3)=='numeric') {
          match3.test=wilcox.test(Matched.data.PR$Match3, Matched.data.Return$Match3) #Match 3 is always numerical
          success.Match3=ifelse(match3.test$p.value>value.match, 1, 0)
        }  
        
        if (class(Matched.data.all$Match4)=='factor') {
          match4.test=chisq.test(table(Matched.data.all$Match4, as.factor(Matched.data.all$match)),simulate.p.value=TRUE)
          success.Match4=ifelse(match4.test$p.value>value.match, 1, 0)
          
        }
        if (class(Matched.data.all$Match4)=='numeric') {
          match4.test=wilcox.test(Matched.data.PR$Match4, Matched.data.Return$Match4) #Match 3 is always numerical
          success.Match4=ifelse(match4.test$p.value>value.match, 1, 0)
        }  
        
        if (class(Matched.data.all$Match5)=='factor') {
          match5.test=chisq.test(table(Matched.data.all$Match5, as.factor(Matched.data.all$match)),simulate.p.value=TRUE)
          success.Match5=ifelse(match5.test$p.value>value.match, 1, 0)
          
        }
        if (class(Matched.data.all$Match5)=='numeric') {
          match5.test=wilcox.test(Matched.data.PR$Match5, Matched.data.Return$Match5) #Match 3 is always numerical
          success.Match5=ifelse(match5.test$p.value>value.match, 1, 0)
        } 
        
        if (class(Matched.data.all$Match6)=='factor') {
          match6.test=chisq.test(table(Matched.data.all$Match6, as.factor(Matched.data.all$match)),simulate.p.value=TRUE)
          success.Match6=ifelse(match6.test$p.value>value.match, 1, 0)
          
        }
        if (class(Matched.data.all$Match6)=='numeric') {
          match6.test=wilcox.test(Matched.data.PR$Match6, Matched.data.Return$Match6) #Match 3 is always numerical
          success.Match6=ifelse(match6.test$p.value>value.match, 1, 0)
        } 
        
        #Leave loop conditions
        success.match=ifelse(nrow(Matched.data.PR)>(.9*N.returnee), 1, 0)         
        #exit code
        success=ifelse(success.match==1 & success.age==1 & success.edu==1 &  
                         success.Match1==1 & 
                         success.Match2==1 &
                         success.Match3==1 &
                         success.Match4==1 &
                         success.Match5==1 &
                         success.Match6==1, 1,0)
      } #updated
      if (n==7) {
        m.out <- matchit(match ~ Match1+AGE+EDU + Match2+ Match3+ Match4 +Match5 +Match6 +Match7, method='nearest',  data = matching, link='probit')
        
        Matched.data.all=match.data(m.out)
        Matched.data.PR=subset(Matched.data.all, match==0)
        Matched.data.Return=subset(Matched.data.all, match==1)
        
        if (class(Matched.data.all$EDU)=="factor") {
          edu.test=chisq.test(table(Matched.data.all$EDU, as.factor(Matched.data.all$match)),simulate.p.value=TRUE)
          success.edu=ifelse(edu.test$p.value>value.match, 1, 0)
        }
        if (class(Matched.data.all$EDU)=="numeric") {
          edu.test=wilcox.test(Matched.data.PR$EDU, Matched.data.Return$EDU)
          success.edu=ifelse(edu.test$p.value>value.match, 1, 0)
        }
        
        if (class(Matched.data.all$AGE)=="factor") {
          age.test=chisq.test(table(Matched.data.all$AGE, as.factor(Matched.data.all$match)),simulate.p.value=TRUE)
          success.age=ifelse(age.test$p.value>value.match, 1, 0)
        }
        if (class(Matched.data.all$AGE)=="numeric") {
          age.test=wilcox.test(Matched.data.PR$AGE, Matched.data.Return$AGE)
          success.age=ifelse(age.test$p.value>value.match, 1, 0)
        }
        
        if (class(Matched.data.all$Match1)=='factor') {
          match1.test=chisq.test(table(Matched.data.all$Match1, as.factor(Matched.data.all$match)),simulate.p.value=TRUE)
          success.Match1=ifelse(match1.test$p.value>value.match, 1, 0)
          
        }
        if (class(Matched.data.all$Match1)=='numeric') {
          match1.test=wilcox.test(Matched.data.PR$Match1, Matched.data.Return$Match1) #Match 3 is always numerical
          success.Match1=ifelse(match1.test$p.value>value.match, 1, 0)
        }
        
        if (class(Matched.data.all$Match2)=='factor') {
          match2.test=anova(lm(match~Match2, data=Matched.data.all))
          success.Match2=ifelse(match2.test$`Pr(>F)`[1]>value.match, 1, 0)
          
        }
        if (class(Matched.data.all$Match2)=='numeric') {
          match2.test=wilcox.test(Matched.data.PR$Match2, Matched.data.Return$Match2) #Match 3 is always numerical
          success.Match2=ifelse(match2.test$p.value>value.match, 1, 0)
        }
        
        if (class(Matched.data.all$Match3)=='factor') {
          match3.test=anova(lm(match~Match3, data=Matched.data.all))
          success.Match3=ifelse(match3.test$`Pr(>F)`[1]>value.match, 1, 0)
          
        }
        if (class(Matched.data.all$Match3)=='numeric') {
          match3.test=wilcox.test(Matched.data.PR$Match3, Matched.data.Return$Match3) #Match 3 is always numerical
          success.Match3=ifelse(match3.test$p.value>value.match, 1, 0)
        }  
        
        if (class(Matched.data.all$Match4)=='factor') {
          match4.test=anova(lm(match~Match4, data=Matched.data.all))
          success.Match4=ifelse(match4.test$`Pr(>F)`[1]>value.match, 1, 0)
          
        }
        if (class(Matched.data.all$Match4)=='numeric') {
          match4.test=wilcox.test(Matched.data.PR$Match4, Matched.data.Return$Match4) #Match 3 is always numerical
          success.Match4=ifelse(match4.test$p.value>value.match, 1, 0)
        }  
        
        if (class(Matched.data.all$Match5)=='factor') {
          match5.test=anova(lm(match~Match5, data=Matched.data.all))
          success.Match5=ifelse(match5.test$`Pr(>F)`[1]>value.match, 1, 0)
          
        }
        if (class(Matched.data.all$Match5)=='numeric') {
          match5.test=wilcox.test(Matched.data.PR$Match5, Matched.data.Return$Match5) #Match 3 is always numerical
          success.Match5=ifelse(match5.test$p.value>value.match, 1, 0)
        } 
        
        if (class(Matched.data.all$Match6)=='factor') {
          match6.test=anova(lm(match~Match6, data=Matched.data.all))
          success.Match6=ifelse(match6.test$`Pr(>F)`[1]>value.match, 1, 0)
          
        }
        if (class(Matched.data.all$Match6)=='numeric') {
          match6.test=wilcox.test(Matched.data.PR$Match6, Matched.data.Return$Match6) #Match 3 is always numerical
          success.Match6=ifelse(match6.test$p.value>value.match, 1, 0)
        } 
        
        if (class(Matched.data.all$Match7)=='factor') {
          match7.test=anova(lm(match~Match7, data=Matched.data.all))
          success.Match7=ifelse(match7.test$`Pr(>F)`[1]>value.match, 1, 0)
          
        }
        if (class(Matched.data.all$Match7)=='numeric') {
          match7.test=wilcox.test(Matched.data.PR$Match7, Matched.data.Return$Match7) #Match 3 is always numerical
          success.Match7=ifelse(match7.test$p.value>value.match, 1, 0)
        } 
        #Leave loop conditions
        success.match=ifelse(nrow(Matched.data.PR)>(.9*N.returnee), 1, 0)         
        #exit code
        success=ifelse(success.match==1 & success.age==1 & success.edu==1 &  
                         success.Match1==1 & 
                         success.Match2==1 &
                         success.Match3==1 &
                         success.Match4==1 &
                         success.Match5==1 &
                         success.Match6==1 &
                         success.Match7==1, 1,0)
      }
      if (n==8) {
        m.out <- matchit(match ~ Match1+AGE+EDU + Match2+ Match3+ Match4 +Match5 +Match6 +Match7 +Match8, method='nearest',  data = matching, link='probit')
        
        Matched.data.all=match.data(m.out)
        Matched.data.PR=subset(Matched.data.all, match==0)
        Matched.data.Return=subset(Matched.data.all, match==1)
        
        #Tests to leave loop
        if (class(Matched.data.all$EDU)=="factor") {
          edu.test=chisq.test(table(Matched.data.all$EDU, as.factor(Matched.data.all$match)),simulate.p.value=TRUE)
          success.edu=ifelse(edu.test$p.value>value.match, 1, 0)
        }
        if (class(Matched.data.all$EDU)=="numeric") {
          edu.test=wilcox.test(Matched.data.PR$EDU, Matched.data.Return$EDU)
          success.edu=ifelse(edu.test$p.value>value.match, 1, 0)
        }
        
        if (class(Matched.data.all$AGE)=="factor") {
          age.test=chisq.test(table(Matched.data.all$AGE, as.factor(Matched.data.all$match)),simulate.p.value=TRUE)
          success.age=ifelse(age.test$p.value>value.match, 1, 0)
        }
        if (class(Matched.data.all$AGE)=="numeric") {
          age.test=wilcox.test(Matched.data.PR$AGE, Matched.data.Return$AGE)
          success.age=ifelse(age.test$p.value>value.match, 1, 0)
        }
        
        if (class(Matched.data.all$Match1)=='factor') {
          match1.test=chisq.test(table(Matched.data.all$Match1, as.factor(Matched.data.all$match)),simulate.p.value=TRUE)
          success.Match1=ifelse(match1.test$p.value>value.match, 1, 0)
          
        }
        if (class(Matched.data.all$Match1)=='numeric') {
          match1.test=wilcox.test(Matched.data.PR$Match1, Matched.data.Return$Match1) #Match 3 is always numerical
          success.Match1=ifelse(match1.test$p.value>value.match, 1, 0)
        }
        
        if (class(Matched.data.all$Match2)=='factor') {
          match2.test=anova(lm(match~Match2, data=Matched.data.all))
          success.Match2=ifelse(match2.test$`Pr(>F)`[1]>value.match, 1, 0)
          
        }
        if (class(Matched.data.all$Match2)=='numeric') {
          match2.test=wilcox.test(Matched.data.PR$Match2, Matched.data.Return$Match2) #Match 3 is always numerical
          success.Match2=ifelse(match2.test$p.value>value.match, 1, 0)
        }
        
        if (class(Matched.data.all$Match3)=='factor') {
          match3.test=anova(lm(match~Match3, data=Matched.data.all))
          success.Match3=ifelse(match3.test$`Pr(>F)`[1]>value.match, 1, 0)
          
        }
        if (class(Matched.data.all$Match3)=='numeric') {
          match3.test=wilcox.test(Matched.data.PR$Match3, Matched.data.Return$Match3) #Match 3 is always numerical
          success.Match3=ifelse(match3.test$p.value>value.match, 1, 0)
        }  
        
        if (class(Matched.data.all$Match4)=='factor') {
          match4.test=anova(lm(match~Match4, data=Matched.data.all))
          success.Match4=ifelse(match4.test$`Pr(>F)`[1]>value.match, 1, 0)
          
        }
        if (class(Matched.data.all$Match4)=='numeric') {
          match4.test=wilcox.test(Matched.data.PR$Match4, Matched.data.Return$Match4) #Match 3 is always numerical
          success.Match4=ifelse(match4.test$p.value>value.match, 1, 0)
        }  
        
        if (class(Matched.data.all$Match5)=='factor') {
          match5.test=anova(lm(match~Match5, data=Matched.data.all))
          success.Match5=ifelse(match5.test$`Pr(>F)`[1]>value.match, 1, 0)
          
        }
        if (class(Matched.data.all$Match5)=='numeric') {
          match5.test=wilcox.test(Matched.data.PR$Match5, Matched.data.Return$Match5) #Match 3 is always numerical
          success.Match5=ifelse(match5.test$p.value>value.match, 1, 0)
        } 
        
        if (class(Matched.data.all$Match6)=='factor') {
          match6.test=anova(lm(match~Match6, data=Matched.data.all))
          success.Match6=ifelse(match6.test$`Pr(>F)`[1]>value.match, 1, 0)
          
        }
        if (class(Matched.data.all$Match6)=='numeric') {
          match6.test=wilcox.test(Matched.data.PR$Match6, Matched.data.Return$Match6) #Match 3 is always numerical
          success.Match6=ifelse(match6.test$p.value>value.match, 1, 0)
        } 
        
        if (class(Matched.data.all$Match7)=='factor') {
          match7.test=anova(lm(match~Match7, data=Matched.data.all))
          success.Match7=ifelse(match7.test$`Pr(>F)`[1]>value.match, 1, 0)
          
        }
        if (class(Matched.data.all$Match7)=='numeric') {
          match7.test=wilcox.test(Matched.data.PR$Match7, Matched.data.Return$Match7) #Match 3 is always numerical
          success.Match7=ifelse(match7.test$p.value>value.match, 1, 0)
        } 
        
        if (class(Matched.data.all$Match8)=='factor') {
          match8.test=anova(lm(match~Match8, data=Matched.data.all))
          success.Match8=ifelse(match8.test$`Pr(>F)`[1]>value.match, 1, 0)
          
        }
        if (class(Matched.data.all$Match8)=='numeric') {
          match8.test=wilcox.test(Matched.data.PR$Match8, Matched.data.Return$Match8) #Match 3 is always numerical
          success.Match8=ifelse(match8.test$p.value>value.match, 1, 0)
        } 
        
        #Leave loop conditions
        success.match=ifelse(nrow(Matched.data.PR)>(.9*N.returnee), 1, 0)         
        #exit code
        success=ifelse(success.match==1 & success.age==1 & success.edu==1 &  
                         success.Match1==1 & 
                         success.Match2==1 &
                         success.Match3==1 &
                         success.Match4==1 &
                         success.Match5==1 &
                         success.Match6==1 &
                         success.Match7==1 &
                         success.Match8==1, 1,0)
      }
      if (n==9) {
        m.out <- matchit(match ~ Match1+AGE+EDU + Match2+ Match3+ Match4 +Match5 +Match6 +Match7 +Match8 +Match9, method='nearest',  data = matching, link='probit')
        
        Matched.data.all=match.data(m.out)
        Matched.data.PR=subset(Matched.data.all, match==0)
        Matched.data.Return=subset(Matched.data.all, match==1)
        
        #Tests to leave loop
        if (class(Matched.data.all$EDU)=="factor") {
          edu.test=chisq.test(table(Matched.data.all$EDU, as.factor(Matched.data.all$match)),simulate.p.value=TRUE)
          success.edu=ifelse(edu.test$p.value>value.match, 1, 0)
        }
        if (class(Matched.data.all$EDU)=="numeric") {
          edu.test=wilcox.test(Matched.data.PR$EDU, Matched.data.Return$EDU)
          success.edu=ifelse(edu.test$p.value>value.match, 1, 0)
        }
        
        if (class(Matched.data.all$AGE)=="factor") {
          age.test=chisq.test(table(Matched.data.all$AGE, as.factor(Matched.data.all$match)),simulate.p.value=TRUE)
          success.age=ifelse(age.test$p.value>value.match, 1, 0)
        }
        if (class(Matched.data.all$AGE)=="numeric") {
          age.test=wilcox.test(Matched.data.PR$AGE, Matched.data.Return$AGE)
          success.age=ifelse(age.test$p.value>value.match, 1, 0)
        }
        
        if (class(Matched.data.all$Match1)=='factor') {
          match1.test=chisq.test(table(Matched.data.all$Match1, as.factor(Matched.data.all$match)),simulate.p.value=TRUE)
          success.Match1=ifelse(match1.test$p.value>value.match, 1, 0)
          
        }
        if (class(Matched.data.all$Match1)=='numeric') {
          match1.test=wilcox.test(Matched.data.PR$Match1, Matched.data.Return$Match1) #Match 3 is always numerical
          success.Match1=ifelse(match1.test$p.value>value.match, 1, 0)
        }
        
        if (class(Matched.data.all$Match2)=='factor') {
          match2.test=anova(lm(match~Match2, data=Matched.data.all))
          success.Match2=ifelse(match2.test$`Pr(>F)`[1]>value.match, 1, 0)
          
        }
        if (class(Matched.data.all$Match2)=='numeric') {
          match2.test=wilcox.test(Matched.data.PR$Match2, Matched.data.Return$Match2) #Match 3 is always numerical
          success.Match2=ifelse(match2.test$p.value>value.match, 1, 0)
        }
        
        if (class(Matched.data.all$Match3)=='factor') {
          match3.test=anova(lm(match~Match3, data=Matched.data.all))
          success.Match3=ifelse(match3.test$`Pr(>F)`[1]>value.match, 1, 0)
          
        }
        if (class(Matched.data.all$Match3)=='numeric') {
          match3.test=wilcox.test(Matched.data.PR$Match3, Matched.data.Return$Match3) #Match 3 is always numerical
          success.Match3=ifelse(match3.test$p.value>value.match, 1, 0)
        }  
        
        if (class(Matched.data.all$Match4)=='factor') {
          match4.test=anova(lm(match~Match4, data=Matched.data.all))
          success.Match4=ifelse(match4.test$`Pr(>F)`[1]>value.match, 1, 0)
          
        }
        if (class(Matched.data.all$Match4)=='numeric') {
          match4.test=wilcox.test(Matched.data.PR$Match4, Matched.data.Return$Match4) #Match 3 is always numerical
          success.Match4=ifelse(match4.test$p.value>value.match, 1, 0)
        }  
        
        if (class(Matched.data.all$Match5)=='factor') {
          match5.test=anova(lm(match~Match5, data=Matched.data.all))
          success.Match5=ifelse(match5.test$`Pr(>F)`[1]>value.match, 1, 0)
          
        }
        if (class(Matched.data.all$Match5)=='numeric') {
          match5.test=wilcox.test(Matched.data.PR$Match5, Matched.data.Return$Match5) #Match 3 is always numerical
          success.Match5=ifelse(match5.test$p.value>value.match, 1, 0)
        } 
        
        if (class(Matched.data.all$Match6)=='factor') {
          match6.test=anova(lm(match~Match5, data=Matched.data.all))
          success.Match6=ifelse(match6.test$`Pr(>F)`[1]>value.match, 1, 0)
          
        }
        if (class(Matched.data.all$Match6)=='numeric') {
          match6.test=wilcox.test(Matched.data.PR$Match6, Matched.data.Return$Match6) #Match 3 is always numerical
          success.Match6=ifelse(match6.test$p.value>value.match, 1, 0)
        } 
        
        if (class(Matched.data.all$Match7)=='factor') {
          match7.test=anova(lm(match~Match7, data=Matched.data.all))
          success.Match7=ifelse(match7.test$`Pr(>F)`[1]>value.match, 1, 0)
          
        }
        if (class(Matched.data.all$Match7)=='numeric') {
          match7.test=wilcox.test(Matched.data.PR$Match7, Matched.data.Return$Match7) #Match 3 is always numerical
          success.Match7=ifelse(match7.test$p.value>value.match, 1, 0)
        } 
        
        if (class(Matched.data.all$Match8)=='factor') {
          match8.test=anova(lm(match~Match8, data=Matched.data.all))
          success.Match8=ifelse(match8.test$`Pr(>F)`[1]>value.match, 1, 0)
          
        }
        if (class(Matched.data.all$Match8)=='numeric') {
          match8.test=wilcox.test(Matched.data.PR$Match8, Matched.data.Return$Match8) #Match 3 is always numerical
          success.Match8=ifelse(match8.test$p.value>value.match, 1, 0)
        } 
        
        if (class(Matched.data.all$Match9)=='factor') {
          match9.test=anova(lm(match~Match9, data=Matched.data.all))
          success.Match9=ifelse(match9.test$`Pr(>F)`[1]>value.match, 1, 0)
          
        }
        if (class(Matched.data.all$Match9)=='numeric') {
          match9.test=wilcox.test(Matched.data.PR$Match9, Matched.data.Return$Match9) #Match 3 is always numerical
          success.Match9=ifelse(match9.test$p.value>value.match, 1, 0)
        } 
        
        #Leave loop conditions
        success.match=ifelse(nrow(Matched.data.PR)>(.9*N.returnee), 1, 0)         
        #exit code
        success=ifelse(success.match==1 & success.age==1 & success.edu==1 &  
                         success.Match1==1 & 
                         success.Match2==1 &
                         success.Match3==1 &
                         success.Match4==1 &
                         success.Match5==1 &
                         success.Match6==1 &
                         success.Match7==1 &
                         success.Match8==1 &
                         success.Match9==1, 1,0)
      }
      if (n==10) {
        m.out <- matchit(match ~ Match1+AGE+EDU + Match2+ Match3+ Match4 +Match5 +Match6 +Match7 +Match8 +Match9 +Match10, method='nearest',  data = matching, link='probit')
        
        Matched.data.all=match.data(m.out)
        Matched.data.PR=subset(Matched.data.all, match==0)
        Matched.data.Return=subset(Matched.data.all, match==1)
        
        #Tests to leave loop
        
        if (class(Matched.data.all$EDU)=="factor") {
          edu.test=chisq.test(table(Matched.data.all$EDU, as.factor(Matched.data.all$match)),simulate.p.value=TRUE)
          success.edu=ifelse(edu.test$p.value>value.match, 1, 0)
        }
        if (class(Matched.data.all$EDU)=="numeric") {
          edu.test=wilcox.test(Matched.data.PR$EDU, Matched.data.Return$EDU)
          success.edu=ifelse(edu.test$p.value>value.match, 1, 0)
        }
        
        if (class(Matched.data.all$AGE)=="factor") {
          age.test=chisq.test(table(Matched.data.all$AGE, as.factor(Matched.data.all$match)),simulate.p.value=TRUE)
          success.age=ifelse(age.test$p.value>value.match, 1, 0)
        }
        if (class(Matched.data.all$AGE)=="numeric") {
          age.test=wilcox.test(Matched.data.PR$AGE, Matched.data.Return$AGE)
          success.age=ifelse(age.test$p.value>value.match, 1, 0)
        }
        
        if (class(Matched.data.all$Match1)=='factor') {
          match1.test=chisq.test(table(Matched.data.all$Match1, as.factor(Matched.data.all$match)),simulate.p.value=TRUE)
          success.Match1=ifelse(match1.test$p.value>value.match, 1, 0)
          
        }
        if (class(Matched.data.all$Match1)=='numeric') {
          match1.test=wilcox.test(Matched.data.PR$Match1, Matched.data.Return$Match1) #Match 3 is always numerical
          success.Match1=ifelse(match1.test$p.value>value.match, 1, 0)
        }
        
        if (class(Matched.data.all$Match2)=='factor') {
          match2.test=anova(lm(match~Match2, data=Matched.data.all))
          success.Match2=ifelse(match2.test$`Pr(>F)`[1]>value.match, 1, 0)
          
        }
        if (class(Matched.data.all$Match2)=='numeric') {
          match2.test=wilcox.test(Matched.data.PR$Match2, Matched.data.Return$Match2) #Match 3 is always numerical
          success.Match2=ifelse(match2.test$p.value>value.match, 1, 0)
        }
        
        if (class(Matched.data.all$Match3)=='factor') {
          match3.test=anova(lm(match~Match3, data=Matched.data.all))
          success.Match3=ifelse(match3.test$`Pr(>F)`[1]>value.match, 1, 0)
          
        }
        if (class(Matched.data.all$Match3)=='numeric') {
          match3.test=wilcox.test(Matched.data.PR$Match3, Matched.data.Return$Match3) #Match 3 is always numerical
          success.Match3=ifelse(match3.test$p.value>value.match, 1, 0)
        }  
        
        if (class(Matched.data.all$Match4)=='factor') {
          match4.test=anova(lm(match~Match4, data=Matched.data.all))
          success.Match4=ifelse(match4.test$`Pr(>F)`[1]>value.match, 1, 0)
          
        }
        if (class(Matched.data.all$Match4)=='numeric') {
          match4.test=wilcox.test(Matched.data.PR$Match4, Matched.data.Return$Match4) #Match 3 is always numerical
          success.Match4=ifelse(match4.test$p.value>value.match, 1, 0)
        }  
        
        if (class(Matched.data.all$Match5)=='factor') {
          match5.test=anova(lm(match~Match5, data=Matched.data.all))
          success.Match5=ifelse(match5.test$`Pr(>F)`[1]>value.match, 1, 0)
          
        }
        if (class(Matched.data.all$Match5)=='numeric') {
          match5.test=wilcox.test(Matched.data.PR$Match5, Matched.data.Return$Match5) #Match 3 is always numerical
          success.Match5=ifelse(match5.test$p.value>value.match, 1, 0)
        } 
        
        if (class(Matched.data.all$Match6)=='factor') {
          match6.test=anova(lm(match~Match6, data=Matched.data.all))
          success.Match6=ifelse(match6.test$`Pr(>F)`[1]>value.match, 1, 0)
          
        }
        if (class(Matched.data.all$Match6)=='numeric') {
          match6.test=wilcox.test(Matched.data.PR$Match6, Matched.data.Return$Match6) #Match 3 is always numerical
          success.Match6=ifelse(match6.test$p.value>value.match, 1, 0)
        } 
        
        if (class(Matched.data.all$Match7)=='factor') {
          match7.test=anova(lm(match~Match7, data=Matched.data.all))
          success.Match7=ifelse(match7.test$`Pr(>F)`[1]>value.match, 1, 0)
          
        }
        if (class(Matched.data.all$Match7)=='numeric') {
          match7.test=wilcox.test(Matched.data.PR$Match7, Matched.data.Return$Match7) #Match 3 is always numerical
          success.Match7=ifelse(match7.test$p.value>value.match, 1, 0)
        } 
        
        if (class(Matched.data.all$Match8)=='factor') {
          match8.test=anova(lm(match~Match8, data=Matched.data.all))
          success.Match8=ifelse(match8.test$`Pr(>F)`[1]>value.match, 1, 0)
          
        }
        if (class(Matched.data.all$Match8)=='numeric') {
          match8.test=wilcox.test(Matched.data.PR$Match8, Matched.data.Return$Match8) #Match 3 is always numerical
          success.Match8=ifelse(match8.test$p.value>value.match, 1, 0)
        } 
        
        if (class(Matched.data.all$Match9)=='factor') {
          match9.test=anova(lm(match~Match9, data=Matched.data.all))
          success.Match9=ifelse(match9.test$`Pr(>F)`[1]>value.match, 1, 0)
          
        }
        if (class(Matched.data.all$Match9)=='numeric') {
          match9.test=wilcox.test(Matched.data.PR$Match9, Matched.data.Return$Match9) #Match 3 is always numerical
          success.Match9=ifelse(match9.test$p.value>value.match, 1, 0)
        } 
        
        if (class(Matched.data.all$Match10)=='factor') {
          match10.test=anova(lm(match~Match10, data=Matched.data.all))
          success.Match10=ifelse(match10.test$`Pr(>F)`[1]>value.match, 1, 0)
          
        }
        if (class(Matched.data.all$Match10)=='numeric') {
          match10.test=wilcox.test(Matched.data.PR$Match10, Matched.data.Return$Match10) #Match 3 is always numerical
          success.Match10=ifelse(match10.test$p.value>value.match, 1, 0)
        } 
        
        #Leave loop conditions
        success.match=ifelse(nrow(Matched.data.PR)>(.9*N.returnee), 1, 0)         
        #exit code
        success=ifelse(success.match==1 & success.age==1 & success.edu==1 &  
                         success.Match1==1 & 
                         success.Match2==1 &
                         success.Match3==1 &
                         success.Match4==1 &
                         success.Match5==1 &
                         success.Match6==1 &
                         success.Match7==1 &
                         success.Match8==1 &
                         success.Match9==1 &
                         success.Match10==1, 1,0)
      }
      
    } #Matches dataset based on predefined number of match categories. Age and edu always matched. 
    
    {#fail code
      if (success!=1) {        
        count=count+1
        restart=TRUE
        
      }
      if (count>PR.match.count) {
        restart=TRUE
        success=1
      }
      if (success==1) {        
        restart=FALSE
        #this was added because chisq() sometimes breaks when no subjects have a value in a cell
      }
    } #Fail code
  }
  #Create labled databases
  Returnee.FollowUp.Iteration=Matched.data.Return
  Returnee.Baseline.Iteration=Returnee.baseline.data[which(Returnee.baseline.data$ID %in% Returnee.FollowUp.Iteration$ID),]
  Pseudo.Replace.Iteration=Matched.data.PR
  
  #set up matching for baseline and retunree baseline
  Baseline.temp.NoPRs=Full.Baseline.data[-which(Full.Baseline.data$ID %in% Pseudo.Replace.Iteration$ID),] #removes chosen set of PRs from baseline pool
  Baseline.temp.pool=Baseline.temp.NoPRs[-which(Baseline.temp.NoPRs$ID %in% Returnee.Baseline.Iteration$ID),] #removes chosen set of returnees from baseline pool 
  
  while (baseline.success==0 & restart==FALSE) {
    #resets databases
    m.out.baseline=NULL
    Matched.baseline.all=NULL
    Matched.baseline.chosen=NULL
    Matched.data.Return=NULL
    base.selection=TRUE
    
    if (base.selection==TRUE) {
      age_bounds=describe(Returnee.Baseline.Iteration$AGE)
      u_bound=age_bounds$max  
      l_bound=age_bounds$min
      Baseline.temp.pool$keep=ifelse(Baseline.temp.pool$AGE>u_bound,
                                     -1,
                                     ifelse(Baseline.temp.pool$AGE<l_bound,
                                            -1,1))
      
      Baseline.temp.pool.2=subset(Baseline.temp.pool, keep==1) #removes anyone outside age range
      if (nrow(Baseline.temp.pool.2)>(N.base.pool)) {
        base.selection=FALSE
      }
      Baseline.pool <- Baseline.temp.pool.2[sample(nrow(Baseline.temp.pool.2), N.base.start), ] # starting sample size
      #Baseline.pool=Baseline.temp.pool.2
      Baseline.pool$match=0
      Baseline.pool$keep=NULL
      
      Baseline.pool$match=0
      Returnee.Baseline.Iteration$match=1
      #loop for matching the Baseline to the Returnee baseline
      baseline.matching=rbind(Baseline.pool, Returnee.Baseline.Iteration)  #not sure if this is going to work? check ns
      
    }
    
    { 
      if (baseline.match.age.only==TRUE) {
        n=9000 # this makes it so the rest of the if statements do not run.
        m.out.baseline <- matchit(match ~ AGE, method='nearest',  data = baseline.matching, link='probit')
        
        Matched.baseline.all=match.data(m.out.baseline)
        Matched.baseline.chosen=subset(Matched.baseline.all, match==0)
        Matched.data.Return=subset(Matched.baseline.all, match==1)
        
        #Tests to leave loop
        age.test.base=wilcox.test(Matched.baseline.chosen$AGE, Matched.data.Return$AGE)
        #Leave loop conditions
        baseline.success.age=ifelse(age.test.base$p.value>value.match, 1, 0)
        baseline.success.match=ifelse(nrow(Matched.data.PR)>(.9*N.returnee), 1, 0)
        
        #exit code
        baseline.success=ifelse(baseline.success.match==1 & baseline.success.age==1, 1,0)
        base.print="baseline matched on age only"

        
      }
      if (n==1) {
        m.out.baseline <- matchit(match ~ Match1+AGE+EDU, method='nearest',  data = baseline.matching, link='probit')
        
        Matched.baseline.all=match.data(m.out.baseline)
        Matched.baseline.chosen=subset(Matched.baseline.all, match==0)
        Matched.data.Return=subset(Matched.baseline.all, match==1)
        
        #Tests to leave loop
        if (class(Matched.data.all$EDU)=="factor") {
          edu.test.base=chisq.test(table(Matched.baseline.all$EDU, as.factor(Matched.baseline.all$match)))
          success.edu.base=ifelse(edu.test.base$p.value>value.match, 1, 0)
        }
        if (class(Matched.data.all$EDU)=="numeric") {
          edu.test.base=wilcox.test(Matched.baseline.all$EDU, Matched.data.Return$EDU)
          success.edu.base=ifelse(edu.test.base$p.value>value.match, 1, 0)
        }
        
        if (class(Matched.data.all$AGE)=="factor") {
          age.test.base=chisq.test(table(Matched.baseline.all$AGE, as.factor(Matched.baseline.all$match)))
          baseline.success.age=ifelse(age.test.base$p.value>value.match, 1, 0)
        }
        if (class(Matched.data.all$AGE)=="numeric") {
          age.test.base=wilcox.test(Matched.baseline.all$AGE, Matched.data.Return$AGE)
          baseline.success.age=ifelse(age.test.base$p.value>value.match, 1, 0)
        }
        
        age.test=wilcox.test(Matched.baseline.chosen$AGE, Matched.data.Return$AGE)
        edu.test=wilcox.test(Matched.baseline.chosen$EDU, Matched.data.Return$EDU)
        if (class(Matched.baseline.all$Match1)=='factor') {
          match1.test=anova(lm(match~Match1, data=Matched.baseline.all))
          baseline.success.Match1=ifelse(match1.test$`Pr(>F)`[1]>value.match, 1, 0)
          
        }
        if (class(Matched.baseline.all$Match1)=='numeric') {
          match1.test=wilcox.test(Matched.baseline.chosen$Match1, Matched.data.Return$Match1) #Match 3 is always numerical
          baseline.success.Match1=ifelse(match1.test$p.value>value.match, 1, 0)
        }
        
        #Leave loop conditions
        baseline.success.match=ifelse(nrow(Matched.data.PR)>(.9*N.returnee), 1, 0)
        baseline.success.age=ifelse(age.test$p.value>value.match, 1, 0)
        baseline.success.edu=ifelse(edu.test$p.value>value.match, 1, 0)
        
        #exit code
        baseline.success=ifelse(baseline.success.match==1 & baseline.success.age==1 & baseline.success.edu==1 &   baseline.success.Match1==1, 1,0)
        
      }
      if (n==2) {
        m.out.baseline <- matchit(match ~ Match1+AGE+EDU + Match2, method='nearest',  data = baseline.matching, link='probit')
        
        Matched.baseline.all=match.data(m.out.baseline)
        Matched.baseline.chosen=subset(Matched.baseline.all, match==0)
        Matched.data.Return=subset(Matched.baseline.all, match==1)
        if (class(Matched.baseline.all$Match1)=='factor') {
          match1.test=anova(lm(match~Match1, data=Matched.baseline.all))
          baseline.success.Match1=ifelse(match1.test$`Pr(>F)`[1]>value.match, 1, 0)
          
        }
        if (class(Matched.baseline.all$Match1)=='numeric') {
          match1.test=wilcox.test(Matched.baseline.chosen$Match1, Matched.data.Return$Match1) #Match 3 is always numerical
          baseline.success.Match1=ifelse(match1.test$p.value>value.match, 1, 0)
        }
        
        if (class(Matched.baseline.all$Match2)=='factor') {
          match2.test=chisq.test(table(Matched.baseline.all$Match2, as.factor(Matched.baseline.all$match)))
          baseline.success.Match2=ifelse(match2.test$p.value>value.match, 1, 0)
          
        }
        if (class(Matched.baseline.all$Match2)=='numeric') {
          match2.test=wilcox.test(Matched.baseline.chosen$Match2, Matched.data.Return$Match2) #Match 3 is always numerical
          baseline.success.Match2=ifelse(match2.test$p.value>value.match, 1, 0)
        }
        
        if (class(Matched.data.all$EDU)=="factor") {
          edu.test.base=chisq.test(table(Matched.baseline.all$EDU, as.factor(Matched.baseline.all$match)))
          success.edu.base=ifelse(edu.test.base$p.value>value.match, 1, 0)
        }
        if (class(Matched.data.all$EDU)=="numeric") {
          edu.test.base=wilcox.test(Matched.baseline.all$EDU, Matched.data.Return$EDU)
          success.edu.base=ifelse(edu.test.base$p.value>value.match, 1, 0)
        }
        
        if (class(Matched.data.all$AGE)=="factor") {
          age.test.base=chisq.test(table(Matched.baseline.all$AGE, as.factor(Matched.baseline.all$match)))
          baseline.success.age=ifelse(age.test.base$p.value>value.match, 1, 0)
        }
        if (class(Matched.data.all$AGE)=="numeric") {
          age.test.base=wilcox.test(Matched.baseline.all$AGE, Matched.data.Return$AGE)
          baseline.success.age=ifelse(age.test.base$p.value>value.match, 1, 0)
        }
        
        #Leave loop conditions
        baseline.success.match=ifelse(nrow(Matched.data.PR)>(.9*N.returnee), 1, 0)
        #exit code
        baseline.success=ifelse(baseline.success.match==1 & baseline.success.age==1 & baseline.success.edu==1 &  baseline.success.Match1==1 & baseline.success.Match2==1, 1,0)
        
      }
      if (n==3) {
        m.out.baseline <- matchit(match ~ Match1+AGE+EDU + Match2+ Match3, method='nearest',  data = baseline.matching, link='probit')
        
        Matched.baseline.all=match.data(m.out.baseline)
        Matched.baseline.chosen=subset(Matched.baseline.all, match==0)
        Matched.data.Return=subset(Matched.baseline.all, match==1)
        
        if (class(Matched.data.all$EDU)=="factor") {
          edu.test.base=chisq.test(table(Matched.baseline.all$EDU, as.factor(Matched.baseline.all$match)))
          success.edu.base=ifelse(edu.test.base$p.value>value.match, 1, 0)
        }
        if (class(Matched.data.all$EDU)=="numeric") {
          edu.test.base=wilcox.test(Matched.baseline.all$EDU, Matched.data.Return$EDU)
          success.edu.base=ifelse(edu.test.base$p.value>value.match, 1, 0)
        }
        
        if (class(Matched.data.all$AGE)=="factor") {
          age.test.base=chisq.test(table(Matched.baseline.all$AGE, as.factor(Matched.baseline.all$match)))
          baseline.success.age=ifelse(age.test.base$p.value>value.match, 1, 0)
        }
        if (class(Matched.data.all$AGE)=="numeric") {
          age.test.base=wilcox.test(Matched.baseline.all$AGE, Matched.data.Return$AGE)
          baseline.success.age=ifelse(age.test.base$p.value>value.match, 1, 0)
        }
        
        if (class(Matched.baseline.all$Match1)=='factor') {
          match1.test=anova(lm(match~Match1, data=Matched.baseline.all))
          baseline.success.Match1=ifelse(match1.test$`Pr(>F)`[1]>value.match, 1, 0)
          
        }
        if (class(Matched.baseline.all$Match1)=='numeric') {
          match1.test=wilcox.test(Matched.baseline.chosen$Match1, Matched.data.Return$Match1) #Match 3 is always numerical
          baseline.success.Match1=ifelse(match1.test$p.value>value.match, 1, 0)
        }
        
        if (class(Matched.baseline.all$Match2)=='factor') {
          match2.test=anova(lm(match~Match2, data=Matched.baseline.all))
          baseline.success.Match2=ifelse(match2.test$`Pr(>F)`[1]>value.match, 1, 0)
          
        }
        if (class(Matched.baseline.all$Match2)=='numeric') {
          match2.test=wilcox.test(Matched.baseline.chosen$Match2, Matched.data.Return$Match2) #Match 3 is always numerical
          baseline.success.Match2=ifelse(match2.test$p.value>value.match, 1, 0)
        }
        
        if (class(Matched.baseline.all$Match3)=='factor') {
          match3.test=anova(lm(match~Match3, data=Matched.baseline.all))
          baseline.success.Match3=ifelse(match3.test$`Pr(>F)`[1]>value.match, 1, 0)
          
        }
        if (class(Matched.baseline.all$Match3)=='numeric') {
          match3.test=wilcox.test(Matched.baseline.chosen$Match3, Matched.data.Return$Match3) #Match 3 is always numerical
          baseline.success.Match3=ifelse(match3.test$p.value>value.match, 1, 0)
        }  
        
        
        #Leave loop conditions
        baseline.success.match=ifelse(nrow(Matched.baseline.chosen)==(nrow(Matched.data.Return)), 1, 0)         
        #exit code
        baseline.success=ifelse(baseline.success.match==1 & baseline.success.age==1 & baseline.success.edu==1 &  
                                  baseline.success.Match1==1 & 
                                  baseline.success.Match2==1 &
                                  baseline.success.Match3==1, 1,0)
      }
      if (n==4) {
        m.out.baseline <- matchit(match ~ Match1+AGE+EDU + Match2+ Match3+ Match4, method='nearest',  data = baseline.matching, link='probit')
        
        Matched.baseline.all=match.data(m.out.baseline)
        Matched.baseline.chosen=subset(Matched.baseline.all, match==0)
        Matched.data.Return=subset(Matched.baseline.all, match==1)
        
        if (class(Matched.data.all$EDU)=="factor") {
          edu.test.base=chisq.test(table(Matched.baseline.all$EDU, as.factor(Matched.baseline.all$match)))
          success.edu.base=ifelse(edu.test.base$p.value>value.match, 1, 0)
        }
        if (class(Matched.data.all$EDU)=="numeric") {
          edu.test.base=wilcox.test(Matched.baseline.all$EDU, Matched.data.Return$EDU)
          success.edu.base=ifelse(edu.test.base$p.value>value.match, 1, 0)
        }
        
        if (class(Matched.data.all$AGE)=="factor") {
          age.test.base=chisq.test(table(Matched.baseline.all$AGE, as.factor(Matched.baseline.all$match)))
          baseline.success.age=ifelse(age.test.base$p.value>value.match, 1, 0)
        }
        if (class(Matched.data.all$AGE)=="numeric") {
          age.test.base=wilcox.test(Matched.baseline.all$AGE, Matched.data.Return$AGE)
          baseline.success.age=ifelse(age.test.base$p.value>value.match, 1, 0)
        }
        
        if (class(Matched.baseline.all$Match1)=='factor') {
          match1.test=anova(lm(match~Match1, data=Matched.baseline.all))
          baseline.success.Match1=ifelse(match1.test$`Pr(>F)`[1]>value.match, 1, 0)
          
        }
        if (class(Matched.baseline.all$Match1)=='numeric') {
          match1.test=wilcox.test(Matched.baseline.chosen$Match1, Matched.data.Return$Match1) #Match 3 is always numerical
          baseline.success.Match1=ifelse(match1.test$p.value>value.match, 1, 0)
        }
        
        if (class(Matched.baseline.all$Match2)=='factor') {
          match2.test=anova(lm(match~Match2, data=Matched.baseline.all))
          baseline.success.Match2=ifelse(match2.test$`Pr(>F)`[1]>value.match, 1, 0)
          
        }
        if (class(Matched.baseline.all$Match2)=='numeric') {
          match2.test=wilcox.test(Matched.baseline.chosen$Match2, Matched.data.Return$Match2) #Match 3 is always numerical
          baseline.success.Match2=ifelse(match2.test$p.value>value.match, 1, 0)
        }
        
        if (class(Matched.baseline.all$Match3)=='factor') {
          match3.test=anova(lm(match~Match3, data=Matched.baseline.all))
          baseline.success.Match3=ifelse(match3.test$`Pr(>F)`[1]>value.match, 1, 0)
          
        }
        if (class(Matched.baseline.all$Match3)=='numeric') {
          match3.test=wilcox.test(Matched.baseline.chosen$Match3, Matched.data.Return$Match3) #Match 3 is always numerical
          baseline.success.Match3=ifelse(match3.test$p.value>value.match, 1, 0)
        }  
        
        if (class(Matched.baseline.all$Match4)=='factor') {
          match4.test=anova(lm(match~Match4, data=Matched.baseline.all))
          baseline.success.Match4=ifelse(match4.test$`Pr(>F)`[1]>value.match, 1, 0)
          
        }
        if (class(Matched.baseline.all$Match4)=='numeric') {
          match4.test=wilcox.test(Matched.baseline.chosen$Match4, Matched.data.Return$Match4) #Match 3 is always numerical
          baseline.success.Match4=ifelse(match4.test$p.value>value.match, 1, 0)
        }  
        
        #Leave loop conditions
        baseline.success.match=ifelse(nrow(Matched.data.PR)>(.9*N.returnee), 1, 0)
        #exit code
        baseline.success=ifelse(baseline.success.match==1 & baseline.success.age==1 & baseline.success.edu==1 &  
                                  baseline.success.Match1==1 & 
                                  baseline.success.Match2==1 &
                                  baseline.success.Match3==1 &
                                  baseline.success.Match4==1, 1,0)
      }
      if (n==5) {
        m.out.baseline <- matchit(match ~ Match1+AGE+EDU + Match2+ Match3+ Match4 +Match5, method='nearest',  data = baseline.matching, link='probit')
        
        Matched.baseline.all=match.data(m.out.baseline)
        Matched.baseline.chosen=subset(Matched.baseline.all, match==0)
        Matched.data.Return=subset(Matched.baseline.all, match==1)
        
        if (class(Matched.data.all$EDU)=="factor") {
          edu.test.base=chisq.test(table(Matched.baseline.all$EDU, as.factor(Matched.baseline.all$match)))
          success.edu.base=ifelse(edu.test.base$p.value>value.match, 1, 0)
        }
        if (class(Matched.data.all$EDU)=="numeric") {
          edu.test.base=wilcox.test(Matched.baseline.all$EDU, Matched.data.Return$EDU)
          success.edu.base=ifelse(edu.test.base$p.value>value.match, 1, 0)
        }
        
        if (class(Matched.data.all$AGE)=="factor") {
          age.test.base=chisq.test(table(Matched.baseline.all$AGE, as.factor(Matched.baseline.all$match)))
          baseline.success.age=ifelse(age.test.base$p.value>value.match, 1, 0)
        }
        if (class(Matched.data.all$AGE)=="numeric") {
          age.test.base=wilcox.test(Matched.baseline.all$AGE, Matched.data.Return$AGE)
          baseline.success.age=ifelse(age.test.base$p.value>value.match, 1, 0)
        }
        
        if (class(Matched.baseline.all$Match1)=='factor') {
          match1.test=anova(lm(match~Match1, data=Matched.baseline.all))
          baseline.success.Match1=ifelse(match1.test$`Pr(>F)`[1]>value.match, 1, 0)
          
        }
        if (class(Matched.baseline.all$Match1)=='numeric') {
          match1.test=wilcox.test(Matched.baseline.chosen$Match1, Matched.data.Return$Match1) #Match 3 is always numerical
          baseline.success.Match1=ifelse(match1.test$p.value>value.match, 1, 0)
        }
        
        if (class(Matched.baseline.all$Match2)=='factor') {
          match2.test=anova(lm(match~Match2, data=Matched.baseline.all))
          baseline.success.Match2=ifelse(match2.test$`Pr(>F)`[1]>value.match, 1, 0)
          
        }
        if (class(Matched.baseline.all$Match2)=='numeric') {
          match2.test=wilcox.test(Matched.baseline.chosen$Match2, Matched.data.Return$Match2) #Match 3 is always numerical
          baseline.success.Match2=ifelse(match2.test$p.value>value.match, 1, 0)
        }
        
        if (class(Matched.baseline.all$Match3)=='factor') {
          match3.test=anova(lm(match~Match3, data=Matched.baseline.all))
          baseline.success.Match3=ifelse(match3.test$`Pr(>F)`[1]>value.match, 1, 0)
          
        }
        if (class(Matched.baseline.all$Match3)=='numeric') {
          match3.test=wilcox.test(Matched.baseline.chosen$Match3, Matched.data.Return$Match3) #Match 3 is always numerical
          baseline.success.Match3=ifelse(match3.test$p.value>value.match, 1, 0)
        }  
        
        if (class(Matched.baseline.all$Match4)=='factor') {
          match4.test=anova(lm(match~Match4, data=Matched.baseline.all))
          baseline.success.Match4=ifelse(match4.test$`Pr(>F)`[1]>value.match, 1, 0)
          
        }
        if (class(Matched.baseline.all$Match4)=='numeric') {
          match4.test=wilcox.test(Matched.baseline.chosen$Match4, Matched.data.Return$Match4) #Match 3 is always numerical
          baseline.success.Match4=ifelse(match4.test$p.value>value.match, 1, 0)
        }  
        
        if (class(Matched.baseline.all$Match5)=='factor') {
          match5.test=anova(lm(match~Match5, data=Matched.baseline.all))
          baseline.success.Match5=ifelse(match5.test$`Pr(>F)`[1]>value.match, 1, 0)
          
        }
        if (class(Matched.baseline.all$Match5)=='numeric') {
          match5.test=wilcox.test(Matched.baseline.chosen$Match5, Matched.data.Return$Match5) #Match 3 is always numerical
          baseline.success.Match5=ifelse(match5.test$p.value>value.match, 1, 0)
        } 
        
        #Leave loop conditions
        baseline.success.match=ifelse(nrow(Matched.data.PR)>(.9*N.returnee), 1, 0)
        #exit code
        baseline.success=ifelse(baseline.success.match==1 & baseline.success.age==1 & baseline.success.edu==1 &  
                                  baseline.success.Match1==1 & 
                                  baseline.success.Match2==1 &
                                  baseline.success.Match3==1 &
                                  baseline.success.Match4==1 &
                                  baseline.success.Match5==1, 1,0)
      }
      if (n==6) {
        m.out.baseline <- matchit(match ~ Match1+AGE+EDU + Match2+ Match3+ Match4 +Match5 +Match6, method='nearest',  data = baseline.matching, link='probit')
        
        Matched.baseline.all=match.data(m.out.baseline)
        Matched.baseline.chosen=subset(Matched.baseline.all, match==0)
        Matched.data.Return=subset(Matched.baseline.all, match==1)
        
        if (class(Matched.data.all$EDU)=="factor") {
          edu.test.base=chisq.test(table(Matched.baseline.all$EDU, as.factor(Matched.baseline.all$match)))
          success.edu.base=ifelse(edu.test.base$p.value>value.match, 1, 0)
        }
        if (class(Matched.data.all$EDU)=="numeric") {
          edu.test.base=wilcox.test(Matched.baseline.all$EDU, Matched.data.Return$EDU)
          success.edu.base=ifelse(edu.test.base$p.value>value.match, 1, 0)
        }
        
        if (class(Matched.data.all$AGE)=="factor") {
          age.test.base=chisq.test(table(Matched.baseline.all$AGE, as.factor(Matched.baseline.all$match)))
          baseline.success.age=ifelse(age.test.base$p.value>value.match, 1, 0)
        }
        if (class(Matched.data.all$AGE)=="numeric") {
          age.test.base=wilcox.test(Matched.baseline.all$AGE, Matched.data.Return$AGE)
          baseline.success.age=ifelse(age.test.base$p.value>value.match, 1, 0)
        }
        
        if (class(Matched.baseline.all$Match1)=='factor') {
          match1.test=anova(lm(match~Match1, data=Matched.baseline.all))
          baseline.success.Match1=ifelse(match1.test$`Pr(>F)`[1]>value.match, 1, 0)
          
        }
        if (class(Matched.baseline.all$Match1)=='numeric') {
          match1.test=wilcox.test(Matched.baseline.chosen$Match1, Matched.data.Return$Match1) #Match 3 is always numerical
          baseline.success.Match1=ifelse(match1.test$p.value>value.match, 1, 0)
        }
        
        if (class(Matched.baseline.all$Match2)=='factor') {
          match2.test=anova(lm(match~Match2, data=Matched.baseline.all))
          baseline.success.Match2=ifelse(match2.test$`Pr(>F)`[1]>value.match, 1, 0)
          
        }
        if (class(Matched.baseline.all$Match2)=='numeric') {
          match2.test=wilcox.test(Matched.baseline.chosen$Match2, Matched.data.Return$Match2) #Match 3 is always numerical
          baseline.success.Match2=ifelse(match2.test$p.value>value.match, 1, 0)
        }
        
        if (class(Matched.baseline.all$Match3)=='factor') {
          match3.test=anova(lm(match~Match3, data=Matched.baseline.all))
          baseline.success.Match3=ifelse(match3.test$`Pr(>F)`[1]>value.match, 1, 0)
          
        }
        if (class(Matched.baseline.all$Match3)=='numeric') {
          match3.test=wilcox.test(Matched.baseline.chosen$Match3, Matched.data.Return$Match3) #Match 3 is always numerical
          baseline.success.Match3=ifelse(match3.test$p.value>value.match, 1, 0)
        }  
        
        if (class(Matched.baseline.all$Match4)=='factor') {
          match4.test=anova(lm(match~Match4, data=Matched.baseline.all))
          baseline.success.Match4=ifelse(match4.test$`Pr(>F)`[1]>value.match, 1, 0)
          
        }
        if (class(Matched.baseline.all$Match4)=='numeric') {
          match4.test=wilcox.test(Matched.baseline.chosen$Match4, Matched.data.Return$Match4) #Match 3 is always numerical
          baseline.success.Match4=ifelse(match4.test$p.value>value.match, 1, 0)
        }  
        
        if (class(Matched.baseline.all$Match5)=='factor') {
          match5.test=anova(lm(match~Match5, data=Matched.baseline.all))
          baseline.success.Match5=ifelse(match5.test$`Pr(>F)`[1]>value.match, 1, 0)
          
        }
        if (class(Matched.baseline.all$Match5)=='numeric') {
          match5.test=wilcox.test(Matched.baseline.chosen$Match5, Matched.data.Return$Match5) #Match 3 is always numerical
          baseline.success.Match5=ifelse(match5.test$p.value>value.match, 1, 0)
        } 
        
        if (class(Matched.baseline.all$Match6)=='factor') {
          match6.test=anova(lm(match~Match6, data=Matched.baseline.all))
          baseline.success.Match6=ifelse(match6.test$`Pr(>F)`[1]>value.match, 1, 0)
          
        }
        if (class(Matched.baseline.all$Match6)=='numeric') {
          match6.test=wilcox.test(Matched.baseline.chosen$Match6, Matched.data.Return$Match6) #Match 3 is always numerical
          baseline.success.Match6=ifelse(match6.test$p.value>value.match, 1, 0)
        } 
        
        #Leave loop conditions
        baseline.success.match=ifelse(nrow(Matched.data.PR)>(.9*N.returnee), 1, 0)         

        #exit code
        baseline.success=ifelse(baseline.success.match==1 & baseline.success.age==1 & baseline.success.edu==1 &  
                                  baseline.success.Match1==1 & 
                                  baseline.success.Match2==1 &
                                  baseline.success.Match3==1 &
                                  baseline.success.Match4==1 &
                                  baseline.success.Match5==1 &
                                  baseline.success.Match6==1, 1,0)
      }
      if (n==7) {
        m.out.baseline <- matchit(match ~ Match1+AGE+EDU + Match2+ Match3+ Match4 +Match5 +Match6 +Match7, method='nearest',  data = baseline.matching, link='probit')
        
        Matched.baseline.all=match.data(m.out.baseline)
        Matched.baseline.chosen=subset(Matched.baseline.all, match==0)
        Matched.data.Return=subset(Matched.baseline.all, match==1)
        
        #Tests to leave loop
        if (class(Matched.data.all$EDU)=="factor") {
          edu.test.base=chisq.test(table(Matched.baseline.all$EDU, as.factor(Matched.baseline.all$match)))
          success.edu.base=ifelse(edu.test.base$p.value>value.match, 1, 0)
        }
        if (class(Matched.data.all$EDU)=="numeric") {
          edu.test.base=wilcox.test(Matched.baseline.all$EDU, Matched.data.Return$EDU)
          success.edu.base=ifelse(edu.test.base$p.value>value.match, 1, 0)
        }
        
        if (class(Matched.data.all$AGE)=="factor") {
          age.test.base=chisq.test(table(Matched.baseline.all$AGE, as.factor(Matched.baseline.all$match)))
          baseline.success.age=ifelse(age.test.base$p.value>value.match, 1, 0)
        }
        if (class(Matched.data.all$AGE)=="numeric") {
          age.test.base=wilcox.test(Matched.baseline.all$AGE, Matched.data.Return$AGE)
          baseline.success.age=ifelse(age.test.base$p.value>value.match, 1, 0)
        }
        
        if (class(Matched.baseline.all$Match1)=='factor') {
          match1.test=anova(lm(match~Match1, ata=Matched.baseline.all))
          baseline.success.Match1=ifelse(match1.test$`Pr(>F)`[1]>value.match, 1, 0)
          
        }
        if (class(Matched.baseline.all$Match1)=='numeric') {
          match1.test=wilcox.test(Matched.baseline.chosen$Match1, Matched.data.Return$Match1) #Match 3 is always numerical
          baseline.success.Match1=ifelse(match1.test$p.value>value.match, 1, 0)
        }
        
        if (class(Matched.baseline.all$Match2)=='factor') {
          match2.test=anova(lm(match~Match2, data=Matched.baseline.all))
          baseline.success.Match2=ifelse(match2.test$`Pr(>F)`[1]>value.match, 1, 0)
          
        }
        if (class(Matched.baseline.all$Match2)=='numeric') {
          match2.test=wilcox.test(Matched.baseline.chosen$Match2, Matched.data.Return$Match2) #Match 3 is always numerical
          baseline.success.Match2=ifelse(match2.test$p.value>value.match, 1, 0)
        }
        
        if (class(Matched.baseline.all$Match3)=='factor') {
          match3.test=anova(lm(match~Match3, data=Matched.baseline.all))
          baseline.success.Match3=ifelse(match3.test$`Pr(>F)`[1]>value.match, 1, 0)
          
        }
        if (class(Matched.baseline.all$Match3)=='numeric') {
          match3.test=wilcox.test(Matched.baseline.chosen$Match3, Matched.data.Return$Match3) #Match 3 is always numerical
          baseline.success.Match3=ifelse(match3.test$p.value>value.match, 1, 0)
        }  
        
        if (class(Matched.baseline.all$Match4)=='factor') {
          match4.test=anova(lm(match~Match4, data=Matched.baseline.all))
          baseline.success.Match4=ifelse(match4.test$`Pr(>F)`[1]>value.match, 1, 0)
          
        }
        if (class(Matched.baseline.all$Match4)=='numeric') {
          match4.test=wilcox.test(Matched.baseline.chosen$Match4, Matched.data.Return$Match4) #Match 3 is always numerical
          baseline.success.Match4=ifelse(match4.test$p.value>value.match, 1, 0)
        }  
        
        if (class(Matched.baseline.all$Match5)=='factor') {
          match5.test=anova(lm(match~Match5, data=Matched.baseline.all))
          baseline.success.Match5=ifelse(match5.test$`Pr(>F)`[1]>value.match, 1, 0)
          
        }
        if (class(Matched.baseline.all$Match5)=='numeric') {
          match5.test=wilcox.test(Matched.baseline.chosen$Match5, Matched.data.Return$Match5) #Match 3 is always numerical
          baseline.success.Match5=ifelse(match5.test$p.value>value.match, 1, 0)
        } 
        
        if (class(Matched.baseline.all$Match6)=='factor') {
          match6.test=anova(lm(match~Match6, data=Matched.baseline.all))
          baseline.success.Match6=ifelse(match6.test$`Pr(>F)`[1]>value.match, 1, 0)
          
        }
        if (class(Matched.baseline.all$Match6)=='numeric') {
          match6.test=wilcox.test(Matched.baseline.chosen$Match6, Matched.data.Return$Match6) #Match 3 is always numerical
          baseline.success.Match6=ifelse(match6.test$p.value>value.match, 1, 0)
        } 
        
        if (class(Matched.baseline.all$Match7)=='factor') {
          match7.test=anova(lm(match~Match7, data=Matched.baseline.all))
          baseline.success.Match7=ifelse(match7.test$`Pr(>F)`[1]>value.match, 1, 0)
          
        }
        if (class(Matched.baseline.all$Match7)=='numeric') {
          match7.test=wilcox.test(Matched.baseline.chosen$Match7, Matched.data.Return$Match7) #Match 3 is always numerical
          baseline.success.Match7=ifelse(match7.test$p.value>value.match, 1, 0)
        } 
        
        #Leave loop conditions
        baseline.success.match=ifelse(nrow(Matched.data.PR)>(.9*N.returnee), 1, 0)         
        #exit code
        baseline.success=ifelse(baseline.success.match==1 & baseline.success.age==1 & baseline.success.edu==1 &  
                                  baseline.success.Match1==1 & 
                                  baseline.success.Match2==1 &
                                  baseline.success.Match3==1 &
                                  baseline.success.Match4==1 &
                                  baseline.success.Match5==1 &
                                  baseline.success.Match6==1 &
                                  baseline.success.Match7==1, 1,0)
      }
      if (n==8) {
        m.out.baseline <- matchit(match ~ Match1+AGE+EDU + Match2+ Match3+ Match4 +Match5 +Match6 +Match7 +Match8, method='nearest',  data = baseline.matching, link='probit')
        
        Matched.baseline.all=match.data(m.out.baseline)
        Matched.baseline.chosen=subset(Matched.baseline.all, match==0)
        Matched.data.Return=subset(Matched.baseline.all, match==1)
        
        #Tests to leave loop
        if (class(Matched.data.all$EDU)=="factor") {
          edu.test.base=chisq.test(table(Matched.baseline.all$EDU, as.factor(Matched.baseline.all$match)))
          baseline.success.edu=ifelse(edu.test.base$p.value>value.match, 1, 0)
        }
        if (class(Matched.data.all$EDU)=="numeric") {
          edu.test.base=wilcox.test(Matched.baseline.all$EDU, Matched.data.Return$EDU)
          baseline.success.edu=ifelse(edu.test.base$p.value>value.match, 1, 0)
        }
        
        if (class(Matched.data.all$AGE)=="factor") {
          age.test.base=chisq.test(table(Matched.baseline.all$AGE, as.factor(Matched.baseline.all$match)))
          baseline.success.age=ifelse(age.test.base$p.value>value.match, 1, 0)
        }
        if (class(Matched.data.all$AGE)=="numeric") {
          age.test.base=wilcox.test(Matched.baseline.all$AGE, Matched.data.Return$AGE)
          baseline.success.age=ifelse(age.test.base$p.value>value.match, 1, 0)
        }
        
        if (class(Matched.baseline.all$Match1)=='factor') {
          match1.test=anova(lm(match~Match1, data=Matched.baseline.all))
          baseline.success.Match1=ifelse(match1.test$`Pr(>F)`[1]>value.match, 1, 0)
          
        }
        if (class(Matched.baseline.all$Match1)=='numeric') {
          match1.test=wilcox.test(Matched.baseline.chosen$Match1, Matched.data.Return$Match1) #Match 3 is always numerical
          baseline.success.Match1=ifelse(match1.test$p.value>value.match, 1, 0)
        }
        
        if (class(Matched.baseline.all$Match2)=='factor') {
          match2.test=anova(lm(match~Match2, data=Matched.baseline.all))
          baseline.success.Match2=ifelse(match2.test$`Pr(>F)`[1]>value.match, 1, 0)
          
        }
        if (class(Matched.baseline.all$Match2)=='numeric') {
          match2.test=wilcox.test(Matched.baseline.chosen$Match2, Matched.data.Return$Match2) #Match 3 is always numerical
          baseline.success.Match2=ifelse(match2.test$p.value>value.match, 1, 0)
        }
        
        if (class(Matched.baseline.all$Match3)=='factor') {
          match3.test=anova(lm(match~Match3, data=Matched.baseline.all))
          baseline.success.Match3=ifelse(match3.test$`Pr(>F)`[1]>value.match, 1, 0)
          
        }
        if (class(Matched.baseline.all$Match3)=='numeric') {
          match3.test=wilcox.test(Matched.baseline.chosen$Match3, Matched.data.Return$Match3) #Match 3 is always numerical
          baseline.success.Match3=ifelse(match3.test$p.value>value.match, 1, 0)
        }  
        
        if (class(Matched.baseline.all$Match4)=='factor') {
          match4.test=anova(lm(match~Match4, data=Matched.baseline.all))
          baseline.success.Match4=ifelse(match4.test$`Pr(>F)`[1]>value.match, 1, 0)
          
        }
        if (class(Matched.baseline.all$Match4)=='numeric') {
          match4.test=wilcox.test(Matched.baseline.chosen$Match4, Matched.data.Return$Match4) #Match 3 is always numerical
          baseline.success.Match4=ifelse(match4.test$p.value>value.match, 1, 0)
        }  
        
        if (class(Matched.baseline.all$Match5)=='factor') {
          match5.test=anova(lm(match~Match5, data=Matched.baseline.all))
          baseline.success.Match5=ifelse(match5.test$`Pr(>F)`[1]>value.match, 1, 0)
          
        }
        if (class(Matched.baseline.all$Match5)=='numeric') {
          match5.test=wilcox.test(Matched.baseline.chosen$Match5, Matched.data.Return$Match5) #Match 3 is always numerical
          baseline.success.Match5=ifelse(match5.test$p.value>value.match, 1, 0)
        } 
        
        if (class(Matched.baseline.all$Match6)=='factor') {
          match6.test=anova(lm(match~Match6, data=Matched.baseline.all))
          baseline.success.Match6=ifelse(match6.test$`Pr(>F)`[1]>value.match, 1, 0)
          
        }
        if (class(Matched.baseline.all$Match6)=='numeric') {
          match6.test=wilcox.test(Matched.baseline.chosen$Match6, Matched.data.Return$Match6) #Match 3 is always numerical
          baseline.success.Match6=ifelse(match6.test$p.value>value.match, 1, 0)
        } 
        
        if (class(Matched.baseline.all$Match7)=='factor') {
          match7.test=anova(lm(match~Match7, data=Matched.baseline.all))
          baseline.success.Match7=ifelse(match7.test$`Pr(>F)`[1]>value.match, 1, 0)
          
        }
        if (class(Matched.baseline.all$Match7)=='numeric') {
          match7.test=wilcox.test(Matched.baseline.chosen$Match7, Matched.data.Return$Match7) #Match 3 is always numerical
          baseline.success.Match7=ifelse(match7.test$p.value>value.match, 1, 0)
        } 
        
        if (class(Matched.baseline.all$Match8)=='factor') {
          match8.test=anova(lm(match~Match8, data=Matched.baseline.all))
          baseline.success.Match8=ifelse(match8.test$`Pr(>F)`[1]>value.match, 1, 0)
          
        }
        if (class(Matched.baseline.all$Match8)=='numeric') {
          match8.test=wilcox.test(Matched.baseline.chosen$Match8, Matched.data.Return$Match8) #Match 3 is always numerical
          baseline.success.Match8=ifelse(match8.test$p.value>value.match, 1, 0)
        } 
        
        #Leave loop conditions
        baseline.success.match=ifelse(nrow(Matched.data.PR)>(.9*N.returnee), 1, 0)         
        #exit code
        baseline.success=ifelse(baseline.success.match==1 & baseline.success.age==1 & baseline.success.edu==1 &  
                                  baseline.success.Match1==1 & 
                                  baseline.success.Match2==1 &
                                  baseline.success.Match3==1 &
                                  baseline.success.Match4==1 &
                                  baseline.success.Match5==1 &
                                  baseline.success.Match6==1 &
                                  baseline.success.Match7==1 &
                                  baseline.success.Match8==1, 1,0)
      }
      if (n==9) {
        m.out.baseline <- matchit(match ~ Match1+AGE+EDU + Match2+ Match3+ Match4 +Match5 +Match6 +Match7 +Match8 +Match9, method='nearest',  data = baseline.matching, link='probit')
        
        Matched.baseline.all=match.data(m.out.baseline)
        Matched.baseline.chosen=subset(Matched.baseline.all, match==0)
        Matched.data.Return=subset(Matched.baseline.all, match==1)
        
        #Tests to leave loop
        if (class(Matched.data.all$EDU)=="factor") {
          edu.test.base=chisq.test(table(Matched.baseline.all$EDU, as.factor(Matched.baseline.all$match)))
          baseline.success.edu=ifelse(edu.test.base$p.value>value.match, 1, 0)
        }
        if (class(Matched.data.all$EDU)=="numeric") {
          edu.test.base=wilcox.test(Matched.baseline.all$EDU, Matched.data.Return$EDU)
          baseline.success.edu=ifelse(edu.test.base$p.value>value.match, 1, 0)
        }
        
        if (class(Matched.data.all$AGE)=="factor") {
          age.test.base=chisq.test(table(Matched.baseline.all$AGE, as.factor(Matched.baseline.all$match)))
          baseline.success.age=ifelse(age.test.base$p.value>value.match, 1, 0)
        }
        if (class(Matched.data.all$AGE)=="numeric") {
          age.test.base=wilcox.test(Matched.baseline.all$AGE, Matched.data.Return$AGE)
          baseline.success.age=ifelse(age.test.base$p.value>value.match, 1, 0)
        }
        
        if (class(Matched.baseline.all$Match1)=='factor') {
          match1.test=anova(lm(match~Match1, data=Matched.baseline.all))
          baseline.success.Match1=ifelse(match1.test$`Pr(>F)`[1]>value.match, 1, 0)
          
        }
        if (class(Matched.baseline.all$Match1)=='numeric') {
          match1.test=wilcox.test(Matched.baseline.chosen$Match1, Matched.data.Return$Match1) #Match 3 is always numerical
          baseline.success.Match1=ifelse(match1.test$p.value>value.match, 1, 0)
        }
        
        if (class(Matched.baseline.all$Match2)=='factor') {
          match2.test=anova(lm(match~Match2, data=Matched.baseline.all))
          baseline.success.Match2=ifelse(match2.test$`Pr(>F)`[1]>value.match, 1, 0)
          
        }
        if (class(Matched.baseline.all$Match2)=='numeric') {
          match2.test=wilcox.test(Matched.baseline.chosen$Match2, Matched.data.Return$Match2) #Match 3 is always numerical
          baseline.success.Match2=ifelse(match2.test$p.value>value.match, 1, 0)
        }
        
        if (class(Matched.baseline.all$Match3)=='factor') {
          match3.test=anova(lm(match~Match3, data=Matched.baseline.all))
          baseline.success.Match3=ifelse(match3.test$`Pr(>F)`[1]>value.match, 1, 0)
          
        }
        if (class(Matched.baseline.all$Match3)=='numeric') {
          match3.test=wilcox.test(Matched.baseline.chosen$Match3, Matched.data.Return$Match3) #Match 3 is always numerical
          baseline.success.Match3=ifelse(match3.test$p.value>value.match, 1, 0)
        }  
        
        if (class(Matched.baseline.all$Match4)=='factor') {
          match4.test=anova(lm(match~Match4, data=Matched.baseline.all))
          baseline.success.Match4=ifelse(match4.test$`Pr(>F)`[1]>value.match, 1, 0)
          
        }
        if (class(Matched.baseline.all$Match4)=='numeric') {
          match4.test=wilcox.test(Matched.baseline.chosen$Match4, Matched.data.Return$Match4) #Match 3 is always numerical
          baseline.success.Match4=ifelse(match4.test$p.value>value.match, 1, 0)
        }  
        
        if (class(Matched.baseline.all$Match5)=='factor') {
          match5.test=anova(lm(match~Match5, data=Matched.baseline.all))
          baseline.success.Match5=ifelse(match5.test$`Pr(>F)`[1]>value.match, 1, 0)
          
        }
        if (class(Matched.baseline.all$Match5)=='numeric') {
          match5.test=wilcox.test(Matched.baseline.chosen$Match5, Matched.data.Return$Match5) #Match 3 is always numerical
          baseline.success.Match5=ifelse(match5.test$p.value>value.match, 1, 0)
        } 
        
        if (class(Matched.baseline.all$Match6)=='factor') {
          match6.test=anova(lm(match~Match5, data=Matched.baseline.all))
          baseline.success.Match6=ifelse(match6.test$`Pr(>F)`[1]>value.match, 1, 0)
          
        }
        if (class(Matched.baseline.all$Match6)=='numeric') {
          match6.test=wilcox.test(Matched.baseline.chosen$Match6, Matched.data.Return$Match6) #Match 3 is always numerical
          baseline.success.Match6=ifelse(match6.test$p.value>value.match, 1, 0)
        } 
        
        if (class(Matched.baseline.all$Match7)=='factor') {
          match7.test=anova(lm(match~Match7, data=Matched.baseline.all))
          baseline.success.Match7=ifelse(match7.test$`Pr(>F)`[1]>value.match, 1, 0)
          
        }
        if (class(Matched.baseline.all$Match7)=='numeric') {
          match7.test=wilcox.test(Matched.baseline.chosen$Match7, Matched.data.Return$Match7) #Match 3 is always numerical
          baseline.success.Match7=ifelse(match7.test$p.value>value.match, 1, 0)
        } 
        
        if (class(Matched.baseline.all$Match8)=='factor') {
          match8.test=anova(lm(match~Match8, data=Matched.baseline.all))
          baseline.success.Match8=ifelse(match8.test$`Pr(>F)`[1]>value.match, 1, 0)
          
        }
        if (class(Matched.baseline.all$Match8)=='numeric') {
          match8.test=wilcox.test(Matched.baseline.chosen$Match8, Matched.data.Return$Match8) #Match 3 is always numerical
          baseline.success.Match8=ifelse(match8.test$p.value>value.match, 1, 0)
        } 
        
        if (class(Matched.baseline.all$Match9)=='factor') {
          match9.test=anova(lm(match~Match9, data=Matched.baseline.all))
          baseline.success.Match9=ifelse(match9.test$`Pr(>F)`[1]>value.match, 1, 0)
          
        }
        if (class(Matched.baseline.all$Match9)=='numeric') {
          match9.test=wilcox.test(Matched.baseline.chosen$Match9, Matched.data.Return$Match9) #Match 3 is always numerical
          baseline.success.Match9=ifelse(match9.test$p.value>value.match, 1, 0)
        } 
        
        #Leave loop conditions
        baseline.success.match=ifelse(nrow(Matched.data.PR)>(.9*N.returnee), 1, 0)         
        #exit code
        baseline.success=ifelse(baseline.success.match==1 & baseline.success.age==1 & baseline.success.edu==1 &  
                                  baseline.success.Match1==1 & 
                                  baseline.success.Match2==1 &
                                  baseline.success.Match3==1 &
                                  baseline.success.Match4==1 &
                                  baseline.success.Match5==1 &
                                  baseline.success.Match6==1 &
                                  baseline.success.Match7==1 &
                                  baseline.success.Match8==1 &
                                  baseline.success.Match9==1, 1,0)
      }
      if (n==10) {
        m.out.baseline <- matchit(match ~ Match1+AGE+EDU + Match2+ Match3+ Match4 +Match5 +Match6 +Match7 +Match8 +Match9 +Match10, method='nearest',  data = baseline.matching, link='probit')
        
        Matched.baseline.all=match.data(m.out.baseline)
        Matched.baseline.chosen=subset(Matched.baseline.all, match==0)
        Matched.data.Return=subset(Matched.baseline.all, match==1)
        
        #Tests to leave loop
        if (class(Matched.data.all$EDU)=="factor") {
          edu.test.base=chisq.test(table(Matched.baseline.all$EDU, as.factor(Matched.baseline.all$match)))
          baseline.success.edu=ifelse(edu.test.base$p.value>value.match, 1, 0)
        }
        if (class(Matched.data.all$EDU)=="numeric") {
          edu.test.base=wilcox.test(Matched.baseline.all$EDU, Matched.data.Return$EDU)
          baseline.success.edu=ifelse(edu.test.base$p.value>value.match, 1, 0)
        }
        
        if (class(Matched.data.all$AGE)=="factor") {
          age.test.base=chisq.test(table(Matched.baseline.all$AGE, as.factor(Matched.baseline.all$match)))
          baseline.success.age=ifelse(age.test.base$p.value>value.match, 1, 0)
        }
        if (class(Matched.data.all$AGE)=="numeric") {
          age.test.base=wilcox.test(Matched.baseline.all$AGE, Matched.data.Return$AGE)
          baseline.success.age=ifelse(age.test.base$p.value>value.match, 1, 0)
        }
        
        if (class(Matched.baseline.all$Match1)=='factor') {
          match1.test=anova(lm(match~Match1, data=Matched.baseline.all))
          baseline.success.Match1=ifelse(match1.test$`Pr(>F)`[1]>value.match, 1, 0)
          
        }
        if (class(Matched.baseline.all$Match1)=='numeric') {
          match1.test=wilcox.test(Matched.baseline.chosen$Match1, Matched.data.Return$Match1) #Match 3 is always numerical
          baseline.success.Match1=ifelse(match1.test$p.value>value.match, 1, 0)
        }
        
        if (class(Matched.baseline.all$Match2)=='factor') {
          match2.test=anova(lm(match~Match2, data=Matched.baseline.all))
          baseline.success.Match2=ifelse(match2.test$`Pr(>F)`[1]>value.match, 1, 0)
          
        }
        if (class(Matched.baseline.all$Match2)=='numeric') {
          match2.test=wilcox.test(Matched.baseline.chosen$Match2, Matched.data.Return$Match2) #Match 3 is always numerical
          baseline.success.Match2=ifelse(match2.test$p.value>value.match, 1, 0)
        }
        
        if (class(Matched.baseline.all$Match3)=='factor') {
          match3.test=anova(lm(match~Match3, data=Matched.baseline.all))
          baseline.success.Match3=ifelse(match3.test$`Pr(>F)`[1]>value.match, 1, 0)
          
        }
        if (class(Matched.baseline.all$Match3)=='numeric') {
          match3.test=wilcox.test(Matched.baseline.chosen$Match3, Matched.data.Return$Match3) #Match 3 is always numerical
          baseline.success.Match3=ifelse(match3.test$p.value>value.match, 1, 0)
        }  
        
        if (class(Matched.baseline.all$Match4)=='factor') {
          match4.test=anova(lm(match~Match4, data=Matched.baseline.all))
          baseline.success.Match4=ifelse(match4.test$`Pr(>F)`[1]>value.match, 1, 0)
          
        }
        if (class(Matched.baseline.all$Match4)=='numeric') {
          match4.test=wilcox.test(Matched.baseline.chosen$Match4, Matched.data.Return$Match4) #Match 3 is always numerical
          baseline.success.Match4=ifelse(match4.test$p.value>value.match, 1, 0)
        }  
        
        if (class(Matched.baseline.all$Match5)=='factor') {
          match5.test=anova(lm(match~Match5, data=Matched.baseline.all))
          baseline.success.Match5=ifelse(match5.test$`Pr(>F)`[1]>value.match, 1, 0)
          
        }
        if (class(Matched.baseline.all$Match5)=='numeric') {
          match5.test=wilcox.test(Matched.baseline.chosen$Match5, Matched.data.Return$Match5) #Match 3 is always numerical
          baseline.success.Match5=ifelse(match5.test$p.value>value.match, 1, 0)
        } 
        
        if (class(Matched.baseline.all$Match6)=='factor') {
          match6.test=anova(lm(match~Match6, data=Matched.baseline.all))
          baseline.success.Match6=ifelse(match6.test$`Pr(>F)`[1]>value.match, 1, 0)
          
        }
        if (class(Matched.baseline.all$Match6)=='numeric') {
          match6.test=wilcox.test(Matched.baseline.chosen$Match6, Matched.data.Return$Match6) #Match 3 is always numerical
          baseline.success.Match6=ifelse(match6.test$p.value>value.match, 1, 0)
        } 
        
        if (class(Matched.baseline.all$Match7)=='factor') {
          match7.test=anova(lm(match~Match7, data=Matched.baseline.all))
          baseline.success.Match7=ifelse(match7.test$`Pr(>F)`[1]>value.match, 1, 0)
          
        }
        if (class(Matched.baseline.all$Match7)=='numeric') {
          match7.test=wilcox.test(Matched.baseline.chosen$Match7, Matched.data.Return$Match7) #Match 3 is always numerical
          baseline.success.Match7=ifelse(match7.test$p.value>value.match, 1, 0)
        } 
        
        if (class(Matched.baseline.all$Match8)=='factor') {
          match8.test=anova(lm(match~Match8, data=Matched.baseline.all))
          baseline.success.Match8=ifelse(match8.test$`Pr(>F)`[1]>value.match, 1, 0)
          
        }
        if (class(Matched.baseline.all$Match8)=='numeric') {
          match8.test=wilcox.test(Matched.baseline.chosen$Match8, Matched.data.Return$Match8) #Match 3 is always numerical
          baseline.success.Match8=ifelse(match8.test$p.value>value.match, 1, 0)
        } 
        
        if (class(Matched.baseline.all$Match9)=='factor') {
          match9.test=anova(lm(match~Match9, data=Matched.baseline.all))
          baseline.success.Match9=ifelse(match9.test$`Pr(>F)`[1]>value.match, 1, 0)
          
        }
        if (class(Matched.baseline.all$Match9)=='numeric') {
          match9.test=wilcox.test(Matched.baseline.chosen$Match9, Matched.data.Return$Match9) #Match 3 is always numerical
          baseline.success.Match9=ifelse(match9.test$p.value>value.match, 1, 0)
        } 
        
        if (class(Matched.baseline.all$Match10)=='factor') {
          match10.test=anova(lm(match~Match10, data=Matched.baseline.all))
          baseline.success.Match10=ifelse(match10.test$`Pr(>F)`[1]>value.match, 1, 0)
          
        }
        if (class(Matched.baseline.all$Match10)=='numeric') {
          match10.test=wilcox.test(Matched.baseline.chosen$Match10, Matched.data.Return$Match10) #Match 3 is always numerical
          baseline.success.Match10=ifelse(match10.test$p.value>value.match, 1, 0)
        } 
        #Leave loop conditions
        baseline.success.match=ifelse(nrow(Matched.data.PR)>(.9*N.returnee), 1, 0)         
        #exit code
        baseline.success=ifelse(baseline.success.match==1 & baseline.success.age==1 & baseline.success.edu==1 &  
                                  baseline.success.Match1==1 & 
                                  baseline.success.Match2==1 &
                                  baseline.success.Match3==1 &
                                  baseline.success.Match4==1 &
                                  baseline.success.Match5==1 &
                                  baseline.success.Match6==1 &
                                  baseline.success.Match7==1 &
                                  baseline.success.Match8==1 &
                                  baseline.success.Match9==1 &
                                  baseline.success.Match10==1, 1,0)
      }
      
    } #Matches dataset based on predefined number of match categories. Age and edu always matched. 
    
    {       #fail code
      if (baseline.success!=1) {
        baseline.count=baseline.count+1
        
      }
      if (baseline.count>base.match.count) {
        restart=TRUE
        baseline.success=1
        
      }}#Fail code
    
    
    
  }
  
  if (restart==FALSE) {
    
    
    if (transform.log==TRUE) {
      #combine bootstrapped results   
      Difference.Score.Iteration=exp(mean(Returnee.FollowUp.Iteration$COGCOG))-exp(mean(Pseudo.Replace.Iteration$COGCOG))
      Attrition.Effect.Iteration=exp(mean(Returnee.Baseline.Iteration$COGCOG))-exp(mean(Matched.baseline.chosen$COGCOG))
      Practice.Effect.Iteration=Difference.Score.Iteration-Attrition.Effect.Iteration
      
      Matched.baseline.chosen$COGCOG=exp(Matched.baseline.chosen$COGCOG)-3
      Returnee.Baseline.Iteration$COGCOG=exp(Returnee.Baseline.Iteration$COGCOG)-3
      Returnee.FollowUp.Iteration$COGCOG=exp(Returnee.FollowUp.Iteration$COGCOG)-3
      Pseudo.Replace.Iteration$COGCOG=exp(Pseudo.Replace.Iteration$COGCOG)-3
    }
    if (transform.1log==TRUE) {
      Difference.Score.Iteration=exp(1/mean(Returnee.FollowUp.Iteration$COGCOG))-exp(1/mean(Pseudo.Replace.Iteration$COGCOG))
      Attrition.Effect.Iteration=exp(1/mean(Returnee.Baseline.Iteration$COGCOG))-exp(1/mean(Matched.baseline.chosen$COGCOG))
      Practice.Effect.Iteration=Difference.Score.Iteration-Attrition.Effect.Iteration
      
      Matched.baseline.chosen$COGCOG=exp(1/Matched.baseline.chosen$COGCOG)-3
      Returnee.Baseline.Iteration$COGCOG=exp(1/Returnee.Baseline.Iteration$COGCOG)-3
      Returnee.FollowUp.Iteration$COGCOG=exp(1/Returnee.FollowUp.Iteration$COGCOG)-3
      Pseudo.Replace.Iteration$COGCOG=exp(1/Pseudo.Replace.Iteration$COGCOG)-3
      
      
    }
    if (transform.sqrt==TRUE) {
      #combine bootstrapped results   
      Difference.Score.Iteration=(mean(Returnee.FollowUp.Iteration$COGCOG))^2-(mean(Pseudo.Replace.Iteration$COGCOG))^2
      Attrition.Effect.Iteration=(mean(Returnee.Baseline.Iteration$COGCOG))^2-(mean(Matched.baseline.chosen$COGCOG))^2
      Practice.Effect.Iteration=Difference.Score.Iteration-Attrition.Effect.Iteration
      
      Matched.baseline.chosen$COGCOG=((Matched.baseline.chosen$COGCOG)^2)-3
      Returnee.Baseline.Iteration$COGCOG=((Returnee.Baseline.Iteration$COGCOG)^2)-3
      Returnee.FollowUp.Iteration$COGCOG=((Returnee.FollowUp.Iteration$COGCOG)^2)-3
      Pseudo.Replace.Iteration$COGCOG=((Pseudo.Replace.Iteration$COGCOG)^2)-3
    }
    
    #if  NEGATIVE skew
    if (transform.sqrt.NEG==TRUE) {
      
      Difference.Score.Iteration=(mean(Returnee.FollowUp.Iteration$COGCOG))^2-(mean(Pseudo.Replace.Iteration$COGCOG))^2
      Attrition.Effect.Iteration=(mean(Returnee.Baseline.Iteration$COGCOG))^2-(mean(Matched.baseline.chosen$COGCOG))^2
      Practice.Effect.Iteration=Difference.Score.Iteration-Attrition.Effect.Iteration
      
      Matched.baseline.chosen$COGCOG=((Matched.baseline.chosen$COGCOG)^2)-3
      Returnee.Baseline.Iteration$COGCOG=((Returnee.Baseline.Iteration$COGCOG)^2)-3
      Returnee.FollowUp.Iteration$COGCOG=((Returnee.FollowUp.Iteration$COGCOG)^2)-3
      Pseudo.Replace.Iteration$COGCOG=((Pseudo.Replace.Iteration$COGCOG)^2)-3
      
      x=sqrt(max(y+1)-y)
      X^2=m.sqrt.neg-y
      m.sqrt.neg=max(y+1)
      y=-x^2+m.sqrt.neg
      
      Returnee.baseline.data$COGCOG=sqrt(max(Returnee.baseline.data$COGCOG+1)-Returnee.baseline.data$COGCOG)
      Returnee.FollowUp.data$COGCOG=sqrt(max(Returnee.FollowUp.data$COGCOG+1)-Returnee.FollowUp.data$COGCOG)
      Full.Baseline.data$COGCOG=sqrt(max(Full.Baseline.data$COGCOG+1)-Full.Baseline.data$COGCOG)
      Attritors.baseline$COGCOG=sqrt(max(Attritors.baseline$COGCOG+1)-Attritors.baseline$COGCOG)
      hist(Returnee.baseline.data$COGCOG)
      hist(Returnee.FollowUp.data$COGCOG)
      hist(Full.Baseline.data$COGCOG)
      hist(Attritors.baseline$COGCOG)
      
      
    }#not finished yet
    #if moderate positive skew
    if (transform.log.NEG==TRUE) {
      Returnee.baseline.data$COGCOG=log(max(Returnee.baseline.data$COGCOG+1)-Returnee.baseline.data$COGCOG)
      Returnee.FollowUp.data$COGCOG=log(max(Returnee.FollowUp.data$COGCOG+1)-Returnee.FollowUp.data$COGCOG)
      Full.Baseline.data$COGCOG=log(max(Full.Baseline.data$COGCOG+1)-Full.Baseline.data$COGCOG)
      Attritors.baseline$COGCOG=log(max(Attritors.baseline$COGCOG+1)-Attritors.baseline$COGCOG)
      hist(Returnee.baseline.data$COGCOG)
      hist(Returnee.FollowUp.data$COGCOG)
      hist(Full.Baseline.data$COGCOG)
      hist(Attritors.baseline$COGCOG)
      
      y=log(x+1)-x
      
      exp(y)=x+1-exp(x)
      exp(y)-1=x-exp(x)
      exp(y)-1=x(1-exp(1))
      y= Returnee.baseline.data$COGCOG
      x=(exp(y)-1)/(1-exp(1))
      
      
    } #not finished yet
    #if heavy positive skew
    if (transform.1log.NEG==TRUE) {
      Returnee.baseline.data$COGCOG=1/(max(Returnee.baseline.data$COGCOG+1)-Returnee.baseline.data$COGCOG)
      Returnee.FollowUp.data$COGCOG=1/(max(Returnee.FollowUp.data$COGCOG+1)-Returnee.FollowUp.data)
      Full.Baseline.data$COGCOG=1/(max(Full.Baseline.data$COGCOG+1)-Full.Baseline.data)
      Attritors.baseline$COGCOG=1/(max(Attritors.baseline$COGCOG+1)-Attritors.baseline)
      hist(Returnee.baseline.data$COGCOG)
      hist(Returnee.FollowUp.data$COGCOG)
      hist(Full.Baseline.data$COGCOG)
      hist(Attritors.baseline$COGCOG)
    } #not finished yet
    
    if (transform.log==FALSE & transform.1log==FALSE & transform.sqrt==FALSE) {
      
      Difference.Score.Iteration=mean(Returnee.FollowUp.Iteration$COGCOG)-mean(Pseudo.Replace.Iteration$COGCOG)
      Attrition.Effect.Iteration=mean(Returnee.Baseline.Iteration$COGCOG)-mean(Matched.baseline.chosen$COGCOG)
      Practice.Effect.Iteration=Difference.Score.Iteration-Attrition.Effect.Iteration
    }
    
    Difference.Score.Iteration=(mean(Returnee.FollowUp.Iteration$COGCOG))-(mean(Pseudo.Replace.Iteration$COGCOG))
    Attrition.Effect.Iteration=(mean(Returnee.Baseline.Iteration$COGCOG))-(mean(Matched.baseline.chosen$COGCOG))
    Practice.Effect.Iteration=Difference.Score.Iteration-Attrition.Effect.Iteration
        #combine bootstrapped results   

    Practice.Effect=rbind(Practice.Effect, Practice.Effect.Iteration)
    Attrition.Effect=rbind(Attrition.Effect, Attrition.Effect.Iteration)
    
    Mean.Bootstrap.data.temp=data.frame('baseline'=mean(Matched.baseline.chosen$COGCOG),
                                        'baseline.SD'=SD(Matched.baseline.chosen$COGCOG),
                                        'returnee.baseline'=mean(Returnee.Baseline.Iteration$COGCOG),
                                        'returnee.baseline.SD'=SD(Returnee.Baseline.Iteration$COGCOG),
                                        'returnee.FU'=mean(Returnee.FollowUp.Iteration$COGCOG), 
                                        'returnee.FU.SD'=SD(Returnee.FollowUp.Iteration$COGCOG),
                                        'replcacement'=mean(Pseudo.Replace.Iteration$COGCOG),
                                        'replcacement.SD'=SD(Pseudo.Replace.Iteration$COGCOG))
    
    
    Mean.Bootstrap.data=rbind(Mean.Bootstrap.data,Mean.Bootstrap.data.temp )
    
    databaselist.returnee.followup[[PE]]=Returnee.FollowUp.Iteration
    databaselist.replacement.baseline[[PE]]=Pseudo.Replace.Iteration
    databaselist.basleine[[PE]]=Matched.baseline.chosen
    databaselist.returnee.baseline[[PE]]=Returnee.Baseline.Iteration
    
    restart=TRUE
    PE=PE+1
    progress(100*PE/N.iteration)
    Sys.sleep(0.01)
  }
  
  
} 

#saves out results
{
  time=proc.time() - ptm #if run immediatly after main while(PE) loop, will give how much time elapsed
  print(Names.Match.Xe)
  Practice.Effect.final=subset(Practice.Effect, Pract.effect!=1000)
  Attrition.Effect.final=subset(Attrition.Effect, Pract.effect!=1000)
  
  Mean.Bootstrap.data.final=subset(Mean.Bootstrap.data,  
                                   baseline!=1000 & baseline.SD!=1000 &
                                     returnee.baseline!=1000 & returnee.baseline.SD!=1000 & returnee.FU!=1000 & 
                                     returnee.FU.SD!=1000 & replcacement!=1000 & replcacement.SD!=1000)
  
  SD=mean(Mean.Bootstrap.data.final$returnee.FU.SD)
  M1=mean(Mean.Bootstrap.data.final$returnee.FU)
  Madj=mean(Mean.Bootstrap.data.final$returnee.FU)-mean(Practice.Effect.final$Pract.effect)
  SDpooled= sqrt(((SD*SD+SD*SD)/2))
  CohenD=(M1-Madj)/SDpooled
  
  Pe.effect.table=data.frame("mean score PE-unadj"    =M1,
                             "mean score PE-adj"      =Madj,
                             "Practice Effect"        =mean(Practice.Effect.final$Pract.effect),
                             "PE: Cohen's D"          =CohenD,
                             "attrition effect"       =mean(Attrition.Effect.final$Pract.effect))
  

  if (transform.1log==TRUE) {
    transform=('Data was 1/log transformed for analysis')
  }
  if (transform.log==TRUE) {
    transform=('Data was log transformed for analysis')
  }
  if (transform.sqrt==TRUE) {
    transform=("data was SQRT transformed for analysis")
  }
  if (transform.1log==FALSE & transform.log==FALSE & transform.sqrt==FALSE) {
    transform="Data was not transformed for analyses"
  }
  
} 

#---------------------------------------------------------------------------------------------#  

                                    #####view results####
                                      time
                                      list.settings
                                      transform
                                      Names.Match.Xe
                                      base.print
                                      describe(Practice.Effect.final)
                                      describe(Attrition.Effect.final)
                                      describe(Mean.Bootstrap.data.final)
                                      Pe.effect.table
                                      #####view results####
#---------------------------------------------------------------------------------------------#  
              
