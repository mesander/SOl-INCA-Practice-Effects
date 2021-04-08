###### Notes + in progress code ######

-UPDATED MATCH CODE FOR chisq.test(). i HAVE DONE IT FOR MATCH1, AGE, AND EDY. NEED TO DO IT FOR REST OF MATCHES. tHEN ALSO FOR BASELINE MATCHING
-as.factor(Matched.data.all$match)),simulate.p.value=TRUE) needs to be set for all chi square, though may be issue here. 
-removed transformations, taking PE after matching
-?how to save out propensity scores?; ?Caliper feature of matchit (e.g., caliper=.1)?
  
  SJT.crosstab? Potential descriptive stat package that will chose the correct test baesd on what variable you are giving it 
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
  
  
} #In progress: Removes perfect scores at both time points.

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
########################      last update: 4/6/21      ########################      
###############################################################################

#--------------------------------------------------------------#
#              Block 0: Initial Setup
#
#
#----------#                                            
###Block0###        <requires edits>
{
  #1. load libraries
  library(psych)
  library(MatchIt)
  library(readr)
  library(dplyr)
  library(haven)
  library(tidyverse)
  library(progress)
  library(svMisc)
  
  #2. Load main database
      #note, data must be in wide format. Must include an education, variable. Age at 2 time points, Cog variable at 2 time points
  MainData<- MainData.ADNI
  
  #3. identify baseline diagnosis variable
      #SOL-INCA: MainData$Base_impaired=ifelse(MainData$total_6item_v1<4,0,1)
  
      #ADNI: Data.V1$MCI_any=(Data.V1$MCI_any-1)*-1 # sets MCI_any to have CN = 1, MCI=0
          #commented out so that MCI_Any=1 -> MCI selected
      
  
  
  
} #Block0: Identify main database. Select baseline diagnosis variable
#----------#                                            
#
#
#--------------------------------------------------------------#
#              Block 1-3 Set up databases to be compared. Block 1 and 2 require manual edits
#
#
#----------#                                            
###Block1###        <requires manual edits>
{ 
  #read in data
    Data.A1 <- as_tibble(MainData)

  #rename  data variables to match what's in script
  Data.B2 <- Data.A1 %>% 
    rename(
      ID     = RID,
      AGE_V1 = Age.v1,
      AGE_V2 = Age.v2,
      V1DATE = EXAMDATE.v1,
      V2DATE = EXAMDATE.v2,
      BASEDX = MCI_any,
      COG_V1 = CATANIMSC.v1,
      COG_V2 = CATANIMSC.v2
    )
  
} #Block 1: Renames variables for script. Creates first database. Loads libraries
#----------#

###Block2###        <requires manual edits>
{
  
  #Unique matching variables for this dataset
  Match.Xe=subset(Data.B2, select = c('ID','PTEDUCAT','ANARTERR', 'PTGENDER')) #ID variables
  Names.Match.Xe=names(Match.Xe)
  Names.Match.Xe[1]="AGE"   #renames ID to AGE for a printout later.

  #in below loop, rename your variables to "Match#". The script supports up to Match10. EDU always = "EDU"
  Match.Xe <- Match.Xe %>% 
    rename(
      EDU    = PTEDUCAT,
      Match1 = ANARTERR,
      Match2 = PTGENDER
      
      
    )
  #Use below code to confirm that your Xe are the correct class.
  #may have to transform variables for normality 
  Match.Xe$Match1=as.numeric(log(Match.Xe$Match1+1))
  Match.Xe$Match2=as.factor(Match.Xe$Match2)
  
  #removes anyonw with missing matching variables. Necessary for matchit()
  Match.Xe.reduced=na.omit(Match.Xe) 
  
} #Block 2: Identifies matching Xe. NOTE-listwise removal of anyone with NA value in matching Xe.
#----------#

###Block3###
{
  #creates True V2 age
  
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
  
  #tracks size of databases
  Return.fullbaseline.size=nrow(Full.Baseline.data)
  Return.baseline.size=nrow(Returnee.baseline.data)
  SAMPLESIZE=data.frame(
    'NUmber of Full Baseline'     =nrow(Full.Baseline.data),
    'NUmber of Returnee Follows'  =nrow(Returnee.FollowUp.data),
    'NUmber of Returnee Baselines'=nrow(Returnee.baseline.data),
    "If !=0 then error in Block3" =(nrow(Returnee.FollowUp.data)-nrow(Returnee.baseline.data))
    
  )
  
  
  
} #Block 3: Selects for DX at baseline. Creates main 3 databases: All, returnee baseline, returnee follow-up. 
#----------#
#
#
#--------------------------------------------------------------#
#           Block 4-5: Define, record, and printout script parameters.
#
#
#----------#
###Block4###        <requires edits>
{#Block6: Manually set parameters
  
  #identify cognitive variable of interest, saved for printout later
  TestVariable="COLOR-WORD"
  
  print(SAMPLESIZE)  #provides sizes of databases. Need to set parameters in Block 5
  
  #select sample sizes, matching details
  N.returnee=200  #number of returnees to start each iteration
  N.returnee.min=51 #minimum number of returnees in sample
  N.PR.Pool=200   #sets minimal number of subjects in potential PR pool. 
  N.PR.Start=300  #number of PR to initially randomly sample. 
  N.base.pool=150 #sets minimum number of subjects in potential baseline pool
  N.base.start=200    #number of baseline to initially randomly sample.
  baseline.match.age.only=TRUE #this makes it so the baseline is age matched at each iteration only. Not matched to returnees on anythigne else. 
  
  #select number of iterations
  N.iteration=2001
  PE=1
  
  #select P value level for group testing to confirm Matchit correctly assigned groups
  value.match=.8
  AGECALIPER=.1 # how far apart in years, the matched groups can be. 
  
  #Loop counters
  PR.match.count=250 #number of attempts for matching PR to Returnees
  base.match.count=250 #number of attempts for matching baseline to returnees
} #Block 4: set up parameters for script
#----------#

###Block5###
{
  
  
  
  #rests saved databsaes
  Bootstrap.data=data.frame(Pract.effect=1000)
  Practice.Effect=data.frame(Pract.effect=1000)
  Difference.Effect=data.frame(Pract.effect=1000)
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
  Mean.Bootstrap.data.temp=NULL
  Mean.Bootstrap.data.final=NULL
  Pe.effect.table=NULL
  
  list.settings=data.frame("Variable of interest"=TestVariable,
                           "N returnee selected"=N.returnee,
                           "N potential replacments"=N.PR.Pool,
                           "N replacments start match with"=N.PR.Start,
                           "Max attempts to match returrnee-replacments"=PR.match.count,
                           "N potential baseline subs"=N.base.pool,
                           "N potential baseline start with"=N.base.start,
                           "Max attempts to match returrnee-baseline"=base.match.count,
                           "Max iterations"=N.iteration-1,
                           "Iterations achieved"=PE,
                           "P value match"=value.match,
                           "baseline matched to only age"=baseline.match.age.only,
                           "Total number returnees" =Return.baseline.size,
                           "Total number with baseline"=Return.fullbaseline.size
  )
  
  
  #prints out settings
  
  
}# Block 5: Records parameter values in "settings." Resets temporary databases used by main script
#----------#
#
#
#--------------------------------------------------------------#
#
#
#----------#
#----------#=
#displays settings; check prior to running main script#
print(list.settings)      # provides all parameters
#----------#
#----------#

###############################################################################################
###############################################################################################
###############################################################################################
#---------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------#
#--------------------------this section runs the program. ------------------------------------#  
Count.Fail=0
ptm <- proc.time() #starts timer to see how long script takes
while (PE<N.iteration) {
  {
    n=(ncol(Match.Xe)-2) #sets the number of match varaibles. See matchit() in main script. 
    base.print="Fully matched"
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
    
  } #resets conditions for all loops
  
  while (success==0) {
    #selection a random pool of returnees
    r.sample.returnees <- Returnee.FollowUp.data[sample(nrow(Returnee.FollowUp.data), N.returnee), ] # starting sample size
    r.sample.returnees$match=1  #will be used in match.it()
    r.sample.returnees.ID=subset(r.sample.returnees, select = c('ID','match','AGE','COGCOG'))
    #Pseudo-replacments: end up with an a pool that is mostly random selected, except within age range of returnees
    #has loop in it that defines number of people necessaqry for PR pool
    while (PR.selection==TRUE) {
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
    Potential.PR.V3$match=0  #will be used in match.it()
    Potential.PR.V3$keep=NULL
    #Potential.PR.ID=subset(Potential.PR.V3, select = c('ID','match','AGE','COGCOG'))
    #combine datasets so can use MatchIt package. 'match" is IDifying Xe [0=returnee, 1=PR]
    matching=rbind(Potential.PR.V3, r.sample.returnees)
    
    { 
      #potentially make this section a function
      if (n==1) {
        m.out <- matchit(match ~ Match1+AGE+EDU, method='nearest',  data = matching, 
                         caliper = AGECALIPER, mavars= ~AGE,
                         link='probit')
        
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
        success.match=ifelse(nrow(Matched.data.PR)>(N.returnee.min), 1, 0)
        
        #exit code
        success=ifelse(success.match==1 & success.age==1 & success.edu==1 &   success.Match1==1, 1,0)
        
      } #updated
      if (n==2) {
        m.out <- matchit(match ~ Match1+AGE+EDU + Match2, method='nearest',  data = matching,
                         caliper = AGECALIPER, mavars= ~AGE,
                         link='probit')
        
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
        success.match=ifelse(nrow(Matched.data.PR)>(N.returnee.min), 1, 0)
        #exit code
        success=ifelse(success.match==1 & success.age==1 & success.edu==1 &  success.Match1==1 & success.Match2==1, 1,0)
        
      } #updated
      if (n==3) {
        m.out <- matchit(match ~ Match1+AGE+EDU + Match2+ Match3, method='nearest',  data = matching,
                         caliper = AGECALIPER, mavars= ~AGE,
                         link='probit')
        
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
        m.out <- matchit(match ~ Match1+AGE+EDU + Match2+ Match3+ Match4, method='nearest',  data = matching,
                         caliper = AGECALIPER, mavars= ~AGE,
                         link='probit')
        
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
        success.match=ifelse(nrow(Matched.data.PR)>(N.returnee.min), 1, 0)
        #exit code
        success=ifelse(success.match==1 & success.age==1 & success.edu==1 &  
                         success.Match1==1 & 
                         success.Match2==1 &
                         success.Match3==1 &
                         success.Match4==1, 1,0)
      } #updated
      if (n==5) {
        m.out <- matchit(match ~ Match1+AGE+EDU + Match2+ Match3+ Match4 +Match5, method='nearest',  data = matching,
                         caliper = AGECALIPER, mavars= ~AGE,
                         link='probit')
        
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
        success.match=ifelse(nrow(Matched.data.PR)>(N.returnee.min), 1, 0)
        #exit code
        success=ifelse(success.match==1 & success.age==1 & success.edu==1 &  
                         success.Match1==1 & 
                         success.Match2==1 &
                         success.Match3==1 &
                         success.Match4==1 &
                         success.Match5==1, 1,0)
      } #updated
      if (n==6) {
        m.out <- matchit(match ~ Match1+AGE+EDU + Match2+ Match3+ Match4 +Match5 +Match6, method='nearest',  data = matching,
                         caliper = AGECALIPER, mavars= ~AGE,
                         link='probit')
        
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
        success.match=ifelse(nrow(Matched.data.PR)>(N.returnee.min), 1, 0)         
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
        m.out <- matchit(match ~ Match1+AGE+EDU + Match2+ Match3+ Match4 +Match5 +Match6 +Match7, method='nearest',  data = matching,
                         caliper = AGECALIPER, mavars= ~AGE,
                         link='probit')
        
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
        success.match=ifelse(nrow(Matched.data.PR)>(N.returnee.min), 1, 0)         
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
        m.out <- matchit(match ~ Match1+AGE+EDU + Match2+ Match3+ Match4 +Match5 +Match6 +Match7 +Match8, method='nearest',  data = matching,
                         caliper = AGECALIPER, mavars= ~AGE,
                         link='probit')
        
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
        success.match=ifelse(nrow(Matched.data.PR)>(N.returnee.min), 1, 0)         
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
        m.out <- matchit(match ~ Match1+AGE+EDU + Match2+ Match3+ Match4 +Match5 +Match6 +Match7 +Match8 +Match9, method='nearest',  data = matching,
                         caliper = AGECALIPER, mavars= ~AGE,
                         link='probit')
        
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
        success.match=ifelse(nrow(Matched.data.PR)>(N.returnee.min), 1, 0)         
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
        m.out <- matchit(match ~ Match1+AGE+EDU + Match2+ Match3+ Match4 +Match5 +Match6 +Match7 +Match8 +Match9 +Match10, method='nearest',  data = matching,
                         caliper = AGECALIPER, mavars= ~AGE,
                         link='probit')
        
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
        success.match=ifelse(nrow(Matched.data.PR)>(N.returnee.min), 1, 0)         
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
      if (success==1 &count<PR.match.count) {        
        restart=FALSE
        #this was added because chisq() sometimes breaks when no subjects have a value in a cell
      }
    } #Fail code
  } #mathces pseduo replacments to returnees follow-up
  #Create labled databases
  Returnee.FollowUp.Iteration=Matched.data.Return
  Returnee.Baseline.Iteration=Returnee.baseline.data[which(Returnee.baseline.data$ID %in% Returnee.FollowUp.Iteration$ID),]
  Pseudo.Replace.Iteration=Matched.data.PR
  
  #set up matching for baseline and retunree baseline
  Baseline.temp.NoPRs=Full.Baseline.data[-which(Full.Baseline.data$ID %in% Pseudo.Replace.Iteration$ID),] #removes chosen set of PRs from baseline pool
  Baseline.temp.pool=Baseline.temp.NoPRs[-which(Baseline.temp.NoPRs$ID %in% Returnee.Baseline.Iteration$ID),] #removes chosen set of returnees from baseline pool 
  
  while (baseline.success==0 & restart==FALSE) {
    #resets databases; may be repeated
    m.out.baseline=NULL
    Matched.baseline.all=NULL
    Matched.baseline.chosen=NULL
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
    
    #the if loops below are outdated. I only have the age-matched baseline running
    { 
      if (baseline.match.age.only==TRUE) {
        n=9000 # this makes it so the rest of the if statements do not run.
        m.out.baseline <- matchit(match ~ AGE, method='nearest',  caliper=AGECALIPER,data = baseline.matching, link='probit')
        
        Matched.baseline.all=match.data(m.out.baseline)
        Matched.baseline.chosen=subset(Matched.baseline.all, match==0)
        Matched.data.Return=subset(Matched.baseline.all, match==1)
        
        #Tests to leave loop
        age.test.base=wilcox.test(Matched.baseline.chosen$AGE, Matched.data.Return$AGE)
        #Leave loop conditions
        baseline.success.age=ifelse(age.test.base$p.value>value.match, 1, 0)
        baseline.success.match=ifelse(nrow(Matched.data.PR)>(N.returnee.min), 1, 0)
        
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
        baseline.success.match=ifelse(nrow(Matched.data.PR)>(N.returnee.min), 1, 0)
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
        baseline.success.match=ifelse(nrow(Matched.data.PR)>(N.returnee.min), 1, 0)
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
        baseline.success.match=ifelse(nrow(Matched.data.PR)>(N.returnee.min), 1, 0)
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
        baseline.success.match=ifelse(nrow(Matched.data.PR)>(N.returnee.min), 1, 0)
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
        baseline.success.match=ifelse(nrow(Matched.data.PR)>(N.returnee.min), 1, 0)         
        
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
        baseline.success.match=ifelse(nrow(Matched.data.PR)>(N.returnee.min), 1, 0)         
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
        baseline.success.match=ifelse(nrow(Matched.data.PR)>(N.returnee.min), 1, 0)         
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
        baseline.success.match=ifelse(nrow(Matched.data.PR)>(N.returnee.min), 1, 0)         
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
        baseline.success.match=ifelse(nrow(Matched.data.PR)>(N.returnee.min), 1, 0)         
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
      if (baseline.count>base.match.count | restart==TRUE) {
        restart=TRUE
        baseline.success=1
        
      }}#Fail code
    
    
    
  } #matches returnees to a baseline sample
  
  if (restart==TRUE) {
    Count.Fail=Count.Fail+1
  }
  #calcuates practice effects.
  if (restart==FALSE) {
    Mean.Bootstrap.data.temp=data.frame('baseline'=mean(Matched.baseline.chosen$COGCOG),
                                        'baseline.SD'=SD(Matched.baseline.chosen$COGCOG),
                                        'returnee.baseline'=mean(Returnee.Baseline.Iteration$COGCOG),
                                        'returnee.baseline.SD'=SD(Returnee.Baseline.Iteration$COGCOG),
                                        'returnee.FU'=mean(Returnee.FollowUp.Iteration$COGCOG), 
                                        'returnee.FU.SD'=SD(Returnee.FollowUp.Iteration$COGCOG),
                                        'replcacement'=mean(Pseudo.Replace.Iteration$COGCOG),
                                        'replcacement.SD'=SD(Pseudo.Replace.Iteration$COGCOG))
    Mean.Bootstrap.data=rbind(Mean.Bootstrap.data,Mean.Bootstrap.data.temp )
    
    #combines databses
    databaselist.returnee.followup[[PE]]=Returnee.FollowUp.Iteration
    databaselist.replacement.baseline[[PE]]=Pseudo.Replace.Iteration
    databaselist.basleine[[PE]]=Matched.baseline.chosen
    databaselist.returnee.baseline[[PE]]=Returnee.Baseline.Iteration
    
    #restarts loop, adds 1 to counter, records progress
    restart=TRUE
    PE=PE+1
    progress(100*PE/N.iteration)
    Sys.sleep(0.01)
  }
  
} 
#---------------------------------------#

{
  time=proc.time() - ptm #if run immediatly after main while(PE) loop, will give how much time elapsed
  Mean.Bootstrap.data.final=subset(Mean.Bootstrap.data,  
                                   baseline!=1000 & baseline.SD!=1000 &
                                     returnee.baseline!=1000 & returnee.baseline.SD!=1000 & returnee.FU!=1000 & 
                                     returnee.FU.SD!=1000 & replcacement!=1000 & replcacement.SD!=1000)
  
  #calcuate practice effects
  Post.matching.DiffScore=mean(Mean.Bootstrap.data.final$returnee.FU)-mean(Mean.Bootstrap.data.final$replcacement)
  Post.matching.AttrScore=mean(Mean.Bootstrap.data.final$returnee.baseline)-mean(Mean.Bootstrap.data.final$baseline)
  Post.matching.PracEffect=Post.matching.DiffScore-Post.matching.AttrScore
  
  #calculate effect size
  post.Madj=mean(Mean.Bootstrap.data.final$returnee.FU)-Post.matching.PracEffect
  SD=mean(Mean.Bootstrap.data.final$returnee.FU.SD)
  M1=mean(Mean.Bootstrap.data.final$returnee.FU)
  post.Madj=mean(Mean.Bootstrap.data.final$returnee.FU)-Post.matching.PracEffect
  SDpooled= sqrt(((SD*SD+SD*SD)/2))
  CohenD=(M1-post.Madj)/SDpooled
  date=Sys.time()
  Pe.effect.table=data.frame("Date and time"          =date,
                             "mean score PE-unadj"    =M1,
                             "mean score PE-adj"      =post.Madj,
                             "Practice Effect"        =Post.matching.PracEffect,
                             "Avg PE Cohens D"        =CohenD)
  
  
  Post.matching.Results=data.frame(
    "Difference score"= Post.matching.DiffScore,
    "Attrition effect"= Post.matching.AttrScore,
    "Practice effect" = Post.matching.PracEffect,
    "Avg PE Cohens d" = CohenD
  )
  
} #saves  results


#--------------------------this section Views results----------------------------------------#  
TestVariable   #provides what variable looking at
Pe.effect.table #displays practice effect, attrition effect, and PE cohen's d
describe(Mean.Bootstrap.data.final) #gives descriptives for matched samples
Post.matching.Results
Names.Match.Xe #shows what was matched on
base.print     #was baseline age matched or fully matched
time           #provides how long it took
list.settings  #provides settings for that run
#---------------------------------------------------------------------------------------------#  
#---------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------#
###############################################################################################
###############################################################################################
###############################################################################################