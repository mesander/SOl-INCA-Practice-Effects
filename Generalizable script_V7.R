############################################################################### ♀
########################      Practice effect          ########################
########################      Pseudo-replacements      ########################
########################      last update: 5/11/21      ########################
###############################################################################
#progress notes
    #-new additions: better tracking of success of matching. in results
      #porportional baseline
      #-readded log transformation b/c distributions were different between samples
        #transformation reduces effects of outliers.  
   #?-do we want to add a matching condition for the std.mean dif


#--------------------------------------------------------------#
#              Block 0: Initial Setup
#
#
#----------#                                            
library(psych)
library(MatchIt)
library(readr)
library(dplyr)
library(haven)
library(tidyverse)
library(progress)
library(svMisc)
library(compareGroups)
#----------#                                            
#
#
  #--------------------------------------------------------------#
  #              Block 1-2 Set up databases to be compared. Block 1 requires manual edits
  #
  #
  #----------#                                            
  
    
    #background subsets
  MainData.V1.dom<-subset(MainData.SOLINCA, bkgrd1_c7_v1==0 & lang_pref_v1==1)
  MainData.V1.cna<-subset(MainData.SOLINCA, bkgrd1_c7_v1==1 & lang_pref_v1==1)
  MainData.V1.cub<-subset(MainData.SOLINCA, bkgrd1_c7_v1==2 & lang_pref_v1==1)
  MainData.V1.mex<-subset(MainData.SOLINCA, bkgrd1_c7_v1==3 & lang_pref_v1==1)
  MainData.V1.pur<-subset(MainData.SOLINCA, bkgrd1_c7_v1==4 & lang_pref_v1==1)
  MainData.V1.sua<-subset(MainData.SOLINCA, bkgrd1_c7_v1==5 & lang_pref_v1==1)
  MainData.V1.mul<-subset(MainData.SOLINCA, bkgrd1_c7_v1==6 & lang_pref_v1==1)
  #site subsets
  MainData.V1.bronx<-subset(MainData.SOLINCA, centernum_v1==1)
  MainData.V1.chicgo<-subset(MainData.SOLINCA, centernum_v1==2)
  MainData.V1.miami<-subset(MainData.SOLINCA, centernum_v1==3)
  MainData.V1.sandiego<-subset(MainData.SOLINCA, centernum_v1==4)
  #gender diference
  MainData.V1.female<-subset(MainData.SOLINCA, gendernum_v1==0)
  MainData.V1.male<-subset(MainData.SOLINCA, gendernum_v1==1)
  #us born
  MainData.V1.nonUSA<-subset(MainData.SOLINCA, us_born_v1==0)
  MainData.V1.USA<-subset(MainData.SOLINCA, us_born_v1==1)
  #langauge: need to figure out if V1, V2 different? 
  MainData.V1.spanish<-subset(MainData.SOLINCA, lang_pref_v1==1 & lang_pref_v2==1)
  MainData.V1.english<-subset(MainData.SOLINCA, lang_pref_v1==2 & lang_pref_v2==2)

### Block1###        <requires manual edits>
{
  #load in main dataset
  MainData<- MainData.V1.spanish
          # note, data must be in wide format. 
          # Must include an education, variable. 
          #Age at 2 time points, Cog variable at 2 time points
  
  #confirm that selecting the correct diagnosis
  MainData$Base_impaired=ifelse(MainData$total_6item_v1<4,0,1)                   
  
  Data.A1 <- as_tibble(MainData)
        # rename data variables to match what's in script

  Data.B2 <- Data.A1 %>% rename(
      ID     = ID, 
      AGE_V1 = age_v1,
      AGE_V2 = age_v2, 
      BASEDX = Base_impaired, 
      COG_V1 = sevlt_recall_v1, 
      COG_V2 = SEVLT_RECALL_v2
    )
        # identify cognitive variable of interest, saved for printout later
  TestVariable <- "SEVLT_RECALL_v2"
  
        # Specify list of variable to match on. Do not include AGE, which will
        # be added automatically
  matchingVars <- c('education_c3_v1','frame_cvd_risk_10yr_v1', 'gendernum_v1','bkgrd1_c7_v1','income_c5_v1','yrsus_v1')
  #matchingVars <- c('education_c3_v1', 'gendernum_v1','bkgrd1_c7_v1','income_c5_v1','yrsus_v1')
  
  
  #'education_c3_v1', 'gendernum_v1','centernum_v1','nativity_subscore_mesa_v1',
  #  'bkgrd1_c7_v1','us_born_v1','income_v2','agg_phys_v1','agg_ment_v1'
  
        #adjust variable to be categorical if <4 levels
  #Data.B2$bkgrd1_c7_v1=as.factor(Data.B2$bkgrd1_c7_v1)
  #Data.B2$income_v2=as.factor(Data.B2$income_v2)
  
  
        # select sample sizes, matching details
  N.returnee.percent <- .2  # percent of returnees to start each iteration
  N.returnee.min <- 80  # minimum number of returnees in sample
  N.PR.Pool.percent <- .2  # sets minimal number of subjects in potential PR pool.
      # select number of iterations
  N.iteration <- 3000
  
      # select P value level for group testing to confirm Matchit correctly
         # assigned groups
  value.match <- 0.3 #P values > value.match when comparing PR and Return
  AGECALIPER <- 0.1  # how far apart in years, the matched groups can be.

      #select max number of matching attempts before force quiting
  count.quit=900 #if fails to match > count.quit times, will exit program
} # Block 1: Renames variables for script. Creates first database. Loads libraries
#----------#♦
  ### Block2###  [Automatic]
  {
    
    # Unique matching variables for this dataset
    Match.Xe <- subset(Data.B2, select = c("ID", matchingVars))  # ID variables
    Names.Match.Xe <- names(Match.Xe)
    Names.Match.Xe[1] <- "AGE"  # renames ID to AGE for a printout later. 
    
    # removes anyone with missing matching variables. Necessary for
    #matchit()
    Match.Xe.reduced <- na.omit(Match.Xe)
    
    # create baseline dataset with people who have that cog variable. And
    #meet DX criteria
    Baseline.Data.A1 <- subset(Data.B2, COG_V1 != "NA" & BASEDX == 1)
    
    # create follow-up dataset with people who have that cog variable at
    #basleine and FU. Renames Cog variable [COGCOG]
    FollowUp.Data.A1 <- subset(Data.B2, COG_V2 != "NA" & COG_V1 != "NA", 
                               select = "ID")
    FollowUp.Data.B2 <- merge(Baseline.Data.A1, FollowUp.Data.A1, by = "ID")
    
    # rename variables for script. Creates temporary subsample databases.
    Full.Baseline <- subset(Baseline.Data.A1, select = c("ID", "COG_V1", 
                                                         "AGE_V1"))
    Full.Baseline <- Full.Baseline %>% rename(COGCOG = COG_V1, AGE = AGE_V1)
    Returnee.Baseline <- subset(FollowUp.Data.B2, select = c("ID", "COG_V1", 
                                                             "AGE_V1"))
    Returnee.Baseline <- Returnee.Baseline %>% rename(COGCOG = COG_V1, 
                                                      AGE = AGE_V1)
    
    Returnee.FollowUp <- subset(FollowUp.Data.B2, select = c("ID", "COG_V2", 
                                                             "AGE_V2"))
    Returnee.FollowUp <- Returnee.FollowUp %>% rename(COGCOG = COG_V2, 
                                                      AGE = AGE_V2)
    
    # create databases used by script. Reduces samples to those with
    #Matching Xe.
    Returnee.baseline.data <- merge(Returnee.Baseline, Match.Xe.reduced, 
                                    by = "ID")
    Returnee.FollowUp.data <- merge(Returnee.FollowUp, Match.Xe.reduced, 
                                    by = "ID")
    Full.Baseline.data <- merge(Full.Baseline, Match.Xe.reduced, by = "ID")
    # create ID only lists.
    Full.Baseline.ID <- Full.Baseline.data$ID
    Returnee.Baseline.ID <- Returnee.baseline.data$ID
    Returnee.FollowUp.ID <- Returnee.FollowUp.data$ID
    # create attrition database [note this is not the PR sample. Though
    #they can be in it]
    Attritors.baseline <- Full.Baseline.data[-which(Full.Baseline.data$ID %in% 
                                                      Returnee.baseline.data$ID), ]
    
    # tracks size of databases
    Return.fullbaseline.size <- nrow(Full.Baseline.data)
    Return.baseline.size <- nrow(Returnee.baseline.data)
    
    # Sets up list store matching results 
    N.values=19
    results <- data.frame(matrix(nrow=N.iteration, ncol=N.values))
    names(results) <- c("Test",
                        "Nonporportional-baseline-Mean", "Nonporportional-Baseline-SD",
                        "Returnee-baseline-Mean","Returnee-baseline-SD",
                        "Returnee-FollowUp-Mean", "Returnee-FollowUp-SD",
                        "Replacement-Mean","Replacement-SD",
                        "Avg-std.mean.dif-PR-Return", "Avg-eCDF-PR-Return","Avg-std.mean.diff.age-PR-Return",
                        "Avg-std.pair.dist-PR-Return",
                        "PR-Return-Matching", 
                        "Porportional-baselines-Mean", "Avg-number-returnees",
                        "Avg-number-replacements","Avg-number-baseline",
                        "Avg-number-attempts-to-match")
    
    #sets up Attrition rate and Retention rate for that test.
    #Needed for Proportional Baseline
    Returnee.weight=nrow(Returnee.FollowUp)/nrow(Full.Baseline)
    Attrition.weight=1-Returnee.weight
    
    #Creates empty lists to store participant lists later.
    databaselist.returnee.followup <- list()
    databaselist.replacement.baseline <- list()
    databaselist.basleine <- list()
    databaselist.returnee.baseline <- list()
    
    
    # select sample sizes, matching details
    N.returnee <- round(nrow(Returnee.FollowUp.data)*N.returnee.percent)  # percent of returnees to start each iteration
    N.PR.Pool <- round(nrow(Full.Baseline.data)*N.PR.Pool.percent)  # sets minimal number of subjects in potential PR pool.
    #Sets up storage for parameters
    list.settings <- data.frame(`Variable of interest` = TestVariable, 
                                `N returnee selected` = N.returnee, `Minimum PR start pool` = N.PR.Pool, 
                                `Max iterations` = N.iteration,
                                `P value match` = value.match,
                                `max attempts to match`=count.quit)
    
    #Skew: identifies Skewness of cognitive data
    { #skew for returnee baseline data
      MAX.BASE=max(Returnee.baseline.data$COGCOG)
      none.transform=describe(Returnee.baseline.data$COGCOG)
      skew.p.sqrt=describe(sqrt(Returnee.baseline.data$COGCOG))
      skew.p.log=describe(log(Returnee.baseline.data$COGCOG+3))
      skew.p.1_log=describe(1/log(Returnee.baseline.data$COGCOG+3))
      skew.n.sqrt=describe(sqrt(MAX.BASE+1-Returnee.baseline.data$COGCOG))
      skew.n.log=describe(log(MAX.BASE+1-Returnee.baseline.data$COGCOG))
      skew.n.1_log=describe(1/(MAX.BASE+1-Returnee.baseline.data$COGCOG))
      skew.sum.returnee.baseline=data.frame(No_transformation = none.transform$skew, 
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
      
      MAX=max(Returnee.baseline.data$COGCOG)
      
      #creates more readable skew data for output
      Skew.data.a<-rbind(skew.sum.returnee.baseline,skew.sum.FU.v2)
      Skew.data.b<-rbind(Skew.data.a,skew.sum.allbaseline)
      skew.average<-sapply(Skew.data.b, mean)
      Skew.data.final<-rbind(Skew.data.b,skew.average)
      rownames(Skew.data.final)[1]<-"returnee Baseline Data Skew"
      rownames(Skew.data.final)[2]<-"returnee FollowUp Data Skew"
      rownames(Skew.data.final)[3]<-"All Baseline Subs Data Skew"
      rownames(Skew.data.final)[4]<-"Average all datasets   Skew"}  #Skew code.
    
  } 
  #----------#
  #--------------------------------------------------------------#
  #                     Manually Check Setup                     #
  #--------------------------------------------------------------#
  # 1. Displays settings; check prior to running main script
  # 2. Provides skew statistics based on each transformation option
  # 3. Provides option to manually change class of matching variables
  
  #------------------------------#
  #   1. Display settings
  list.settings
  
  cat(" Number of Full Baseline:      ", nrow(Full.Baseline.data),        "\n", 
      "Number of Returnee Follows:   ", nrow(Returnee.FollowUp.data),     "\n", 
      "Number of Returnee Baselines: ", nrow(Returnee.baseline.data),     "\n", 
      "If !=0 then error in Settings:  ", (nrow(Returnee.FollowUp.data) - 
                                             nrow(Returnee.baseline.data)))
  
  #------------------------------#
  #   2. Skewness
  
  #histograms of non-transformed data. Look for outliers
  hist(Returnee.baseline.data$COGCOG)
  hist(Returnee.FollowUp.data$COGCOG)
  hist(Full.Baseline.data$COGCOG)
  
  #optional transformation block
  Skew.data.final  #provides skewness output
  
  #To select transformation, set  '=TRUE'.
  #POSITIVE SKEW OPTIONS
  transform.sqrt=FALSE  # pos_skew_sqrt
  transform.log=FALSE  # pos_skew_log
  transform.1log=FALSE  # pos_skew_1_div_log 
  #NEGATIVE SKEW OPTIONS
  transform.sqrt.NEG=FALSE  # neg_skew_sqrt5
  transform.log.NEG=FALSE   # neg_skew_log
  transform.1log.NEG=FALSE  # neg_skew_1_div_log 
  
  #------------------------------#
  #   2. Matching Variables    [in progress]
  #optional checking of matching variables
  #Use below code to confirm that your Xe are the correct class.
  #may have to transform variables for normality
  sapply(Match.Xe, class)
  
  #---------------------------------------------------------------------------------------------#
  #---------------------------------------------------------------------------------------------#
  #--------------------------this section runs the program. ------------------------------------#  
  Count.Fail <- 0
  ptm <- proc.time()  # starts timer to see how long script takes
  
  for (i in seq(N.iteration)) {
    
    
    {
      count <- 0
      count.exit=FALSE
      success.age <- 0
      success.match <- 0
      success.matchingVars <- 0
      total.count <- 0
      success <- 0
      restart <- FALSE  # sets up matching fail condition
      PR.selection <- TRUE
      Potential.PR.V1 <-NULL
      Matched.baseline.chosen <- NULL
      Returnee.Baseline.Iteration <- NULL
      Returnee.FollowUp.Iteration <- NULL
      Pseudo.Replace.Iteration <- NULL
      Porportional.baseline.noPRs.noReturnee <- NULL
      transform.set=sum(transform.sqrt,transform.log,transform.1log,transform.sqrt.NEG,transform.log.NEG,transform.1log.NEG)
    } # resets conditions for all loops
    
    if (transform.set>1) {
      print("erorr in transformation settings")
      break
      
    } #quits loop if incorrect transformations. 
    
    if (N.returnee<N.returnee.min) {
      print("too few returnees selected. change N.returnee.percent")
      break
      
    } #quits loop too few returnees selected
    
    while (success == 0) {
      # selection a random pool of returnees
      r.sample.returnees <- Returnee.FollowUp.data[sample(nrow(Returnee.FollowUp.data), 
                                                          N.returnee), ]  # starting sample size
      r.sample.returnees$match <- 1  # will be used in match.it()
      r.sample.returnees.ID <- subset(r.sample.returnees, select = c("ID", 
                                                                     "match", "AGE", "COGCOG"))
      
      # Pseudo-replacments: end up with an a pool that is mostly random
      ## selected, except within age range of returnees has loop in it that
      ### defines number of people necessaqry for PR pool
      while (PR.selection == TRUE) {
        Potential.PR.V1 <- Full.Baseline.data[-which(Full.Baseline.data$ID %in% 
                                                       r.sample.returnees$ID), ]
        age_bounds <- describe(r.sample.returnees$AGE)
        u_bound <- age_bounds$max
        l_bound <- age_bounds$min
        Potential.PR.V1$keep <- ifelse(Potential.PR.V1$AGE > u_bound, 
                                       -1, ifelse(Potential.PR.V1$AGE < l_bound, -1, 1))
        
        Potential.PR.V2 <- subset(Potential.PR.V1, keep == 1)  # removes anyone outside age range
        if (nrow(Potential.PR.V2) > N.PR.Pool) {
          PR.selection <- FALSE
        }
      }
      Potential.PR.V2$match <- 0  # will be used in match.it()
      Potential.PR.V2$keep <- NULL
      # package. 'match' is IDifying Xe [0=returnee, 1=PR]
      matching <- rbind(Potential.PR.V2, r.sample.returnees)
      
      {
        # Match subjects based on matchingVars
        ## Combine all variables to be matched into a formula
        fmlaMatching <- as.formula(paste0("match ~ ", paste(c("AGE", matchingVars), collapse = " + ")))
        m.out <- matchit(fmlaMatching, method = "nearest", 
                         data = matching, caliper = AGECALIPER, mavars = ~AGE, link = "probit")
        Matched.data.all <- match.data(m.out)
        Matched.data.PR <- subset(Matched.data.all, match==0)
        Matched.data.Return <- subset(Matched.data.all, match==1)
        
        #records effectivness of matching
        
        summary.match<-summary(m.out, un=FALSE)
        summary.match.df<-as.data.frame(summary.match$sum.matched)
        std.mean.diff.iteration<-mean(summary.match.df$`Std. Mean Diff.`)
        std.mean.diff.age.iteration<-summary.match.df[2,3]
        std.mean.eCDF.iteration<-mean(summary.match.df$`eCDF Mean`)
        std.mean.pair.dist.iteration<-mean(summary.match.df$`eCDF Mean`)
        
        
        # Compare all groups. Automatically determines test based on normality
        match.test <- compareGroups(fmlaMatching, data=Matched.data.all, method=NA, chisq.test.perm=TRUE)
        # Determine whether all p values are above threshold
        match.p <- getResults(match.test, "p.overall")
        success.matchingVars <- ifelse(all(match.p>value.match), 1, 0)
        
        # Leave loop conditions
        success.match <- ifelse(nrow(Matched.data.PR) > (N.returnee.min), 
                                1, 0)
        # exit code
        success <- ifelse(success.match == 1 & success.matchingVars == 1, 1, 0)
        
        
      }  # Matches dataset based on predefined number of match categories. Age and edu always matched.
      
      
      {
        # fail code
        if (success!=1) {
          count <- count + 1
          restart <- TRUE
        }
        
        if (success == 1) {
          restart <- FALSE
          # this was added because chisq() sometimes breaks when no subjects have JAE: Maybe not necessary anymore?
          # a value in a cell
        }
        
        if (count>count.quit) {
          count.exit=TRUE
          success=1
        }
      }  # Fail code
      
      
      success
      success.match
      success.matchingVars
      match.test
      summary.match
      count
      
    }  # mathces pseduo replacments to returnees follow-up
    
    
    if (count.exit==TRUE) {
      print("Unable to complete first match attempt. Reconsider matching variables/settings")
      break
    } # Exit if fails to match 
    
    { 
      Returnee.FollowUp.Iteration <- Matched.data.Return
      Returnee.Baseline.Iteration <- Returnee.baseline.data[which(Returnee.baseline.data$ID %in% 
                                                                    Returnee.FollowUp.Iteration$ID), ]
      Pseudo.Replace.Iteration <- Matched.data.PR
      Porportional.baseline.noPRs <- Full.Baseline.data[-which(Full.Baseline.data$ID %in% 
                                                                 Pseudo.Replace.Iteration$ID), ]  # selects people who are not PRs.
      
      #Porportional.baseline.noPRs.noReturnee=no POSSIBLE returnees, also removes attritors. 
      #this dataset is the "attritors' for that iteration. 
      #this dataset is later combined with the returnee iteration to get proportions  correct. 
      
      # selects people who are not returnees in that iteration
      Porportional.baseline.noPRs.noReturnee <- Porportional.baseline.noPRs[-which(Porportional.baseline.noPRs$ID %in% 
                                                                                     Returnee.baseline.data$ID), ]  
      #Selects subects who are broadly within the age range of Returnees
      Porportional.baseline.noPRs.noReturnee=subset(Porportional.baseline.noPRs.noReturnee, 
                                                    AGE>(min(Returnee.baseline.data$AGE)+0.08333333) & AGE<(max(Returnee.baseline.data$AGE)+0.08333333))
      
      
      
    } #Creates iteration-specific databases
    
    if (restart == TRUE) {
      Count.Fail <- Count.Fail + 1
      i=i-1
    } #adds to count, Prevents calcuation of PE if error.
    
    if (restart == FALSE) {
      
      ##Optional transformation block.##
      if (transform.sqrt==TRUE) {
        mean.returnee.baseline<-(mean(sqrt(Returnee.Baseline.Iteration$COGCOG)))^2
        mean.returnee.followup<-(mean(sqrt(Returnee.FollowUp.Iteration$COGCOG)))^2
        mean.pseudo.replace   <-(mean(sqrt(Pseudo.Replace.Iteration$COGCOG)))^2
        mean.porp.baseline    <-(mean(sqrt(Porportional.baseline.noPRs.noReturnee$COGCOG)))^2
        mean.nonporp.baseline <-(mean(sqrt(Porportional.baseline.noPRs$COGCOG)))^2
        
      } #done
      if (transform.log==TRUE) {
        mean.returnee.baseline<-exp(mean(log(Returnee.Baseline.Iteration$COGCOG+.1)))-.1
        mean.returnee.followup<-exp(mean(log(Returnee.FollowUp.Iteration$COGCOG+.1)))-.1
        mean.pseudo.replace   <-exp(mean(log(Pseudo.Replace.Iteration$COGCOG+.1)))-.1
        mean.porp.baseline    <-exp(mean(log(Porportional.baseline.noPRs.noReturnee$COGCOG+.1)))-.1
        mean.nonporp.baseline <-exp(mean(log(Porportional.baseline.noPRs$COGCOG+.1)))-.1
        
      }  #done.
      if (transform.1log==TRUE) {
        
        mean.returnee.baseline<-exp(1/mean(1/log(Returnee.Baseline.Iteration$COGCOG+.1)))-.1
        mean.returnee.followup<-exp(1/mean(1/log(Returnee.FollowUp.Iteration$COGCOG+.1)))-.1
        mean.pseudo.replace   <-exp(1/mean(1/log(Pseudo.Replace.Iteration$COGCOG+.1)))-.1
        mean.porp.baseline    <-exp(1/mean(1/log(Porportional.baseline.noPRs.noReturnee$COGCOG+.1)))-.1
        mean.nonporp.baseline <-exp(1/mean(1/log(Porportional.baseline.noPRs$COGCOG+.1)))-.1
        
      } #done
      
      #if  NEGATIVE skew
      if (transform.sqrt.NEG==TRUE) {
        
        mean.returnee.baseline<-mean(MAX.AllB+.1-sqrt(MAX.AllB+.1-Returnee.Baseline.Iteration$COGCOG)^2)
        mean.returnee.followup<-mean(MAX.AllB+.1-sqrt(MAX.AllB+.1-Returnee.FollowUp.Iteration$COGCOG)^2)
        mean.pseudo.replace   <-mean(MAX.AllB+.1-sqrt(MAX.AllB+.1-Pseudo.Replace.Iteration$COGCOG)^2)
        mean.porp.baseline    <-mean(MAX.AllB+.1-sqrt(MAX.AllB+.1-Porportional.baseline.noPRs.noReturnee$COGCOG)^2)
        mean.nonporp.baseline <-mean(MAX.AllB+.1-sqrt(MAX.AllB+.1-Porportional.baseline.noPRs$COGCOG)^2)
        
      }#done
      #if moderate positive skew
      if (transform.log.NEG==TRUE) {
        
        mean.returnee.baseline<-(MAX.AllB+.1)-exp(mean((log(MAX.AllB+.1-Returnee.Baseline.Iteration$COGCOG))))
        mean.returnee.followup<-(MAX.AllB+.1)-exp(mean((log(MAX.AllB+.1-Returnee.FollowUp.Iteration$COGCOG))))
        mean.pseudo.replace   <-(MAX.AllB+.1)-exp(mean((log(MAX.AllB+.1-Pseudo.Replace.Iteration$COGCOG))))
        mean.porp.baseline    <-(MAX.AllB+.1)-exp(mean((log(MAX.AllB+.1-Porportional.baseline.noPRs.noReturnee$COGCOG))))
        mean.nonporp.baseline <-(MAX.AllB+.1)-exp(mean((log(MAX.AllB+.1-Porportional.baseline.noPRs$COGCOG))))
        
      } #done
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
      #If no transformation is applied
      if (transform.sqrt==FALSE & transform.log==FALSE & transform.1log==FALSE &
          transform.sqrt.NEG==FALSE & transform.log.NEG==FALSE & transform.1log.NEG==FALSE) {
        mean.returnee.baseline<-mean(Returnee.Baseline.Iteration$COGCOG)
        mean.returnee.followup<-mean(Returnee.FollowUp.Iteration$COGCOG)
        mean.pseudo.replace   <-mean(Pseudo.Replace.Iteration$COGCOG)
        mean.porp.baseline    <-mean(Porportional.baseline.noPRs.noReturnee$COGCOG)
        mean.nonporp.baseline <-mean(Porportional.baseline.noPRs$COGCOG)
        
      } #done
      
      #saves the datasets
      results[i,"Test"]<- TestVariable
      results[i,"Nonporportional-baseline-Mean"]<- mean.nonporp.baseline
      results[i,"Nonporportional-Baseline-SD"]<- SD(Porportional.baseline.noPRs$COGCOG)
      results[i,"Returnee-baseline-Mean"]<- mean.returnee.baseline 
      results[i,"Returnee-baseline-SD"]<- SD(Returnee.Baseline.Iteration$COGCOG)
      results[i,"Returnee-FollowUp-Mean"]<- mean.returnee.followup
      results[i,"Returnee-FollowUp-SD"]<- SD(Returnee.FollowUp.Iteration$COGCOG) 
      results[i,"Replacement-Mean"]<- mean.pseudo.replace
      results[i,"Replacement-SD"]<- SD(Pseudo.Replace.Iteration$COGCOG)
      results[i,"PR-Return-Matching"]<- toString(Names.Match.Xe)
      results[i,"Avg-std.mean.dif-PR-Return"]<- std.mean.diff.iteration
      results[i,"Avg-std.mean.diff.age-PR-Return"]<-std.mean.diff.age.iteration
      results[i,"Avg-eCDF-PR-Return"]<- std.mean.pair.dist.iteration
      results[i,"Avg-std.pair.dist-PR-Return"]<-std.mean.pair.dist.iteration
      results[i,"Porportional-baselines-Mean"]<- (Attrition.weight*mean.porp.baseline+Returnee.weight*mean.returnee.baseline)
      results[i,"Avg-number-returnees"]<-  nrow(Returnee.Baseline.Iteration)
      results[i,"Avg-number-replacements"]<- nrow(Pseudo.Replace.Iteration)
      results[i,"Avg-number-baseline"]<-  nrow(Porportional.baseline.noPRs.noReturnee)+nrow(Returnee.Baseline.Iteration)
      results[i,"Avg-number-attempts-to-match"]<-  count
      
      
      
      
      
      std.mean.diff.iteration<-mean(summary.match.df$`Std. Mean Diff.`)
      std.mean.diff.age.iteration<-summary.match.df[2,3]
      std.mean.pair.dist.iteration<-mean(summary.match.df$`Std. Pair Dist.`)
      
      
      # combines databses JAE: instead of adding to list, add to row i on results dataframe. Column names would be variables
      databaselist.returnee.followup[[i]] <- Returnee.FollowUp.Iteration
      databaselist.replacement.baseline[[i]] <- Pseudo.Replace.Iteration
      databaselist.basleine[[i]] <- Matched.baseline.chosen
      databaselist.returnee.baseline[[i]] <- Porportional.baseline.noPRs.noReturnee
      
      # restarts loop, adds 1 to counter, records progress
      restart <- TRUE
      progress(100 * i/N.iteration)
      Sys.sleep(0.01)
    }   # calcuates practice effects.
    #---------------------------------------#
  } 
  
  {
    time <- proc.time() - ptm  # if run immediatly after main while(PE) loop, will give how much time elapsed
    
    #calcuates practice effect and attrition effect
    Post.matching.DiffScore.new <- mean(results$`Returnee-FollowUp-Mean`, na.rm=TRUE) - 
      mean(results$`Replacement-Mean`, na.rm=TRUE)
    Post.matching.AttrScore.new <- mean(results$`Returnee-baseline-Mean`, na.rm=TRUE) - 
      mean(results$`Porportional-baselines-Mean`, na.rm=TRUE)
    Post.matching.PracEffect.new <- Post.matching.DiffScore.new - Post.matching.AttrScore.new
    
    
    # calculate effect size
    SD <- mean(results$`Returnee-FollowUp-SD`, na.rm=TRUE)
    M1 <- mean(results$`Returnee-FollowUp-Mean`, na.rm=TRUE)
    post.Madj <- M1 - Post.matching.PracEffect.new
    SDpooled <- sqrt(((SD * SD + SD * SD)/2))
    CohenD <- (M1 - post.Madj)/SDpooled
    
    #Stores time, saves out iteration results
    date <- Sys.time()
    day<- Sys.Date()
    Save.Data<- paste("Practice.Effect.Data_", TestVariable, day, sep = "")
    
    #Stores effect sizes in a chart
    Pe.effect.table <- data.frame(`Test-Name`= TestVariable,`Date-and-time` = date, `mean-score-PE-unadj` = M1, 
                                  `mean-score-PE-adj` = post.Madj, `Practice-Effect` = Post.matching.PracEffect.new, 
                                  `PE-Cohens D` = CohenD, `Attrition-Effect`=Post.matching.AttrScore.new, `Difference-Score`=Post.matching.DiffScore.new)
    
    PE.Table.Data<- paste("Practice.Effect.Results_", TestVariable, day, sep = "")
    
    
    write.csv(results, file = paste(Save.Data, ".csv", sep = ""))
    write.csv(Pe.effect.table, file = paste(PE.Table.Data, ".csv", sep = ""))
    
  }  # Records results. Saves out database
  
  #--------------------------this section Views results----------------------------------------#  
  TestVariable     # provides what variable looking at
  Pe.effect.table  # displays practice effect, attrition effect, and PE cohen's d
  matchingVars     # displays how matched variables
  describe(results)# prints out means of groups 
  time             # provides how long matching took
  list.settings    # provides settings for that run
  
  #---------------------------------------------------------------------------------------------#  
  #---------------------------------------------------------------------------------------------#
  #---------------------------------------------------------------------------------------------#