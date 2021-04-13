###### Notes + in progress code ######
# Hi McKenna and Jeremy. Thanks again for doing this.
# This script is a little editied because VETSA has a time between visits variable
# So I've removed "V1DATE" and "V2DATE" and added a "days_between_visits" variable
# #the VETSA data I think we should use removed attrition replacement subjects, and does not care about DX.
#
#
#
#

Here's the major changes I'm thinking:
  There is a package called compareGroups that looks like it will compare groups on multiple variables. 
I believe the output is a table listing p values. We can just check that all values in that column are 
above the specified threshold. This will take the place of the Match# chunks.
I believe that within each iteration, results are stored in variables 
and then rbinded to the previous iteration results. What we can do is initiate an empty dataframe at the very 
beginning that contains N.iteration rows, and columns corresponding to values youd like to save in each iteration:
results = data.frame(matrix(nrow=N.iteration, ncol=Nvariables))
Instead of a while loop, it will be a for loop that iterates 1 to N.iterations. You can then save values with something like:
results[i, valueColumn] = value
where i is the iteration number and valueColumn is the name of the variable you want to save.
I *think* this should shorten the script by a decent amount, and initiating a results dataframe with the size 
you are expecting the output to be may help track down if things go wrong (and, at least in python, can be preferable 
                                                                           for memory and speed, although I don't think that's a concern here).

I started to convert it from a while loop to a for loop, and te value of the loop number will be kept as i rather than PE.
I indicated where you can initialize a large dataframe to hold results. I made some comments throughout, you can search for "JAE" to find them. 


############################################################################### ♀
########################      Practice effect          ########################
########################      Pseudo-replacements      ########################
########################      last update: 4/6/21      ########################
###############################################################################

#--------------------------------------------------------------#
#              Block 0: Initial Setup
#
#
#----------#                                            
### Block0###        <requires edits>
{
  # 1. load libraries
  library(psych)
  library(MatchIt)
  library(readr)
  library(dplyr)
  library(haven)
  library(tidyverse)
  library(progress)
  library(svMisc)
  library(compareGroups)


  # 2. Load main database
  # note, data must be in wide format. Must include an education, variable. Age at 2 time points, Cog variable at 2 time points
  MainData <- MainData.ADNI

  # 3. identify baseline diagnosis variable
  # VETSA: I did not use a diagnosis variable.
  # SOL-INCA: MainData$Base_impaired=ifelse(MainData$total_6item_v1<4,0,1)

  # ADNI: Data.V1$MCI_any=(Data.V1$MCI_any-1)*-1 # sets MCI_any to have CN = 1, MCI=0
  # commented out so that MCI_Any=1 -> MCI selected
} # Block0: Identify main database. Select baseline diagnosis variable
#----------#                                            
#
#
#--------------------------------------------------------------#
#              Block 1-3 Set up databases to be compared. Block 1 and 2 require manual edits
#
#
#----------#                                            
### Block1###        <requires manual edits>
{
  # read in data
  Data.A1 <- as_tibble(MainData)
   # rename  data variables to match what's in script
  #"LDELTOTAL.v1"  "AVDEL30MIN.v1" "TRAASCOR.v1"   "TRABSCOR.v1"   "CATANIMSC.v1"  "BNTTOTAL.v1" 
  #"LDELTOTAL.v2"  "AVDEL30MIN.v2" "TRAASCOR.v2"   "TRABSCOR.v2"   "CATANIMSC.v2"  "BNTTOTAL.v2"
  
  Data.B2 <- Data.A1 %>%
    rename(
      ID = RID, # Subject ID
      AGE_V1 = Age.v1, # Age at time 1
      AGE_V2 = Age.v2, # Age at time 2
      BASEDX = MCI_any, # Diagnosis at baseline
      COG_V1 = TRAASCOR.v1, # Cognitive variable at time 1
      COG_V2 = TRAASCOR.v2 # Cognitive variable at time 2
    )
} # Block 1: Renames variables for script. Creates first database. Loads libraries
#----------#

### Block2### <requires manual edits>
{
  # Specify list of variable to match on. Do not include AGE, which will
  # be added automatically
  matchingVars <- c("PTEDUCAT", "PTGENDER",'ANARTERR')
  
  # Unique matching variables for this dataset
  Match.Xe <- subset(Data.B2, select = c("ID", matchingVars))  # ID variables
  Names.Match.Xe <- names(Match.Xe)
  Names.Match.Xe[1] <- "AGE"  # renames ID to AGE for a printout later. 
  
  # Use below code to confirm that your Xe are the correct class.  may
  # have to transform variables for normality
  sapply(Match.Xe, class)
  
  # removes anyonw with missing matching variables. Necessary for
  # matchit()
  Match.Xe.reduced <- na.omit(Match.Xe)
  
}  # Block 2: Identifies matching Xe. NOTE-listwise removal of anyone with NA value in matching Xe.
#----------#

### Block3###
{
  # create baseline dataset with peopel who have that cog variable. And
  #     meet DX crieria.
  Baseline.Data.A1 <- subset(Data.B2, COG_V1 != "NA" & BASEDX == 1)
  # create follow-up dataset with peopel who have that cog variable at
  # basleine and FU. Renames Cog variable [COGCOG]
  FollowUp.Data.A1 <- subset(Data.B2, COG_V2 != "NA" & COG_V1 != "NA", 
                             select = "ID")
  FollowUp.Data.B2 <- merge(Baseline.Data.A1, FollowUp.Data.A1, by = "ID")
  
  # rename variables for script
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
  
  # create Databases used by script. Reduces samples to those with
  #     Matching Xe.
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
  # they can be in it]
  Attritors.baseline <- Full.Baseline.data[-which(Full.Baseline.data$ID %in% 
                                                    Returnee.baseline.data$ID), ]
  
  # tracks size of databases
  Return.fullbaseline.size <- nrow(Full.Baseline.data)
  Return.baseline.size <- nrow(Returnee.baseline.data)

}  # Block 3: Selects for DX at baseline. Creates main 3 databases: All, returnee baseline, returnee follow-up.
#----------#
# 
#--------------------------------------------------------------#
# Block 4-5: Define, record, and printout script parameters.
#----------#
### Block4### <requires edits> Block6: Manually set parameters
{
  
  # identify cognitive variable of interest, saved for printout later
  TestVariable <- "Trails A"
  
  # JAE: Alternate way to print sample size info
  cat(" Number of Full Baseline:      ", nrow(Full.Baseline.data), "\n", 
      "Number of Returnee Follows:   ", nrow(Returnee.FollowUp.data), 
      "\n", "Number of Returnee Baselines: ", nrow(Returnee.baseline.data), 
      "\n", "If !=0 then error in Block3:  ", (nrow(Returnee.FollowUp.data) - 
                                                 nrow(Returnee.baseline.data)))
  
  # select sample sizes, matching details
  N.returnee <- 100  # number of returnees to start each iteration
  N.returnee.min <- 80  # minimum number of returnees in sample
  N.PR.Pool <- 150  # sets minimal number of subjects in potential PR pool.
  N.PR.Start <- 100  # number of PR to initially randomly sample.
  N.base.pool <- 100  # sets minimum number of subjects in potential baseline pool
  N.base.start <- 150  # number of baseline to initially randomly sample.

  # select number of iterations
  N.iteration <- 2001
  
  #IF BELOW IS TRUE, THEN BASELINE MATCHED ONLY ON AGE.
  baseline.match.age.only == TRUE
  
  # select P value level for group testing to confirm Matchit correctly
  # assigned groups
  value.match <- 0.8
  AGECALIPER <- 0.1  # how far apart in years, the matched groups can be.
  
  # Loop counters
  PR.match.count <- 250  # number of attempts for matching PR to Returnees
  base.match.count <- 250  # number of attempts for matching baseline to returnees
}  # Block 4: set up parameters for script
#----------#

### Block5###
{
 
  # JAE: Initialize a dataframe to hold results here. Rows equal to N iterations,
  # columns equal to number of values you want to save. Instead of individual 1D dataframe
  # that you have below, they'll each be a column in the larger dataframe. 
  N.values=10 #? MSC: not sure about this
  results <- data.frame(matrix(nrow=N.iteration, ncol=N.values))
  names(results) <- c("Test",
                      "Matched-baseline-Mean", "Matched-Baseline-SD",
                      "Returnee-baseline-Mean","Returnee-baseline-SD",
                      "Returnee-FollowUp-Mean", "Returnee-FollowUp-SD",
                      "Replacement-Mean","Replacement-SD","PR-Return-Matching")
  databaselist.returnee.followup <- list()
  databaselist.replacement.baseline <- list()
  databaselist.basleine <- list()
  databaselist.returnee.baseline <- list()
  Mean.Bootstrap.data.temp <- NULL
  Mean.Bootstrap.data.final <- NULL

  list.settings <- data.frame(`Variable of interest` = TestVariable, 
                              `N returnee selected` = N.returnee, `N potential replacments` = N.PR.Pool, 
                              `N replacments start match with` = N.PR.Start, `Max attempts to match returrnee-replacments` = PR.match.count, 
                              `N potential baseline subs` = N.base.pool, `N potential baseline start with` = N.base.start, 
                              `Max attempts to match returrnee-baseline` = base.match.count, 
                              `Max iterations` = N.iteration - 1, `Iterations achieved` = PE, 
                              `P value match` = value.match, `BASELINE MATCHED ON AGE ONLY` = baseline.match.age.only,
                              `Total number returnees` = Return.baseline.size, `Total number with baseline` = Return.fullbaseline.size)
  
  
  # prints out settings
}  # Block 5: Records parameter values in 'settings.' Resets temporary databases used by main script
#----------#
# 
#--------------------------------------------------------------#
# 
#----------#
#----------#=
# displays settings; check prior to running main script#
print(list.settings)  # provides all parameters
#----------#
#----------#

############################################################################################### 
#---------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------#
#--------------------------this section runs the program. ------------------------------------#  
Count.Fail <- 0
ptm <- proc.time()  # starts timer to see how long script takes
# JAE: instead of while loop, we'll iterate over 1:N.iteration
for (i in seq(N.iteration)) {
  {
    base.print <- "Fully matched"
    count <- 0
    success.age <- 0
    success.match <- 0
    success.matchingVars <- 0
    total.count <- 0
    baseline.count <- 0
    success <- 0
    baseline.success <- 0
    base.selection <- TRUE
    restart <- FALSE  # sets up matching fail condition
    PR.selection <- TRUE
    Matched.baseline.chosen <- NULL
    Returnee.Baseline.Iteration <- NULL
    Returnee.FollowUp.Iteration <- NULL
    Pseudo.Replace.Iteration <- NULL
  }  # resets conditions for all loops
  
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
    Potential.PR.V3 <- Potential.PR.V2[sample(nrow(Potential.PR.V2), 
                                              N.PR.Start), ]  # starting sample size
    Potential.PR.V3$match <- 0  # will be used in match.it()
    Potential.PR.V3$keep <- NULL
    # package. 'match' is IDifying Xe [0=returnee, 1=PR]
    matching <- rbind(Potential.PR.V3, r.sample.returnees)
    
    {
      # Match subjects based on matchingVars
      ## Combine all variables to be matched into a formula
      fmlaMatching <- as.formula(paste0("match ~ ", paste(c("AGE", matchingVars), collapse = " + ")))
      m.out <- matchit(fmlaMatching, method = "nearest", 
                       data = matching, caliper = AGECALIPER, mavars = ~AGE, link = "probit")
      Matched.data.all <- match.data(m.out)
      Matched.data.PR <- subset(Matched.data.all, match==0)
      Matched.data.Return <- subset(Matched.data.all, match==1)      
      # Compare all groups. Automatically determines test based on normality
      match.test <- compareGroups(fmlaMatching, data=Matched.data.all, method=NA)
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
      if (success != 1) {
        count <- count + 1
        restart <- TRUE
      }
      if (count > PR.match.count) {
        restart <- TRUE
        success <- 1
      }
      if (success == 1 & count < PR.match.count) {
        restart <- FALSE
        # this was added because chisq() sometimes breaks when no subjects have JAE: Maybe not necessary anymore?
        # a value in a cell
      }
    }  # Fail code
  }  # mathces pseduo replacments to returnees follow-up
  # Create labled databases
  Returnee.FollowUp.Iteration <- Matched.data.Return
  Returnee.Baseline.Iteration <- Returnee.baseline.data[which(Returnee.baseline.data$ID %in% 
                                                                Returnee.FollowUp.Iteration$ID), ]
  Pseudo.Replace.Iteration <- Matched.data.PR
  
  # set up matching for baseline and retunree baseline
  Baseline.temp.NoPRs <- Full.Baseline.data[-which(Full.Baseline.data$ID %in% 
                                                     Pseudo.Replace.Iteration$ID), ]  # removes chosen set of PRs from baseline pool
  Baseline.temp.pool <- Baseline.temp.NoPRs[-which(Baseline.temp.NoPRs$ID %in% 
                                                     Returnee.Baseline.Iteration$ID), ]  # removes chosen set of returnees from baseline pool
  
  while (baseline.success == 0 & restart == FALSE) {
    # resets databases; may be repeated
    m.out.baseline <- NULL
    Matched.baseline.all <- NULL
    Matched.baseline.chosen <- NULL
    base.selection <- TRUE
    
    if (base.selection == TRUE) {
      age_bounds <- describe(Returnee.Baseline.Iteration$AGE)
      u_bound <- age_bounds$max
      l_bound <- age_bounds$min
      Baseline.temp.pool$keep <- ifelse(Baseline.temp.pool$AGE > u_bound, 
                                        -1, ifelse(Baseline.temp.pool$AGE < l_bound, -1, 1))
      
      Baseline.temp.pool.2 <- subset(Baseline.temp.pool, keep==1)  # removes anyone outside age range
      if (nrow(Baseline.temp.pool.2) > (N.base.pool)) {
        base.selection <- FALSE
      }
      Baseline.pool <- Baseline.temp.pool.2[sample(nrow(Baseline.temp.pool.2), 
                                                   N.base.start), ]  # starting sample size
      # Baseline.pool=Baseline.temp.pool.2
      Baseline.pool$match <- 0
      Baseline.pool$keep <- NULL
      
      Baseline.pool$match <- 0
      Returnee.Baseline.Iteration$match <- 1
      # loop for matching the Baseline to the Returnee baseline
      baseline.matching <- rbind(Baseline.pool, Returnee.Baseline.Iteration)  # not sure if this is going to work? check ns
    }

    {
      if (baseline.match.age.only == TRUE) {
        fmlaMatching <- as.formula(paste0("match ~ ", paste(c("AGE"), collapse = " + ")))
        
      }
      if (baseline.match.age.only == FALSE) {
        fmlaMatching <- as.formula(paste0("match ~ ", paste(c("AGE", matchingVars), collapse = " + ")))
        
      }
      
      # Match baseline subjects based on matchingVars
      # Combine all variables to be matched into a formula
      m.out.baseline <- matchit(fmlaMatching, method = "nearest", data = baseline.matching, 
                                link = "probit")
      Matched.baseline.all <- match.data(m.out.baseline)
      Matched.baseline.chosen <- subset(Matched.baseline.all, match==0)
      Matched.data.Return <- subset(Matched.baseline.all, match==1)     
      # Compare all groups. Automatically determines test based on normality
      match.baseline.test <- compareGroups(fmlaMatching, data=Matched.baseline.all, method=NA)
      # Determine whether all p values are above threshold
      match.baseline.p <- getResults(match.baseline.test, "p.overall")
      baseline.success.matchingVars <- ifelse(all(match.baseline.p>value.match), 1, 0)
      
      # Leave loop conditions JAE: Do the success variales below need to be initialized as 0 anywhere?
      baseline.success.match <- ifelse(nrow(Matched.data.PR) > (N.returnee.min), 1, 0) # JAE: Shouhld this be checking Matched.data.Return?
      # exit code
      baseline.success <- ifelse(baseline.success.match == 1 & baseline.success.matchingVars == 1, 1, 0)


    }  # Matches dataset based on predefined number of match categories. Age always matched.
    
    {
      # fail code
      if (baseline.success != 1) {
        baseline.count <- baseline.count + 1
      }
      if (baseline.count > base.match.count | restart == TRUE) {
        restart <- TRUE
        baseline.success <- 1
      }
    }  # Fail code
  }  # matches returnees to a baseline sample
  
  if (restart == TRUE) {
    Count.Fail <- Count.Fail + 1
  }
  # calcuates practice effects.
  if (restart == FALSE) {
    results[i,"Test"]<- TestVariable
    results[i,"Matched-baseline-Mean"]<- mean(Matched.baseline.chosen$COGCOG)
    results[i,"Matched-Baseline-SD"]<- SD(Matched.baseline.chosen$COGCOG)
    results[i,"Returnee-baseline-Mean"]<- mean(Returnee.Baseline.Iteration$COGCOG) 
    results[i,"Returnee-baseline-SD"]<- SD(Returnee.Baseline.Iteration$COGCOG)
    results[i,"Returnee-FollowUp-Mean"]<- mean(Returnee.FollowUp.Iteration$COGCOG)
    results[i,"Returnee-FollowUp-SD"]<- SD(Returnee.FollowUp.Iteration$COGCOG) 
    results[i,"Replacement-Mean"]<- mean(Pseudo.Replace.Iteration$COGCOG)
    results[i,"Replacement-SD"]<- SD(Pseudo.Replace.Iteration$COGCOG)
    results[i,"PR-Return-Matching"]<- toString(Names.Match.Xe)

    # combines databses JAE: instead of adding to list, add to row i on results dataframe. Column names would be variables
    databaselist.returnee.followup[[i]] <- Returnee.FollowUp.Iteration
    databaselist.replacement.baseline[[i]] <- Pseudo.Replace.Iteration
    databaselist.basleine[[i]] <- Matched.baseline.chosen
    databaselist.returnee.baseline[[i]] <- Returnee.Baseline.Iteration
    
    # restarts loop, adds 1 to counter, records progress
    restart <- TRUE
    progress(100 * i/N.iteration)
    Sys.sleep(0.01)
  }
  #---------------------------------------#
  
 
}

 {
    time <- proc.time() - ptm  # if run immediatly after main while(PE) loop, will give how much time elapsed

    
    
    # calcuate practice effects
    Post.matching.DiffScore <- mean(results$`Returnee-FollowUp-Mean`, na.rm=TRUE) - 
      mean(results$`Replacement-Mean`, na.rm=TRUE)
    Post.matching.AttrScore <- mean(results$`Returnee-baseline-Mean`, na.rm=TRUE) - 
      mean(results$`Matched-baseline-Mean`, na.rm=TRUE)
    Post.matching.PracEffect <- Post.matching.DiffScore - Post.matching.AttrScore
    
    
    # calculate effect size
    SD <- mean(results$`Returnee-FollowUp-SD`, na.rm=TRUE)
    M1 <- mean(results$`Returnee-FollowUp-Mean`, na.rm=TRUE)
    post.Madj <- M1 - Post.matching.PracEffect
    SDpooled <- sqrt(((SD * SD + SD * SD)/2))
    CohenD <- (M1 - post.Madj)/SDpooled
    date <- Sys.time()
    Pe.effect.table <- data.frame(`Test-Name`= TestVariable,`Date-and-time` = date, `mean-score-PE-unadj` = M1, 
                                  `mean-score-PE-adj` = post.Madj, `Practice-Effect` = Post.matching.PracEffect, 
                                  `PE-Cohens D` = CohenD, `Attrition-Effect`=Post.matching.AttrScore, `Difference-Score`=Post.matching.DiffScore)
    
    day<- Sys.Date()
    Save.Data<- paste("Practice.Effect.Data_", TestVariable, day, sep = "")
    PE.Table.Data<- paste("Practice.Effect.Results_", TestVariable, day, sep = "")
    head(mtcars_subset)
    write.csv(results, file = paste(Save.Data, ".csv", sep = ""))
    write.csv(Pe.effect.table, file = paste(Save.Data, ".csv", sep = ""))
    
    
    
    
  }  # saves  results
#--------------------------this section Views results----------------------------------------#  
TestVariable  # provides what variable looking at
Pe.effect.table  # displays practice effect, attrition effect, and PE cohen's d
describe(results)
base.print  # was baseline age matched or fully matched
time  # provides how long it took
list.settings  # provides settings for that run
#---------------------------------------------------------------------------------------------#  
#---------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------#
############################################################################################### 