#White-headed woodpecker Integrated Population Model
# Ana Miller-ter Kuile
# May 23, 2023

# Load packages -----------------------------------------------------------

package.list <- c("tidyverse", 
                  "LaplacesDemon") #has rcat function

## Installing them if they aren't already on the computer
new.packages <- package.list[!(package.list %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## And loading them
for(i in package.list){library(i, character.only = T)}

# Model -------------------------------------------------------------------

#This is an IPM that combines data from nest stages (egg counts, 
# nestling counts, fledgling counts) and from adult occupancy in three
# CFLRP's to estimate population abundance across three different
# forest locations

# Aspects of the model:
## 1. Using previous statistical models run on each stage separately, we incorporate
### covariate effects on each stage by sampling from the posterior 
### samples from those statistical models for each stage. This introduces stochasticity
### even in "deterministic" elements of the model
## 3. Right now I've determined breeding:nonbreeding ratio for the total 
### adult population (not just females) since I correct for female ratios
### anywhere breeding proportion is incorporated (fledglings recruited)

#Current approaches
##1. YEAR 0 = First year of data collection
##2. any [y] indexed value is "end of year" population (to help
### think through recursive structures)
##3. "Data": 
####a. covariate effect posteriors
####b. covariate values per points

#NEED TO DO:
#impute missing values before loop for:
# Tmax_eg[i]
# PPT_ne[i]
# Tmax_ne[i]
# InitDay[i]

# Empty dataframes and list -----------------------------------------------


#create a list of dataframes for each iteration we run through
df_list <- list()
r2_list <- list()

#create a dataframe at the end of each iteration that 
#includes the population values we care about
pop.df <- data.frame(phi.egg = numeric(),
                     phi.nestling = numeric(),
                     phi.adult = numeric(),
                     pop.lambda = numeric(),
                     prop.survival = numeric(),
                     prop.recruitment = numeric())

r2.df <- data.frame(egg.r2 = numeric(),
                    nestling.r2 = numeric(),
                    adult.r2 = numeric())

# Start of loop -----------------------------------------------------------

for(iter in 1:n.iter){#number of iterations

  # Covarate effect sampler -------------------------------------------------
  
  #Right now:
  # N_samples == number of posterior samples in datasets
  #N_samples assumed to be equal per model (could change, but need to then
  #redefine per loop)
  #index for each submodel selects covariates for each iteration of the 
  #population model from the same iteration of the sample list, thus 
  #maintaining correlatino between covariates in a given iteration

  #for all samplers, need a probability for each observation

  #since N_samples == for all submodels, can set one global one above

  #each sample has equal likelihood of being selected from categorical:
  pi <- rep(1/N_samples, N_samples)
  
  #create index for each loop
  #this will sample from the same "row" (sample) in each of the 
  #submodels. This is fine within models because it keeps correlation
  #structure, it is also fine across models because they are independent
  #of each other 
  index <- rcat(n = 1, p = pi)


  #Egg number
  #egg lambda for the iteration is the egg lambda in the egg lambda array
  #that matches the random number index
  egg.lambda <- egg.lambda.array[index, forest] #pulls row of the sampled index for the forest selected

  #EGG Survival
  #e0 <- e0.array[index, forest]  #pulls row of the sampled index for the forest selected
  e0 <- e0.array[index]
  #loop by the number of treatments
  e1TreatmentID <- rep(NA, n.trt)
  
  for(t in 1:n.trt){
    e1TreatmentID[t] <- e1Trt.array[index,t] #row, then column is treatment type
  }
  
  e <- rep(NA, 4)
  
  for(egg.cov in 2:4){
    e[egg.cov] <- e.array[index, egg.cov] #rows = sample, column = covariate ID
  }
  
  #NESTLING Survival
  #n0 <- n0.array[index, forest] #pulls row of the sampled index for the forest selected
  n0 <- n0.array[index]
  
  n1TreatmentID <- rep(NA, n.trt)
  
  for(t in 1:n.trt){
    n1TreatmentID[t] <- n1Trt.array[index,t]
  }
  
  n2SpeciesID <- rep(NA, n.species)
  
  for(s in 1:n.species){
    n2SpeciesID[s] <- n2Spec.array[index,s]
  }
  
  n <- rep(NA, 8)
  for(nstl.cov in 3:8){
    n[nstl.cov] <- n.array[index, nstl.cov]
  }
  
  #ADULT initial values/survival
  a0 <- a0.array[index]  #pulls row of the sampled index for the forest selected
  
  a <- rep(NA, 5)
  for(ad.cov in 1:5){
    a[ad.cov] <- a.array[index, ad.cov]
  }
  
  #female prob
  pF <- propF.array[index, forest] #pulls from first forest in array for now
  
  # Non-yearly population rates ---------------------------------------------

  #breeding proportion - from occupancy data
  #fledgling survival - from Kozma et al. 2022
  #transition rates between breeding/non-breeding: just a really small number
  
  #breeding proporiton
  #based mean and precision on the average of yearly breeding proportions
  #What is the variance year to year of proportion Breeding?
  #Use that to then set prior based on the beta distribution:
  #dbeta(a.propBr, b.propBr)
  #E(p) = a/(a+b)
  #Var(p) = ab/((a+b)^2*(a+b+1))
  #Solve for A and B when we know the mean and variance
  #E(p) = 0.64
  #Var(p) = 0.04
  #a = 2.88
  #b = 1.62
  pBr <- rbeta(n = 1, shape1 = 2.88, shape2 = 1.62)
  
  #do uniform between small numbers here
  transition <- runif(n = 1, min = 0, max = 0.1) 
  
  #FLEDGLING survival
  #we don't know fledgling survival, so we'll estimate it
  #based on previous research from:
  #Kozma et al 2022: 86% (0.74-0.98) - after out of natal territories
  #same as for breedign proportions above
  #E(f) = 0.86
  #Var(f) (from SD^2, SD = 0.12) = 0.01
  #alpha = 9.49
  #beta = 1.54
  phi.fledge <- rbeta(n = 1, shape1 = 9.49, shape2 = 1.54)

  # Nest-level values ----------------------------------------------------

  #values of survival for each nest - egg and nestling
  p.phi.egg <- rep(NA, n.nests)
  p.phi.nestling <- rep(NA, n.nests)
  for(i in 1:n.nests){
    #EGG survival
    #e's from previous statistical models,
    p.phi.egg[i] <- plogis(
      e0 +
        e1TreatmentID[TreatmentID[i]] +
        e[2]*ForestCV[i] +
        e[3]*PPT_eg[i] +
        e[4]*Tmax_eg[i] 
    )
  
    #NESTLING survival
    #again, n0 and n1... from statistical models
    p.phi.nestling[i] <- plogis(
      n0 + 
        n1TreatmentID[TreatmentID[i]] +
        n2SpeciesID[SpeciesID[i]] +
        n[3]*LandBu[i] +
        n[4]*InitDay[i] +
        n[5]*NestHt[i] +
        n[6]*PPT_ne[i] +
        n[7]*Tmax_ne[i] +
        n[8]*Tmax_ne[i]^2)

}

  # Yearly nest rates ------------------------------------------------------

  eggs <- rep(NA, n.years)
  for(y in 2:n.years){
    #FECUNDITY
    #per adult egg production is not based on any covariates,
    #but we estimated egg.lambda from statistical model:
    eggs[y] <- rpois(n = 1, lambda = egg.lambda)
  }
  
  phi.egg <- rep(NA, n.years)
  phi.nestling <- rep(NA, n.years)
  phi.egg.yes <- rep(NA, n.years)
  phi.egg.no <- rep(NA, n.years)
  phi.nestling.yes <- rep(NA, n.years)
  phi.nestling.no <- rep(NA, n.years)
  
  #nest.start[y] and nest.end[y] are indexing so
  #that the phi's is only calculated for nests in that year,
  # fixes issues that nests weren't all surveyed every year
  for(y in 1:n.years){ 
    #if the year had nest surveys, this is the value we will pull from
    phi.egg.yes[y] <- mean(p.phi.egg[nest.start[y]:nest.end[y]])
    phi.nestling.yes[y] <- mean(p.phi.nestling[nest.start[y]:nest.end[y]])
  }
  
  for(y in 2:(n.years-1)){
    #if the year didn't have nest surveys, this is the value we will pull from
    #only produces non NA value if the year is "sandwiched" between
    #the start and end years
    phi.egg.no[y] <- mean(p.phi.egg[nest.start[y-1]:nest.end[y+1]])
    phi.nestling.no[y] <- mean(p.phi.nestling[nest.start[y-1]:nest.end[y+1]])
  }
  
  for(y in 1:n.years){
    #I have an indicator "data" object of whether a forest site was surveyed
    #in a particular year (1 == surveyed; 0 == not surveyed)
    phi.egg[y] <- if_else(surveyed[y]>0, #if surveyed,
                         #take mean of the values for nests for that year
                         phi.egg.yes[y],
                         #if not surveyed, take the estimate based on the
                         #values from previous and next year, accounting
                         #for temporal correlation between treatment amounts
                         phi.egg.no[y])
  
    #same for nestling survival
    phi.nestling[y] <- if_else(surveyed[y]>0,#if surveyed,
                              #take mean of the values for nests for that year
                              phi.nestling.yes[y],
                              #if not surveyed, take the estimate based on the
                              #values from previous and next year, accounting
                              #for temporal correlation between treatment amounts
                              phi.nestling.no[y])
  }

# Year 1 adults and eggs --------------------------------------------------

  p.psi1 <- rep(NA, n.points)
  pBA0 <- rep(NA, n.points)
  Z.Br0 <- rep(NA, n.points)
  point.Br0 <- rep(NA, n.points)
  pNBA0 <- rep(NA, n.points)
  point.NBr0 <- rep(NA, n.points)
  point.egg <- rep(NA, n.points)
  Z.NBr0 <- rep(NA, n.points)
  #beginning of year 1 adults are based on initial occupancy 
  #from dynamic occupancy model
  #for points on transects AND background points
  for(i in 1:n.points){
    #initial year 1 occupancy and it's covariates
    #from the dynamic occupancy model
    p.psi1[i] <- plogis(
      a0 + 
        a[1]*aTmean_sp[i, 1] +
        a[2]*aTmean_wt[i, 1] +
        a[3]*aPPT_wt[i, 1] +
        a[4]*aLandHa[i, 1] +
        a[5]*aLowCC[i, 1]
    )
  
    #Year 0 (beginning of first year) adult #s at each point
    #based on occupancy probability (any adult)
    #corrected for whether it is in the breeding population based
    #on breedign ratios
    #and corrected for whether it is female or not
    #for breeding - 1/2 of breeding pair is female
    #for non-breeding - number female based on female ratio
    #this is the probability a breeding female is there:
    pBA0[i] <- p.psi1[i]*pBr*0.5
    #sample number of breeding females:
    Z.Br0[i] <- rbinom(n = 1, size = 1, prob = pBA0[i]) 
    #this si the probability a non-breeding female is there:
    pNBA0[i] <- p.psi1[i]*(1-pBr)*pF
    #sample number of non-breeding females
    Z.NBr0[i] <- rbinom(n = 1, size = 1, prob = pNBA0[i])
    
    #make first year eggs stochastic
    point.egg[i] <- rpois(n = 1, lambda = egg.lambda)
    point.egg[i] <- ifelse(Z.Br0[i] > 0, point.egg[i], 0)
  }

  #total number of breeding and non-breeding across all points
  N_Br0 <- sum(Z.Br0)
  N_NBr0 <- sum(Z.NBr0)
  eggs[1] <- sum(point.egg)

  # Year 1 nest stage total values ---------------------------------------
  
  N_egg <- rep(NA, n.years)
  N_nestling <- rep(NA, n.years)
  N_fledge <- rep(NA, n.years)
  #for the first year - use the breeding adult population 
  #estimated from the adult occupancy for "year 0" (end of year 0/start of year 1)
  #total number of eggs is based on total adults*fecundity (above point loop for year 1)
  #stochastic egg value from above point loop
  N_egg[1] <- eggs[1]   #modeled adults at "end of last year"
  #nestling number is based on egg survival*total eggs
  N_nestling[1] <- phi.egg[1]*N_egg[1]
  #fledgeling number is based on nestling survival*total nestlings
  N_fledge[1] <- phi.nestling[1]*N_nestling[1]

  # Yearly adult survival rate ----------------------------------------------

  p.phi.adult <- matrix(rep(NA), n.points, n.years)
  #for surveyed points on transects
  for(i in 1:n.points){
    for(y in 1:n.years){
      #ADULT survival
      #adult survival estimate
      #this is from the "persistence" (phi) term covariates 
      # in the dynamic occupancy model
      p.phi.adult[i,y] <- plogis(
       a0 + 
         a[1]*aTmean_sp[i, y] +
         a[2]*aTmean_wt[i, y] +
         a[3]*aPPT_wt[i, y] +
         a[4]*aLandHa[i, y] +
         a[5]*aLowCC[i, y]
      )
      
    }
  }
  
  #yearly mean value derived from point values
  phi.adult <- rep(NA, n.years)
  for(y in 1:n.years){
    
    phi.adult[y] <- mean(p.phi.adult[,y])
    
  }
  
  #phi.adult could also be a stochastic node based on data from
  #Kozma et al. 2022 (https://doi.org/10.1676/22-00014), 
  #in which adult survival from a CJS model
  #in Washington managed forests was:
  #mean: 0.85 (0.78–0.93)
  #SD = 0.07
  #var = 0.005
  #a = 20.825
  #b = 3.669
  #check to see if values estimated from model look like these
  #to proceed
  #otherwise, not yearly and:
  #phi.adult <- rbeta(n = 1, shape1 = 20.83, shape2 = 3.67)

  # Year 1 adult numbers ----------------------------------------------------
  N_Br <- rep(NA, n.years)
  N_NBr <- rep(NA, n.years)
  # RecruitmentBr <- rep(NA, n.years)
  # SurvivalBr <- rep(NA, n.years)
  # RecruitmentNBr <- rep(NA, n.years)
  # SurvivalNBr <- rep(NA, n.years)
  #use initial occupancy to figure this out:
  #adult pop at the END of year 1 is dependent on 
  #number fledged and the initial adult number
  N_Br[1] <- phi.fledge*N_fledge[1]*pBr*pF + #how many fledglings survive, 
    #go into breeding population, and are female
    N_Br0*phi.adult[1] + #breeding adults last year that survived through this year
    N_NBr0*phi.adult[1]*transition - #non-breeding adults that transitioned to breeding
    #subtract breeding that went non-breeding
    N_Br0*phi.adult[1]*transition
  
  # RecruitmentBr[1] <- phi.fledge*N_fledge[1]*pBr*pF
  # SurvivalBr[1] <- N_Br0*phi.adult[1] + #breeding adults last year that survived through this year
  #   N_NBr0*phi.adult[1]*transition - #non-breeding adults that transitioned to breeding
  #   #subtract breeding that went non-breeding
  #   N_Br0*phi.adult[1]*transition
  
  N_NBr[1] <- phi.fledge*N_fledge[1]*(1-pBr)*pF + #how many fledglings survive,
    #go into non-breeding population, and are female
    N_NBr0*phi.adult[1] + #breeding adults that survived from last year
    N_Br0*phi.adult[1]*transition - #non-breeding adults that transitioned to breeding
    #subtract line 226 (nonbreeding that went to breeding)
    N_NBr0*phi.adult[1]*transition
  
  # RecruitmentNBr[1] <- phi.fledge*N_fledge[1]*(1-pBr)*pF
  # SurvivalNBr[1] <- N_NBr0*phi.adult[1] + #breeding adults last year that survived through this year
  #   N_Br0*phi.adult[1]*transition - #non-breeding adults that transitioned to breeding
  #   #subtract breeding that went non-breeding
  #   N_NBr0*phi.adult[1]*transition
  # 
  #add in: track total yearly "recruitment" and "survival" components

  # Years 2+ population numbers --------------------------------------------

  for(y in 2:n.years){
    #Nest stages
    #total number of eggs is based on total adults*fecundity
    N_egg[y] <- eggs[y]*N_Br[y-1]   #modeled adults at "end of last year"/"beginning of this year"
    #nestling number is based on egg survival*total eggs
    N_nestling[y] <- phi.egg[y]*N_egg[y]
    #fledgeling number is based on nestling survival*total nestlings
    N_fledge[y] <- phi.nestling[y]*N_nestling[y]
    
    #Adults
    #adults in the next year would be:
    #additional new fledglings -> adults is equal fledglings times
    #fledgling survival times proportion that likely went to breeding pop
    #times the number that are female
    #adults at the end of the year:
    #number of fledglings that survived this year
    N_Br[y] <- phi.fledge*N_fledge[y]*pBr*pF + #number of fledglings that survive,
      #go into breeeding population, and are female
      N_Br[y-1]*phi.adult[y] + #breeding adults last year that survived through this year
      N_NBr[y-1]*phi.adult[y]*transition - #non-breeding adults that transitioned to breeding
      #subtract line 236 (breeding that went non-breeding)
      N_Br[y-1]*phi.adult[y]*transition
    #N_Br[y-1]*phi.adult[y]*emmigrate #adults that emmigrated elsewhere
    #immigrate #adults that immigrated into the population
    
    # RecruitmentBr[y] <- phi.fledge*N_fledge[y]*pBr*pF
    # SurvivalBr[y] <- N_Br[y-1]*phi.adult[y] + #breeding adults last year that survived through this year
    #   N_NBr[y-1]*phi.adult[y]*transition - #non-breeding adults that transitioned to breeding
    #   #subtract breeding that went non-breeding
    #   N_Br[y-1]*phi.adult[y]*transition
    
    #additional new fledglings -> adults is equal fledglings times
    #fledgling survival times proportion that likely went to breeding pop
    #times the number that are female
    N_NBr[y] <- phi.fledge*N_fledge[y]*(1-pBr)*pF + #numbeer of fledgeling that survive,
      #go into non-breeding population, and are male
      N_NBr[y-1]*phi.adult[y] + #breeding adults that survived from last year
      N_Br[y-1]*phi.adult[y]*transition - #non-breeding adults that transitioned to breeding
      #subtract line 226 (nonbreeding that went to breeding)
      N_NBr[y-1]*phi.adult[y]*transition
    #N_NBr[y-1]*phi.adult[y]*emmigrate #adults that emmigrated elsewhere
    #immigrate #adults that immigrated into population

    # RecruitmentNBr[y] <- phi.fledge*N_fledge[y]*(1-pBr)*pF
    # SurvivalNBr[y] <- N_NBr[y-1]*phi.adult[y] + #breeding adults last year that survived through this year
    #   N_Br[y-1]*phi.adult[y]*transition - #non-breeding adults that transitioned to breeding
    #   #subtract breeding that went non-breeding
    #   N_NBr[y-1]*phi.adult[y]*transition

    }

  # Population growth -------------------------------------------------------

  #get total number of adults per year
  N_0 <- N_Br0 + N_NBr0
  
  #"end" of each year population
  N <- rep(NA, n.years)
  for(t in 1:n.years){
    N[t] <- N_Br[t] + N_NBr[t]
  }
  
  pop.lambda <- rep(NA, n.years)
  #year 1 growth from year 0 values
  pop.lambda[1] <- N[1]/
    N_0
  
  ##POPULATION GROWTH RATES
  for(y in 2:n.years){
    #   #population growth is a function of:
    pop.lambda[y] <-
      #adults in next year
      N[y]/ #divided by
      #adults this year
      N[y-1]
    #
    #   #lambda < 1 = population decreases
    #   #lambda == 1 = population stable
    #   #lambda > 1 = population increases
    
  }
  
  #recruitment and survival proportion per year
  # Recruit <- rep(NA, n.years)
  # Survive <- rep(NA, n.years)
  # prop.survival <- rep(NA, n.years)
  # prop.recruitment <- rep(NA, n.years)
  # for(t in 1:n.years){
  #   Recruit[t] <-  RecruitmentBr[t] + RecruitmentNBr[t]
  #   Survive[t] <- SurvivalBr[t] + SurvivalNBr[t]
  #   prop.survival[t] <- Survive[t]/N[t]
  #   prop.recruitment[t] <- Recruit[t]/N[t]
  # }

  # Populate dataframe ------------------------------------------------------

  #make the dataframe filled with the 
  #values of importance
for(y in 1:n.years){
  pop.df[y, 1] <- phi.egg[y]
  pop.df[y, 2] <- phi.nestling[y]
  pop.df[y, 3] <- phi.adult[y]
  pop.df[y, 4] <- pop.lambda[y]
  # pop.df[y, 5] <- prop.survival[y]
  # pop.df[y, 6] <- prop.recruitment[y]
}
  

# R^2 dataframe -----------------------------------------------------------
  
  #dataframe of relationships between lifestages and lambda
  #for missing years, don't want to draw inference for those
  #years, so we can create a new df that accounts for any missing
  #years
  surveyed.pop.df <- pop.df[survey_rows,]
  egg.lambda.reg <- lm(surveyed.pop.df$pop.lambda ~ surveyed.pop.df$phi.egg)
  egg.lambda.sum <- summary(egg.lambda.reg)
  egg.r2 <- egg.lambda.sum$adj.r.squared
  nestling.lambda.reg <- lm(surveyed.pop.df$pop.lambda ~ surveyed.pop.df$phi.nestling)
  nestling.lambda.sum <- summary(nestling.lambda.reg)
  nestling.r2 <- nestling.lambda.sum$adj.r.squared
  adult.lambda.reg <- lm(pop.df$pop.lambda ~ pop.df$phi.adult)
  adult.lambda.sum <- summary(adult.lambda.reg)
  adult.r2 <- adult.lambda.sum$adj.r.squared
  
  r2.df[1,1] <- egg.r2
  r2.df[1,2] <- nestling.r2
  r2.df[1,3] <- adult.r2

  # Generate list of dataframes ---------------------------------------------

  df_list[[iter]] <- pop.df
  
  r2_list[[iter]] <- r2.df
  }

final <- bind_rows(df_list, .id = "iter") %>%
  group_by(iter) %>%
  mutate(yearID = 1:n())

r2 <- bind_rows(r2_list, .id = 'iter') 
