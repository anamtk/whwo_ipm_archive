# Hierarchical partitioning code for egg survyval model
# Ana Miller-ter Kuile
# December 11, 2023

# this script runs each model covariate set on the egg survival
#model and pulls out the LL for each model


# Load packages -----------------------------------------------------------

package.list <- c("here", "tidyverse", 
                  "jagsUI",
                  'rjags',
                  'mcmcplots',
                  "coda")

## Installing them if they aren't already on the computer
new.packages <- package.list[!(package.list %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## And loading them
for(i in package.list){library(i, character.only = T)}

#functions
source(here('code',
            '00_functions',
            'hp_functions.R'))

# Load data ---------------------------------------------------------------

data <- readRDS(here('data',
                     '01_parameter_model_inputs',
                     '03_nestling_survival',
                     'jags_input_data',
                     'nestling_s_data_list_emfwor.RDS'))



# Global values -----------------------------------------------------------

#parameters to track for convergence and also Log-Likelihood
params <- c("n0",
            'n1TreatmentID',
            'n2SpeciesID',
            'n',
            'LL')

params2 <- c("n0",
            'n1TreatmentID',
            'n2SpeciesID',
            'n')
# Run models --------------------------------------------------------------

####
#FULL MODEL
####

full_mod <- here("code", 
                 "02_parameter_models", 
                 "03_nestling_survival",
                 "jags", 
                 'hierarchical_partitioning',
                 "nstls_THC.R")

nstls_THC <- run_jags_mod(model = full_mod, name = "full")

nstls_THC_samps <- update_jags_mod(model = full_mod)

saveRDS(nstls_THC_samps, here('data',
                             '03_hierarchical_partitioning',
                             'emfwor',
                             'nestling_survival',
                             'nstls_THC_samps_emfwor.RDS'))

####
#NULL MODEL
####

null_mod <- here("code", 
                 "02_parameter_models", 
                 "03_nestling_survival",
                 "jags", 
                 'hierarchical_partitioning',
                 "nstls_null.R")

nstls_null <- run_jags_mod(model = null_mod, name = "null")

nstls_null_samps <- update_jags_mod(model = null_mod)

saveRDS(nstls_null_samps, here('data',
                              '03_hierarchical_partitioning',
                              'emfwor',
                              'nestling_survival',
                              'nstls_null_samps_emfwor.RDS'))
####
#Treatment + Habitat model
####

th_mod <- here("code", 
               "02_parameter_models", 
               "03_nestling_survival",
               "jags", 
               'hierarchical_partitioning',
               "nstls_TH.R")

nstls_TH <- run_jags_mod(model = th_mod, name = "th")

nstls_TH_samps <- update_jags_mod(model = th_mod)

saveRDS(nstls_TH_samps, here('data',
                              '03_hierarchical_partitioning',
                             'emfwor',
                              'nestling_survival',
                              'nstls_TH_samps_emfwor.RDS'))

####
#Treatment + Climate model
####

tc_mod <-  here("code", 
                "02_parameter_models", 
                "03_nestling_survival",
                "jags", 
                'hierarchical_partitioning',
                "nstls_TC.R")

nstls_TC <- run_jags_mod(model = tc_mod, name = "tc")

nstls_TC_samps <- update_jags_mod(model = tc_mod)

saveRDS(nstls_TC_samps, here('data',
                              '03_hierarchical_partitioning',
                             'emfwor',
                              'nestling_survival',
                              'nstls_TC_samps_emfwor.RDS'))

####
#Habitat + Climate Model
####

hc_mod <- here("code", 
               "02_parameter_models", 
               "03_nestling_survival",
               "jags", 
               'hierarchical_partitioning',
               "nstls_HC.R")

nstls_HC <- run_jags_mod(model = hc_mod, name = "hc")

nstls_HC_samps <- update_jags_mod(model = hc_mod)

saveRDS(nstls_HC_samps, here('data',
                              '03_hierarchical_partitioning',
                             'emfwor',
                              'nestling_survival',
                              'nstls_HC_samps_emfwor.RDS'))

####
#Treatment model
####

t_mod <- here("code", 
              "02_parameter_models", 
              "03_nestling_survival",
              "jags", 
              'hierarchical_partitioning',
              "nstls_T.R")

nstls_T <- run_jags_mod(model = t_mod, name = "t")

nstls_T_samps <- update_jags_mod(model = t_mod)

saveRDS(nstls_T_samps, here('data',
                              '03_hierarchical_partitioning',
                            'emfwor',
                              'nestling_survival',
                              'nstls_T_samps_emfwor.RDS'))

####
#Habitat model
####

h_mod <- here("code", 
              "02_parameter_models", 
              "03_nestling_survival",
              "jags", 
              'hierarchical_partitioning',
              "nstls_H.R")

nstls_H <- run_jags_mod(model = h_mod, name = "h")

nstls_H_samps <- update_jags_mod(model = h_mod)

saveRDS(nstls_H_samps, here('data',
                              '03_hierarchical_partitioning',
                            'emfwor',
                              'nestling_survival',
                              'nstls_H_samps_emfwor.RDS'))

#### 
#Climate model
####

c_mod <- here("code", 
              "02_parameter_models", 
              "03_nestling_survival",
              "jags", 
              'hierarchical_partitioning',
              "nstls_C.R")

nstls_C <- run_jags_mod(model = c_mod, name = "c")

nstls_C_samps <- update_jags_mod(model = c_mod)

saveRDS(nstls_C_samps, here('data',
                              '03_hierarchical_partitioning',
                            'emfwor',
                              'nestling_survival',
                              'nstls_C_samps_emfwor.RDS'))

# Pull together LLs -------------------------------------------------------
 
LLs <- bind_rows(nstls_null,
                 nstls_T, 
                 nstls_H, nstls_C,
                 nstls_TH, nstls_TC,
                 nstls_HC,nstls_THC)


# Compute partitioning ----------------------------------------------------

#I copied and adapted the following code from this: 
#https://github.com/zipkinlab/Zylstra_etal_2021_NEE/blob/master/FullAnnualCycleModel_2004-2018/HierarchicalPartitioning_Summer_2004-2018.R


outlist <- list(nstls_null = nstls_null,
                nstls_T = nstls_T,
                nstls_H = nstls_H,
                nstls_C = nstls_C,
                nstls_TH = nstls_TH,
                nstls_TC = nstls_TC,
                nstls_HC = nstls_HC,
                nstls_THC = nstls_THC)

#create empty dataframe with column of "model" with names
#from the above list
df <- data.frame(model=names(outlist),stringsAsFactors=FALSE)
#create columns of 1-0 whether each variable group is in the model or not
df$T <- ifelse(grepl('T',df$model),1,0)
df$H <- ifelse(grepl('H',df$model),1,0)
df$C <- ifelse(grepl('C',df$model),1,0)
#get Log likelihoods from the dataframe we created of them:
df$logL <- LLs$LL
#what do they look like?
df[order(-df$logL),]

#Level of hierarchy (0-4 = 0-4 covariate groups, respectively)
df$hier <- apply(df[,c('T','H','C')],1,sum)

#Binary string that indicates which covariates in each model
df$modelc <- paste0(df$T,df$H,df$C)

#Create dataframe with all nested model pairs
allpairs <- expand.grid(model1=df$modelc,model0=df$modelc)
#remove where model 1 = model 2
allpairs <- allpairs[allpairs$model1!=allpairs$model0,]

#the hierarchy of each model in the pair to be compared:
#model 1:
allpairs$hier1 <- df$hier[match(allpairs$model1,df$modelc)]
#model 0:
allpairs$hier0 <- df$hier[match(allpairs$model0,df$modelc)]
#keep all pairs that are one hierarchy way from each other, such
#that the first model is one hiearchical level above the second
#model:
allpairs <- allpairs[allpairs$hier1-allpairs$hier0==1,]

##make binary columns for whether model 1 includes each of the
#three variable pairs
allpairs$T.1 <- as.numeric(substr(allpairs$model1,1,1))
allpairs$H.1 <- as.numeric(substr(allpairs$model1,2,2))
allpairs$C.1 <- as.numeric(substr(allpairs$model1,3,3))

#make binary columns for whether model 0 includes each of the three
#variable paris
allpairs$T.0 <- as.numeric(substr(allpairs$model0,1,1))
allpairs$H.0 <- as.numeric(substr(allpairs$model0,2,2))
allpairs$C.0 <- as.numeric(substr(allpairs$model0,3,3))

#wehether both models or neighter, or only one includes each
#covariate set
allpairs$T <- allpairs$T.1 + allpairs$T.0
allpairs$H <- allpairs$H.1 + allpairs$H.0
allpairs$C <- allpairs$C.1 + allpairs$C.0

#number of covariate sets in both models
allpairs$nBoth <- apply(allpairs[,c('T','H','C')],1,function(x) sum(x==2))

#keep only model pairs where the hierarchy matches how many
#covariate sets are shared among models
allpairs <- allpairs[allpairs$hier0==allpairs$nBoth,]

#Calculate difference in logL for each pair
#get the log liklihoods for the model 1:
allpairs$logL1 <- df$logL[match(allpairs$model1,df$modelc)]
#get the log likelihoods for the model 2: 
allpairs$logL0 <- df$logL[match(allpairs$model0,df$modelc)]
#calcuate the differences between them
allpairs$diff <- abs(allpairs$logL1 - allpairs$logL0)
#allpairs$diff <- allpairs$logL1 - allpairs$logL0
#Identify covariate group that's different in each pair
allpairs$param.T <- ifelse(allpairs$T.1==1 & allpairs$T.0==0,1,0)
allpairs$param.H <- ifelse(allpairs$H.1==1 & allpairs$H.0==0,1,0)
allpairs$param.C <- ifelse(allpairs$C.1==1 & allpairs$C.0==0,1,0) 

#Average logL differences for each covariate group and level of hierarchy
hpl <- data.frame(expand.grid(param=c('T','H','C'),hier=unique(allpairs$hier1),stringsAsFactors=FALSE))
for(i in 1:nrow(hpl)){
  hpl$avg.level[i] <- mean(allpairs$diff[allpairs$hier1==hpl$hier[i] & allpairs[,paste0('param.',hpl$param[i])]==1]) 
}

#Mean of those averages for each covariate group  
hp <- data.frame(param=unique(hpl$param))
for(i in 1:nrow(hp)){
  hp$IC[i] <- mean(hpl$avg.level[hpl$param==hp$param[i]])
}

#Relativize values (divide by total)
hp$IC.perc <- round(hp$IC/sum(hp$IC)*100,1)
hp

saveRDS(hp, here('data',
                 '03_hierarchical_partitioning',
                 'emfwor',
                 'nestling_survival',
                 'nestling_hp_results_emfwor.RDS'))

#The percents are spitting a negative bc full is worse
#than one without treatment. I set diff to be the 
#absolute value of difference and this seems to have worked
# Visualize ---------------------------------------------------------------


ggplot(hp, aes(x = param, y = IC.perc)) +
  geom_bar(stat = "identity") +
  scale_x_discrete(labels = c("Climate", "Nest Habitat", "Treatment")) +
  labs(x = "Covariate group", y = "Relative importance (%)") +
  theme_bw()




