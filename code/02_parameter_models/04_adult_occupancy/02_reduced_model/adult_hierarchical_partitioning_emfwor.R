#Hierarchical partitioning for the adult model
#December 14, 2023

# Load packages -----------------------------------------------------------

package.list <- c("here", "tidyverse")

## Installing them if they aren't already on the computer
new.packages <- package.list[!(package.list %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## And loading them
for(i in package.list){library(i, character.only = T)}
# 
# source(here('code',
#             '00_functions',
#             'hp_functions.R'))
# 

# Load results from Monsoon -----------------------------------------------

adults_THC <- readRDS(here('monsoon',
                            'parameter_models',
                            'adult_occupancy',
                            'hierarchical_partitioning',
                            'outputs',
                            'adult_full_results_emfwor.R')) %>%
  mutate(model = case_when(model == "HC" ~ "THC"))

adults_null <- readRDS(here('monsoon',
                         'parameter_models',
                         'adult_occupancy',
                         'hierarchical_partitioning',
                         'outputs',
                         'adult_null_results_emfwor.R'))

adults_H <- readRDS(here('monsoon',
                  'parameter_models',
                  'adult_occupancy',
                  'hierarchical_partitioning',
                  'outputs',
                  'adult_H_results_emfwor.R'))

adults_T <- readRDS(here('monsoon',
                         'parameter_models',
                         'adult_occupancy',
                         'hierarchical_partitioning',
                         'outputs',
                         'adult_T_results_emfwor.R'))

adults_C <- readRDS(here('monsoon',
                         'parameter_models',
                         'adult_occupancy',
                         'hierarchical_partitioning',
                         'outputs',
                         'adult_C_results_emfwor.R'))

adults_TH <- readRDS(here('monsoon',
                         'parameter_models',
                         'adult_occupancy',
                         'hierarchical_partitioning',
                         'outputs',
                         'adult_TH_results_emfwor.R'))

adults_TC <- readRDS(here('monsoon',
                          'parameter_models',
                          'adult_occupancy',
                          'hierarchical_partitioning',
                          'outputs',
                          'adult_TC_results_emfwor.R'))

adults_HC <- readRDS(here('monsoon',
                          'parameter_models',
                          'adult_occupancy',
                          'hierarchical_partitioning',
                          'outputs',
                          'adult_HC_results_emfwor.R'))



# Calculate partitioning --------------------------------------------------

LLs <- bind_rows(adults_null,
                 adults_T, 
                 adults_H, adults_C,
                 adults_TH, adults_TC,
                 adults_HC, adults_THC)

# Compute partitioning ----------------------------------------------------

#I copied and adapted the following code from this: 
#https://github.com/zipkinlab/Zylstra_etal_2021_NEE/blob/master/FullAnnualCycleModel_2004-2018/HierarchicalPartitioning_Summer_2004-2018.R

outlist <- list(adults_null = adults_null,
                adults_T = adults_T,
                adults_H = adults_H,
                adults_C = adults_C,
                adults_TH = adults_TH,
                adults_TC = adults_TC,
                adults_HC = adults_HC,
                adults_THC = adults_THC)

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


# Save --------------------------------------------------------------------

saveRDS(hp, here('data',
                 '03_hierarchical_partitioning',
                 'emfwor',
                 'adult',
                 'adult_hp_results_emfwor.RDS'))

# Visualize ---------------------------------------------------------------


ggplot(hp, aes(x = param, y = IC.perc)) +
  geom_bar(stat = "identity") +
  scale_x_discrete(labels = c("Climate", "Habitat", "Treatment")) +
  labs(x = "Covariate group", y = "Relative importance (%)") +
  theme_bw()




