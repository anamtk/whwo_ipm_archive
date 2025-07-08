#Test the full IPM
#Ana Miller-ter Kuile
#June 14, 2023

# Load packages -----------------------------------------------------------

package.list <- c("here", "tidyverse",'data.table')

## Installing them if they aren't already on the computer
new.packages <- package.list[!(package.list %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## And loading them
for(i in package.list){library(i, character.only = T)}

source(here('code',
            '00_functions',
            'hp_functions.R'))
# Load data ---------------------------------------------------------------

data_list <- readRDS(here("data",
                          "04_ipm_data_inputs",
                          "IPM_data_list_EMPAID.RDS"))

eggs_list <- list.files(path = here('data',
                                    '03_hierarchical_partitioning',
                                    'empaid',
                                    'egg_survival'),
                        pattern = ".RDS",
                        full.names = T) %>%
  map(readRDS)

file_names <- list.files(path = here('data',
                                           '03_hierarchical_partitioning',
                                     'empaid',
                                           'egg_survival'),
                               pattern = ".RDS") 

file_names <- str_sub(file_names, 1, (nchar(file_names)-11))

names(eggs_list) <- file_names

nestlings_list<- list.files(path = here('data',
                                        '03_hierarchical_partitioning',
                                        'empaid',
                                        'nestling_survival'),
                            pattern = ".RDS",
                            full.names = T) %>%
  map(readRDS)

file_names <- list.files(path = here('data',
                                     '03_hierarchical_partitioning',
                                     'empaid',
                                     'nestling_survival'),
                         pattern = ".RDS") 

file_names <- str_sub(file_names, 1, (nchar(file_names)-11))

names(nestlings_list) <- file_names


adults_list<- list.files(path = here('monsoon',
                                     'parameter_models',
                                     'adult_occupancy',
                                     'hierarchical_partitioning',
                                     'outputs',
                                     'empaid_samples'),
                         pattern = ".RDS",
                         full.names = T) %>%
  map(readRDS)

file_names <- list.files(path = here('monsoon',
                                     'parameter_models',
                                     'adult_occupancy',
                                     'hierarchical_partitioning',
                                     'outputs',
                                     'empaid_samples'),
                         pattern = ".RDS") 

file_names <- str_sub(file_names, 1, (nchar(file_names)-11))

names(adults_list) <- file_names



# Get data objects --------------------------------------------------------

#Pull data out into objects that R will recognize

# All models --------------------------------------------------------------

#these are variables that don't change depending on model:
#covariate sampler data:
N_samples <- data_list$N
#EMFWOR is 1st forest in the intercept arrays
forest <- 1
#egg number:
egg.lambda.array <- data_list$egg.lambda.array
#sex ratio
propF.array <- data_list$propF.array
#for yearly population model components
n.years <- data_list$n.years
n.nests <- data_list$n.nests

#yearly nest values:
surveyed <- data_list$surveyed
nest.start <- data_list$nest.start
nest.end <- data_list$nest.end

#adult portions of model
#point-level values
n.points <- data_list$n.points

#point-level nest values
#number of treatments
n.trt <- data_list$n.trt
TreatmentID <- data_list$TreatmentID
ForestCV <- data_list$ForestCV
PPT_eg <- data_list$PPT_eg
Tmax_eg <- data_list$Tmax_eg
#impute missing data:
Tmax_eg[which(is.na(Tmax_eg))] <- rnorm(1, 
                                        mean = mean(Tmax_eg, na.rm = T),
                                        sd = sd(Tmax_eg, na.rm = T))
n.species <- data_list$n.species
SpeciesID <- data_list$SpeciesID
LandBu <- data_list$LandBu
InitDay <- data_list$InitDay
#impute missing data for init day
InitDay[which(is.na(InitDay))] <- rnorm(1, 
                                        mean = mean(InitDay, na.rm = T),
                                        sd = sd(InitDay, na.rm = T))
NestHt <- data_list$NestHt
PPT_ne <- data_list$PPT_ne
Tmax_ne <- data_list$Tmax_ne
#impute missing data:
Tmax_ne[which(is.na(Tmax_ne))] <- rnorm(1, 
                                        mean = mean(Tmax_ne, na.rm = T),
                                        sd = sd(Tmax_ne, na.rm = T))


aTmean_sp <- data_list$aTmean_sp
aTmean_wt <- data_list$aTmean_wt
aPPT_wt <- data_list$aPPT_wt
aLandHa <- data_list$aLandHa
aLowCC <- data_list$aLowCC

#number of iterations
n.iter <- 1000

# Full model --------------------------------------------------------------

#INDEXING CHECKED AND CORRECT
#these are arrays for just the full model:

#EGG survival:
e0.array <- eggs_list$eggs_THC_samps$e0
e1Trt.array <- eggs_list$eggs_THC_samps$e1TreatmentID
e.array <- eggs_list$eggs_THC_samps$e
#nestling survival:
n0.array <- nestlings_list$nstls_THC_samps$n0
n1Trt.array <- nestlings_list$nstls_THC_samps$n1TreatmentID
n2Spec.array<- nestlings_list$nstls_THC_samps$n2SpeciesID
n.array <- nestlings_list$nstls_THC_samps$n
#ADult portions
a0.array <- data_list$a0.array
a.array <- data_list$a.array

ipm_THC <- hp_ipm_fun(model_path = here('code',
                                     '03_ipm',
                                     '02_HP',
                                     'models',
                                     "ipm_full.R"),
                   name = 'full',
                   start = 2012,
                   end = 2019)

# Null model --------------------------------------------------------------

#INDEXING CHECKED AND CORRECT 

#EGG survival:
e0.array <- eggs_list$eggs_null_samps$e0

#nestling survival:
n0.array <- nestlings_list$nstls_null_samps$n0
#ADult
a0.array <- data_list$a0.array

ipm_null <- hp_ipm_fun(model_path = here('code',
                                        '03_ipm',
                                        '02_HP',
                                        'models',
                                        "ipm_null.R"),
                      name = 'null',
                      start = 2012,
                      end = 2019)


# TH ----------------------------------------------------------------------

#INDEXING CHECKED AND CORRECT
#EGG survival:
e0.array <- eggs_list$eggs_TH_samps$e0
e.array <- eggs_list$eggs_TH_samps$e
e1Trt.array <- eggs_list$eggs_TH_samps$e1TreatmentID

#nestling survival:
n0.array <- nestlings_list$nstls_TH_samps$n0
n1Trt.array <- nestlings_list$nstls_TH_samps$n1TreatmentID
n2Spec.array<- nestlings_list$nstls_TH_samps$n2SpeciesID
n.array <- nestlings_list$nstls_TH_samps$n
#ADult
a0.array <- data_list$a0.array
a.array <- data_list$a.array

ipm_TH <- hp_ipm_fun(model_path = here('code',
                                         '03_ipm',
                                         '02_HP',
                                         'models',
                                         "ipm_th.R"),
                       name = 'th',
                     start = 2012,
                     end = 2019)


# TC ----------------------------------------------------------------------

#INDEXING CHECKED AND CORRECT
#EGG survival:
e0.array <- eggs_list$eggs_TC_samps$e0
e.array <- eggs_list$eggs_TC_samps$e
e1Trt.array <- eggs_list$eggs_TC_samps$e1TreatmentID

#nestling survival:
n0.array <- nestlings_list$nstls_TC_samps$n0
n1Trt.array <- nestlings_list$nstls_TC_samps$n1TreatmentID
n.array <- nestlings_list$nstls_TC_samps$n

#ADult 
a0.array <- data_list$a0.array
a.array <- data_list$a.array

ipm_TC <- hp_ipm_fun(model_path = here('code',
                                       '03_ipm',
                                       '02_HP',
                                       'models',
                                       "ipm_tc.R"),
                     name = 'tc',
                     start = 2012,
                     end = 2019)



# HC ----------------------------------------------------------------------

#INDEXING CHECKED AND CORRECT
#EGG survival:
e0.array <- eggs_list$eggs_HC_samps$e0
e.array <- eggs_list$eggs_HC_samps$e
e1Trt.array <- eggs_list$eggs_HC_samps$e1TreatmentID

#nestling survival:
n0.array <- nestlings_list$nstls_HC_samps$n0
n1Trt.array <- nestlings_list$nstls_HC_samps$n1TreatmentID
n2Spec.array<- nestlings_list$nstls_HC_samps$n2SpeciesID
n.array <- nestlings_list$nstls_HC_samps$n

#ADult 
a0.array <- data_list$a0.array
a.array <- data_list$a.array


ipm_HC <- hp_ipm_fun(model_path = here('code',
                                       '03_ipm',
                                       '02_HP',
                                       'models',
                                       "ipm_hc.R"),
                     name = 'hc',
                     start = 2012,
                     end = 2019)


# T -----------------------------------------------------------------------

#INDEXING CHECKED AND CORRECT
#EGG survival:
e0.array <- eggs_list$eggs_T_samps$e0
e.array <- eggs_list$eggs_T_samps$e
e1Trt.array <- eggs_list$eggs_T_samps$e1TreatmentID

#nestling survival:
n0.array <- nestlings_list$nstls_T_samps$n0
n1Trt.array <- nestlings_list$nstls_T_samps$n1TreatmentID
n.array <- nestlings_list$nstls_T_samps$n

#ADult 
a0.array <- data_list$a0.array
a.array <- data_list$a.array


ipm_T <- hp_ipm_fun(model_path = here('code',
                                       '03_ipm',
                                       '02_HP',
                                       'models',
                                       "ipm_t.R"),
                     name = 't',
                    start = 2012,
                    end = 2019)


# H -----------------------------------------------------------------------

#INDEXING CHECKED AND CORRECT
#EGG survival:
e0.array <- eggs_list$eggs_H_samps$e0
e.array <- eggs_list$eggs_H_samps$e

#nestling survival:
n0.array <- nestlings_list$nstls_H_samps$n0
n2Spec.array<- nestlings_list$nstls_H_samps$n2SpeciesID
n.array <- nestlings_list$nstls_H_samps$n

#ADult 
a0.array <- data_list$a0.array
a.array <- data_list$a.array


ipm_H <- hp_ipm_fun(model_path = here('code',
                                      '03_ipm',
                                      '02_HP',
                                      'models',
                                      "ipm_h.R"),
                    name = 'h',
                    start = 2012,
                    end = 2019)

# C -----------------------------------------------------------------------

#INDEXING CHECKED AND CORRECT
#EGG survival:
e0.array <- eggs_list$eggs_C_samps$e0
e.array <- eggs_list$eggs_C_samps$e

#nestling survival:
n0.array <- nestlings_list$nstls_C_samps$n0
n.array <- nestlings_list$nstls_C_samps$n

#ADult 
a0.array <- data_list$a0.array
a.array <- data_list$a.array

ipm_C <- hp_ipm_fun(model_path = here('code',
                                      '03_ipm',
                                      '02_HP',
                                      'models',
                                      "ipm_c.R"),
                    name = 'c',
                    start = 2012,
                    end = 2019)


# Combine -----------------------------------------------------------------

LLs <- bind_rows(ipm_null,
                 ipm_T, 
                 ipm_H, ipm_C,
                 ipm_TH, ipm_TC,
                 ipm_HC,ipm_THC)


# Summarise across years --------------------------------------------------

#need one mean difference between model and full for each - look
#at full-model comparison for each year, then take the average


LL2 <- LLs %>%
  filter(!is.na(lambda)) %>%
  pivot_wider(names_from = model,
              values_from = lambda) %>%
  rowwise() %>%
  mutate(tc = abs(full - tc),
         th = abs(full - th),
         hc = abs(full - hc),
         c = abs(full - c),
         h = abs(full - h),
         t = abs(full - t),
         null = abs(full - null)) %>%
  ungroup() %>%
  dplyr::select(tc, th,
                hc, c,
                h, t, null) %>%
  pivot_longer(tc:null,
               names_to = "model",
               values_to = "lambda") %>%
  group_by(model) %>%
  summarise(lambda = mean(lambda)) %>%
  ungroup() 

LL3 <- LL2 %>%
  pivot_wider(names_from = 'model',
              values_from = 'lambda')

# Compute partitioning ----------------------------------------------------

#I copied and adapted the following code from this: 
#https://github.com/zipkinlab/Zylstra_etal_2021_NEE/blob/master/FullAnnualCycleModel_2004-2018/HierarchicalPartitioning_Summer_2004-2018.R

outlist <- list(c = LL3$c,
                h = LL3$h,
                hc = LL3$hc,
                null = LL3$null,
                t = LL3$t,
                tc = LL3$tc,
                th = LL3$th
)

#create empty dataframe with column of "model" with names
#from the above list
df <- data.frame(model=names(outlist),stringsAsFactors=FALSE)
#create columns of 1-0 whether each variable group is in the model or not
df$T <- ifelse(grepl('t',df$model),1,0)
df$H <- ifelse(grepl('h',df$model),1,0)
df$C <- ifelse(grepl('c',df$model),1,0)
#get Log likelihoods from the dataframe we created of them:
df$logL <- LL2$lambda
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

#The percents are spitting a negative bc full is worse
#than one without treatment. I set diff to be the 
#absolute value of difference and this seems to have worked


# Export ------------------------------------------------------------------

saveRDS(hp, here('data',
                 '03_hierarchical_partitioning',
                 'empaid',
                 'ipm',
                 'ipm_hp_results_EMPAID.RDS'))

# Visualize ---------------------------------------------------------------


# ggplot(hp, aes(x = param, y = IC.perc)) +
#   geom_bar(stat = "identity") +
#   scale_x_discrete(labels = c("Climate", "Habitat", "Treatment")) +
#   labs(x = "Covariate group", y = "Relative importance (%)") +
#   theme_bw()




