#Submodel parameter posterior arrays
#Ana Miller-ter Kuile
#June 14, 2023

#this script compiles all the statistical model posterior
#parameter arrays for input into the covariate effect
#sampler in the IPM

# Load packages -----------------------------------------------------------

package.list <- c("here", "tidyverse")

## Installing them if they aren't already on the computer
new.packages <- package.list[!(package.list %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## And loading them
for(i in package.list){library(i, character.only = T)}


# Nest portions -----------------------------------------------------------

#nest portions for pulling out posteriors

# Load data ---------------------------------------------------------------

egg_num <- readRDS(here("data",
                        '02_parameter_model_outputs',
                        "01_egg_num",
                        'egg_num_parameters.RDS'))

egg_s <- readRDS(here('data',
                      "02_parameter_model_outputs",
                      "02_egg_survival",
                      'egg_survival_parameters_emfwor.RDS'))

nest_s <- readRDS(here('data',
                       "02_parameter_model_outputs",
                       "03_nestling_survival",
                       'nestling_survival_parameters_emfwor.RDS'))

adult <- readRDS(here('monsoon',
                      'parameter_models',
                      'adult_occupancy',
                      'hierarchical_partitioning',
                      'outputs',
                      'adult_THC_parameter_samples_emfwor.RDS'))

sex_ratio <- readRDS(here("data",
                          "02_parameter_model_outputs",
                          "05_sex_ratios",
                          "sex_ratio_parameters.RDS"))

# Indexing ----------------------------------------------------------------

#number of iterations for all samplers - 
#for now it's the same, but could make different
#for some reason
N <- nrow(egg_num$egg.lambda)

# Pull out arrays ---------------------------------------------------------

#Egg number:
#each column is for a forest
egg.lambda.array <- egg_num$egg.lambda


#Egg survival:
#each column is for a forest
e0.array <- egg_s$e0
e1Trt.array <- egg_s$e1TreatmentID
#(rows = sample/iteration, column = covariate)
e.array <- egg_s$e


#Nestling survival:
#each column is for a forest
n0.array <- nest_s$n0
n1Trt.array <- nest_s$n1TreatmentID
n2Spec.array <- nest_s$n2SpeciesID
n.array <- nest_s$n
#(rows = sample/iteration, column = covariate)


# Adult portions ----------------------------------------------------------

#adult survival
#matrix where each column is a forest
a0.array <- adult$a0

a.array<- adult$a

#number of covariates
#n.ad.cov <-  ncol(a.array)

#female ratio
propF.array <- sex_ratio$propF

# Compile -----------------------------------------------------------------

data <- list(N = N,
             egg.lambda.array = egg.lambda.array,
             e0.array = e0.array,
             e1Trt.array = e1Trt.array,
             e.array = e.array,
             n0.array = n0.array,
             n1Trt.array = n1Trt.array,
             n2Spec.array = n2Spec.array,
             n.array = n.array,
             a0.array = a0.array,
             a.array = a.array,
             propF.array = propF.array)

saveRDS(data, here("data",
                   "04_ipm_data_inputs",
                   "posterior_data_list_emfwor.RDS"))
