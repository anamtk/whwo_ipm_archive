#Combine lists of covariates and posterior covariate effects
#June 14, 2023

#this script combines the covariatres from teh submodels
#with the arrays of posterior samples from each submodel

# Load packages -----------------------------------------------------------

package.list <- c("here", "tidyverse")

## Installing them if they aren't already on the computer
new.packages <- package.list[!(package.list %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## And loading them
for(i in package.list){library(i, character.only = T)}


# Load data ---------------------------------------------------------------

covs <- readRDS(here("data",
                   "04_ipm_data_inputs",
                   "covariate_data_list_EMMAOR.RDS"))

post <- readRDS(here("data",
                   "04_ipm_data_inputs",
                   "posterior_data_list_emmaor.RDS"))


# Combine lists -----------------------------------------------------------

data <- c(covs, post)

saveRDS(data, here("data",
                   "04_ipm_data_inputs",
                   "IPM_data_list_EMMAOR.RDS"))

