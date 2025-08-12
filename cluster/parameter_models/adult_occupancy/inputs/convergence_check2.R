
# Load packages ---------------------------------------------------------------

# Load packages, here and tidyverse for coding ease, 
package.list <- c("here", "dplyr",
                  "tidyr", "ggplot2", 
                  'mcmcplots', "stringr",
                  "coda", "htmltools") #mcmc output


## Installing them if they aren't already on the computer
new.packages <- package.list[!(package.list %in% 
                                 installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## And loading them
for(i in package.list){library(i, character.only = T)}


# Import model ------------------------------------------------------------

mod <- readRDS(file = "/scratch/user/whwo_ipm/parameter_models/adult_occupancy/outputs/adult_occupancy_JAGS2.RDS")


# Check convergence -------------------------------------------------------

mcmcplot(mod$samples,
         dir = '/scratch/user/whwo_ipm/parameter_models/adult_occupancy/outputs/mcmcplots/second/')

# Get RHat per parameter ------------------------------------------------

Rhat <- mod$Rhat

saveRDS(Rhat, '/scratch/user/whwo_ipm/parameter_models/adult_occupancy/outputs/model_Rhat2.RDS')
