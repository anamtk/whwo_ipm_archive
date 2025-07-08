
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

mod <- readRDS(file = "/scratch/atm234/whwo_ipm/parameter_models/adult_occupancy/outputs/adult_occupancy_inits_JAGS.RDS")


# Check convergence -------------------------------------------------------

mcmcplot(mod$samples,
         dir = '/scratch/atm234/whwo_ipm/parameter_models/adult_occupancy/outputs/mcmcplots/initial')

# Get RHat per parameter ------------------------------------------------

Rhat <- mod$Rhat

saveRDS(Rhat, '/scratch/atm234/whwo_ipm/parameter_models/adult_occupancy/outputs/model_init_Rhat.RDS')
