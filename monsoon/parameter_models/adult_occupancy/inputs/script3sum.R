#Monsoon script -Adult N occupancy model
# Ana Miller-ter Kuile
# April 26, 2023

#this script runs the adult N-occupancy model to figure 
#out which covariates to include in the model

# Load packages ---------------------------------------------------------------
Sys.time()


# Load packages
package.list <- c("jagsUI", "coda") 


## Installing them if they aren't already on the computer
new.packages <- package.list[!(package.list %in% 
                                 installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## And loading them
for(i in package.list){library(i, character.only = T)}

# Load Data ---------------------------------------------------------------

#load the formatted data for the JAGS model
model <-  readRDS("/scratch/atm234/whwo_ipm/parameter_models/adult_occupancy/outputs/adult_occupancy_4_JAGS.RDS")

sum <- summary(model$samples)

saveRDS(sum, "/scratch/atm234/whwo_ipm/parameter_models/adult_occupancy/outputs/adult_occupancy_sum4.RDS")


# Parameters to save ------------------------------------------------------

params <- c("z.a1",
            "z.a")


# JAGS model --------------------------------------------------------------

model2 <- update(model,
                 parameters.to.save = params,
                 parallel = TRUE,
                 n.iter = 5000,
                 DIC = TRUE)

#save as an R data object
saveRDS(model2, 
        file ="/scratch/atm234/whwo_ipm/parameter_models/adult_occupancy/outputs/adult_occupancy_JAGS4z.RDS")


sum <- summary(model2$samples)

saveRDS(sum, "/scratch/atm234/whwo_ipm/parameter_models/adult_occupancy/outputs/adult_occupancy_zsum4.RDS")

Sys.time()
