#Monsoon script -Adult N occupancy model
# Ana Miller-ter Kuile
# April 26, 2023

#this script runs the adult occupancy model to figure 
#out which covariates to include in the model

# Load packages ---------------------------------------------------------------
Sys.time()


# Load packages
package.list <- c("jagsUI", "coda",
                  'dplyr', 'stringr',
                  'magrittr', 'tidyr',
                  'mcmcplots','ggplot2') 


## Installing them if they aren't already on the computer
new.packages <- package.list[!(package.list %in% 
                                 installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## And loading them
for(i in package.list){library(i, character.only = T)}

# Load Data ---------------------------------------------------------------

#load the formatted data for the JAGS model
data <- readRDS("/scratch/atm234/whwo_ipm/parameter_models/adult_occupancy/inputs/adult_occupancy_data.RDS")

# Compile data ------------------------------------------------------------
data_list <- list(n.points = data$n.points,
                  n.years = data$n.years,
                  n.start = data$n.start,
                  n.end = data$n.end,
                  #nYear = nYear,
                  n.rep = data$n.rep,
                  n.transects = data$n.transects,
                  n.forests = data$n.forests,
                  n.trt = data$n.trt,
                  n.observer = data$n.observer,
                  TreatmentID = data$TreatmentID,
                  Trees50 = data$Trees50,
                  Trees2550 = data$Trees2550,
                  PercPonderosa = data$PercPonderosa,
                  Tmean_sp = data$Tmean_sp,
                  Tmean_wt = data$Tmean_wt,
                  PPT_sp = data$PPT_sp,
                  PPT_wt = data$PPT_wt,
                  SHDI = data$SHDI,
                  LandBu = data$LandBu,
                  LandHa = data$LandHa,
                  LowCC = data$LowCC,
                  MedCC = data$MedCC,
                  HighCC = data$HighCC,
                  TransectID = data$TransectID, 
                  ForestID = data$ForestID,
                  effort = data$effort,
                  Observer = data$Observer,
                  y.occ = data$y.occ,
                  z = data$z)

# Parameters to save ------------------------------------------------------

params <- c("a0", 
            "a1TrtID",
            "a",
            'b0',
            'b',
            'gamma',
            'e0',
            'e',
            'eps.observer',
            'eps.transect',
            'sig.observer',
            'sig.transect')

# INits -------------------------------------------------------------------

#inits <- readRDS("/scratch/atm234/whwo_ipm/parameter_models/adult_occupancy/inputs/adult_occupancy_inits.RDS")

# JAGS model --------------------------------------------------------------

mod <- jagsUI::jags(data = data_list,
                        #inits = inits,
                        inits = NULL,
                        model.file = '/scratch/atm234/whwo_ipm/parameter_models/adult_occupancy/inputs/adult_dyn_occupancy_phionly.R',
                        parameters.to.save = params,
                        parallel = TRUE,
                        n.chains = 3,
                        n.iter = 4000,
                        DIC = TRUE)

#save as an R data object
saveRDS(mod, 
        file ="/scratch/atm234/whwo_ipm/parameter_models/adult_occupancy/outputs/adult_occupancy_inits_JAGS.RDS")

Sys.time()


# MCMC Plots --------------------------------------------------------------

mcmcplot(mod$samples, 
         dir = '/scratch/atm234/whwo_ipm/parameter_models/adult_occupancy/outputs/mcmcplots/initial')

# Get RHat per parameter ------------------------------------------------

Rhat <- mod$Rhat

saveRDS(Rhat, "/scratch/atm234/whwo_ipm/parameter_models/adult_occupancy/outputs/model_Rhat.RDS")


# Raftery -----------------------------------------------------------------


raf <- raftery.diag(mod$samples)

names <- rownames(raf[[1]]$resmatrix)
ch1 <- raf[[1]]$resmatrix[,2]
ch2 <- raf[[2]]$resmatrix[,2]
ch3 <- raf[[3]]$resmatrix[,2]

raf_all <- as.data.frame(cbind(names, 
                               ch1, ch2, ch3)) %>%
  mutate(ch1 = as.numeric(ch1),
         ch2 = as.numeric(ch2),
         ch3 = as.numeric(ch3)) %>%
  pivot_longer(ch1:ch3,
               names_to = "chain",
               values_to = 'iterations') 

raf_all %>%
  summarise(iterations_90 = quantile(iterations, 
                                     probs = 0.9, 
                                     na.rm = T)/3,
            iterations_95 = quantile(iterations,
                                     probs = 0.95,
                                     na.rm = T)/3,
            max = max(iterations, 
                      na.rm = T)/3)

bu1 <- raf[[1]]$resmatrix[,1]
bu2 <- raf[[2]]$resmatrix[,1]
bu3 <- raf[[3]]$resmatrix[,1]

burn <- as.data.frame(cbind(names, bu1, bu2, bu3)) %>%
  mutate(bu1 = as.numeric(bu1),
         bu2 = as.numeric(bu2),
         bu3 = as.numeric(bu3)) %>%
  filter(!str_detect(names, "z")) %>%
  pivot_longer(bu1:bu3,
               names_to = "chain",
               values_to = 'iterations') 

burn %>%
  summarise(max(iterations, na.rm = T))


