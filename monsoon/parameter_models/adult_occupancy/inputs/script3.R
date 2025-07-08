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
                  'tidyselect',
                  'mcmcplots','ggplot2',
                  'tibble', 'purrr') 


## Installing them if they aren't already on the computer
new.packages <- package.list[!(package.list %in% 
                                 installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## And loading them
for(i in package.list){library(i, character.only = T)}

# Load Data ---------------------------------------------------------------

#load the initial model
mod <- readRDS("/scratch/atm234/whwo_ipm/parameter_models/adult_occupancy/outputs/adult_occupancy_2_JAGS.RDS")

# Get initials from previous model ----------------------------------------

#get the MCMC chains
samples <- mod$samples

#function to make each chain a dataframe
df_fun <- function(chain){
  df <- as.data.frame(chain) %>%
    rownames_to_column(var = "iteration")
  return(df)
}

#use that function on all list elements
samp_dfs <- lapply(samples, df_fun)

#make into one dataframe
samp_df <- bind_rows(samp_dfs, .id = "chain")

#get values for all parameters from the last iteration of the
#chain with the lowest deviance
samp_df2 <- samp_df %>%
  group_by(chain) %>%
  #get mean deviance by chain
  mutate(mean_dev = mean(deviance, na.rm = T)) %>%
  ungroup() %>%
  #get only the chain with the minimum average deviance
  filter(mean_dev == min(mean_dev)) %>%
  #pull out the final iteration from that chain
  filter(iteration == max(iteration)) %>%
  dplyr::select(-chain, -iteration,
                -deviance, -mean_dev) 

#root nodes:
a0 <- as.vector(samp_df2$a0)
b0 <- as.vector(samp_df2$b0)
e0 <- as.vector(samp_df2$e0)
e <- as.vector(samp_df2$e)

a <- samp_df2 %>%
  dplyr::select('a[2]':'a[14]') %>%
  pivot_longer(cols = everything(),
               names_to = "parm",
               values_to= "val") %>%
  dplyr::select('val') %>%
  as_vector()

a <- c(NA, a)

b <- samp_df2 %>%
  dplyr::select('b[1]':'b[10]') %>%
  pivot_longer(cols = everything(),
               names_to = "parm",
               values_to= "val") %>%
  dplyr::select('val') %>%
  as_vector()

a1TrtID <- samp_df2 %>%
  dplyr::select(contains('a1TrtID')) %>%
  pivot_longer(cols = everything(),
               names_to = "parm",
               values_to = "val") %>%
  dplyr::select('val') %>%
  mutate(val = case_when(val == 0 ~ NA_real_,
                         TRUE ~ val)) %>%
  as_vector()

gamma <- samp_df2 %>%
  dplyr::select(contains('gamma')) %>%
  pivot_longer(cols = everything(),
               names_to = "parm",
               values_to= "val") %>%
  dplyr::select('val') %>%
  as_vector()

eps.observer <- samp_df2 %>%
  dplyr::select(contains('eps.observer')) %>%
  pivot_longer(cols = everything(),
               names_to = "parm",
               values_to= "val") %>%
  dplyr::select('val') %>%
  as_vector()

eps.observer <- c(eps.observer[1:81], NA)

eps.transect <- samp_df2 %>%
  dplyr::select(contains('eps.transect')) %>%
  pivot_longer(cols = everything(),
               names_to = "parm",
               values_to= "val") %>%
  dplyr::select('val') %>%
  as_vector()

eps.transect <- c(eps.transect[1:84], NA)

sig.observer <- as.vector(samp_df2$sig.observer)
sig.transect <- as.vector(samp_df2$sig.transect)

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


# INits -------------------------------------------------------------------

#inits <- readRDS("/scratch/atm234/whwo_ipm/parameter_models/adult_occupancy/inputs/adult_occupancy_inits.RDS")
inits <- list(list(a0 = a0, 
                   b0 = b0,
                   e0 = e0,
                   a = a,
                   a1TrtID = a1TrtID,
                   b = b,
                   e = e,
                   gamma = gamma,
                   eps.observer = eps.observer,
                   eps.transect = eps.transect,
                   sig.observer = sig.observer,
                   sig.transect = sig.observer,
                   z = data$z
),
list(a0 = a0 + 0.5, 
     b0 = b0 + 0.5,
     e0 = e0 + 0.5,
     a = a + 0.5,
     a1TrtID = a1TrtID + 0.5,
     b = b + 0.05,
     e = e + 0.005,
     gamma = gamma + 0.05,
     eps.observer = eps.observer + 0.05,
     eps.transect = eps.transect + 0.5,
     sig.observer = sig.observer + 0.05,
     sig.transect = sig.observer + 1,
     z = data$z),
list(a0 = a0 - 0.5, 
     b0 = b0 - 0.5,
     e0 = e0 - 0.5,
     a = a - 0.5,
     a1TrtID = a1TrtID - 0.5,
     b = b - 0.05,
     e = e - 0.005,
     gamma = gamma - 0.05,
     eps.observer = eps.observer - 0.05,
     eps.transect = eps.transect - 0.5,
     sig.observer = sig.observer + 0.08,
     sig.transect = sig.observer + 2,
     z = data$z
))


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

# JAGS model --------------------------------------------------------------

mod <- jagsUI::jags(data = data_list,
                    #inits = inits,
                    inits = NULL,
                    model.file = '/scratch/atm234/whwo_ipm/parameter_models/adult_occupancy/inputs/adult_dyn_occupancy_simplepsi.R',
                    parameters.to.save = params,
                    parallel = TRUE,
                    n.chains = 3,
                    n.iter = 4000,
                    DIC = TRUE)

#save as an R data object
saveRDS(mod, 
        file ="/scratch/atm234/whwo_ipm/parameter_models/adult_occupancy/outputs/adult_occupancy_3_JAGS.RDS")

Sys.time()


# MCMC Plots --------------------------------------------------------------

mcmcplot(mod$samples, 
         dir = '/scratch/atm234/whwo_ipm/parameter_models/adult_occupancy/outputs/mcmcplots/third')

# Get RHat per parameter ------------------------------------------------

Rhat <- mod$Rhat

saveRDS(Rhat, "/scratch/atm234/whwo_ipm/parameter_models/adult_occupancy/outputs/model_Rhat3.RDS")


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


