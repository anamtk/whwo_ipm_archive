#Hierarchical partitioning for the adult model
#Ana Miller-ter Kuile
#December 14, 2023

# Load packages -----------------------------------------------------------

package.list <- c('dplyr', 'tidyr',
                  'purrr',"jagsUI",
                  'tibble', 'stringr',
                  'rjags',
                  'mcmcplots',
                  "coda")

## Installing them if they aren't already on the computer
new.packages <- package.list[!(package.list %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## And loading them
for(i in package.list){library(i, character.only = T)}

# Load data ---------------------------------------------------------------

data <- readRDS("/scratch/atm234/whwo_ipm/parameter_models/adult_occupancy/hp/inputs/reduced_adult_data_list_emfwor.RDS")


data_list <- list(n.points = data$n.points,
                  n.years = data$n.years,
                  n.start = data$n.start,
                  n.end = data$n.end,
                  #nYear = nYear,
                  n.rep = data$n.rep,
                  n.transects = data$n.transects,
                  n.forests = data$n.forests,
                  n.trt = data$n.trt,
                  Tmean_sp = data$Tmean_sp,
                  #PPT_sp = PPT_sp,
                  PPT_wt = data$PPT_wt,
                  #SHDI = SHDI,
                  LandHa = data$LandHa,
                  LowCC = data$LowCC, 
                  #MedCC = MedCC,
                  Tmean_wt = data$Tmean_wt,
                  #HighCC = HighCC,
                  ForestID = data$ForestID,
                  y.occ = data$y.occ,
                  z = data$z)


# Parameters to save ------------------------------------------------------


params <- c("a0", 
           "a",
           'll')


# Run all models ----------------------------------------------------------

Sys.time()

####
#TC MODEL
####

print("tc model")

tc_mod <- '/scratch/atm234/whwo_ipm/parameter_models/adult_occupancy/hp/inputs/adult_TC.R'


#run the model with the specific model defined
mod <- jagsUI::autojags(data = data,
                        inits = NULL,
                        model.file = tc_mod,
                        parameters.to.save = params,
                        parallel = TRUE,
                        iter.increment = 50000,
                        max.iter = 200000,
                        Rhat.limit = 1.2,
                        n.chains = 3,
                        DIC = TRUE)

#make sure it converges
rhat <- gelman.diag(mod$samples, multivariate = F)
rhat <- as.data.frame(rhat$psrf) %>%
  filter(!is.na(`Point est.`))

#pull out Log Likelihood
LL <- summary(mod$samples) 
LL2 <- as.data.frame(LL$statistics) %>%
  rownames_to_column(var = 'par') %>%
  filter(str_detect(par, 'll')) %>%
  summarise(LL = sum(Mean, na.rm = T)) %>%
  mutate(model = "TC") %>%
  dplyr::select(LL, model)


saveRDS(LL2,
        '/scratch/atm234/whwo_ipm/parameter_models/adult_occupancy/hp/outputs/adult_TC_results_emfwor.R')

print("tc model done")

Sys.time()

# Export parameter lists --------------------------------------------------
mod2 <- update(mod,
               n.iter = 350)


a0 <- mod2$sims.list$a0
a <- mod2$sims.list$a

output <- list(a0 = a0,
               a = a)

saveRDS(output, 
        "/scratch/atm234/whwo_ipm/parameter_models/adult_occupancy/hp/outputs/adult_TC_parameter_samples_emfwor.RDS")


