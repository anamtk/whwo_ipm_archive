# Data prep for egg survival JAGS model
# April 19, 2023

# this script preps data for the JAGS submodel for egg survival


# Load packages -----------------------------------------------------------

package.list <- c("here", "tidyverse", 
                  "jagsUI",
                  'rjags',
                  'mcmcplots',
                  "coda")

## Installing them if they aren't already on the computer
new.packages <- package.list[!(package.list %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## And loading them
for(i in package.list){library(i, character.only = T)}

source(here("code", 
            '00_functions',
            'rhat_functions.R'))

# Load data ---------------------------------------------------------------

eggsurv <- read.csv(here("data",
                      '01_parameter_model_inputs',
                      "02_egg_survival",
                      "Egg_Nestling_survival_data.csv"))


# Do by forest covariate effects ------------------------------------------


# EMFWOR ------------------------------------------------------------------


eggsurv_emfwor <- eggsurv %>%
  filter(!is.na(No_young)) %>%
  filter(!is.na(No_eggs))  %>%
  filter(Type == "egg") %>%
  filter(No_eggs >= No_young) %>%
  filter(Project_ID == 'EMFWOR')

# Prep data for jags ------------------------------------------------------

n.nests <- nrow(eggsurv_emfwor)
y.nestling <- as.vector(eggsurv_emfwor$No_young)
Neggs <- as.vector(eggsurv_emfwor$No_eggs)

#Fremont-winema = 1
#Malheur = 2
#Payette =3 
Forest.ID <- eggsurv_emfwor %>%
  dplyr::select(Project_ID) %>%
  mutate(Project_ID = as.numeric(as.factor(Project_ID))) %>%
  as_vector()

n.forests <- length(unique(Forest.ID))

TreatmentID <- eggsurv_emfwor %>%
  mutate(Trt_cat = factor(Trt_cat, levels = c("U", "H", "B", "HB"))) %>%
  dplyr::select(Trt_cat) %>%
  mutate(Trt_cat = as.numeric(as.factor(Trt_cat))) %>%
  as_vector() 
  
n.trt <- length(unique(TreatmentID))

ForestCV <- as.vector(scale(eggsurv_emfwor$a1000_areacv2))
PPT <- as.vector(scale(eggsurv_emfwor$PPT_eg))
Tmax <- as.vector(scale(eggsurv_emfwor$Tmax_eg))

Transect.ID <- eggsurv_emfwor %>%
  mutate(Transect_ID2 = as.numeric(as.factor(Transect_ID2))) %>%
  dplyr::select(Transect_ID2) %>%
  as_vector()

n.transects <- length(unique(Transect.ID))

data_list <- list(n.nests = n.nests,
                  y.nestling = y.nestling,
                  Neggs = Neggs,
                  Forest.ID = Forest.ID,
                  n.forests = n.forests,
                  TreatmentID = TreatmentID,
                  n.trt = n.trt,
                  ForestCV = ForestCV,
                  PPT = PPT,
                  Tmax = Tmax,
                  Transect.ID = Transect.ID,
                  n.transects = n.transects)

saveRDS(data_list, here('data',
                        '01_parameter_model_inputs',
                        '02_egg_survival',
                        'jags_input_data',
                        'egg_s_data_list_emfwor.RDS'))


# Run model ---------------------------------------------------------------

params <- c("e0",
            'e1TreatmentID',
            'e')

model <- here("code", 
              "02_parameter_models", 
              "02_egg_survival",
              "jags", 
              "egg_survival_byforest.R")

eggs_mod <- jagsUI::jags(data = data_list,
                        inits = NULL,
                        model.file = model,
                        parameters.to.save = params,
                        parallel = TRUE,
                        n.chains = 3,
                        n.iter = 4000,
                        DIC = TRUE)

# Check convergence -------------------------------------------------------

mcmcplot(eggs_mod$samples)

rhat <- eggs_mod$Rhat

rhat_graph_fun2(rhat)

ggsave(here('pictures',
            'R',
            'SI',
            'Figure_eggsurvival_convergence_emfwor.png'),
       height = 3.5,
       width = 6,
       units = "in")

# Output summary ----------------------------------------------------------

sum <- summary(eggs_mod$samples)

saveRDS(sum, here("data",
                     "02_parameter_model_outputs",
                     "02_egg_survival",
                     "egg_survival_parameter_summaries_emfwor.RDS"))

# Get the right num of samples --------------------------------------------

eggs_mod2 <- update(object = eggs_mod,
                    n.chains = 3,
                    n.iter = 350)

# Export parameter lists --------------------------------------------------

e0 <- eggs_mod2$sims.list$e0
e1TreatmentID <- eggs_mod2$sims.list$e1TreatmentID
e <- eggs_mod2$sims.list$e

output <- list(e0 = e0,
               e1TreatmentID = e1TreatmentID,
               e = e)

saveRDS(output, here("data",
                     "02_parameter_model_outputs",
                     "02_egg_survival",
                     "egg_survival_parameters_emfwor.RDS"))


# EMMAOR ------------------------------------------------------------------

eggsurv_emmaor <- eggsurv %>%
  filter(!is.na(No_young)) %>%
  filter(!is.na(No_eggs))  %>%
  filter(Type == "egg") %>%
  filter(No_eggs >= No_young) %>%
  filter(Project_ID == 'EMMAOR')

# Prep data for jags ------------------------------------------------------

n.nests <- nrow(eggsurv_emmaor)
y.nestling <- as.vector(eggsurv_emmaor$No_young)
Neggs <- as.vector(eggsurv_emmaor$No_eggs)

#Fremont-winema = 1
#Malheur = 2
#Payette =3 
Forest.ID <- eggsurv_emmaor %>%
  dplyr::select(Project_ID) %>%
  mutate(Project_ID = as.numeric(as.factor(Project_ID))) %>%
  as_vector()

n.forests <- length(unique(Forest.ID))

TreatmentID <- eggsurv_emmaor %>%
  mutate(Trt_cat = factor(Trt_cat, levels = c("U", "H", "B", "HB"))) %>%
  dplyr::select(Trt_cat) %>%
  mutate(Trt_cat = as.numeric(as.factor(Trt_cat))) %>%
  as_vector() 

n.trt <- length(unique(TreatmentID))

ForestCV <- as.vector(scale(eggsurv_emmaor$a1000_areacv2))
PPT <- as.vector(scale(eggsurv_emmaor$PPT_eg))
Tmax <- as.vector(scale(eggsurv_emmaor$Tmax_eg))

Transect.ID <- eggsurv_emmaor %>%
  mutate(Transect_ID2 = as.numeric(as.factor(Transect_ID2))) %>%
  dplyr::select(Transect_ID2) %>%
  as_vector()

n.transects <- length(unique(Transect.ID))

data_list <- list(n.nests = n.nests,
                  y.nestling = y.nestling,
                  Neggs = Neggs,
                  Forest.ID = Forest.ID,
                  n.forests = n.forests,
                  TreatmentID = TreatmentID,
                  n.trt = n.trt,
                  ForestCV = ForestCV,
                  PPT = PPT,
                  Tmax = Tmax,
                  Transect.ID = Transect.ID,
                  n.transects = n.transects)

saveRDS(data_list, here('data',
                        '01_parameter_model_inputs',
                        '02_egg_survival',
                        'jags_input_data',
                        'egg_s_data_list_emmaor.RDS'))


# Run model ---------------------------------------------------------------

params <- c("e0",
            'e1TreatmentID',
            'e')

model <- here("code", 
              "02_parameter_models", 
              "02_egg_survival",
              "jags", 
              "egg_survival_byforest.R")

eggs_mod <- jagsUI::jags(data = data_list,
                         inits = NULL,
                         model.file = model,
                         parameters.to.save = params,
                         parallel = TRUE,
                         n.chains = 3,
                         n.iter = 4000,
                         DIC = TRUE)

# Check convergence -------------------------------------------------------

mcmcplot(eggs_mod$samples)

rhat <- eggs_mod$Rhat

rhat_graph_fun2(rhat)

ggsave(here('pictures',
            'R',
            'SI',
            'Figure_eggsurvival_convergence_emmaor.png'),
       height = 3.5,
       width = 6,
       units = "in")

# Output summary ----------------------------------------------------------

sum <- summary(eggs_mod$samples)

saveRDS(sum, here("data",
                  "02_parameter_model_outputs",
                  "02_egg_survival",
                  "egg_survival_parameter_summaries_emmaor.RDS"))

# Get the right num of samples --------------------------------------------

eggs_mod2 <- update(object = eggs_mod,
                    n.chains = 3,
                    n.iter = 350)

# Export parameter lists --------------------------------------------------

e0 <- eggs_mod2$sims.list$e0
e1TreatmentID <- eggs_mod2$sims.list$e1TreatmentID
e <- eggs_mod2$sims.list$e

output <- list(e0 = e0,
               e1TreatmentID = e1TreatmentID,
               e = e)

saveRDS(output, here("data",
                     "02_parameter_model_outputs",
                     "02_egg_survival",
                     "egg_survival_parameters_emmaor.RDS"))



# EMPAID ------------------------------------------------------------------


eggsurv_empaid <- eggsurv %>%
  filter(!is.na(No_young)) %>%
  filter(!is.na(No_eggs))  %>%
  filter(Type == "egg") %>%
  filter(No_eggs >= No_young) %>%
  filter(Project_ID == 'EMPAID')

# Prep data for jags ------------------------------------------------------

n.nests <- nrow(eggsurv_empaid)
y.nestling <- as.vector(eggsurv_empaid$No_young)
Neggs <- as.vector(eggsurv_empaid$No_eggs)

#Fremont-winema = 1
#Malheur = 2
#Payette =3 
Forest.ID <- eggsurv_empaid %>%
  dplyr::select(Project_ID) %>%
  mutate(Project_ID = as.numeric(as.factor(Project_ID))) %>%
  as_vector()

n.forests <- length(unique(Forest.ID))

TreatmentID <- eggsurv_empaid %>%
  mutate(Trt_cat = factor(Trt_cat, levels = c("U", "H", "B", "HB"))) %>%
  dplyr::select(Trt_cat) %>%
  mutate(Trt_cat = as.numeric(as.factor(Trt_cat))) %>%
  as_vector() 

n.trt <- length(unique(TreatmentID))

ForestCV <- as.vector(scale(eggsurv_empaid$a1000_areacv2))
PPT <- as.vector(scale(eggsurv_empaid$PPT_eg))
Tmax <- as.vector(scale(eggsurv_empaid$Tmax_eg))

Transect.ID <- eggsurv_empaid %>%
  mutate(Transect_ID2 = as.numeric(as.factor(Transect_ID2))) %>%
  dplyr::select(Transect_ID2) %>%
  as_vector()

n.transects <- length(unique(Transect.ID))

data_list <- list(n.nests = n.nests,
                  y.nestling = y.nestling,
                  Neggs = Neggs,
                  Forest.ID = Forest.ID,
                  n.forests = n.forests,
                  TreatmentID = TreatmentID,
                  n.trt = n.trt,
                  ForestCV = ForestCV,
                  PPT = PPT,
                  Tmax = Tmax,
                  Transect.ID = Transect.ID,
                  n.transects = n.transects)

saveRDS(data_list, here('data',
                        '01_parameter_model_inputs',
                        '02_egg_survival',
                        'jags_input_data',
                        'egg_s_data_list_empaid.RDS'))


# Run model ---------------------------------------------------------------

params <- c("e0",
            'e1TreatmentID',
            'e')

model <- here("code", 
              "02_parameter_models", 
              "02_egg_survival",
              "jags", 
              "egg_survival_byforest.R")

eggs_mod <- jagsUI::jags(data = data_list,
                         inits = NULL,
                         model.file = model,
                         parameters.to.save = params,
                         parallel = TRUE,
                         n.chains = 3,
                         n.iter = 4000,
                         DIC = TRUE)

# Check convergence -------------------------------------------------------

mcmcplot(eggs_mod$samples)

rhat <- eggs_mod$Rhat

rhat_graph_fun2(rhat)

ggsave(here('pictures',
            'R',
            'SI',
            'Figure_eggsurvival_convergence_empaid.png'),
       height = 3.5,
       width = 6,
       units = "in")

# Output summary ----------------------------------------------------------

sum <- summary(eggs_mod$samples)

saveRDS(sum, here("data",
                  "02_parameter_model_outputs",
                  "02_egg_survival",
                  "egg_survival_parameter_summaries_empaid.RDS"))

# Get the right num of samples --------------------------------------------

eggs_mod2 <- update(object = eggs_mod,
                    n.chains = 3,
                    n.iter = 350)

# Export parameter lists --------------------------------------------------

e0 <- eggs_mod2$sims.list$e0
e1TreatmentID <- eggs_mod2$sims.list$e1TreatmentID
e <- eggs_mod2$sims.list$e

output <- list(e0 = e0,
               e1TreatmentID = e1TreatmentID,
               e = e)

saveRDS(output, here("data",
                     "02_parameter_model_outputs",
                     "02_egg_survival",
                     "egg_survival_parameters_empaid.RDS"))


# ALL DATA MODEL ----------------------------------------------------------

eggsurv <- eggsurv %>%
  filter(!is.na(No_young)) %>%
  filter(!is.na(No_eggs))  %>%
  filter(Type == "egg") %>%
  filter(No_eggs >= No_young) 

# Prep data for jags ------------------------------------------------------

n.nests <- nrow(eggsurv)
y.nestling <- as.vector(eggsurv$No_young)
Neggs <- as.vector(eggsurv$No_eggs)

#Fremont-winema = 1
#Malheur = 2
#Payette =3 
Forest.ID <- eggsurv %>%
  dplyr::select(Project_ID) %>%
  mutate(Project_ID = as.numeric(as.factor(Project_ID))) %>%
  as_vector()

n.forests <- length(unique(Forest.ID))

TreatmentID <- eggsurv %>%
  mutate(Trt_cat = factor(Trt_cat, levels = c("U", "H", "B", "HB"))) %>%
  dplyr::select(Trt_cat) %>%
  mutate(Trt_cat = as.numeric(as.factor(Trt_cat))) %>%
  as_vector() 

n.trt <- length(unique(TreatmentID))

ForestCV <- as.vector(scale(eggsurv$a1000_areacv2))
PPT <- as.vector(scale(eggsurv$PPT_eg))
Tmax <- as.vector(scale(eggsurv$Tmax_eg))

Transect.ID <- eggsurv %>%
  mutate(Transect_ID2 = as.numeric(as.factor(Transect_ID2))) %>%
  dplyr::select(Transect_ID2) %>%
  as_vector()

n.transects <- length(unique(Transect.ID))

data_list <- list(n.nests = n.nests,
                  y.nestling = y.nestling,
                  Neggs = Neggs,
                  Forest.ID = Forest.ID,
                  n.forests = n.forests,
                  TreatmentID = TreatmentID,
                  n.trt = n.trt,
                  ForestCV = ForestCV,
                  PPT = PPT,
                  Tmax = Tmax,
                  Transect.ID = Transect.ID,
                  n.transects = n.transects)

saveRDS(data_list, here('data',
                        '01_parameter_model_inputs',
                        '02_egg_survival',
                        'jags_input_data',
                        'egg_s_data_list.RDS'))


# Run model ---------------------------------------------------------------

params <- c("e0",
            'e1TreatmentID',
            'e')

model <- here("code", 
              "02_parameter_models", 
              "02_egg_survival",
              "jags", 
              "egg_survival_byforest.R")

eggs_mod <- jagsUI::jags(data = data_list,
                         inits = NULL,
                         model.file = model,
                         parameters.to.save = params,
                         parallel = TRUE,
                         n.chains = 3,
                         n.iter = 4000,
                         DIC = TRUE)

# Check convergence -------------------------------------------------------

mcmcplot(eggs_mod$samples)

rhat <- eggs_mod$Rhat

rhat_graph_fun2(rhat)

ggsave(here('pictures',
            'R',
            'SI',
            'Figure_eggsurvival_convergence.png'),
       height = 3.5,
       width = 6,
       units = "in")

# Output summary ----------------------------------------------------------

sum <- summary(eggs_mod$samples)

saveRDS(sum, here("data",
                  "02_parameter_model_outputs",
                  "02_egg_survival",
                  "egg_survival_parameter_summaries.RDS"))

