# Data prep for nestling survival JAGS model
# April 19, 2023

# this script preps data for the JAGS submodel for nestling survival


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

nstsurv <- read.csv(here("data",
                         '01_parameter_model_inputs',
                         "02_egg_survival",
                         "Egg_Nestling_survival_data.csv"))

# Filter for nests with >0 nestlings --------------------------------------

nstsurv <- nstsurv %>%
  filter(No_young >= NoFL_uncert) %>%
  filter(No_young > 0)


# Per forest --------------------------------------------------------------


# EMFWOR ------------------------------------------------------------------


nstsurv_emfwor <- nstsurv %>%
  filter(Project_ID == "EMFWOR")

# Prep data ---------------------------------------------------------------

n.nests <- nrow(nstsurv_emfwor)

y.fledgling <- as.vector(nstsurv_emfwor$NoFL_uncert)
Nnestlings <- as.vector(nstsurv_emfwor$No_young)

Forest.ID <- nstsurv_emfwor %>%
  mutate(Project_ID = as.numeric(as.factor(Project_ID))) %>%
  dplyr::select(Project_ID) %>%
  as_vector()

n.forests <- length(unique(Forest.ID))

Transect.ID <- nstsurv_emfwor %>%
  mutate(Transect_ID2 = as.numeric(as.factor(Transect_ID2))) %>%
  dplyr::select(Transect_ID2) %>%
  as_vector()

n.transects <- length(unique(Transect.ID))

TreatmentID <- nstsurv_emfwor %>%
  mutate(Trt_cat = factor(Trt_cat, levels = c("U", "H", "B", "HB"))) %>%
  dplyr::select(Trt_cat) %>%
  mutate(Trt_cat = as.numeric(as.factor(Trt_cat))) %>%
  as_vector() 

n.trt <- length(unique(TreatmentID))

unique(nstsurv_emfwor$Tree_sp)

SpeciesID <- nstsurv_emfwor %>%
  mutate(Tree_sp = factor(Tree_sp, levels = c("PIPO", "POTR5"))) %>%
  dplyr::select(Tree_sp) %>%
  mutate(Tree_sp = as.numeric(as.factor(Tree_sp))) %>%
  as_vector() 

n.species <- length(unique(SpeciesID))

LandBu <- as.vector(scale(nstsurv_emfwor$a1000_RxBu))
InitDay <- as.vector(scale(nstsurv_emfwor$Init_day))
NestHt <- as.vector(scale(nstsurv_emfwor$Nest_Ht))
PPT <- as.vector(scale(nstsurv_emfwor$PPT_ne))
Tmax <- as.vector(scale(nstsurv_emfwor$Tmax_ne))

data_list <- list(n.nests = n.nests,
                  y.fledgling = y.fledgling,
                  Nnestlings = Nnestlings,
                  Forest.ID = Forest.ID, 
                  n.forests = n.forests,
                  Transect.ID = Transect.ID,
                  n.transects = n.transects,
                  TreatmentID = TreatmentID,
                  n.trt = n.trt,
                  SpeciesID = SpeciesID,
                  n.species = n.species,
                  LandBu = LandBu,
                  InitDay =InitDay,
                  NestHt = NestHt,
                  PPT = PPT,
                  Tmax = Tmax)

saveRDS(data_list, here('data',
                        '01_parameter_model_inputs',
                        '03_nestling_survival',
                        'jags_input_data',
                        'nestling_s_data_list_emfwor.RDS'))

# Run model ---------------------------------------------------------------

params <- c("n0",
            'n1TreatmentID',
            'n2SpeciesID',
            'n')

model <- here("code", 
              "02_parameter_models", 
              "03_nestling_survival",
              "jags", 
              "nestling_survival_byforest.R")

nsts_mod <- jagsUI::jags(data = data_list,
                         inits = NULL,
                         model.file = model,
                         parameters.to.save = params,
                         parallel = TRUE,
                         n.chains = 3,
                         n.iter = 4000,
                         DIC = TRUE)

# Check convergence -------------------------------------------------------

mcmcplot(nsts_mod$samples)

rhat <- nsts_mod$Rhat

rhat_graph_fun2(rhat)

ggsave(here('pictures',
            'R',
            'SI',
            'Figure_nestlingsurvival_convergence_emfwor.png'),
       height = 3.5,
       width = 6,
       units = "in")

# Save summary ------------------------------------------------------------

sum <- summary(nsts_mod$samples)

saveRDS(sum, here("data",
                     "02_parameter_model_outputs",
                     "03_nestling_survival",
                     "nestling_survival_parameter_summaries_emfwor.RDS"))

# Update for number of samples --------------------------------------------

nsts_mod2 <- update(object = nsts_mod,
                    n.chains = 3,
                    n.iter = 350)

# Export parameter lists --------------------------------------------------

n0 <- nsts_mod2$sims.list$n0
n1TreatmentID <- nsts_mod2$sims.list$n1TreatmentID
n2SpeciesID <- nsts_mod2$sims.list$n2SpeciesID
n <- nsts_mod2$sims.list$n

output <- list(n0 = n0,
               n1TreatmentID = n1TreatmentID,
               n2SpeciesID = n2SpeciesID,
               n = n)

saveRDS(output, here("data",
                     "02_parameter_model_outputs",
                     "03_nestling_survival",
                     "nestling_survival_parameters_emfwor.RDS"))


# EMMAOR ------------------------------------------------------------------
nstsurv_emmaor <- nstsurv %>%
  filter(Project_ID == "EMMAOR")

# Prep data ---------------------------------------------------------------

n.nests <- nrow(nstsurv_emmaor)

y.fledgling <- as.vector(nstsurv_emmaor$NoFL_uncert)
Nnestlings <- as.vector(nstsurv_emmaor$No_young)

Forest.ID <- nstsurv_emmaor %>%
  mutate(Project_ID = as.numeric(as.factor(Project_ID))) %>%
  dplyr::select(Project_ID) %>%
  as_vector()

n.forests <- length(unique(Forest.ID))

Transect.ID <- nstsurv_emmaor %>%
  mutate(Transect_ID2 = as.numeric(as.factor(Transect_ID2))) %>%
  dplyr::select(Transect_ID2) %>%
  as_vector()

n.transects <- length(unique(Transect.ID))

TreatmentID <- nstsurv_emmaor %>%
  mutate(Trt_cat = factor(Trt_cat, levels = c("U", "H", "B", "HB"))) %>%
  dplyr::select(Trt_cat) %>%
  mutate(Trt_cat = as.numeric(as.factor(Trt_cat))) %>%
  as_vector() 

n.trt <- length(unique(TreatmentID))

unique(nstsurv_emmaor$Tree_sp)

SpeciesID <- nstsurv_emmaor %>%
  mutate(Tree_sp = factor(Tree_sp, levels = c("PIPO", "POTR5", 
                                              "JUOC", "PSME", "Abies"))) %>%
  dplyr::select(Tree_sp) %>%
  mutate(Tree_sp = as.numeric(as.factor(Tree_sp))) %>%
  as_vector() 

n.species <- length(unique(SpeciesID))

LandBu <- as.vector(scale(nstsurv_emmaor$a1000_RxBu))
InitDay <- as.vector(scale(nstsurv_emmaor$Init_day))
NestHt <- as.vector(scale(nstsurv_emmaor$Nest_Ht))
PPT <- as.vector(scale(nstsurv_emmaor$PPT_ne))
Tmax <- as.vector(scale(nstsurv_emmaor$Tmax_ne))

data_list <- list(n.nests = n.nests,
                  y.fledgling = y.fledgling,
                  Nnestlings = Nnestlings,
                  Forest.ID = Forest.ID, 
                  n.forests = n.forests,
                  Transect.ID = Transect.ID,
                  n.transects = n.transects,
                  TreatmentID = TreatmentID,
                  n.trt = n.trt,
                  SpeciesID = SpeciesID,
                  n.species = n.species,
                  LandBu = LandBu,
                  InitDay =InitDay,
                  NestHt = NestHt,
                  PPT = PPT,
                  Tmax = Tmax)

saveRDS(data_list, here('data',
                        '01_parameter_model_inputs',
                        '03_nestling_survival',
                        'jags_input_data',
                        'nestling_s_data_list_emmaor.RDS'))

# Run model ---------------------------------------------------------------

params <- c("n0",
            'n1TreatmentID',
            'n2SpeciesID',
            'n')

model <- here("code", 
              "02_parameter_models", 
              "03_nestling_survival",
              "jags", 
              "nestling_survival_byforest.R")

nsts_mod <- jagsUI::jags(data = data_list,
                         inits = NULL,
                         model.file = model,
                         parameters.to.save = params,
                         parallel = TRUE,
                         n.chains = 3,
                         n.iter = 4000,
                         DIC = TRUE)

# Check convergence -------------------------------------------------------

mcmcplot(nsts_mod$samples)

rhat <- nsts_mod$Rhat

rhat_graph_fun2(rhat)

ggsave(here('pictures',
            'R',
            'SI',
            'Figure_nestlingsurvival_convergence_emmaor.png'),
       height = 3.5,
       width = 6,
       units = "in")

# Save summary ------------------------------------------------------------

sum <- summary(nsts_mod$samples)

saveRDS(sum, here("data",
                  "02_parameter_model_outputs",
                  "03_nestling_survival",
                  "nestling_survival_parameter_summaries_emmaor.RDS"))

# Update for number of samples --------------------------------------------

nsts_mod2 <- update(object = nsts_mod,
                    n.chains = 3,
                    n.iter = 350)

# Export parameter lists --------------------------------------------------

n0 <- nsts_mod2$sims.list$n0
n1TreatmentID <- nsts_mod2$sims.list$n1TreatmentID
n2SpeciesID <- nsts_mod2$sims.list$n2SpeciesID
n <- nsts_mod2$sims.list$n

output <- list(n0 = n0,
               n1TreatmentID = n1TreatmentID,
               n2SpeciesID = n2SpeciesID,
               n = n)

saveRDS(output, here("data",
                     "02_parameter_model_outputs",
                     "03_nestling_survival",
                     "nestling_survival_parameters_emmaor.RDS"))


# EMPAID ------------------------------------------------------------------

nstsurv_empaid <- nstsurv %>%
  filter(Project_ID == "EMPAID")

# Prep data ---------------------------------------------------------------

n.nests <- nrow(nstsurv_empaid)

y.fledgling <- as.vector(nstsurv_empaid$NoFL_uncert)
Nnestlings <- as.vector(nstsurv_empaid$No_young)

Forest.ID <- nstsurv_empaid %>%
  mutate(Project_ID = as.numeric(as.factor(Project_ID))) %>%
  dplyr::select(Project_ID) %>%
  as_vector()

n.forests <- length(unique(Forest.ID))

Transect.ID <- nstsurv_empaid %>%
  mutate(Transect_ID2 = as.numeric(as.factor(Transect_ID2))) %>%
  dplyr::select(Transect_ID2) %>%
  as_vector()

n.transects <- length(unique(Transect.ID))

TreatmentID <- nstsurv_empaid %>%
  mutate(Trt_cat = factor(Trt_cat, levels = c("U", "H", "B", "HB"))) %>%
  dplyr::select(Trt_cat) %>%
  mutate(Trt_cat = as.numeric(as.factor(Trt_cat))) %>%
  as_vector() 

n.trt <- length(unique(TreatmentID))
unique(nstsurv_empaid$Tree_sp)

SpeciesID <- nstsurv_empaid %>%
  # mutate(Tree_sp = factor(Tree_sp, levels = c("PIPO", "POTR5", 
  #                                             "JUOC", "PSME", "Abies"))) %>%
  mutate(Tree_sp = factor(Tree_sp, levels = c("PIPO", "PSME"))) %>%
  dplyr::select(Tree_sp) %>%
  mutate(Tree_sp = as.numeric(as.factor(Tree_sp))) %>%
  as_vector() 

n.species <- length(unique(SpeciesID))

LandBu <- as.vector(scale(nstsurv_empaid$a1000_RxBu))
InitDay <- as.vector(scale(nstsurv_empaid$Init_day))
NestHt <- as.vector(scale(nstsurv_empaid$Nest_Ht))
PPT <- as.vector(scale(nstsurv_empaid$PPT_ne))
Tmax <- as.vector(scale(nstsurv_empaid$Tmax_ne))

data_list <- list(n.nests = n.nests,
                  y.fledgling = y.fledgling,
                  Nnestlings = Nnestlings,
                  Forest.ID = Forest.ID, 
                  n.forests = n.forests,
                  Transect.ID = Transect.ID,
                  n.transects = n.transects,
                  TreatmentID = TreatmentID,
                  n.trt = n.trt,
                  SpeciesID = SpeciesID,
                  n.species = n.species,
                  LandBu = LandBu,
                  InitDay =InitDay,
                  NestHt = NestHt,
                  PPT = PPT,
                  Tmax = Tmax)

saveRDS(data_list, here('data',
                        '01_parameter_model_inputs',
                        '03_nestling_survival',
                        'jags_input_data',
                        'nestling_s_data_list_empaid.RDS'))

# Run model ---------------------------------------------------------------

params <- c("n0",
            'n1TreatmentID',
            'n2SpeciesID',
            'n')

model <- here("code", 
              "02_parameter_models", 
              "03_nestling_survival",
              "jags", 
              "nestling_survival_byforest.R")

nsts_mod <- jagsUI::jags(data = data_list,
                         inits = NULL,
                         model.file = model,
                         parameters.to.save = params,
                         parallel = TRUE,
                         n.chains = 3,
                         n.iter = 4000,
                         DIC = TRUE)

# Check convergence -------------------------------------------------------

mcmcplot(nsts_mod$samples)

rhat <- nsts_mod$Rhat

rhat_graph_fun2(rhat)

ggsave(here('pictures',
            'R',
            'SI',
            'Figure_nestlingsurvival_convergence_empaid.png'),
       height = 3.5,
       width = 6,
       units = "in")

# Save summary ------------------------------------------------------------

sum <- summary(nsts_mod$samples)

saveRDS(sum, here("data",
                  "02_parameter_model_outputs",
                  "03_nestling_survival",
                  "nestling_survival_parameter_summaries_empaid.RDS"))

# Update for number of samples --------------------------------------------

nsts_mod2 <- update(object = nsts_mod,
                    n.chains = 3,
                    n.iter = 350)

# Export parameter lists --------------------------------------------------

n0 <- nsts_mod2$sims.list$n0
n1TreatmentID <- nsts_mod2$sims.list$n1TreatmentID
n2SpeciesID <- nsts_mod2$sims.list$n2SpeciesID
n <- nsts_mod2$sims.list$n

output <- list(n0 = n0,
               n1TreatmentID = n1TreatmentID,
               n2SpeciesID = n2SpeciesID,
               n = n)

saveRDS(output, here("data",
                     "02_parameter_model_outputs",
                     "03_nestling_survival",
                     "nestling_survival_parameters_empaid.RDS"))



# ALL FORESTS DATA --------------------------------------------------------

# Prep data ---------------------------------------------------------------

n.nests <- nrow(nstsurv)

y.fledgling <- as.vector(nstsurv$NoFL_uncert)
Nnestlings <- as.vector(nstsurv$No_young)

Forest.ID <- nstsurv %>%
  mutate(Project_ID = as.numeric(as.factor(Project_ID))) %>%
  dplyr::select(Project_ID) %>%
  as_vector()

n.forests <- length(unique(Forest.ID))

Transect.ID <- nstsurv %>%
  mutate(Transect_ID2 = as.numeric(as.factor(Transect_ID2))) %>%
  dplyr::select(Transect_ID2) %>%
  as_vector()

n.transects <- length(unique(Transect.ID))

TreatmentID <- nstsurv %>%
  mutate(Trt_cat = factor(Trt_cat, levels = c("U", "H", "B", "HB"))) %>%
  dplyr::select(Trt_cat) %>%
  mutate(Trt_cat = as.numeric(as.factor(Trt_cat))) %>%
  as_vector() 

n.trt <- length(unique(TreatmentID))
unique(nstsurv$Tree_sp)

SpeciesID <- nstsurv %>%
  mutate(Tree_sp = factor(Tree_sp, levels = c("PIPO", "POTR5",
                                              "JUOC", "PSME", "Abies"))) %>%
  #mutate(Tree_sp = factor(Tree_sp, levels = c("PIPO", "PSME"))) %>%
  dplyr::select(Tree_sp) %>%
  mutate(Tree_sp = as.numeric(as.factor(Tree_sp))) %>%
  as_vector() 

n.species <- length(unique(SpeciesID))

LandBu <- as.vector(scale(nstsurv$a1000_RxBu))
InitDay <- as.vector(scale(nstsurv$Init_day))
NestHt <- as.vector(scale(nstsurv$Nest_Ht))
PPT <- as.vector(scale(nstsurv$PPT_ne))
Tmax <- as.vector(scale(nstsurv$Tmax_ne))

data_list <- list(n.nests = n.nests,
                  y.fledgling = y.fledgling,
                  Nnestlings = Nnestlings,
                  Forest.ID = Forest.ID, 
                  n.forests = n.forests,
                  Transect.ID = Transect.ID,
                  n.transects = n.transects,
                  TreatmentID = TreatmentID,
                  n.trt = n.trt,
                  SpeciesID = SpeciesID,
                  n.species = n.species,
                  LandBu = LandBu,
                  InitDay =InitDay,
                  NestHt = NestHt,
                  PPT = PPT,
                  Tmax = Tmax)

saveRDS(data_list, here('data',
                        '01_parameter_model_inputs',
                        '03_nestling_survival',
                        'jags_input_data',
                        'nestling_s_data_list.RDS'))

# Run model ---------------------------------------------------------------

params <- c("n0",
            'n1TreatmentID',
            'n2SpeciesID',
            'n')

model <- here("code", 
              "02_parameter_models", 
              "03_nestling_survival",
              "jags", 
              "nestling_survival_byforest.R")

nsts_mod <- jagsUI::jags(data = data_list,
                         inits = NULL,
                         model.file = model,
                         parameters.to.save = params,
                         parallel = TRUE,
                         n.chains = 3,
                         n.iter = 4000,
                         DIC = TRUE)

# Check convergence -------------------------------------------------------

mcmcplot(nsts_mod$samples)

rhat <- nsts_mod$Rhat

rhat_graph_fun2(rhat)

ggsave(here('pictures',
            'R',
            'SI',
            'Figure_nestlingsurvival_convergence.png'),
       height = 3.5,
       width = 6,
       units = "in")

# Save summary ------------------------------------------------------------

sum <- summary(nsts_mod$samples)

saveRDS(sum, here("data",
                  "02_parameter_model_outputs",
                  "03_nestling_survival",
                  "nestling_survival_parameter_summaries.RDS"))

