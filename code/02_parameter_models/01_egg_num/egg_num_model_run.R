# Data prep for egg JAGS model
# April 19, 2023

# this script preps data for the JAGS submodel for egg productivity


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

eggs <- read.csv(here("data",
                      '01_parameter_model_inputs',
                      "01_egg_num",
                      "Egg_production_data.csv"))


# Data objects ------------------------------------------------------------

n.nests <- nrow(eggs)
y.egg <- eggs %>%
  dplyr::select(No_eggs) %>%
  as_vector()

Forest.ID <- eggs %>%
  dplyr::select(Project_ID) %>%
  mutate(Project_ID = as.numeric(as.factor(Project_ID))) %>%
  as_vector()

n.forests <- length(unique(Forest.ID))

data_list <- list(n.nests = n.nests,
                  y.egg = y.egg,
                  Forest.ID = Forest.ID,
                  n.forests = n.forests)

# Run model ---------------------------------------------------------------

params <- c('egg.lambda')

model <- here("code", 
              "02_parameter_models", 
              "01_egg_num",
              "jags", 
              "egg_number_byforest.R")


egg_mod <- jagsUI::jags(data = data_list,
                        inits = NULL,
                        model.file = model,
                        parameters.to.save = params,
                        parallel = TRUE,
                        n.chains = 3,
                        n.iter = 350,
                        DIC = TRUE)

# Check convergence -------------------------------------------------------

mcmcplot(egg_mod$samples)

gelman.diag(egg_mod$samples)

rhat <- egg_mod$Rhat

rhat_graph_fun2(rhat)

ggsave(here('pictures',
            'R',
            'SI',
            'Figure_eggnumber_convergence.png'),
       height = 3.5,
       width = 6,
       units = "in")


# Export lambda object ----------------------------------------------------

egg.lambda <- egg_mod$sims.list$egg.lambda

output <- list(egg.lambda = egg.lambda)

saveRDS(output, here("data",
                     "02_parameter_model_outputs",
                     "01_egg_num",
                     "egg_num_parameters.RDS"))
