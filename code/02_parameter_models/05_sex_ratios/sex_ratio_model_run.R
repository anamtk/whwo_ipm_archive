# Data prep for sex ratio JAGS model
# Ana Miller-ter Kuile
# April 19, 2023

# this script preps data for the JAGS submodel for sex ratio


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


# Load data ---------------------------------------------------------------

sexratio <- read.csv(here("data",
                          '01_parameter_model_inputs',
                          "05_sex_ratios",
                          "Sex_ratio_data.csv"))

# Prep data for jags ------------------------------------------------------

n.nests <- nrow(sexratio)
y <- as.vector(sexratio$NoFNstl)
n.total <- as.vector(sexratio$NoFNstl + sexratio$NoMNstl)

#Fremont-winema = 1
#Malheur = 2
#Payette =3 
Forest.ID <- sexratio %>%
  dplyr::select(Project_ID) %>%
  mutate(Project_ID = as.numeric(as.factor(Project_ID))) %>%
  as_vector()

n.forests <- length(unique(Forest.ID))

YearID <- sexratio %>%
  mutate(Year_located = as.numeric(as.factor(Year_located))) %>%
  dplyr::select(Year_located) %>%
  as_vector() 

n.yr <- length(unique(YearID))

BroodSize <- as.vector(scale(sexratio$NoFNstl + sexratio$NoMNstl))
InitDay <- as.vector(scale(sexratio$Init_day))

Transect.ID <- sexratio %>%
  mutate(Transect_ID2 = as.numeric(as.factor(Transect_ID2))) %>%
  dplyr::select(Transect_ID2) %>%
  as_vector()

n.transects <- length(unique(Transect.ID))

data_list <- list(n.nests = n.nests,
                  y = y,
                  n.total = n.total,
                  Forest.ID = Forest.ID,
                  n.forests = n.forests,
                  Transect.ID = Transect.ID,
                  n.transects = n.transects,
                  BroodSize = BroodSize,
                  InitDay = InitDay,
                  YearID = YearID,
                  n.yr = n.yr)

saveRDS(data_list, here('data',
                        '01_parameter_model_inputs',
                        '05_sex_ratios',
                        'jags_input_data',
                        'sexratio_data_list.RDS'))


# Run model ---------------------------------------------------------------

params <- c("f0",
            'f')

model <- here("code", 
              "02_parameter_models", 
              "05_sex_ratios",
              "jags", 
              "sexratio_byforest_allcovariates.R")

sex_mod <- jagsUI::jags(data = data_list,
                        inits = NULL,
                        model.file = model,
                        parameters.to.save = params,
                        parallel = TRUE,
                        n.chains = 3,
                        n.iter = 4000,
                        DIC = TRUE)

# Check convergence -------------------------------------------------------

mcmcplot(sex_mod$samples)

rhat <- sex_mod$Rhat



#this function plots one histogram overall for the whole model
# to diagnose gelman-rubin stats
rhat_graph_fun2 <- function(list){
  
  #this creates a dtaaframe out of the Rhat values from the model
  df <- data.frame(id = names(list),
                   Rhat = unlist(lapply(list, paste, collapse = ","))) %>%
    #splits Rhat by , when that list element had more than one value
    mutate(Rhat = str_split(Rhat, ",")) %>%
    #unnests - so makes each Rhat a new row in the df
    unnest(c(Rhat)) %>%
    #make sure Rhat is a numeric
    mutate(Rhat = as.numeric(Rhat)) 
  
  
  #plot histogram and make sure all below 1.1
  plot <- ggplot(df, aes(x = Rhat)) +
    geom_histogram() +
    geom_vline(xintercept = 1.2, linetype = 2) +
    theme_bw() +
    scale_y_sqrt() 
  
  return(plot)
}

rhat_graph_fun2(rhat)
#nothing is important, so run a simplified model

ggsave(here('pictures',
            'R',
            'SI',
            'Figure_sexratio_convergence.png'),
       height = 3.5,
       width = 6,
       units = "in")
# Run simple --------------------------------------------------------------


params <- c('propF')

model <- here("code", 
              "02_parameter_models", 
              "05_sex_ratios",
              "jags", 
              "sexratio_byforest_nocovs.R")

sex_mod2 <- jagsUI::jags(data = data_list,
                        inits = NULL,
                        model.file = model,
                        parameters.to.save = params,
                        parallel = TRUE,
                        n.chains = 3,
                        n.iter = 4000,
                        DIC = TRUE)


# Output summary ----------------------------------------------------------

sum <- summary(sex_mod2$samples)

ratios <- as.data.frame(sum$quantiles) %>%
  rownames_to_column(var = 'parm') %>%
  filter(parm != "deviance") %>%
  dplyr::select(parm, `2.5%`,
                `50%`, `97.5%`) %>%
  rename('lci' = `2.5%`,
         'median' = `50%`,
         'uci' = `97.5%`)

#Fremont-winema = 1
#Malheur = 2
#Payette =3
saveRDS(ratios, here("data",
                     "02_parameter_model_outputs",
                     "05_sex_ratios",
                     "sex_ratio_parameter_summaries.RDS"))

# Get the right num of samples --------------------------------------------

sex_mod3 <- update(object = sex_mod2,
                    n.chains = 3,
                    n.iter = 350)

# Export parameter lists --------------------------------------------------

propF <- sex_mod3$sims.list$propF

output <- list(propF = propF)

saveRDS(output, here("data",
                     "02_parameter_model_outputs",
                     "05_sex_ratios",
                     "sex_ratio_parameters.RDS"))



