# Functions for hierarchical partitioning
# December 11, 2023

# these are functions for running the hierarchical partitioning
#process


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


# LL summary function -----------------------------------------------------


#function to run a generic model
run_jags_mod <- function(model, name){
  
  #run the model with the specific model defined
  mod <- jagsUI::jags(data = data,
                      inits = NULL,
                      model.file = model,
                      parameters.to.save = params,
                      parallel = TRUE,
                      n.chains = 3,
                      n.iter = 4000,
                      DIC = TRUE)
  
  #make sure it converges
  rhat <- gelman.diag(mod$samples, multivariate = F)
  rhat <- as.data.frame(rhat$psrf) %>%
    filter(!is.na(`Point est.`))
  
  a <- "converged"
  b <- "not converged"
  
  #prints out if it converged or not
  ifelse(all(rhat$`Point est.`<= 1.1), print(a), print(b))
  
  #pull out Log Likelihood
  LL <- summary(mod$samples) 
  LL <- as.data.frame(LL$statistics) %>%
    rownames_to_column(var = 'par') %>%
    filter(par == "LL") %>%
    mutate(model = name) %>%
    dplyr::select(Mean, model) %>%
    rename(LL = Mean)
  
  #returns DF of Log likelihood and model name that you define
  return(LL)
  
}



# Update submodel for samples of covariates -------------------------------

# LL summary function -----------------------------------------------------


#function to run a generic model
update_jags_mod <- function(model){
  
  #run the model with the specific model defined
  mod <- jagsUI::jags(data = data,
                      inits = NULL,
                      model.file = model,
                      parameters.to.save = params,
                      parallel = TRUE,
                      n.chains = 3,
                      n.iter = 4000,
                      DIC = TRUE)
  
  #make sure it converges
  rhat <- gelman.diag(mod$samples, multivariate = F)
  rhat <- as.data.frame(rhat$psrf) %>%
    filter(!is.na(`Point est.`))
  
  a <- "converged"
  b <- "not converged"
  
  #prints out if it converged or not
  ifelse(all(rhat$`Point est.`<= 1.1), print(a), print(b))
  
  update_mod <- update(mod,
                       parameters.to.save = params,
                       n.iter = 350)

  #Pull out parameters
  sims <- update_mod$sims.list
  
  sims <- sims[params2]
  
  #returns sims list to then pull out parameters from 
  return(sims)
  
}
# LL for adult model ------------------------------------------------------

#function to run a generic model
run_jags_mod2 <- function(model, iter, name){
  
  #run the model with the specific model defined
  mod <- jagsUI::jags(data = data,
                      inits = NULL,
                      model.file = model,
                      parameters.to.save = params,
                      parallel = TRUE,
                      n.chains = 3,
                      n.iter = iter,
                      DIC = TRUE)
  
  #make sure it converges
  rhat <- gelman.diag(mod$samples, multivariate = F)
  rhat <- as.data.frame(rhat$psrf) %>%
    filter(!is.na(`Point est.`))

  a <- "converged"
  b <- "not converged"

  #prints out if it converged or not
  ifelse(all(rhat$`Point est.`<= 1.1), print(a), print(b))

  #pull out Log Likelihood
  LL <- summary(mod$samples) 
  LL2 <- as.data.frame(LL$statistics) %>%
    rownames_to_column(var = 'par') %>%
    filter(str_detect(par, 'll')) %>%
    summarize(LL = sum(Mean, na.rm = T)) %>%
    mutate(model = name) %>%
    dplyr::select(LL, model)
  
  #returns DF of Log likelihood and model name that you define
  return(LL2)
  
}


# Autojags option for above -----------------------------------------------

#function to run a generic model
run_jags_mod3 <- function(model, name){
  
  #run the model with the specific model defined
  mod <- jagsUI::autojags(data = data,
                          inits = NULL,
                          model.file = model,
                          parameters.to.save = params,
                          parallel = TRUE,
                          Rhat.limit = 1.2,
                          n.thin = 10,
                          iter.increment = 40000,
                          n.chains = 3,
                          DIC = TRUE)
  
  #make sure it converges
  rhat <- gelman.diag(mod$samples, multivariate = F)
  rhat <- as.data.frame(rhat$psrf) %>%
    filter(!is.na(`Point est.`))
  
  a <- "converged"
  b <- "not converged"
  
  #prints out if it converged or not
  ifelse(all(rhat$`Point est.`<= 1.1), print(a), print(b))
  
  #pull out Log Likelihood
  LL <- summary(mod$samples) 
  LL2 <- as.data.frame(LL$statistics) %>%
    rownames_to_column(var = 'par') %>%
    filter(str_detect(par, 'll')) %>%
    summarise(LL = sum(Mean, na.rm = T)) %>%
    mutate(model = name) %>%
    dplyr::select(LL, model)
  
  #returns DF of Log likelihood and model name that you define
  return(LL2)
  
}


# IPM HP function ---------------------------------------------------------


#function to run an ipm HP
hp_ipm_fun <- function(model_path, name, start, end){

  #run the model with the specific model defined
  source(model_path)
  
  yearIDs <- as.data.frame(cbind(year = c(start:end),
                                 yearID = 1:length(start:end)))
  
  df <- final %>%
    left_join(yearIDs, by = "yearID")
  
  #pull out Log Likelihood
  LL <- df %>%
    group_by(year) %>%
    summarise(lambda = mean(pop.lambda)) %>%
    ungroup() %>%
    mutate(model = name) %>%
    dplyr::select(lambda, year, model)

  #returns DF of Log likelihood and model name that you define
  return(LL)
  
}

