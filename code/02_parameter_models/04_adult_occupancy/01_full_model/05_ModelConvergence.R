# Assess model convergence and posteriors
# Ana Miller-ter Kuile
# May 9, 2023

#this script looks at convergence

# Load packages ---------------------------------------------------------------

# Load packages, here and tidyverse for coding ease, 
package.list <- c("here", "tidyverse",  
                  "mcmcplots", "jagsUI") #posterior graphing)


## Installing them if they aren't already on the computer
new.packages <- package.list[!(package.list %in% 
                                 installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## And loading them
for(i in package.list){library(i, character.only = T)}

#get model diagnostic plotting functions

# Graph RHat per parameter ------------------------------------------------

rhat_graph_fun <- function(list){
  
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
    scale_y_sqrt() +
    #this is facetted by the parameter ID so you
    # can find problematic parameters
    facet_wrap(~ id)
  
  return(plot)
}


# Gelman for total model --------------------------------------------------

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

# Load model output -------------------------------------------------------

Rhat <- readRDS(here("monsoon",
                     "parameter_models",
                     "adult_occupancy",
                     "outputs",
                     "model_Rhat3.RDS"))

# Gelman-Rubin ------------------------------------------------------------

#both have converged
rhat_graph_fun2(Rhat)
rhat_graph_fun(Rhat)

ggsave(here('pictures',
            'R',
            'SI',
            'Figure_adultoccupancy_convergence.png'),
       height = 3.5,
       width = 6,
       units = "in")
