# Data prep for adult occupancy all variables
# April 19, 2023

# this script preps data for the JAGS model looking for important
#variables for adult n occuapncy


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

occ <- read.csv(here("data",
                     '01_parameter_model_inputs',
                     "04_adult_occupancy",
                     "01_occupancy_observations.csv"))

covariates <- read.csv(here("data",
                            "01_parameter_model_inputs",
                            "04_adult_occupancy",
                            "n_occ_covariates.csv"))

occ_years <- occ %>%
  distinct(Point_ID, Year) %>%
  group_by(Point_ID) %>%
  arrange(Year) %>%
  mutate(year.num = 1:n()) %>%
  ungroup()

occ_years2 <- occ %>%
  distinct(Point_ID, Year) %>%
  mutate(Year = as.factor(Year)) %>%
  group_by(Point_ID) %>%
  complete(Year) %>%
  arrange(Year) %>%
  mutate(year.num = 1:n()) %>%
  mutate(Year = as.numeric(as.character(Year))) %>%
  ungroup()

occ2 <- occ %>%
  full_join(occ_years2, by = c("Point_ID", "Year"))

occ <- occ %>%
  full_join(occ_years, by = c("Point_ID", "Year"))

covariates <- covariates %>%
  left_join(occ_years2, by = c("Point_ID", "Year")) %>%
  ungroup()

# Prep data objects -------------------------------------------------------

# Prep data objects -------------------------------------------------------


#vectors
n.points <- occ %>%
  distinct(Point_ID) %>%
  tally() %>%
  as_vector()

# nYear <- occ %>%
#   distinct(Point_ID, year.num) %>%
#   group_by(Point_ID) %>%
#   tally() %>%
#   ungroup() %>%
#   dplyr::select(n) %>%
#   as_vector()

n.years <- occ %>%
  distinct(Year) %>%
  tally() %>%
  as_vector()

#matrix
n.rep <- occ2 %>%
  group_by(Point_ID) %>%
  mutate(year.num = as.factor(year.num)) %>%
  complete(year.num) %>%
  ungroup()  %>%
  group_by(Point_ID, year.num) %>%
  tally() %>%
  ungroup() %>%
  pivot_wider(names_from = year.num,
              values_from = 'n') %>%
  column_to_rownames(var = "Point_ID") %>%
  as.matrix()

order <- rownames(n.rep)

occ <- occ %>%
  arrange(factor(Point_ID, levels = order))


n.start <- occ %>%
  mutate(Year1 = as.numeric(as.factor(Year))) %>%
  group_by(Point_ID) %>%
  filter(Year1 == min(Year1)) %>%
  ungroup() %>%
  distinct(Point_ID, Year1) %>%
  dplyr::select(Year1) %>%
  as_vector()

n.end <- occ %>%
  mutate(Year1 = as.numeric(as.factor(Year))) %>%
  group_by(Point_ID) %>%
  filter(Year1 == max(Year1)) %>%
  ungroup() %>%
  distinct(Point_ID, Year1) %>%
  dplyr::select(Year1) %>%
  as_vector()

n.trt <- length(unique(occ$Trt_150))
n.observer <- length(unique(occ$Observer))
n.transects <- length(unique(occ$Transect_ID))

#vectors
ForestID <- occ %>%
  mutate(Forest = str_sub(Point_ID, start = 1, end = 6)) %>%
  distinct(Point_ID, Forest) %>%
  arrange(factor(Point_ID, levels = order)) %>%
  dplyr::select(Forest) %>%
  mutate(Forest = as.numeric(as.factor(Forest))) %>%
  as_vector()

n.forests <- length(unique(ForestID)) 

Tmean_sp <- covariates %>%
  mutate(Tmean_sp = scale(Tave_sp)) %>%
  distinct(Point_ID, year.num, Tmean_sp) %>%
  pivot_wider(names_from = "year.num",
              values_from = "Tmean_sp") %>%
  column_to_rownames(var = "Point_ID") %>%
  dplyr::select('1', '2', '3', '4', '5', '6',
                '7', '8', '9', '10') %>%
  as.matrix()

Tmean_wt <- covariates %>%
  mutate(Tmean_wt = scale(Tave_wt)) %>%
  distinct(Point_ID, year.num, Tmean_wt) %>%
  pivot_wider(names_from = "year.num",
              values_from = "Tmean_wt") %>%
  column_to_rownames(var = "Point_ID") %>%
  dplyr::select('1', '2', '3', '4', '5', '6', 
                '7', '8', '9', '10') %>%
  as.matrix()

# PPT_sp <- covariates %>%
#   mutate(PPT_sp = scale(PPT_sp)) %>%
#   distinct(Point_ID, year.num, PPT_sp) %>%
#   pivot_wider(names_from = "year.num",
#               values_from = "PPT_sp") %>%
#   column_to_rownames(var = "Point_ID") %>%
#   dplyr::select('1', '2', '3', '4', '5', '6', 
#                 '7', '8', '9', '10') %>%
#   as.matrix()

PPT_wt <- covariates %>%
  mutate(PPT_wt = scale(PPT_wt)) %>%
  distinct(Point_ID, year.num, PPT_wt) %>%
  pivot_wider(names_from = "year.num",
              values_from = "PPT_wt") %>%
  column_to_rownames(var = "Point_ID") %>%
  dplyr::select('1', '2', '3', '4', '5', '6', 
                '7', '8', '9', '10') %>%
  as.matrix()
# SHDI <- covariates %>%
#   mutate(SHDI = scale(SHDI)) %>%
#   distinct(Point_ID, year.num, SHDI) %>%
#   pivot_wider(names_from = "year.num",
#               values_from = "SHDI") %>%
#   column_to_rownames(var = "Point_ID") %>%
#   dplyr::select('1', '2', '3', '4', '5', '6', 
#                 '7', '8', '9', '10') %>%
#   as.matrix()

LandHa <- covariates %>%
  mutate(LandHa = scale(Ha)) %>%
  distinct(Point_ID, year.num, LandHa) %>%
  pivot_wider(names_from = "year.num",
              values_from = "LandHa") %>%
  column_to_rownames(var = "Point_ID") %>%
  dplyr::select('1', '2', '3', '4', '5', '6',
                '7', '8', '9', '10') %>%
  as.matrix()

# HighCC <- covariates %>%
#   mutate(high_canopy = scale(high_canopy)) %>%
#   distinct(Point_ID, year.num, high_canopy) %>%
#   pivot_wider(names_from = "year.num",
#               values_from = "high_canopy") %>%
#   column_to_rownames(var = "Point_ID") %>%
#   dplyr::select('1', '2', '3', '4', '5', '6', 
#                 '7', '8', '9', '10') %>%
#   as.matrix()

# MedCC <- covariates %>%
#   mutate(med_canopy = scale(med_canopy)) %>%
#   distinct(Point_ID, year.num, med_canopy) %>%
#   pivot_wider(names_from = "year.num",
#               values_from = "med_canopy") %>%
#   column_to_rownames(var = "Point_ID") %>%
#   dplyr::select('1', '2', '3', '4', '5', '6', 
#                 '7', '8', '9', '10') %>%
#   as.matrix()
# 
LowCC <- covariates %>%
  mutate(low_canopy = scale(low_canopy)) %>%
  distinct(Point_ID, year.num, low_canopy) %>%
  pivot_wider(names_from = "year.num",
              values_from = "low_canopy") %>%
  column_to_rownames(var = "Point_ID") %>%
  dplyr::select('1', '2', '3', '4', '5', '6',
                '7', '8', '9', '10') %>%
  as.matrix()

#response data - y.occ[i,t]
y.occ <- occ2 %>%
  group_by(Point_ID, year.num) %>%
  summarise(sum = sum(presence, na.rm = T)) %>%
  pivot_wider(names_from = year.num,
              values_from = sum) %>%
  column_to_rownames(var = "Point_ID") %>%
  as.matrix()

#arrays

npoints <- n.points
nyears <- n.years
nreps <- max(occ$Visit_no, na.rm = T)

z <- occ2 %>%
  group_by(Point_ID, year.num) %>%
  summarise(z = sum(presence)) %>%
  ungroup() %>%
  mutate(z = case_when(z > 1 ~ 1,
                       z == 1 ~ 1,
                       z == 0 ~ NA_real_)) %>%
  pivot_wider(names_from = year.num,
              values_from = z) %>%
  column_to_rownames(var= "Point_ID") %>%
  as.matrix()

# Compile for jags --------------------------------------------------------

data_list <- list(n.points = n.points,
                  n.years = n.years,
                  n.start = n.start,
                  n.end = n.end,
                  #nYear = nYear,
                  n.rep = n.rep,
                  n.transects = n.transects,
                  n.forests = n.forests,
                  n.trt = n.trt,
                  Tmean_sp = Tmean_sp,
                  #PPT_sp = PPT_sp,
                  PPT_wt = PPT_wt,
                  #SHDI = SHDI,
                  LandHa = LandHa,
                  LowCC = LowCC, 
                  #MedCC = MedCC,
                  Tmean_wt = Tmean_wt,
                  #HighCC = HighCC,
                  ForestID = ForestID,
                  y.occ = y.occ,
                  z = z)

saveRDS(data_list, here('data', 
             '01_parameter_model_inputs',
             '04_adult_occupancy',
             'jags_input_data',
             'reduced_adult_data_list.RDS'))

# Run jags ----------------------------------------------------------------

parms <- c("a0", 
           "a")

model <- here("code", "02_parameter_models",
              "04_adult_occupancy", 
              "jags",
              'adult_dyn_occupancy_reduced.R')

#takes ~ 6 minutes to run 4000
Sys.time()
mod1 <- jagsUI::jags(data = data_list,
                    inits = NULL,
                    parameters.to.save = parms,
                    model.file = model,
                    n.chains = 3,
                    n.iter = 10000,
                    #n.thin = 10,
                    # n.burnin = 1000,
                    # n.iter = 41000,
                    parallel = TRUE,
                    DIC = TRUE)
Sys.time()

mcmcplot(mod1$samples)
gelman.diag(mod1$samples)


# Export ------------------------------------------------------------------

adult_sum <- summary(mod1$samples)

saveRDS(adult_sum, here('data',
                        '02_parameter_model_outputs',
                        '04_adult_occupancy',
                        'adult_parameter_summaries.RDS'))
# Raftery -----------------------------------------------------------------

raf <- raftery.diag(mod1$samples)

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

ggplot(raf_all, aes(x = iterations/3)) +
  geom_histogram() 

raf_all %>%
  summarise(iterations_90 = quantile(iterations, 
                                     probs = 0.9, 
                                     na.rm = T)/3,
            iterations_95 = quantile(iterations,
                                     probs = 0.95,
                                     na.rm = T)/3,
            max = max(iterations, 
                      na.rm = T)/3)
# A tibble: 1 × 3
# iterations_90 iterations_95   max
# <dbl>         <dbl> <dbl>
#   51682.        80509. 242484.

bu1 <- raf[[1]]$resmatrix[,1]
bu2 <- raf[[2]]$resmatrix[,1]
bu3 <- raf[[3]]$resmatrix[,1]

burn <- as.data.frame(cbind(names, bu1, bu2, bu3)) %>%
  mutate(bu1 = as.numeric(bu1),
         bu2 = as.numeric(bu2),
         bu3 = as.numeric(bu3)) %>%
  filter(!str_detect(names, "z")) %>%
  filter(!str_detect(names, "wA")) %>%
  filter(!str_detect(names, "wB")) %>%
  pivot_longer(bu1:bu3,
               names_to = "chain",
               values_to = 'iterations') 

burn %>%
  summarise(max(iterations, na.rm = T))
#225


# Check convergence -------------------------------------------------------

mod$Rhat

# Get the right num of samples --------------------------------------------

mod2 <- update(object = mod,
                    n.chains = 3,
                    n.iter = 350)

# Export parameter lists --------------------------------------------------

a0 <- mod2$sims.list$a0
b0 <- mod2$sims.list$b0
a <- mod2$sims.list$a
b <- mod2$sims.list$b

output <- list(a0 = a0,
               b0 = b0,
               a =a,
               b = b)

saveRDS(output, here("data",
                     "02_parameter_model_outputs",
                     "04_adult_occupancy",
                     "adult_parameters.RDS"))
