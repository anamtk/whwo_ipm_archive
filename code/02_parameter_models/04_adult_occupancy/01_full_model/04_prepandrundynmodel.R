# Data prep for adult occupancy all variables
# April 19, 2023

# this script preps data for the JAGS model looking for important
#variables for adult n occuapncy


# Load packages -----------------------------------------------------------

package.list <- c("here", "tidyverse", 
                  "jagsUI",
                  'rjags',
                  'mcmcplots',
                  "coda", "GGally")

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


# Check correlation among predictors --------------------------------------

cov2 <- covariates %>%
  dplyr::select(Tave_wt, Tave_sp, PPT_wt, PPT_sp, Ha, Bu, SHDI, low_canopy,
                med_canopy, high_canopy)

ggpairs(cov2)
#looks like SHDI and HA are highly correlated (0.663)
#and so are low and med canopy (0.620)
#I'm going to choose Ha because it's easier to explain and 
#also low canopy so I have the high-low contrast

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
  

TransectID <- occ %>%
  distinct(Point_ID, Transect_ID) %>%
  filter(!is.na(Transect_ID)) %>%
  arrange(factor(Point_ID, levels = order)) %>%
  dplyr::select(Transect_ID) %>%
  mutate(Transect_ID = as.numeric(as.factor(Transect_ID))) %>%
  as_vector()

#matrices:
#NOTE: I removed 2021, so column '10' is gone from this 
## and will need to be added back when we add back in 2021
TreatmentID <- covariates %>%
  mutate(Trt_150 = factor(Trt_150, 
                          levels = c("U", "H", "B", "HB"))) %>%
 dplyr::select(Point_ID, year.num, Trt_150) %>%
  mutate(Trt_150 = as.numeric(as.factor(Trt_150))) %>%
  pivot_wider(names_from = "year.num",
              values_from = "Trt_150",
              values_fill = NA) %>%
  column_to_rownames(var = "Point_ID") %>%
  dplyr::select('1', '2', '3', '4', '5', '6', 
                '7', '8', '9', '10') %>%
  as.matrix()

Trees50 <- covariates %>%
  mutate(Trees_50 = scale(Trees_50)) %>%
  distinct(Point_ID, year.num, Trees_50) %>%
  pivot_wider(names_from = "year.num",
              values_from = "Trees_50") %>%
  column_to_rownames(var = "Point_ID") %>%
  dplyr::select('1', '2', '3', '4', '5', '6', 
                '7', '8', '9', '10') %>%
  as.matrix()
  

Trees2550 <- covariates %>%
  mutate(Trees_2550 = scale(Trees_2550)) %>%
  distinct(Point_ID, year.num, Trees_2550) %>%
  pivot_wider(names_from = "year.num",
              values_from = "Trees_2550") %>%
  column_to_rownames(var = "Point_ID") %>%
  dplyr::select('1', '2', '3', '4', '5', '6', 
                '7', '8', '9', '10') %>%
  as.matrix()

PercPonderosa <- covariates %>%
  mutate(pPIPO = scale(pPIPO)) %>%
  distinct(Point_ID, year.num, pPIPO) %>%
  pivot_wider(names_from = "year.num",
              values_from = "pPIPO") %>%
  column_to_rownames(var = "Point_ID") %>%
  dplyr::select('1', '2', '3', '4', '5', '6', 
                '7', '8', '9', '10') %>%
  as.matrix()

Tmean_sp <- covariates %>%
  mutate(Tave_sp = scale(Tave_sp)) %>%
  distinct(Point_ID, year.num, Tave_sp) %>%
  pivot_wider(names_from = "year.num",
              values_from = "Tave_sp") %>%
  column_to_rownames(var = "Point_ID") %>%
  dplyr::select('1', '2', '3', '4', '5', '6', 
                '7', '8', '9', '10') %>%
  as.matrix()

Tmean_wt <- covariates %>%
  mutate(Tave_wt = scale(Tave_wt)) %>%
  distinct(Point_ID, year.num, Tave_wt) %>%
  pivot_wider(names_from = "year.num",
              values_from = "Tave_wt") %>%
  column_to_rownames(var = "Point_ID") %>%
  dplyr::select('1', '2', '3', '4', '5', '6', 
                '7', '8', '9', '10') %>%
  as.matrix()

PPT_sp <- covariates %>%
  mutate(PPT_sp = scale(PPT_sp)) %>%
  distinct(Point_ID, year.num, PPT_sp) %>%
  pivot_wider(names_from = "year.num",
              values_from = "PPT_sp") %>%
  column_to_rownames(var = "Point_ID") %>%
  dplyr::select('1', '2', '3', '4', '5', '6', 
                '7', '8', '9', '10') %>%
  as.matrix()

PPT_wt <- covariates %>%
  mutate(PPT_wt = scale(PPT_wt)) %>%
  distinct(Point_ID, year.num, PPT_wt) %>%
  pivot_wider(names_from = "year.num",
              values_from = "PPT_wt") %>%
  column_to_rownames(var = "Point_ID") %>%
  dplyr::select('1', '2', '3', '4', '5', '6', 
                '7', '8', '9', '10') %>%
  as.matrix()

SHDI <- covariates %>%
  mutate(SHDI = scale(SHDI)) %>%
  distinct(Point_ID, year.num, SHDI) %>%
  pivot_wider(names_from = "year.num",
              values_from = "SHDI") %>%
  column_to_rownames(var = "Point_ID") %>%
  dplyr::select('1', '2', '3', '4', '5', '6', 
                '7', '8', '9', '10') %>%
  as.matrix()

LandBu <- covariates %>%
  mutate(Bu = scale(Bu)) %>%
  distinct(Point_ID, year.num, Bu) %>%
  pivot_wider(names_from = "year.num",
              values_from = "Bu") %>%
  column_to_rownames(var = "Point_ID") %>%
  dplyr::select('1', '2', '3', '4', '5', '6', 
                '7', '8', '9', '10') %>%
  as.matrix()

LandHa <- covariates %>%
  mutate(Ha = scale(Ha)) %>%
  distinct(Point_ID, year.num, Ha) %>%
  pivot_wider(names_from = "year.num",
              values_from = "Ha") %>%
  column_to_rownames(var = "Point_ID") %>%
  dplyr::select('1', '2', '3', '4', '5', '6', 
                '7', '8', '9', '10') %>%
  as.matrix()

LowCC <- covariates %>%
  mutate(low_canopy = scale(low_canopy)) %>%
  distinct(Point_ID, year.num, low_canopy) %>%
  pivot_wider(names_from = "year.num",
              values_from = "low_canopy") %>%
  column_to_rownames(var = "Point_ID") %>%
  dplyr::select('1', '2', '3', '4', '5', '6', 
                '7', '8', '9', '10') %>%
  as.matrix()

MedCC <- covariates %>%
  mutate(med_canopy = scale(med_canopy)) %>%
  distinct(Point_ID, year.num, med_canopy) %>%
  pivot_wider(names_from = "year.num",
              values_from = "med_canopy") %>%
  column_to_rownames(var = "Point_ID") %>%
  dplyr::select('1', '2', '3', '4', '5', '6', 
                '7', '8', '9', '10') %>%
  as.matrix()

HighCC <- covariates %>%
  mutate(high_canopy = scale(high_canopy)) %>%
  distinct(Point_ID, year.num, high_canopy) %>%
  pivot_wider(names_from = "year.num",
              values_from = "high_canopy") %>%
  column_to_rownames(var = "Point_ID") %>%
  dplyr::select('1', '2', '3', '4', '5', '6', 
                '7', '8', '9', '10') %>%
  as.matrix()


#arrays

npoints <- n.points
nyears <- n.years
nreps <- max(occ$Visit_no, na.rm = T)

occ2 <- occ2 %>%
  mutate(Point_ID = as.numeric(as.factor(Point_ID))) %>%
  mutate(Observer = as.numeric(as.factor(Observer))) %>%
  mutate(Survey_length = scale(Survey_length))

yr <- occ2$year.num
point <- occ2$Point_ID
rep <- occ2$Visit_no

effort <- array(NA, dim = c(npoints, nreps, nyears))

for(i in 1:dim(occ2)[1]){
  effort[point[i], rep[i], yr[i]] <- occ2[i, 8]
}

Observer <- array(NA, dim = c(npoints, nreps, nyears))

for(i in 1:dim(occ2)[1]){
  Observer[point[i], rep[i], yr[i]] <- occ2[i,9]
}

y.occ <- array(NA, dim = c(npoints, nreps, nyears))

for(i in 1:dim(occ2)[1]){
  y.occ[point[i], rep[i], yr[i]] <- occ2[i, 10]
}



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
                  n.observer = n.observer,
                  TreatmentID = TreatmentID,
                  Trees50 = Trees50,
                  Trees2550 = Trees2550,
                  PercPonderosa = PercPonderosa,
                  Tmean_sp = Tmean_sp,
                  Tmean_wt = Tmean_wt,
                  PPT_sp = PPT_sp,
                  PPT_wt = PPT_wt,
                  SHDI = SHDI,
                  LandBu = LandBu,
                  LandHa = LandHa,
                  LowCC = LowCC,
                  MedCC = MedCC,
                  HighCC = HighCC,
                  TransectID = TransectID, 
                  ForestID = ForestID,
                  effort = effort,
                  Observer = Observer,
                  y.occ = y.occ,
                  z = z)


# Run jags ----------------------------------------------------------------

parms <- c("a0", 
           "a1TrtID",
           "a",
           'b0',
           'b',
           'gamma2',
           'e0',
           'e',
           'eps.observer',
           'eps.transect',
           'sig.observer',
           'sig.transect')

model <- here("code", "02_parameter_models",
              "04_adult_occupancy", 
              "jags",
              "adult_dyn_occupancy_full.R")

#takes a long time so i'm running initial model on monsoon
Sys.time()
mod <- jagsUI::jags(data = data_list,
                    inits = NULL,
                    parameters.to.save = parms,
                    model.file = model,
                    n.chains = 3,
                    n.iter = 1,
                    parallel = TRUE,
                    DIC = TRUE)
Sys.time()

# Export ------------------------------------------------------------------

saveRDS(data_list, here("monsoon",
                        "parameter_models",
                        "adult_occupancy",
                        'inputs',
                        'all_beta',
                        "adult_occupancy_data.RDS"))

saveRDS(occ2, here("data",
                   '00_occupancy_all_vars',
                   '05_occupancy_observations_with_indexing.RDS'))


