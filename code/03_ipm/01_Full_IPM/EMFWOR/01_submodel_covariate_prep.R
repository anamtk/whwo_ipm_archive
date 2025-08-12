#Submodel covariate compilations
#June 13, 2023

#this script compiles all the covariate data for
#point-level variables for each of the submodels in the IPM
#and generates the indexing vectors for the egg and nestling
#survival models

# Load packages -----------------------------------------------------------

package.list <- c("here", "tidyverse")

## Installing them if they aren't already on the computer
new.packages <- package.list[!(package.list %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## And loading them
for(i in package.list){library(i, character.only = T)}

# General things right now ------------------------------------------------

#start with just one forest
#Summary:
#EMFWOR:
## Nests: 2015-2021 with no gaps
## Adults: 2015-2021 with no gaps
#EMMAOR:
## Nests: 2014-2021 with no nests in 2018 and 2020
## Adults: all transects surveyed 2014-2019, but missing 2018
#EMPAID:
## Nests: 2012-2019 with no gaps
## Adults: some transects surveyed 2012-2019, others 2013-2019

# Nest Portions -----------------------------------------------------------

#prep for nest portions

# Load data ---------------------------------------------------------------

nest_data <- read.csv(here("data",
                           "03_ipm_data_prep",
                           "Egg_nestling_survival_data.csv"))
# Prep data ---------------------------------------------------------------

nest_data2 <- nest_data %>%
  filter(Project_ID == "EMFWOR") %>%
  #filter(Year_located != 2021) %>%
  arrange(Year_located)%>%
  mutate(num = 1:n())

# Indexing for both egg and nestling --------------------------------------

nest.start <- nest_data2 %>%
  group_by(Year_located) %>%
  mutate(min = min(num)) %>%
  ungroup() %>%
  distinct(Year_located, min) %>%
  dplyr::select(min) %>%
  as_vector()

#to deal with missing data
#nest.start <- c(rep(NA, 3), nest.start)

nest.end <- nest_data2 %>%
  group_by(Year_located) %>%
  mutate(max = max(num)) %>%
  ungroup() %>%
  distinct(Year_located, max) %>%
  dplyr::select(max) %>%
  as_vector()

#to deal with missing data
#nest.end <- c(rep(NA, 3),
 #              nest.end)

# n.years <- nest_data2 %>%
#   distinct(Year_located) %>%
#   tally() %>%
#   as_vector()

n.nests <- nrow(nest_data2)

surveyed <- nest_data2 %>%
  distinct(Year_located) %>%
  #no missing years
  mutate(surveyed = 1) %>%
  dplyr::select(surveyed) %>%
  as_vector()

# Covariates for egg and nestling -----------------------------------------

TreatmentID <- nest_data2 %>%
  mutate(Trt_cat = factor(Trt_cat, levels = c("U", "H", "B", "HB"))) %>%
  dplyr::select(Trt_cat) %>%
  mutate(Trt_cat = as.numeric(as.factor(Trt_cat))) %>%
  as_vector() 

unique(nest_data2$Tree_sp)

#EMFWOR only has PIPO and POTR5
SpeciesID <- nest_data2 %>%
  mutate(Tree_sp = factor(Tree_sp, levels = c("PIPO", "POTR5"))) %>%
  dplyr::select(Tree_sp) %>%
  mutate(Tree_sp = as.numeric(as.factor(Tree_sp))) %>%
  as_vector() 

#egg covariates
#LandHa <- as.vector(scale(nest_data2$a1000_Ha))
PPT_eg <- as.vector(scale(nest_data2$PPT_eg))
Tmax_eg <- as.vector(scale(nest_data2$Tmax_eg))
ForestCV <- as.vector(scale(nest_data2$a1000_areacv2))

#nestling covariates
LandBu <- as.vector(scale(nest_data2$a1000_RxBu))
InitDay <- as.vector(scale(nest_data2$Init_day))
NestHt <- as.vector(scale(nest_data2$Nest_Ht))
# Trees2550 <- as.vector(scale(nest_data2$Trees_2550))
# PercForest <- as.vector(scale(nest_data2$a1000_pland2))
PPT_ne <- as.vector(scale(nest_data2$PPT_ne))
Tmax_ne <- as.vector(scale(nest_data2$Tmax_ne))

# Adult Portions ----------------------------------------------------------

#have covariates for initial occupancy and for survival after year 1

# Load data ---------------------------------------------------------------

adult_covs <- read.csv(here("data",
                       "03_ipm_data_prep",
                       "adult_bkgrnd_point_covariates_emfwor.csv"))


adult <- adult_covs %>%
  filter(year %in% c(2015:2021))

n.points <- adult %>%
  distinct(ID) %>%
  tally() %>%
  as_vector()

# a[1]*Tmean_wt[i, 1] +
#   a[2]*PPT_sp[i, 1] +
#   a[3]*PPT_wt[i, 1] +
#   a[4]*LandHa[i, 1] +
#   a[5]*HighCC[i, 1]

#persistence across years will be a matrix of 
#values for all years from the first to the last for each point
aTmean_sp <- adult %>%
  dplyr::select(ID, Tave_sp, year) %>%
  mutate(Tave_sp = scale(Tave_sp)) %>%
  pivot_wider(names_from = year,
              values_from=Tave_sp) %>%
  column_to_rownames(var = 'ID') %>%
  as.matrix()

aTmean_wt <- adult %>%
  dplyr::select(ID, Tave_wt, year) %>%
  mutate(Tave_wt = scale(Tave_wt)) %>%
  pivot_wider(names_from = year,
              values_from=Tave_wt) %>%
  column_to_rownames(var = 'ID') %>%
  as.matrix()

aPPT_wt <- adult %>%
  dplyr::select(ID, PPT_wt, year) %>%
  mutate(PPT_wt = scale(PPT_wt)) %>%
  pivot_wider(names_from = year,
              values_from=PPT_wt) %>%
  column_to_rownames(var = 'ID') %>%
  as.matrix()

aLandHa <- adult %>%
  dplyr::select(ID, LandHa, year) %>%
  mutate(LandHa = scale(LandHa)) %>%
  pivot_wider(names_from = year,
              values_from = LandHa) %>%
  column_to_rownames(var = 'ID') %>%
  as.matrix()

aLowCC <- adult %>%
  dplyr::select(ID, LowCC, year) %>%
  mutate(LowCC = scale(LowCC)) %>%
  pivot_wider(names_from = year,
              values_from = LowCC) %>%
  column_to_rownames(var = 'ID') %>%
  as.matrix()

# Random points -----------------------------------------------------------
#update later with actual random point data - this is juts
#to test model
#n.grids <- 10

# a0gPPT_sp <- adult %>%
#   filter(Year == 2015) %>%
#   dplyr::select(PPT_sp) %>%
#   mutate(PPT_sp = scale(PPT_sp)) %>%
#   top_n(10) %>%
#   as_vector() 
# 
# a0gSHDI <- adult %>%
#   filter(Year == 2015) %>%
#   dplyr::select(SHDI) %>%
#   mutate(SHDI = scale(SHDI)) %>%
#   top_n(10) %>%
#   as_vector()
# 
# a0gHighCC <- adult %>%
#   filter(Year == 2015) %>%
#   dplyr::select(high_canopy) %>%
#   mutate(high_canopy = scale(high_canopy)) %>%
#   top_n(10) %>%
#   as_vector()
# 
# agPPT_wt <- adult %>%
#   dplyr::select(Point_ID, PPT_wt, Year) %>%
#   mutate(PPT_wt = scale(PPT_wt)) %>%
#   pivot_wider(names_from = Year,
#               values_from=PPT_wt) %>%
#   column_to_rownames(var = 'Point_ID') %>%
#   top_n(10) %>%
#   as.matrix()
# 
# agSHDI <- adult %>%
#   dplyr::select(Point_ID, SHDI, Year) %>%
#   mutate(SHDI = scale(SHDI)) %>%
#   pivot_wider(names_from = Year,
#               values_from=SHDI) %>%
#   column_to_rownames(var = 'Point_ID') %>%
#   top_n(10) %>%
#   as.matrix()


# Covariate posterior indexing --------------------------------------------

n.years <- length(unique(adult$year))

nest_data2 %>%
  distinct(Tree_sp)

n.species <- nest_data2 %>%
  distinct(Tree_sp) %>%
  tally() %>%
  as_vector()

n.trt <- nest_data2 %>%
  distinct(Trt_cat) %>%
  tally() %>%
  as_vector()


# Create data list --------------------------------------------------------

data <- list(#indexing variables
             n.nests = n.nests,
             nest.start = nest.start,
             nest.end = nest.end,
             n.years = n.years,
             n.points = n.points,
             n.species = n.species,
             n.trt = n.trt,
             #n.grids = n.grids,
             surveyed = surveyed,
             #nest variables
             TreatmentID = TreatmentID,
             SpeciesID = SpeciesID,
             #LandHa = LandHa,
             ForestCV = ForestCV,
             PPT_eg = PPT_eg,
             Tmax_eg = Tmax_eg,
             LandBu = LandBu,
             InitDay = InitDay,
             NestHt = NestHt,
             # Trees2550 = Trees2550,
             # PercForest = PercForest,
             PPT_ne = PPT_ne,
             Tmax_ne = Tmax_ne,
             #adult variables
             aTmean_wt = aTmean_wt, 
             aTmean_sp = aTmean_sp, 
             aPPT_wt = aPPT_wt, 
             aLandHa = aLandHa,
             aLowCC = aLowCC)

saveRDS(data, here("data",
                   "04_ipm_data_inputs",
                   "covariate_data_list_EMFWOR.RDS"))
