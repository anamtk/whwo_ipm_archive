# Combine all data for N-occupancy model
# April 21, 2023

# this script combines observation, local, landscape, and climate
# data for the occupancy model for the IPM


# Load packages -----------------------------------------------------------

package.list <- c("here", "tidyverse", 
                  "readxl")

## Installing them if they aren't already on the computer
new.packages <- package.list[!(package.list %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## And loading them
for(i in package.list){library(i, character.only = T)}


# Load datasets -----------------------------------------------------------

obs <- read.csv(here("data",
                     "00_occupancy_all_vars",
                     "01_occupancy_observations.csv"))

climate <- read.csv(here("data",
                     "00_occupancy_all_vars",
                     "02_occupancy_climateseasonal.csv"))

land <- read.csv(here("data",
                     "00_occupancy_all_vars",
                     "04_occupancy_landscape.csv"))


# Combine datasets --------------------------------------------------------

treatment_data <- obs %>%
  dplyr::select(Transect_ID, Point_ID, Year, Trt_150) %>%
  mutate(Trt_150 = case_when((Point_ID == "EMFWOR_RIMT1-04" &
                                Year == "2020") ~ "HB",
                             TRUE ~ Trt_150)) %>%
  distinct(Transect_ID,Point_ID, Year, Trt_150) %>%
  mutate(Year = as.factor(Year)) %>%
  group_by(Point_ID, Transect_ID) %>%
  complete(Year) %>%
  fill(Trt_150, .direction = "up") %>%
  ungroup() %>%
  filter(Transect_ID != "EMPAID_CW-F") %>%
  mutate(Year = as.numeric(as.character(Year)))

covariate_data <- treatment_data %>%
  left_join(climate, (by = c("Transect_ID", "Year"))) %>%
  full_join(land, by = c("Point_ID" = "POINT_ID", "Year")) %>%
  filter(Transect_ID != "EMPAID_CW-F") %>%
  filter(!is.na(Transect_ID))

write.csv(covariate_data,here("data",
               "01_parameter_model_inputs",
               "04_adult_occupancy",
               "n_occ_covariates.csv"))
