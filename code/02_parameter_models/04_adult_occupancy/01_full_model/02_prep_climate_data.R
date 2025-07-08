# Occupancy data compiling
# April 20, 2023
# Ana Miller-ter Kuile

# this script combines data for the adult occupancy model

# Load packages -----------------------------------------------------------


package.list <- c("here", "tidyverse", 
                  "readxl", "GGally")

## Installing them if they aren't already on the computer
new.packages <- package.list[!(package.list %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## And loading them
for(i in package.list){library(i, character.only = T)}


source(here("code",
            "00_functions",
            "tidy_functions.R"))
# Load data ---------------------------------------------------------------


#load climate dtaa
climate <- read.csv(here("data_raw",
                         "climate_data",
                         "CFLRP_ClimateNA_data_transects_nests_2011-2021.csv"))



# Select needed climate data ----------------------------------------------

climate1 <- climate %>%
  dplyr::select(TransID, ClimateYear, 
                Tave_wt, Tave_sp, PPT_wt, PPT_sp, 
                Tave04, Tave05, Tave06,
                Tave07, Tave08, PPT04, PPT05, PPT06, PPT07,
                PPT08) %>%
  pivot_longer(Tave04:PPT08,
               names_to = "measurement_month",
               values_to = "value") %>%
  mutate(measurement = str_sub(measurement_month, start = 1, end = -3),
         month = str_sub(measurement_month, start = -2, end = length(measurement_month))) %>%
  mutate(month = as.numeric(month)) %>%
  dplyr::select(-measurement_month) %>%
  pivot_wider(names_from = "measurement",
              values_from = "value") %>%
  mutate(TransID = case_when(TransID %in% c("EMPAID_A",
                                            "EMPAID_B",
                                            "EMPAID_D",
                                            "EMPAID_E",
                                            "EMPAID_G") ~ fun_insert(x = TransID,
                                                                     pos = 7,
                                                                     insert = "CW-"),
                             TransID %in% c("EMPAID_H",
                                            "EMPAID_I",
                                            "EMPAID_J") ~ fun_insert(x = TransID,
                                                                     pos = 7,
                                                                     insert = "RB-"),
                             TransID %in% c("EMPAID_K",
                                            "EMPAID_L",
                                            "EMPAID_M",
                                            "EMPAID_N") ~ fun_insert(x = TransID,
                                                                     pos = 7,
                                                                     insert = "WRF-"),
                             TransID %in% c("EMPAID_P",
                                            "EMPAID_Q",
                                            "EMPAID_R") ~ fun_insert(x = TransID,
                                                                     pos = 7,
                                                                     insert = "LC-"),
                             TransID %in% c("EMPAID_S",
                                            "EMPAID_T",
                                            "EMPAID_U") ~ fun_insert(x = TransID,
                                                                     pos = 7,
                                                                     insert = "B-"),
                             TransID %in% c("EMPAID_V",
                                            "EMPAID_W") ~ fun_insert(x = TransID,
                                                                     pos = 7,
                                                                     insert = "MFW-"),
                             TRUE ~ TransID)) %>%
  rename("Transect_ID" = "TransID",
         "Year" = "ClimateYear")

# write.csv(climate1, here("data",
#                      "00_occupancy_all_vars",
#                      "02_occupancy_climate.csv"),
#           row.names = F)


# Correlation check -------------------------------------------------------


climate2 <- climate1 %>%
  distinct(Transect_ID, Year, Tave_wt, Tave_sp, PPT_wt, PPT_sp) 

#looks okay to have both winter and spring temp,
#but that winter and spring ppt are somewhat correlated - we'll see
climate2 %>%
  dplyr::select(Tave_wt, Tave_sp, PPT_wt, PPT_sp) %>%
  ggpairs()

write.csv(climate2, here("data",
                         "00_occupancy_all_vars",
                         "02_occupancy_climateseasonal.csv"),
          row.names = F)
#END SCRIPT