#Background point prep
#Ana Miller-ter Kuile
#January 7, 2025

#this script preps the background point data for the model

# Load packages -----------------------------------------------------------

package.list <- c("here", "tidyverse")

## Installing them if they aren't already on the computer
new.packages <- package.list[!(package.list %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## And loading them
for(i in package.list){library(i, character.only = T)}

# Read in data ------------------------------------------------------------

land <- read.csv(here('data_raw',
                       'landscape_data',
                       'background_points',
                       'EMMAOR_SCIbased_IPMdata_backgroundpts_CCandFACTS_ThinData.csv'))

clim <- read.csv(here('data_raw',
                            'climate_data',
                            'background_points',
                            'IPM_backgroundpts_climatedata_2000-2023_n12061_reduced_variables_MAL.csv'))


# Prep variables ----------------------------------------------------------

#neeed these varaibles, and in a format
#where row is point ID, and columns for each year in the dataset
#Tmean_sp[i,t] 
#Tmean_wt[i, t] 
#PPT_wt[i,t] 
#LandHa[i,t] 
#LowCC[i,t] 

# Clean up landscape variables --------------------------------------------

#LandHa and LowCC @ 1km

LowCC <- land %>%
  dplyr::select(ID,
                sci11_1040_1km:sci21_1040_1km) %>%
  pivot_longer(sci11_1040_1km:sci21_1040_1km,
               names_to = "year",
               values_to = "LowCC") %>%
  mutate(year = str_sub(year, 4, 5)) %>%
  mutate(year = paste0("20", year, sep = "")) %>%
  mutate(year = as.numeric(year))%>%
  mutate(ID = str_replace(ID, "-", "_"))

LandHa <- land %>%
  dplyr::select(ID,
                Thin0211:Thin0223) %>%
  pivot_longer(Thin0211:Thin0223,
               names_to = "year",
               values_to = "LandHa") %>%
  mutate(year = str_sub(year, 7, 8)) %>%
  mutate(year = paste0("20", year, sep = "")) %>%
  mutate(year = as.numeric(year)) %>%
  mutate(ID = str_replace(ID, "-", "_"))


# Climate variables -------------------------------------------------------

#Tmean_sp[i,t] 
#Tmean_wt[i, t] 
#PPT_wt[i,t] 
  
Tmean_sp <- clim %>%
  dplyr::select(ID_Pt, YR, Tave_sp) %>%
  rename(year = YR,
         ID = ID_Pt)

Tmean_wt <- clim %>%
  dplyr::select(ID_Pt, YR, Tave_wt) %>%
  rename(year = YR,
         ID = ID_Pt)

PPT_wt <- clim %>%
  dplyr::select(ID_Pt, YR, PPT_wt) %>%
  rename(year = YR,
         ID = ID_Pt) 

# Combine into tidy DF ----------------------------------------------------

df <- purrr::reduce(list(LowCC,
                   LandHa,
                   Tmean_sp,
                   Tmean_wt, 
                   PPT_wt), dplyr::left_join, by = c("ID", "year"))


write.csv(df,here("data",
               "03_ipm_data_prep",
               "adult_bkgrnd_point_covariates_emmaor.csv"))





