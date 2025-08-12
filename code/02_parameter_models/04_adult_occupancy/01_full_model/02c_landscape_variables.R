# Prep landscape variables for adult occupancy
# April 20, 2023

# this script preps the landscape variables for occupancy at 1000 m scale


# Load packages -----------------------------------------------------------

package.list <- c("here", "tidyverse", 
                  "readxl", "GGally")

## Installing them if they aren't already on the computer
new.packages <- package.list[!(package.list %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## And loading them
for(i in package.list){library(i, character.only = T)}


# Load data ---------------------------------------------------------------

meta <- read_xlsx(here("data_raw",
                      "site_data",
                      "Survey_points.xlsx"))

emfwor <- read.csv(here("data_raw",
                        "landscape_data",
                        'EMFWOR_SCIbased_IPMdata_transects_nests_CCandFACTS_ThinData',
                        "EMFWOR_SCIbased_IPMdata_n270_transect_pts_CCandFACTS_ThinData.csv"))

emfworb <- read.csv(here("data_raw",
                        "landscape_data",
                        'LF',
                        "EMFWOR_LF-based_OccupData_n270_transect_pts.csv"))

emmaor <- read.csv(here("data_raw",
                        "landscape_data", 
                        "EMMAOR_SCIbased_IPMdata_transects_nests_CCandFACTS_ThinData",
                        "EMMAOR_SCIbased_IPMdata_n300_transect_pts_CCandFACTS_ThinData.csv"))

emmaorb <- read.csv(here("data_raw",
                         "landscape_data",
                         'LF',
                         "EMMAOR_LF-based_OccupData_n300_transect_pts.csv"))

empaid <- read.csv(here("data_raw",
                        "landscape_data", 
                        "EMPAID_SCIbased_IPMdata_transects_nests_CCandFACTS_ThinData",
                        "EMPAID_SCIbased_IPMdata_n280_transect_pts_CCandFACTS_ThinData.csv"))

empaidb <- read.csv(here("data_raw",
                         "landscape_data",
                         'LF',
                         "EMPAID_LF-based_OccupData_n280_transect_pts.csv"))

# Select columns of interest ----------------------------------------------

emfwor1 <- emfwor %>%
  dplyr::select(POINT_ID, sci12_010_1km:sci21_GT70_1km)

emfworb1 <- emfworb %>%
  dplyr::select(POINT_ID,
                Ha211_1000, Ha212_1000, Ha213_1000,
                Ha214_1000, Ha215_1000, Ha216_1000, Ha217_1000,
                Ha218_1000, Ha219_1000, Ha220_1000, Ha221_1000,
                Bu211_1000, Bu212_1000, Bu213_1000,
                Bu214_1000, Bu215_1000, Bu216_1000, Bu217_1000,
                Bu218_1000, Bu219_1000, Bu220_1000, Bu221_1000,
                X1shdi211, X1shdi212, X1shdi213, X1shdi214,
                X1shdi215, X1shdi216, X1shdi217, X1shdi218, 
                X1shdi219, X1shdi220, X1shdi221)

emmaor1 <- emmaor %>%
  dplyr::select(POINT_ID, sci12_010_1km:sci21_GT70_1km)

emmaorb1 <- emmaorb %>%
  dplyr::select(POINT_ID,
                Ha211_1000, Ha212_1000, Ha213_1000,
                Ha214_1000, Ha215_1000, Ha216_1000, Ha217_1000,
                Ha218_1000, Ha219_1000, Ha220_1000, Ha221_1000,
                Bu211_1000, Bu212_1000, Bu213_1000,
                Bu214_1000, Bu215_1000, Bu216_1000, Bu217_1000,
                Bu218_1000, Bu219_1000, Bu220_1000, Bu221_1000,
                X1shdi211, X1shdi212, X1shdi213, X1shdi214,
                X1shdi215, X1shdi216, X1shdi217, X1shdi218, 
                X1shdi219, X1shdi220, X1shdi221)

empaid1 <- empaid %>%
  dplyr::select(POINT_ID, sci12_010_1km:sci21_GT70_1km)

empaidb1 <- empaidb %>%
  dplyr::select(POINT_ID,
                Ha211_1000, Ha212_1000, Ha213_1000,
                Ha214_1000, Ha215_1000, Ha216_1000, Ha217_1000,
                Ha218_1000, Ha219_1000, Ha220_1000, Ha221_1000,
                Bu211_1000, Bu212_1000, Bu213_1000,
                Bu214_1000, Bu215_1000, Bu216_1000, Bu217_1000,
                Bu218_1000, Bu219_1000, Bu220_1000, Bu221_1000,
                X1shdi211, X1shdi212, X1shdi213, X1shdi214,
                X1shdi215, X1shdi216, X1shdi217, X1shdi218, 
                X1shdi219, X1shdi220, X1shdi221)

# Treatment categorizing --------------------------------------------------

trt_fun <- function(df){
  
  txdf <- df %>%
    dplyr::select(POINT_ID:Bu221_1000) %>%
    pivot_longer(Ha211_1000:Bu221_1000,
                 names_to = "variable_year",
                 values_to = "values") %>%
    mutate(Year = str_sub(variable_year, start = 4, end = 5),
           Year = paste("20", Year, sep = ""),
           Trt_type = str_sub(variable_year, start = 1, end = 2)) %>%
    dplyr::select(-variable_year) %>%
    pivot_wider(names_from = 'Trt_type',
                values_from = "values")
  
  divdf <- df %>%
    dplyr::select(POINT_ID, X1shdi211:X1shdi221) %>%
    pivot_longer(X1shdi211:X1shdi221,
                 names_to = "variable_year",
                 values_to = "SHDI") %>%
    mutate(Year = str_sub(variable_year, start = 8, end = 9),
           Year = paste("20", Year, sep = "")) %>%
    dplyr::select(-variable_year)
  
  totdf <- txdf %>%
    left_join(divdf, by = c("POINT_ID", "Year"))
  
  return(totdf)
  
}

emfwortx <- trt_fun(df = emfworb1)

empaidtx <- trt_fun(df = empaidb1)

emmaortx <- trt_fun(df = emmaorb1)

txall <- emfwortx %>%
  bind_rows(empaidtx, emmaortx)

# Canopy Cover Prep -------------------------------------------------------

canopy_fun <- function(df){
  df2 <- df %>%
    pivot_longer(sci12_010_1km:sci21_GT70_1km,
                 names_to = 'variable_year',
                 values_to = "values") %>%
    mutate(Year = str_sub(variable_year, start = 4, end = 5),
           Year = paste("20", Year, sep = "")) %>%
    separate(variable_year,
             into = c("Year", "canopy_class", "scale"),
             sep = "_") %>%
    mutate(Year = str_sub(Year, 4, 5),
           Year = paste("20", Year, sep = "")) %>%
    mutate(Canopy = case_when(canopy_class == "010" ~ "no_canopy",
                              canopy_class == "1040" ~ "low_canopy",
                              canopy_class == "4070" ~ "med_canopy",
                              canopy_class == "GT70" ~ 'high_canopy',
                              TRUE ~ NA_character_)) %>%
    dplyr::select(POINT_ID, Year, values, Canopy) %>%
    pivot_wider(names_from = "Canopy", 
                values_from = "values")
  
  return(df2)
}

emfwor_c1 <- canopy_fun(emfwor1) 

emmaor_c1 <- canopy_fun(emmaor1)

empaid_c1 <- canopy_fun(empaid1) 

canopy_all <- emfwor_c1 %>%
  bind_rows(emmaor_c1, empaid_c1)


# Canopy check ------------------------------------------------------------

# Canopy Cover Prep -------------------------------------------------------

canopy_fun2 <- function(df){
  df2 <- df %>%
    pivot_longer(sci12_010_1km:sci21_GT70_1km,
                 names_to = 'variable_year',
                 values_to = "values") %>%
    mutate(Year = str_sub(variable_year, start = 4, end = 5),
           Year = paste("20", Year, sep = "")) %>%
    separate(variable_year,
             into = c("Year", "canopy_class", "scale"),
             sep = "_") %>%
    mutate(Year = str_sub(Year, 4, 5),
           Year = paste("20", Year, sep = "")) %>%
    mutate(Canopy = case_when(canopy_class %in% c('010', '1040') ~ "low",
                               canopy_class %in% c('4070', 'GT70') ~ "high",
                               TRUE ~ NA_character_)) %>%
    group_by(POINT_ID, Year, Canopy) %>%
    summarise(values = sum(values, na.rm = T)) %>%
    dplyr::select(POINT_ID, Year, values, Canopy) %>%
    pivot_wider(names_from = "Canopy", 
                values_from = "values",
                values_fill = 0) %>%
    ungroup() 
  
  return(df2)
}

emfwor_c2 <- canopy_fun2(emfwor1) 

emmaor_c2 <- canopy_fun2(emmaor1)

empaid_c2 <- canopy_fun2(empaid1) 

canopy_all2<- emfwor_c2 %>%
  bind_rows(emmaor_c2, empaid_c2)

canopy_all2 %>%
  dplyr::select(high, low) %>%
  ggpairs()


# Combine -----------------------------------------------------------------

landscape_all <- txall %>%
  left_join(canopy_all, by = c("POINT_ID", "Year")) %>%
  left_join(canopy_all2, by = c("POINT_ID", "Year"))

                                    
write.csv(landscape_all, here("data",      
                     "00_occupancy_all_vars",
                     "04_occupancy_landscape.csv"),
          row.names = F)


# Check for correlation ---------------------------------------------------

landscape_all %>%
  dplyr::select(Ha, Bu, SHDI, low_canopy, med_canopy, high_canopy) %>%
  ggpairs()

#same as before - low and med canopy correlated; shdi and harvest p>0.6

#END SCRIPT