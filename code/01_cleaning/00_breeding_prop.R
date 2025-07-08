# Breeding ratio checks
# March 7, 2023
# Ana Miller-ter Kuile

#examination of the how many adults observed on same transects as nests


# Load packages -----------------------------------------------------------


# Load packages -----------------------------------------------------------

package.list <- c("here", "tidyverse", 
                  "readxl")

## Installing them if they aren't already on the computer
new.packages <- package.list[!(package.list %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## And loading them
for(i in package.list){library(i, character.only = T)}


# Load datasets -----------------------------------------------------------

adults <- read_xlsx(here("data_raw",
                         "bird_data",
                         "Survey04_point_detections.xlsx"))

adult_meta <- read_xlsx(here('data_raw',
                             "bird_data",
                             "Survey02_points.xlsx"))

adult_survey <- read_xlsx(here('data_raw',
                             "bird_data",
                             "Survey03_point_visits.xlsx"))

nests <- read_xlsx(here("data_raw",
                        "bird_data",
                        "Birds01_nest_locations.xlsx"))


# Select only CLFRP data from each DF -------------------------------------

#select only the CFLRP SITES
cflrp_filter <- function(df, var){
  var <- enquo(var)
  df1 <- df %>%
    #depending on the DF - will be different columsn
    #but str_detect for the three cflrp sites
    filter(str_detect(!!var, "EMFWOR|EMPAID|EMMAOR"))
  return(df1)
}

#Run this function for all dataframes
adults <- cflrp_filter(adults, Visit_ID)

adult_meta <- cflrp_filter(adult_meta, Transect_ID)

adult_survey <- cflrp_filter(adult_survey, Point_ID)

nests <- cflrp_filter(nests, Transect_ID)

# Select only important columns from each dataset -------------------------

adults2 <- adults %>%
  dplyr::select(Visit_ID) %>%
  mutate(Observed = 1)

adult_meta2 <- adult_meta %>%
  dplyr::select(Point_ID, Transect_ID)

adult_survey2 <- adult_survey %>%
  dplyr::select(Visit_ID, Point_ID, Survey_Date, Visit_no)

nests2 <- nests %>%
  dplyr::select(Point_ID, Year_located, Transect_ID) %>%
  mutate(Nest = 1,
         Year_located = as.character(Year_located)) 
  

# Join datasets -----------------------------------------------------------

adult_data <- adult_survey2 %>%
  left_join(adult_meta2, by = "Point_ID") %>%
  left_join(adults2, by = "Visit_ID") %>%
  mutate(Observed = case_when(Observed == 1 ~ 1,
                              TRUE ~ 0)) %>%
  mutate(Year_located = str_sub(Survey_Date, 1, 4)) %>%
  group_by(Point_ID, Transect_ID, Year_located) %>%
  summarise(adult_presence = sum(Observed)) %>%
  mutate(adult_presence = case_when(adult_presence > 0 ~ 1,
                                    adult_presence == 0 ~ 0,
                                    TRUE ~ NA_real_)) %>%
  #remove years where nests weren't systematically searched for
  filter(!(Year_located == 2021 & str_detect(Transect_ID, "FWOR|MAOR")))
  
  

adult_nest <- adult_data %>%
  left_join(nests2, by = c("Point_ID", "Transect_ID", "Year_located")) %>%
  mutate(Nest = case_when(Nest == 1 ~ 1,
                          TRUE ~ 0))
 

adult_nest2 <- adult_nest %>%
  filter(adult_presence == 1 | Nest == 1)

adult_all <- adult_nest2 %>%
  filter(adult_presence == 1)  %>%
  group_by(Year_located) %>%
  tally(name = "adults_all")

adult_nests <- adult_nest2 %>%
  filter((adult_presence == 1 & Nest == 1)) %>%
  group_by(Year_located) %>%
  tally(name = "adults_nests") %>%
  rowwise() %>%
  #assuming that those with nests there are always 2
  mutate(adults_nests = adults_nests*2) %>%
  ungroup()

nests_only <- adult_nest2 %>%
  filter((adult_presence == 0 & Nest == 1)) %>%
  group_by(Year_located) %>%
  tally(name = "nest_only")

breeding_prop <- adult_all %>%
  left_join(adult_nests, by= 'Year_located') %>%
  left_join(nests_only, by = "Year_located") %>%
  mutate(prop = (adults_nests+nest_only)/(adults_all+nest_only))

breeding_prop %>%
  summarise(mean = mean(prop),
            sd = sd(prop))

#2 individuals per breeding pair
breeding_prop %>%
  summarise(mean = mean(prop),
            sd = sd(prop),
            var = var(prop))

#Jon thinks that breeding pop is more like ~80%
#could incorporate some expert opinions in this value in the model...?

#mean = a/(a+b)
#var = (ab)/((a+b)^2(a+b+1))

#solve mean eqn for b:
#a = 1.7855b

#solve var eqn for b:
#b = 1.62

#thus, a = 2.88

#use these as alpha and beta in a beta distribution in population model
