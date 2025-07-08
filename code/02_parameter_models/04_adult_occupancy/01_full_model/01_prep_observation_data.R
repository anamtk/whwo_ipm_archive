# Occupancy data compiling
# April 20, 2023
# Ana Miller-ter Kuile

# this script combines data for the adult occupancy model


# Load packages -----------------------------------------------------------


package.list <- c("here", "tidyverse", 
                  "readxl")

## Installing them if they aren't already on the computer
new.packages <- package.list[!(package.list %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## And loading them
for(i in package.list){library(i, character.only = T)}


# Load data ---------------------------------------------------------------

#NEED
#Predictors
# local: trees, percent ponderosa, 
# landscape %ha, %bu, SHDI
# climate ppt, tmean

#combine the data for each observation at each
#point 

detections <- read_xlsx(here("data_raw",
                            "bird_data",
                            "Survey04_point_detections.xlsx"))

surveys <- read_xlsx(here('data_raw',
                          'bird_data',
                          "Survey03_point_visits.xlsx"))

transects <- read_xlsx(here('data_raw',
                            'site_data',
                            'Survey_points.xlsx'))

# Subset variables before combining ---------------------------------------

detections %>%
  filter(Period == "DU") %>%
  filter(str_detect(Visit_ID, "EM")) %>%
  tally()

#select only detections during surveys
detections1 <- detections %>%
  filter(Period == "DU") %>%
  filter(str_detect(Visit_ID, "EM")) %>%
  filter(Distance_class != "ovr_150") %>%
  distinct(Visit_ID) %>%
  #set all these to presence == 1
  mutate(presence = 1)

surveys1 <- surveys %>%
  filter(str_detect(Visit_ID, "EM"))

# Combine surveys and detections ------------------------------------------

obs <- surveys1 %>%
  left_join(detections1, by = "Visit_ID") %>%
  #set all other presence to == 0
  mutate(presence = case_when(presence == 1 ~ 1,
                              TRUE ~ 0)) %>%
  #combine treatment categories
  mutate(Trt_50 = case_when(`Trt_Type-50` %in% c("HT", "T",
                                                 "OA", "OAT",
                                                 "HTOA", "OAHT",
                                                 "H", "TH", "OT",
                                                 "THT") ~ "H",
                            `Trt_Type-50` %in% c( "OB",
                                                  "B") ~ "B",
                            `Trt_Type-50` %in% c("HTB", "TB", "OATB",
                                                 "HTOAB", "OBHT",
                                                 "OBT", "OBHTB", "OAHTB",
                                                 "OHTB", "OAB", "OJB",
                                                 "HB", "THB", "OHB",
                                                 "HTOB", "OBH", "THTB") ~ "HB",
                            `Trt_Type-50` == "U" ~ "U",
                            TRUE ~ NA_character_)) %>%
  mutate(Trt_150 = case_when(`Trt_Type-150` %in% c("HT", "T",
                                                 "OA", "OAT",
                                                 "HTOA", "OAHT",
                                                 "H", "TH", "OT",
                                                 "THT") ~ "H",
                            `Trt_Type-150` %in% c( "OB",
                                                  "B") ~ "B",
                            `Trt_Type-150` %in% c("HTB", "TB", "OATB",
                                                 "HTOAB", "OBHT", "OTB",
                                                 "OBT", "OBHTB", "OAHTB",
                                                 "OHTB", "OAB", "OJB",
                                                 "HB", "THB", "OHB", "TBH",
                                                 "HTOB", "OBH", "THTB",
                                                 'HTBOA', 'HTOAT', 'OBHTOA', 
                                                 'OBHTOAB', 'TBHB') ~ "HB",
                            `Trt_Type-150` == "U" ~ "U",
                            TRUE ~ NA_character_)) %>%
  dplyr::select(Visit_ID, Point_ID, Survey_Date, Visit_no,
                Survey_length, Observer, presence, Trt_50, Trt_150)
  

# Combine transect IDs ----------------------------------------------------

transects2 <- transects %>%
  dplyr::select(Point_ID, Transect_ID) %>%
  filter(str_detect(Transect_ID, "EM"))

obs2 <- obs %>%
  left_join(transects2, by = "Point_ID") %>%
  separate(Survey_Date,
           into = c("Year", "month", "day"),
           sep = "-",
           remove = F) #%>%
  #bc climate data is messed up, for now filtering out 2021
  #filter(Year != 2021)

write.csv(obs2, here("data",
                     "00_occupancy_all_vars",
                     "01_occupancy_observations.csv"),
          row.names = F)

obs3 <- obs2 %>%
  filter(Transect_ID != "EMPAID_CW-F") %>%
  filter(!is.na(Transect_ID))

write.csv(obs3, here("data",
                     '01_parameter_model_inputs',
                     "04_adult_occupancy",
                     "01_occupancy_observations.csv"),
          row.names = F)



#END SCRIPT