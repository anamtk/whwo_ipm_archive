
# Load packages -----------------------------------------------------------

package.list <- c("here", "tidyverse")

## Installing them if they aren't already on the computer
new.packages <- package.list[!(package.list %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## And loading them
for(i in package.list){library(i, character.only = T)}

# Load data ---------------------------------------------------------------

nest_data <- read.csv(here("data",
                           "03_ipm_data_prep",
                           "Egg_nestling_survival_data.csv"))

occ <- read.csv(here("data",
                     '01_parameter_model_inputs',
                     "04_adult_occupancy",
                     "01_occupancy_observations.csv"))

covariates <- read.csv(here("data",
                            "01_parameter_model_inputs",
                            "04_adult_occupancy",
                            "n_occ_covariates.csv"))
# Prep data ---------------------------------------------------------------

nest_sum <- nest_data %>%
  group_by(Project_ID, Year_located) %>%
  tally()

#Summary:
#EMFWOR has nests from 2015-2021 with no gaps
#EMMAOR has nests from 2014-2021 with no nests in
## 2018 and 2020 - do we assume the 2020 is missing production?
## it seems the 2018 corresponds to no data for surveys of adults
## and adult surveys not in this dataset after 2020, even though
## nests up until 2021 - perhaps deal with this with missing data
#EMPAID has nests from 2012-2019 with no missing years

#look at which years for each transect
occ_sum1 <- occ %>%
  distinct(Year, Point_ID, Transect_ID) %>%
  mutate(Project_ID = str_sub(Point_ID, 1, 6)) %>%
  group_by(Transect_ID, Project_ID, Year) %>%
  tally() 
  
#look at how many years per each transect
occ_sum2 <- occ %>%
  distinct(Year, Point_ID, Transect_ID) %>%
  mutate(Project_ID = str_sub(Point_ID, 1, 6)) %>%
  group_by(Transect_ID, Project_ID, Year) %>%
  tally() %>%
  group_by(Transect_ID, Project_ID) %>%
  tally()

#EMFWOR - all transects were surveyed continuously 2015-2020 (2021)
#EMMAOR - all transects surveyed 2014-2019, but missing 2018
#EMPAID - some transects surveyed 2012 - 2019
## others 2013- 2019

# Explore important variables ---------------------------------------------

#how have the things that matter for each stage changed through 
#time on the landscape? This is where having Jon's input on 
#gridded data will be helpful

#looks like in EMPAID is only obvious shift from fewer U
# and more H/HB nests
nest_data %>%
  group_by(Year_located, Project_ID, Trt_cat) %>%
  tally() %>%
  mutate(Year_located = as.factor(Year_located)) %>%
  ggplot(aes(x = Trt_cat, y = n, fill = Year_located)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ Project_ID)
  
#no clear patterns...
nest_data %>%
  mutate(Year_located = as.numeric(as.factor(Year_located))) %>%
  ggplot(aes(x = Year_located, y = PPT_eg, color = Project_ID)) +
  geom_point() +
  geom_smooth(method = 'lm', se = F)

#no clear paterns
nest_data %>%
  mutate(Year_located = as.numeric(as.factor(Year_located))) %>%
  ggplot(aes(x = Year_located, y = Tmax_eg, color = Project_ID)) +
  geom_point() +
  geom_smooth(method = 'lm', se = F)

#an increase in the amount of harvest around a nest over years
nest_data %>%
  mutate(Year_located = as.numeric(as.factor(Year_located))) %>%
  ggplot(aes(x = Year_located, y = a1000_Ha, color = Project_ID)) +
  geom_point() +
  geom_smooth(method = 'lm', se = F)

mod1 <- lm(a1000_Ha ~ Year_located*Project_ID,
           data = nest_data)
#no difference by forst, but clear increase over time
summary(mod1)

#an incresae in how much burn occured around nest
#with clearest in payette
nest_data %>%
  mutate(Year_located = as.numeric(as.factor(Year_located))) %>%
  ggplot(aes(x = Year_located, y = a1000_RxBu, color = Project_ID)) +
  geom_point() +
  geom_smooth(method = 'lm', se = F)

mod2 <- lm(a1000_RxBu ~ Year_located*Project_ID,
           data = nest_data)
#no clear overall trend of increased Rx burn, but
# sig difference in relationship at Payette over years
# compared to other two forests
summary(mod2)


#maybe fremont has later initiation days throughout series
nest_data %>%
  mutate(Year_located = as.numeric(as.factor(Year_located))) %>%
  ggplot(aes(x = Year_located, y = Init_day, color = Project_ID)) +
  geom_point() +
  geom_smooth(method = 'lm', se = F)

mod3 <- lm(Init_day ~ Year_located*Project_ID,
           data = nest_data)
#increased initiaion day through time with clear differences
#between fremont and other two forests - fremont has positive,
#other two have negative
summary(mod3)

#no clear pattern
nest_data %>%
  mutate(Year_located = as.numeric(as.factor(Year_located))) %>%
  ggplot(aes(x = Year_located, y = Nest_Ht, color = Project_ID)) +
  geom_point() +
  geom_smooth(method = 'lm', se = F)

#no clear trends in how man ysmall trees around nests
nest_data %>%
  mutate(Year_located = as.numeric(as.factor(Year_located))) %>%
  ggplot(aes(x = Year_located, y = Trees_2550, color = Project_ID)) +
  geom_point() +
  geom_smooth(method = 'lm', se = F)

#maybe some trends here?? with opposite on payette vs. 
#fremont-winema, malheur
nest_data %>%
  mutate(Year_located = as.numeric(as.factor(Year_located))) %>%
  ggplot(aes(x = Year_located, y = a1000_pland2, color = Project_ID)) +
  geom_point() +
  geom_smooth(method = 'lm', se = F)

mod4 <- lm(a1000_pland2 ~ Year_located*Project_ID,
           data = nest_data)
#payette has different trend than other two forests - being
#positive through time vs. no clear trend
summary(mod4)

#no clear patterns
nest_data %>%
  mutate(Year_located = as.numeric(as.factor(Year_located))) %>%
  ggplot(aes(x = Year_located, y = PPT_ne, color = Project_ID)) +
  geom_point() +
  geom_smooth(method = 'lm', se = F)

#no clear patterns
nest_data %>%
  mutate(Year_located = as.numeric(as.factor(Year_located))) %>%
  ggplot(aes(x = Year_located, y = Tmax_ne, color = Project_ID)) +
  geom_point() +
  geom_smooth(method = 'lm', se = F)
