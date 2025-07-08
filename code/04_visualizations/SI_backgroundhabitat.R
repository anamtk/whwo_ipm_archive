# Load packages -----------------------------------------------------------

package.list <- c("here", "tidyverse", 
                  "patchwork")

## Installing them if they aren't already on the computer
new.packages <- package.list[!(package.list %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## And loading them
for(i in package.list){library(i, character.only = T)}

theme_set(theme_bw())


# Load datasets -----------------------------------------------------------

#what are the distributions in each stage for habitat and restoration
#variables? Are they more/less important in management-relevant ways?
#(e.g., are they more important when restoration is more common
#on the landscape???)

nest_data <- read.csv(here("data",
                           "03_ipm_data_prep",
                           "Egg_nestling_survival_data.csv")) %>%
  mutate(Forest = case_when(Project_ID == "EMFWOR" ~ "Fremont-Winema",
                            Project_ID == "EMMAOR" ~ "Malheur",
                            Project_ID == "EMPAID" ~ "Payette"))


# Eggs --------------------------------------------------------------------

egg_data <- nest_data %>%
  filter(!is.na(No_eggs)) %>%
  filter(!is.na(No_young)) 

txe <- egg_data %>%
  group_by(Trt_cat, Forest) %>%
  tally()

txcate <- ggplot(txe, aes(x = Trt_cat, y = n)) +
  geom_bar(stat = "identity") +
  facet_wrap(~Forest) +
  labs(x = "Treatment category", y ="Number of nests")

egcv <- ggplot(egg_data, aes(x = Forest, y = a1000_areacv2)) +
  geom_boxplot() +
  labs(x = "Forest population", 
       y = "Coefficient of variation in closed-forest patch size")

egt <- ggplot(egg_data, aes(x = Forest, y = Tmax_eg)) +
  geom_boxplot()+
  labs(x = "Forest population", 
       y = "Nesting season maximum temperature")

egp <- ggplot(egg_data, aes(x = Forest, y = PPT_eg)) +
  geom_boxplot()+
  labs(x = "Forest population", 
       y = "Nesting season cumulative precipitation")

txcate + egcv + egt + egp

ggsave(here('pictures',
            'R',
            "SI",
            'Figure_eggcovariatedists.png'),
       height = 4.5,
       width = 6,
       units = "in")

#Maybe - in dryer habitats with more habitat patchiness (EMMAOR),
#treatment and habiat matter more than in wetter areas with
#less heterogeneity in forest patch size??


# Nestlings ---------------------------------------------------------------

nestling_data <- nest_data %>%
  filter(!is.na(No_young)) %>%
  filter(!is.na(NoFL_uncert)) 
#HB
#%B
#max temp
#PPT
#nest ht
#nest init

txn <- nestling_data %>%
  group_by(Trt_cat, Project_ID, Forest) %>%
  tally()

txcatn <- ggplot(txn, aes(x = Trt_cat, y = n)) +
  geom_bar(stat = "identity") +
  facet_wrap(~Forest) +
  labs(x = "Treatment category",
       y = "Number of nests")

brn <- ggplot(nestling_data, aes(x = Forest, y = a1000_RxBu)) +
  geom_boxplot() +
  labs(x = "Forest",
       y = "Percent burned")
#burn matters more when it's rare on the landscape?
ht <- ggplot(nestling_data, aes(x = Forest, y = Nest_Ht)) +
  geom_boxplot()+
  labs(x = "Forest",
       y = "Nest height (m)")

day <- ggplot(nestling_data, aes(x = Forest, y = Init_day)) +
  geom_boxplot()+
  labs(x = "Forest",
       y = "Nest initiation date")

net <- ggplot(nestling_data, aes(x = Forest, y = Tmax_ne)) +
  geom_boxplot()+
  labs(x = "Forest",
       y = "Maximum temperature")

nep <- ggplot(nestling_data, aes(x = Forest, y = PPT_ne)) +
  geom_boxplot()+
  labs(x = "Forest",
       y = "Precipitation")

txcatn + brn + ht + day + net + nep

#txcatn + brn + net + nep

ggsave(here('pictures',
            'R',
            "SI",
            'Figure_nestlcovariatedists.png'),
       height = 4.5,
       width = 6,
       units = "in")

#tx more important when it's rare on the landscape??
#and or in cooler, drier areas? fremont - coolest, driest that 
#time of year and also has strongest tx effect with least
#tx on landscape

#habitat more important in payette where it's hot and warm?


# Adult -------------------------------------------------------------------

adult <- read.csv(here("data",
                            "01_parameter_model_inputs",
                            "04_adult_occupancy",
                            "n_occ_covariates.csv")) %>%
  mutate(Project_ID = str_sub(Transect_ID, 1, 6))

ha <- ggplot(adult, aes(x = Project_ID, y = Ha)) +
  geom_boxplot()

wtp <- ggplot(adult, aes(x = Project_ID, y = PPT_wt)) +
  geom_boxplot()

stm <- ggplot(adult, aes(x = Project_ID, y = Tave_sp)) +
  geom_boxplot()

wtm <- ggplot(adult, aes(x = Project_ID, y = Tave_wt)) +
  geom_boxplot()

can <- ggplot(adult, aes(x = Project_ID, y = low_canopy)) +
  geom_boxplot()

ha + can + wtp + stm + wtm

ggsave(here('pictures',
                'R',
                "SI",
                'Figure_adultcovariatedists.png'),
       height = 4.5,
       width = 6,
       units = "in")

#low canopy more important when it's more common
#maor, then emfwor have highest

#tx more important when it's more common (empaid has more ha)

#%ha
#winter PPT
#spring temp
#winter temp
#%low canopy

adult %>%
  mutate(Year = factor(Year)) %>%
ggplot(aes(x = Year, y = low_canopy)) +
  geom_boxplot() +
  facet_grid(~Project_ID)

adult %>%
  mutate(Year = factor(Year)) %>%
  ggplot(aes(x = Year, y = Ha)) +
  geom_boxplot() +
  facet_grid(~Project_ID) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

adult %>%
  mutate(Year = factor(Year)) %>%
  ggplot(aes(x = Year, y = PPT_wt)) +
  geom_boxplot() +
  facet_grid(~Project_ID) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

wtyear <- adult %>%
  mutate(Year = factor(Year)) %>%
  ggplot(aes(x = Year, y = Tave_wt)) +
  geom_boxplot() +
  facet_grid(~Project_ID) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

spyear <- adult %>%
  mutate(Year = factor(Year)) %>%
  ggplot(aes(x = Year, y = Tave_sp)) +
  geom_boxplot() +
  facet_grid(~Project_ID) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

wtyear / spyear

ggsave(here('pictures',
            'R',
            "SI",
            'Figure_yearlyclim.png'),
       height = 4.5,
       width = 6,
       units = "in")
# Summary -----------------------------------------------------------------

#EGGS:
#Maybe - in dryer habitats with more habitat patchiness (EMMAOR),
#treatment and habiat matter more than in wetter areas with
#less heterogeneity in forest patch size??

#NESTLINGS:
#tx more important when it's rare on the landscape??
#and or in cooler, drier areas? fremont - coolest, driest that 
#time of year and also has strongest tx effect with least
#tx on landscape
#habitat more important in payette where it's hot and warm?

#ADULTS: 
#low canopy more important when it's more common
#emmaor, then emfwor have highest
#tx more important when it's more common (empaid has more ha)/
#and or in hotter spring/wetter winter areas

