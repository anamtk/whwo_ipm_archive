#Summary stats for occupancy model
#Ana Miller-ter Kuile
#June 26, 2023

#This script takes the summary of the ocupancy
#model and figures out which vraiables need to be 
#included in the IPM based on p-values


# Load packages ---------------------------------------------------------------

# Load packages, here and tidyverse for coding ease, 
package.list <- c("here", "tidyverse", 'patchwork') 


## Installing them if they aren't already on the computer
new.packages <- package.list[!(package.list %in% 
                                 installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## And loading them
for(i in package.list){library(i, character.only = T)}


# Load data ---------------------------------------------------------------

mod <- readRDS(here("monsoon",
                    "parameter_models",
                    "adult_occupancy",
                    "outputs",
                    "adult_occupancy_zsum3.RDS"))

modsum <- readRDS(here('monsoon',
                       'parameter_models',
                       'adult_occupancy',
                       'outputs',
                       'adult_occupancy_sum3.RDS'))


# Pull out stats for p-values ---------------------------------------------

means <- as.data.frame(mod$statistics) %>%
  rownames_to_column(var = "parameter") %>%
  filter(str_detect(parameter, "z")) %>%
  dplyr::select(parameter, Mean) %>%
  mutate(p = case_when(Mean >= 0.5 ~ (1-Mean), #I Think these are 1-tailed p-values but check with Kiona
                       Mean < 0.5 ~ (1 - (1-Mean))))

#persistence
#treatment category 3 a1[3]
#a2, a3, a5,  a8
#Tmean sp, Tmean wt,  PPT wt, LowCC


# Parameter directions ----------------------------------------------------

meds_pers <- as.data.frame(modsum$quantiles) %>%
  rownames_to_column(var = "parameter")  %>%
  filter(!str_detect(parameter, "eps|sig|b|z|gamma")) %>%
  filter(!parameter %in% c("e0", "e", "deviance")) %>%
  filter(!parameter %in% c("b1TrtID[1]", "a1TrtID[1]", "a0")) %>%
  mutate(parameter = case_when(parameter == "a[2]" ~ "Tmean_sp",
                               parameter == "a[3]" ~ "Tmean_wt",
                               parameter == "a[4]" ~ "PPT_sp",
                               parameter == "a[5]" ~ "PPT_wt",
                               parameter == "a[6]" ~ "LandBu",
                               parameter == "a[7]" ~ "LandHa",
                               parameter == "a[8]" ~ "LowCC",
                               parameter == "a[9]" ~ "HighCC",
                               TRUE ~ NA_character_)) %>%
  #a2, a3, a5,  a8
  #Tmean sp, Tmean wt,  PPT wt, LowCC
  mutate(sig = case_when(parameter %in% c('Tmean_wt', 
                                          'Tmean_sp', 
                                          'PPT_wt',
                                          'LowCC') ~ "sig",
                         TRUE ~ "notsig"))

theme_set(theme_bw())

(b <- ggplot(meds_pers)  +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_pointrange(aes(x = reorder(parameter, `50%`), y = `50%`,
                      ymin = `2.5%`, ymax = `97.5%`,
                      color = sig)) +
  scale_color_manual(values = c("black", "#225ea8") )+ 
  labs(x = "Covariate", y = "Median and 95% BCI") +
  coord_flip() +
  theme(legend.position = "none"))

ggsave(here('pictures',
            'R',
            'SI',
            'Figure_occupancyresults.png'),
       height = 3.5,
       width = 6,
       units = "in")
