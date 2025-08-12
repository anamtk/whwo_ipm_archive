#Visualizations of sub-models and hierarchical partitioning
#June 25, 2024

#this script makes a multi-panel figure with covariate
#effects and hierarchical partitioning

# Load packages -----------------------------------------------------------

package.list <- c("here", "tidyverse", 'patchwork')

## Installing them if they aren't already on the computer
new.packages <- package.list[!(package.list %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## And loading them
for(i in package.list){library(i, character.only = T)}

theme_set(theme_classic())


# Load datasets -----------------------------------------------------------

#NOTES FROM LAB MEETING 11/6
#DAG next to the covariate effects figure. 
#Shorten covariate names
#Color bold or somethign to identify categories?
#Organize covariates by category


#treatment levels:
#"U", "H", "B", "HB"
egg_sum <- readRDS(here('data',
                        '02_parameter_model_outputs',
                        '02_egg_survival',
                        'egg_survival_parameter_summaries.RDS'))

egg_sum <- as.data.frame(egg_sum$quantiles) %>%
  rownames_to_column(var = 'parm') %>%
  filter(parm != "deviance") %>%
  filter(!str_detect(parm, 'e0')) %>%
  mutate(parm = case_when(parm == "e1TreatmentID[1]" ~ "Untreated",
                          parm == 'e1TreatmentID[2]' ~ "Stand harvested (yes/no)",
                          parm == "e1TreatmentID[3]" ~ "Stand burned (yes/no)",
                          parm == "e1TreatmentID[4]" ~ "Stand harvested & burned (yes/no)",
                          parm == "e[2]" ~ "Coefficient of variation in closed-forest patch size",
                          parm == "e[3]" ~ "Nesting season cumulative precipitation",
                          parm == "e[4]" ~ "Nesting season maximum temperature")) %>%
  filter(parm != "Untreated") %>%
  mutate(stage = "Egg") %>%
  dplyr::select(parm, `50%`, stage) %>%
  filter(!parm %in% c('Stand harvested', 'Stand harvested & burned'))

#"U", "H", "B", "HB"
#"PIPO", "POTR5", "JUOC", "PSME", "Abies"
nest_sum <- readRDS(here('data',
                         '02_parameter_model_outputs',
                         '03_nestling_survival',
                         'nestling_survival_parameter_summaries.RDS'))

nest_sum <- as.data.frame(nest_sum$quantiles) %>%
  rownames_to_column(var = 'parm') %>%
  filter(parm != "deviance") %>%
  filter(!str_detect(parm, 'n0')) %>%
  mutate(parm = case_when(parm == "n1TreatmentID[1]" ~ "Untreated",
                          parm == 'n1TreatmentID[2]' ~ "Stand harvested (yes/no)",
                          parm == "n1TreatmentID[3]" ~ "Stand burned (yes/no)",
                          parm == "n1TreatmentID[4]" ~ "Stand harvested & burned (yes/no)",
                          parm == 'n2SpeciesID[1]' ~ "Ponderosa",
                          parm == 'n2SpeciesID[2]' ~ "Aspen",
                          parm == 'n2SpeciesID[3]' ~ "Juniper",
                          parm == 'n2SpeciesID[4]' ~ "Douglas Fir",
                          parm == 'n2SpeciesID[5]' ~ "True fir",
                          parm == "n[3]" ~ "Percent landscape burned",
                          parm == "n[4]" ~ "Nest initiation day",
                          parm == "n[5]" ~ "Nest height",
                          parm == "n[6]" ~ "Nesting season cumulative precipitation",
                          parm == "n[7]" ~ "Nesting season maximum temperature",
                          parm == "n[8]" ~ "Nesting season maximum temperature ^2"))%>%
  mutate(parm = case_when(parm == "Aspen" ~ "Nest tree species",
                          TRUE ~ parm)) %>%
  filter(!parm %in% c("Untreated", "Ponderosa")) %>%
  mutate(stage = "Nestling") %>%
  dplyr::select(parm, `50%`, stage)%>%
  filter(!parm %in% c('Stand harvested', 'Stand burned'))

adult_sum <- readRDS(here('data',
                          '02_parameter_model_outputs',
                          '04_adult_occupancy',
                          'adult_parameter_summaries.RDS'))

adult_sum <- as.data.frame(adult_sum$quantiles) %>%
  rownames_to_column(var = 'parm') %>%
  filter(parm != "deviance") %>%
  filter(!str_detect(parm, 'a0')) %>%
  mutate(parm = case_when(parm == "a[1]" ~ "Spring mean temperature",
                          parm == "a[2]" ~ "Winter mean temperature",
                          parm == "a[3]" ~ "Winter cumulative precipitation",
                          parm == "a[4]" ~ "Percent landscape harvested",
                          parm == "a[5]" ~ "Percent open canopy cover"))%>%
  mutate(stage = "Adult") %>%
  dplyr::select(parm, `50%`, stage)

#combine
sums <- egg_sum %>%
  rbind(nest_sum, adult_sum) %>%
  # filter(!parm %in% c("Aspen", "Juniper", "Douglas Fir", "True fir", "Nest initiation day",
  #                   "Nest height")) %>%
  filter(!parm %in% c("Aspen", "Juniper", "Douglas Fir", "True fir")) %>%
  mutate(stage = factor(stage, levels = c("Egg", "Nestling", "Adult"))) %>%
  mutate(parm = factor(parm, levels = c("Coefficient of variation in closed-forest patch size",
                                        "Nest initiation day",
                                        "Nest height",
                                        "Nest tree species",
                                        "Percent open canopy cover",
                                        "Nesting season cumulative precipitation", 
                                        "Nesting season maximum temperature",
                                        "Nesting season maximum temperature ^2",
                                        "Spring mean temperature",
                                        "Winter mean temperature",
                                        "Winter cumulative precipitation", 
                                        "Percent landscape burned",
                                        "Percent landscape harvested",
                                        "Stand harvested (yes/no)",
                                        "Stand burned (yes/no)",
                                        "Stand harvested & burned (yes/no)"))) %>%
  mutate(group = case_when(parm %in% c("Coefficient of variation in closed-forest patch size",
                                       "Nest initiation day",
                                       "Nest height",
                                       "Nest tree species",
                                       "Percent open canopy cover") ~ "Habitat",
                           parm %in% c("Nesting season cumulative precipitation", 
                                       "Nesting season maximum temperature",
                                       "Nesting season maximum temperature ^2",
                                       "Spring mean temperature",
                                       "Winter mean temperature",
                                       "Winter cumulative precipitation") ~ "Climate",
                           parm %in% c("Percent landscape burned",
                                       "Percent landscape harvested",
                                       "Stand harvested (yes/no)",
                                       "Stand burned (yes/no)",
                                       "Stand harvested & burned (yes/no)") ~ "Restoration")) %>%
  mutate(group = factor(group, levels = c("Restoration", "Climate", "Habitat")))

sums2 <- sums %>%
  mutate(effect = case_when(`50%` > 0 ~ 1,
                            `50%` < 0 ~ -1,
                            `50%` == 0 ~ 0))


# Plot --------------------------------------------------------------------

(fig <- ggplot(sums)+
         geom_tile(aes(y = parm, x = 1, fill = `50%`), 
                   color = 'black',
                   linewidth = 0.5,
                   height = 0.85, width = 0.85) +
  facet_grid(stage~., scales = "free") +
  binned_scale(aesthetics = "fill",
               scale_name = "stepsn", 
               palette = function(x) c('#b35806', 
                                       '#f1a340', 
                                       '#f7f7f7', 
                                       '#d8daeb',
                                       '#998ec3',
                                       '#542788'),
               breaks = c(-1.1, -0.3, 0, 0.3, 0.9, 5.4),
               limits = c(-1.2, 5.5),
               labels = c('negative', '', '', '', '', 'positive'),
               guide = "colorsteps") +
  labs(y = "Covariate") +
  guides(fill = guide_legend(title = "Covariate Effect")) +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        strip.background = element_rect(fill = 'white'),
        aspect.ratio = 5))

# ggsave(here('pictures',
#             'R',
#             'Figure_covariateeffects.png'),
#        height = 3.5,
#        width = 6,
#        units = "in")

ggplot(sums2)+
  geom_tile(aes(y = parm, x = 1, fill = effect), 
            color = 'black',
            linewidth = 0.5,
            height = 0.85, width = 0.85) +
  facet_grid(stage~., scales = "free", switch = "y",
             labeller = labeller(
               stage = c(`Egg` = "Egg survival", 
                         `Nestling` = "Nestling survival",
                         `Adult` = "Adult survival")
             )) +
  binned_scale(aesthetics = "fill",
               scale_name = "stepsn", 
               palette = function(x) c('#d8b365',
                                       '#f5f5f5',
                                       '#5ab4ac'),
               breaks = c(-1,  0, 1),
               limits = c(-1.1, 1.1),
               labels = c('negative', '', 'positive'),
               guide = "colorsteps") +
  labs(y = "Covariate") +
  guides(fill = guide_legend(title = "Covariate Effect")) +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        strip.background = element_rect(fill = 'white'),
        aspect.ratio = 5) +
  scale_y_discrete(position = "right")

ggsave(here('pictures',
            'R',
            "SI",
            'Figure_covariatedirections.png'),
       height = 4.5,
       width = 6,
       units = "in")


ggplot(sums2)+
  geom_tile(aes(y = parm, x = 1, fill = group), 
            color = 'black',
            linewidth = 0.5,
            height = 0.85, width = 0.85) +
  facet_grid(stage~., scales = "free", switch = "y",
             labeller = labeller(
               stage = c(`Egg` = "Egg survival", 
                         `Nestling` = "Nestling survival",
                         `Adult` = "Adult survival")
             )) +
  scale_fill_manual(values = c("#985E5C","#F09B33", "#92A687")) +
  # binned_scale(aesthetics = "fill",
  #              scale_name = "stepsn", 
  #              palette = function(x) c('#d8b365',
  #                                      '#f5f5f5',
  #                                      '#5ab4ac'),
  #              breaks = c(-1,  0, 1),
  #              limits = c(-1.1, 1.1),
  #              labels = c('negative', '', 'positive'),
  #              guide = "colorsteps") +
  labs(y = "Covariate") +
  guides(fill = guide_legend(title = "Covariate group")) +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        strip.background = element_rect(fill = 'white'),
        aspect.ratio = 5) +
  scale_y_discrete(position = "right")

ggsave(here('pictures',
            'R',
            'Figure_covariateeffects1.pdf'),
       height = 4.5,
       width = 6,
       units = "in")
