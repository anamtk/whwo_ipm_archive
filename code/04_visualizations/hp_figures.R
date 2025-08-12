#HP visualizations for each life stage
#February 1, 2024

#this script summarizes the visualizations of the hierarchical partitioning
#for each stage for which we have covariates (egg, nestling, and adult
#survival)


# Load packages -----------------------------------------------------------

package.list <- c("here", "tidyverse", 'patchwork')

## Installing them if they aren't already on the computer
new.packages <- package.list[!(package.list %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## And loading them
for(i in package.list){library(i, character.only = T)}


# Load datasets -----------------------------------------------------------

#NOTES FROM LAB MEETING 11/6
#rerun population model HP results with posterior samples
#from each sub statistical model
#do hierarchical partitioning for each stage for each forest
#Could do by binning log likelihood by forest or 
#re-running the models by forest

egg_hp_fwor <- readRDS(here('data',
                       '03_hierarchical_partitioning',
                       'emfwor',
                       'egg_survival',
                       'egg_hp_results_emfwor.RDS')) %>%
  mutate(stage = "Egg survival",
         forest = "EMFWOR") 

egg_hp_maor <- readRDS(here('data',
                            '03_hierarchical_partitioning',
                            'emmaor',
                            'egg_survival',
                            'egg_hp_results_emmaor.RDS')) %>%
  mutate(stage = "Egg survival",
         forest = "EMMAOR")

egg_hp_paid <- readRDS(here('data',
                            '03_hierarchical_partitioning',
                            'empaid',
                            'egg_survival',
                            'egg_hp_results_empaid.RDS')) %>%
  mutate(stage = "Egg survival",
         forest = "EMPAID")

nestling_hp_fwor <- readRDS(here('data',
                            '03_hierarchical_partitioning',
                            'emfwor',
                            'nestling_survival',
                            'nestling_hp_results_emfwor.RDS')) %>%
  mutate(stage = 'Nestling survival',
         forest = "EMFWOR")

nestling_hp_maor <- readRDS(here('data',
                                 '03_hierarchical_partitioning',
                                 'emmaor',
                                 'nestling_survival',
                                 'nestling_hp_results_emmaor.RDS')) %>%
  mutate(stage = 'Nestling survival',
         forest = "EMMAOR")

nestling_hp_paid <- readRDS(here('data',
                                 '03_hierarchical_partitioning',
                                 'empaid',
                                 'nestling_survival',
                                 'nestling_hp_results_empaid.RDS')) %>%
  mutate(stage = 'Nestling survival',
         forest = "EMPAID")

adult_hp_fwor <- readRDS(here('data',
                         '03_hierarchical_partitioning',
                         'emfwor',
                         'adult',
                         'adult_hp_results_emfwor.RDS')) %>%
  mutate(stage = 'Adult survival',
         forest = "EMFWOR")

adult_hp_maor <- readRDS(here('data',
                              '03_hierarchical_partitioning',
                              'emmaor',
                              'adult',
                              'adult_hp_results_emmaor.RDS')) %>%
  mutate(stage = 'Adult survival',
         forest = "EMMAOR")

adult_hp_paid <- readRDS(here('data',
                              '03_hierarchical_partitioning',
                              'empaid',
                              'adult',
                              'adult_hp_results_empaid.RDS')) %>%
  mutate(stage = 'Adult survival',
         forest = "EMPAID")

emfwor_hp <- readRDS(here('data',
                          '03_hierarchical_partitioning',
                          'emfwor',
                          'ipm',
                          'ipm_hp_results_EMFWOR.RDS')) %>%
  mutate(forest = 'EMFWOR') %>%
  mutate(stage = 'Population change')

emmaor_hp <- readRDS(here('data',
                          '03_hierarchical_partitioning',
                          'emmaor',
                          'ipm',
                          'ipm_hp_results_EMMAOR.RDS')) %>%
  mutate(forest = 'EMMAOR')%>%
  mutate(stage = 'Population change')

empaid_hp <- readRDS(here('data',
                          '03_hierarchical_partitioning',
                          'empaid',
                          'ipm',
                          'ipm_hp_results_EMPAID.RDS')) %>%
  mutate(forest = 'EMPAID')%>%
  mutate(stage = 'Population change')


# Visualize ---------------------------------------------------------------

# (eggplot <- ggplot(egg_hp, aes(x = param, y = IC.perc)) +
#   geom_bar(stat = "identity", color = "black") +
#   scale_x_discrete(labels = c("Climate", "Habitat", "Treatment")) +
#   labs(x = "", y = "Relative importance (%)",
#        title = "Egg survival") +
#    ylim(0, 100) )
# 
# (nstlplot <- ggplot(nestl_hp, aes(x = param, y = IC.perc)) +
#   geom_bar(stat = "identity", color = "black") +
#   scale_x_discrete(labels = c("Climate", "Habitat", "Treatment")) +
#   labs(x = "Covariate group", y = "",
#        title = "Nestling survival") +
#     ylim(0, 100) )
# 
# (adultplot <- ggplot(adult_hp, aes(x = param, y = IC.perc)) +
#   geom_bar(stat = "identity", color = "black") +
#   scale_x_discrete(labels = c("Climate", "Habitat", "Treatment")) +
#   labs(x = "", y = "",
#        title = "Adult persistence") +
#     ylim(0, 100))
# 
# eggplot + nstlplot + adultplot +
#   plot_annotation(tag_levels = "A")

# Another way -------------------------------------------------------------

all_hp <- egg_hp_fwor %>%
  bind_rows(egg_hp_maor,
            egg_hp_paid,
            nestling_hp_fwor,
            nestling_hp_maor,
            nestling_hp_paid,
            adult_hp_fwor,
            adult_hp_maor,
            adult_hp_paid,
            emfwor_hp,
            emmaor_hp,
            empaid_hp) %>%
  mutate(stage = factor(stage, levels = c("Egg survival", 
                                          "Nestling survival", 
                                          "Adult survival",
                                          'Population change'))) %>%
  mutate(param = factor(param, levels = c("T", 'C','H'))) %>%
  mutate(param = case_when(param == "T" ~ "Restoration",
                           param == "C" ~ "Climate",
                           param == "H" ~ "Habitat")) %>%
  mutate(param = factor(param, levels = c("Restoration", "Climate", "Habitat")))

forest.labs <- c("Fremont-Winema", "Malheur", "Payette")
names(forest.labs) <- c("EMFWOR", "EMMAOR", "EMPAID")

# (a <- ggplot(all_hp, aes(x = param, y = IC.perc, fill = forest)) +
#   geom_bar(stat = "identity",position = "dodge", color = "black") +
#   scale_x_discrete(labels = c("Climate", "Habitat", "Forest restoration")) +
#   labs(x = "Covariate group", y = "Relative importance (%)") +
#   theme_classic() +
#   facet_grid(forest~stage,
#              labeller = labeller(forest = forest.labs)) +
#     scale_fill_manual(values = c('#66c2a5','#fc8d62', '#8da0cb'),
#                       labels = c("Fremont-Winema", "Malheur", "Payette")) +
#   theme(strip.background = element_rect(fill = "white"),
#         axis.text.x = element_text(angle = 45, hjust =1),
#         legend.position = "none"))

ggplot(all_hp, aes(x = param, y = IC.perc, fill = param)) +
    geom_bar(stat = "identity",position = "dodge", color = "black") +
    scale_x_discrete(labels = c("Restoration", "Climate", "Habitat")) +  
  scale_fill_manual(values = c("#985E5C","#F09B33", "#92A687")) +
    labs(x = "Covariate category", y = "Relative importance (%)") +
    theme_bw() +
  guides(fill = guide_legend(title = "Covariate category")) +
    facet_grid(forest~stage,
               labeller = labeller(forest = forest.labs)) +
    theme(strip.background = element_rect(fill = "white"),
          axis.text.x = element_text(angle = 45, hjust =1),
          legend.position = "none")

# ggsave(here('pictures',
#             'hp_importance.png'))


# IPM hp ------------------------------------------------------------------
# 
# ipm_hp <- emfwor_hp %>%
#   bind_rows(emmaor_hp, empaid_hp)
# 
# forest.labs <- c("Fremont-Winema", "Malheur", "Payette")
# names(forest.labs) <- c("EMFWOR", "EMMAOR", "EMPAID")


# (b <- ggplot(ipm_hp, aes(x = param, y = IC.perc, fill = forest)) +
#   geom_bar(stat = "identity", color = "black") +
#   scale_x_discrete(labels = c("Climate", "Habitat", "Forest restoration")) +
#   labs(x = "Covariate group", y = "Relative importance (%)",
#        fill = "Forest") +
#   theme_classic() +
#   scale_fill_manual(values = c('#66c2a5','#fc8d62', '#8da0cb'),
#                     labels = c("Fremont-Winema", "Malheur", "Payette")) +
#   facet_grid(~forest,
#              labeller = labeller(forest = forest.labs)) +
#   theme(strip.background = element_rect(fill = "white"),
#         axis.text.x = element_text(angle = 45, hjust = 1),
#         legend.position = "none"))


# Save --------------------------------------------------------------------
# a/b +
#   plot_annotation(tag_level = "A") +
#   plot_layout(guides = "collect")

ggsave(here('pictures',
            'R',
            'FigureX_hp.pdf'),
       height = 4.5,
       width = 6,
       units = "in")


