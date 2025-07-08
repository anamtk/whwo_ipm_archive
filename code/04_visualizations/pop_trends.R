
# Load packages -----------------------------------------------------------

package.list <- c("here", "tidyverse", 
                  "patchwork", "MuMIn")

## Installing them if they aren't already on the computer
new.packages <- package.list[!(package.list %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## And loading them
for(i in package.list){library(i, character.only = T)}

theme_set(theme_bw())
# Load data ---------------------------------------------------------------

emfwor <- readRDS(here('data',
                        'ipm_outputs',
                        'EMFWOR_ipm_summary.RDS')) %>%
  mutate(Forest = "EMFWOR")

emmaor <- readRDS(here('data',
                        'ipm_outputs',
                        'EMMAOR_ipm_summary.RDS')) %>%
  mutate(Forest = "EMMAOR")

empaid <- readRDS(here("data",
                     "ipm_outputs",
                     "EMPAID_ipm_summary.rds")) %>%
  mutate(Forest = "EMPAID")

emfwor_lambda <- readRDS(here('data',
                              'ipm_outputs',
                              'EMFWOR_lambda_summary.RDS'))%>%
  mutate(Forest = "EMFWOR")

emmaor_lambda <- readRDS(here('data',
                              'ipm_outputs',
                              'EMMAOR_lambda_summary.RDS')) %>%
  mutate(Forest = "EMMAOR")

empaid_lambda <- readRDS(here('data',
                              'ipm_outputs',
                              'EMPAID_lambda_summary.RDS')) %>%
  mutate(Forest = "EMPAID")

# Combine data ------------------------------------------------------------

all_pops <- emfwor %>%
  bind_rows(emmaor, empaid) 

# EMFWOR -  126/270 total pts treated 2015, 2017-2021
# EMMAOR - 84/300 total pts treated 2014-2016, 2019, 2021
# EMPAID – 147/280 total pts treated 2012-2019

# tx_years <- c(2015, 2017, 2018, 2019, 2020, 2021,
#               2014, 2015, 2016, 2019, 2021,
#               2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019)
starts <- c(2014.5, 2016.5,
            2013.5, 2018.5, 2020.5,
            2011.5)
ends <- c(2015.5, 2021.5,
          2016.5, 2019.5, 2021.5,
          2019.5)
Forest <- c(rep("EMFWOR", 2),
            rep("EMMAOR", 3),
            rep("EMPAID", 1))
tx_df <- as.data.frame(starts) %>%
  bind_cols(Forest = Forest, ends = ends)

all_lambda <- emfwor_lambda %>%
  bind_rows(emmaor_lambda, empaid_lambda)

# Figure 1 ----------------------------------------------------------------

#What are population trends??
#looking at a) yearly, b) by population

forest.labs <- c("Fremont-Winema", "Malheur", "Payette")
names(forest.labs) <- c("EMFWOR", "EMMAOR", "EMPAID")

#yearly populations
(pop1 <- ggplot() +
  geom_hline(yintercept = 1, linetype =2, alpha = 0.4)+
    # geom_vline(data = tx_df, 
    #            aes(xintercept = tx_years, 
    #                group=Forest),
    #            inherit.aes = FALSE,
    #            linetype = 2) +
    geom_rect(data = tx_df, 
              aes(xmin = starts, 
                  xmax = ends,
                  ymin = 0.25,
                  ymax = 1.8,
                  group = Forest),
              inherit.aes = FALSE,
              alpha = 0.4,
              fill = "#985E5C") +
    geom_line(data = all_pops, aes(x = year, y = mu.pop.lambda)) +
    geom_point(data = all_pops, aes(x = year, y = mu.pop.lambda), size = 2) +
    geom_errorbar(data = all_pops, aes(x = year, 
                      ymin = mu.pop.lambda - sd.pop.lambda, 
                      ymax = mu.pop.lambda + sd.pop.lambda), 
                  width = 0) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") +
  labs(x = "Year", y = "Population growth rate") +
  scale_x_continuous(breaks = c(2012, 2013, 2014, 2015, 2016, 2017,
                                2018, 2019, 2020, 2021)) +
  #ylim(0, 3.5)) +
  facet_grid(Forest ~.,
             labeller = labeller(Forest = forest.labs),
             switch = "y") +
    ylim(0.25, 1.8) +
  theme(strip.background = element_rect(fill = "white"),
        axis.text.x = element_text(angle = 45, hjust =1),
        legend.position = "none"))

(pop2 <- ggplot(all_lambda) +
    geom_hline(yintercept = 1, linetype =2, alpha = 0.4)+
  geom_errorbar(aes(x = 1, ymin = mean_lambda - sd_lambda, 
                    ymax = mean_lambda + sd_lambda), 
                width = 0.00) +
  geom_point(aes(x = 1, y = mean_lambda), shape = 21, size = 2.5, 
             fill = "white") +
    labs(x = "Mean") +
  ylim(0.25, 1.8) +
  facet_grid(Forest~ .) +
  theme(axis.text = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank()))

pop1 + pop2 +
  plot_layout(widths = c(6,1))

ggsave(here('pictures',
            'R',
            'FigureX_trends.pdf'),
       height = 4,
       width = 5,
       units = "in")


# Ratios ------------------------------------------------------------------

(pop3 <- all_pops %>%
   ggplot() +
   geom_hline(yintercept = 1, linetype =2, alpha = 0.4)+
   geom_line(aes(x = year, y = mu.pop.lambda)) +
   geom_point(aes(x = year, y = mu.pop.lambda), size = 2) +
   geom_errorbar(aes(x = year, 
                     ymin = mu.pop.lambda - sd.pop.lambda, 
                     ymax = mu.pop.lambda + sd.pop.lambda), 
                 width = 0) +
   theme(axis.text.x = element_text(angle = 45, hjust = 1),
         legend.position = "none") +
   labs(x = "Year", y = "Population growth rate") +
   scale_x_continuous(breaks = c(2012, 2013, 2014, 2015, 2016, 2017,
                                 2018, 2019, 2020, 2021)) +
   #ylim(0, 3.5)) +
   facet_grid(.~Forest,
              labeller = labeller(Forest = forest.labs),
              switch = "y") +
   ylim(0.25, 1.65) +
   theme(strip.background = element_rect(fill = "white"),
         axis.text.x = element_text(angle = 45, hjust =1),
         legend.position = "none"))

(survival <- all_pops %>%
   ggplot() +
   geom_hline(yintercept = 1, linetype =2, alpha = 0.4)+
   geom_line(aes(x = year, y = mu.prop.survival)) +
   geom_point(aes(x = year, y = mu.prop.survival), size = 2) +
   geom_errorbar(aes(x = year, 
                     ymin = mu.prop.survival - sd.prop.survival, 
                     ymax = mu.prop.survival + sd.prop.survival), 
                 width = 0) +
   theme(axis.text.x = element_text(angle = 45, hjust = 1),
         legend.position = "none") +
   labs(x = "Year", y = "Proportion of adults from survival") +
   scale_x_continuous(breaks = c(2012, 2013, 2014, 2015, 2016, 2017,
                                 2018, 2019, 2020, 2021)) +
   #ylim(0, 3.5)) +
   facet_grid(.~Forest,
              labeller = labeller(Forest = forest.labs),
              switch = "y") +
   ylim(0, 1) +
   theme(strip.background = element_rect(fill = "white"),
         axis.text.x = element_text(angle = 45, hjust =1),
         legend.position = "none"))

(recruit <- all_pops %>%
    ggplot() +
    geom_hline(yintercept = 1, linetype =2, alpha = 0.4)+
    geom_line(aes(x = year, y = mu.prop.recruitment)) +
    geom_point(aes(x = year, y = mu.prop.recruitment), size = 2) +
    geom_errorbar(aes(x = year, 
                      ymin = mu.prop.recruitment - sd.prop.recruitment, 
                      ymax = mu.prop.recruitment + sd.prop.recruitment), 
                  width = 0) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none") +
    labs(x = "Year", y = "Proportion of adults from recruitment") +
    scale_x_continuous(breaks = c(2012, 2013, 2014, 2015, 2016, 2017,
                                  2018, 2019, 2020, 2021)) +
    #ylim(0, 3.5)) +
    facet_grid(.~Forest,
               labeller = labeller(Forest = forest.labs),
               switch = "y") +
    ylim(0, 1) +
    theme(strip.background = element_rect(fill = "white"),
          axis.text.x = element_text(angle = 45, hjust =1),
          legend.position = "none"))

pop3/survival

ggsave(here('pictures',
            'R',
            'SI_Figure_trend_survival.pdf'),
       height = 4,
       width = 5,
       units = "in")

ggplot(all_pops, aes(x = mu.pop.lambda, y = mu.prop.survival))+
  geom_point() +
  facet_grid(.~Forest,
             labeller = labeller(Forest = forest.labs),
             switch = "y") 

ggsave(here('pictures',
            'R',
            'SI_Figure_lambda_survival_relationship.pdf'),
       height = 4,
       width = 5,
       units = "in")

ggplot(all_pops, aes(x = mu.pop.lambda, y = mu.prop.recruitment))+
  geom_point() +
  facet_grid(.~Forest,
             labeller = labeller(Forest = forest.labs),
             switch = "y") 

  