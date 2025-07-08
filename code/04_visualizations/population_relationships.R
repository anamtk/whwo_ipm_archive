
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


emfwor_r2 <- readRDS(here('data',
                          'ipm_outputs',
                          'EMFWOR_ipm_correlations.RDS'))%>%
  mutate(Forest = "EMFWOR")


emmaor_r2 <- readRDS(here('data',
                          'ipm_outputs',
                          'EMMAOR_ipm_correlations.RDS'))%>%
  mutate(Forest = "EMMAOR")


empaid_r2 <- readRDS(here('data',
                          'ipm_outputs',
                          'EMPAID_ipm_correlations.RDS'))%>%
  mutate(Forest = "EMPAID")

# Figure 2 ----------------------------------------------------------------

all_pops <- emfwor %>%
  bind_rows(emmaor, empaid) 

all_r2 <- emfwor_r2 %>%
  bind_rows(emmaor_r2, empaid_r2)

forest.labs <- c("Fremont-Winema", "Malheur", "Payette")
names(forest.labs) <- c("EMFWOR", "EMMAOR", "EMPAID")

eggPlot <- ggplot(all_pops, aes(x = mu.phi.egg, y = mu.pop.lambda)) +
  geom_errorbarh(aes(xmin = mu.phi.egg - sd.phi.egg,
                    xmax = mu.phi.egg + sd.phi.egg),
                 alpha = 0.5, color = "#084081") +
  geom_errorbar(aes(ymin = mu.pop.lambda -sd.pop.lambda,
                    ymax = mu.pop.lambda + sd.pop.lambda),
                alpha = 0.5, color = "#084081") +
  geom_point(fill = "#084081", , shape = 21, color = 'black') +
  labs(x = 'Egg survival', y = 'Population growth rate') +
  facet_grid(Forest~.,
             switch = "y",
             labeller = labeller(Forest = forest.labs))+
  theme(strip.background = element_rect(fill = "white"))


nstlPlot <- ggplot(all_pops, aes(x = mu.phi.nestling, y = mu.pop.lambda)) +
  geom_errorbarh(aes(xmin = mu.phi.nestling - sd.phi.nestling,
                     xmax = mu.phi.nestling + sd.phi.nestling),
                 alpha = 0.5, color = '#2b8cbe') +
  geom_errorbar(aes(ymin = mu.pop.lambda -sd.pop.lambda,
                    ymax = mu.pop.lambda + sd.pop.lambda),
                alpha = 0.5, color = "#2b8cbe") +
  geom_point(fill = '#2b8cbe', shape = 21, color = 'black') +
  labs(x = 'Nestling survival', y = 'Population growth rate') +
  facet_grid(Forest~.,
             switch = "y")+
  theme(
    strip.background = element_blank(),
    strip.text = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank())

adulPlot <- ggplot(all_pops, aes(x = mu.phi.adult, y = mu.pop.lambda)) +
  geom_errorbarh(aes(xmin = mu.phi.adult - sd.phi.adult,
                     xmax = mu.phi.adult + sd.phi.adult),
                 alpha = 0.5, color = "#7bccc4") +
  geom_errorbar(aes(ymin = mu.pop.lambda -sd.pop.lambda,
                    ymax = mu.pop.lambda + sd.pop.lambda),
                alpha = 0.5, color = "#7bccc4") +
  geom_point(color = "black", shape = 21, fill = "#7bccc4") +
  labs(x = 'Adult survival', y = 'Population growth rate') +
  facet_grid(Forest~.,
             switch = "y")+
  theme(
    strip.background = element_blank(),
    strip.text = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank())

r2_means <- all_r2 %>%
  group_by(stage, Forest) %>%
  summarise(meanr2 = mean(r2))

(r2Plot <- ggplot() +
  geom_vline(data = r2_means, aes(xintercept = meanr2, 
                                  color = stage),
             linetype = 2) +
  geom_histogram(data = all_r2, aes(x = r2, fill = stage)) +
  facet_grid(Forest~.)+
  scale_fill_manual(values = c("#084081", "#2b8cbe", "#7bccc4"),
                    labels = c("Egg survival", "Nestling survival", 
                               "Adult survival"),
                    name = "Stage") +
  scale_color_manual(values = c("#084081", "#2b8cbe", "#7bccc4"),
                     labels = c("Egg survival", "Nestling survival", 
                                "Adult survival"),
                     name = "Stage") +
  theme(
    strip.background = element_blank(),
    strip.text = element_blank()) +
  labs(x = bquote (~R^2), y = ""))

eggPlot + nstlPlot + adulPlot + r2Plot +
  plot_layout(nrow = 1) 

ggsave(here('pictures',
            'R',
            'FigureX_relationships.pdf'),
       height = 4.5,
       width = 8.5,
       units = "in")

