#SI figure to compare to published survival values
#Ana Miller-ter Kuile
#January 22, 2025

#this script compares published values of adult survival to ours


# Load packages -----------------------------------------------------------


package.list <- c("here", "tidyverse", 
                  "patchwork")

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
  mutate(Forest = "Fremont-Winema")

emmaor <- readRDS(here('data',
                       'ipm_outputs',
                       'EMMAOR_ipm_summary.RDS'))%>%
  mutate(Forest = "Malheur")

empaid <- readRDS(here('data',
                       'ipm_outputs',
                       'EMPAID_ipm_summary.RDS'))%>%
  mutate(Forest = "Payette")


# Published values --------------------------------------------------------

#from: https://doi.org/10.1676/22-00014
kozma.mean <- 0.85
kozma.lci <- 0.78
kozma.uci <- 0.93

# Combine observed --------------------------------------------------------

all <- emfwor %>%
  bind_rows(emmaor, empaid)

mean.adult <- all %>%
  summarise(mean = mean(mu.phi.adult)) %>%
  dplyr::select(mean) %>%
  as_vector()

lci.adult <- all %>%
  summarise(lci_adult = quantile(mu.phi.adult, probs = 0.025, type = 8)) %>%
  dplyr::select(lci_adult) %>%
  as_vector()

uci.adult <- all %>%
  summarise(uci_adult = quantile(mu.phi.adult, probs = 0.975, type = 8)) %>%
  dplyr::select(uci_adult) %>%
  as_vector()

ggplot(all, aes(x = Forest, y = mu.phi.adult)) +  
  geom_rect(aes(ymin = kozma.lci,
                ymax = kozma.uci,
                xmin = -Inf, 
                xmax = Inf),
            fill =  "#ef8a62",
            alpha = 0.1) +
  # geom_rect(aes(ymin = lci.adult,
  #               ymax = uci.adult,
  #               xmin = -Inf, 
  #               xmax = Inf),
  #           fill =  "#67a9cf",
  #           alpha = 0.1) +
  geom_hline(yintercept = kozma.mean,
             linetype = 2,
             color = "#ef8a62") +
  geom_hline(yintercept = mean.adult,
             linetype = 2,
             color = "#67a9cf") +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(height = 0) +
  xlab("Forest population") +
  ylab(expression(paste(phi[ad])))

ggsave(here('pictures',
            'R',
            'SI',
            'Figure_kozma_comparison.png'),
       height = 4.5,
       width = 6,
       units = "in"
       )






