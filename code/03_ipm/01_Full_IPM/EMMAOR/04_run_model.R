#full IPM - EMMAOR
#June 14, 2023

#this script runs the full IPM

# Load packages -----------------------------------------------------------

package.list <- c("here", "tidyverse")

## Installing them if they aren't already on the computer
new.packages <- package.list[!(package.list %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## And loading them
for(i in package.list){library(i, character.only = T)}

# Load data ---------------------------------------------------------------

data_list <- readRDS(here("data",
                   "04_ipm_data_inputs",
                   "IPM_data_list_EMMAOR.RDS"))

# Get data objects --------------------------------------------------------

#Pull data out into objects that R will recognize

#covariate sampler data:
N_samples <- data_list$N
#EMMAOR is 2nd forest in the intercept arrays
forest <- 2
#egg number:
egg.lambda.array <- data_list$egg.lambda.array
#EGG survival:
#number of treatments
n.trt <- data_list$n.trt
e0.array <- data_list$e0.array
e1Trt.array <- data_list$e1Trt.array
e.array <- data_list$e.array
#nestling survival:
n.species <- data_list$n.species
n0.array <- data_list$n0.array
n1Trt.array <- data_list$n1Trt.array
n2Spec.array<- data_list$n2Spec.array
n.array <- data_list$n.array
#ADult portions
a0.array <- data_list$a0.array
a.array <- data_list$a.array
#sex ratio
propF.array <- data_list$propF.array
#for yearly population model components
n.years <- data_list$n.years

#point-level nest values
n.nests <- data_list$n.nests
TreatmentID <- data_list$TreatmentID
ForestCV <- data_list$ForestCV
PPT_eg <- data_list$PPT_eg
Tmax_eg <- data_list$Tmax_eg
#impute missing data:
Tmax_eg[which(is.na(Tmax_eg))] <- rnorm(1, 
                                        mean = mean(Tmax_eg, na.rm = T),
                                        sd = sd(Tmax_eg, na.rm = T))
SpeciesID <- data_list$SpeciesID
LandBu <- data_list$LandBu
InitDay <- data_list$InitDay
#impute missing data for init day
InitDay[which(is.na(InitDay))] <- rnorm(1, 
                                        mean = mean(InitDay, na.rm = T),
                                        sd = sd(InitDay, na.rm = T))
NestHt <- data_list$NestHt
PPT_ne <- data_list$PPT_ne
Tmax_ne <- data_list$Tmax_ne
#impute missing data:
Tmax_ne[which(is.na(Tmax_ne))] <- rnorm(1, 
                                        mean = mean(Tmax_ne, na.rm = T),
                                        sd = sd(Tmax_ne, na.rm = T))

#yearly nest values:
surveyed <- data_list$surveyed
nest.start <- data_list$nest.start
nest.end <- data_list$nest.end
survey_rows <- c(1:4, 6, 8)

#adult portions of model
#point-level values
n.points <- data_list$n.points

aTmean_sp <- data_list$aTmean_sp
aTmean_wt <- data_list$aTmean_wt
aPPT_wt <- data_list$aPPT_wt
aLandHa <- data_list$aLandHa
aLowCC <- data_list$aLowCC



# Run model ---------------------------------------------------------------

n.iter <- 1000

start <- Sys.time()
source(here('code',
            '03_ipm',
            '01_Full_IPM',
            'IPM_models',
            "IPM_November2024.R"))
end <- Sys.time()
(total <- end-start)

# Add metadata ------------------------------------------------------------

#Years of survey for EMMAOR:
#start with just one forest
#EMMAOR:
## Nests: 2014-2021 with no nests in 2018 and 2020
## Adults: all transects surveyed 2014-2019, but missing 2018

yearIDs <- as.data.frame(cbind(year = c(2014:2021),
                               yearID = 1:length(2014:2021)))

emmaor_df <- final %>%
  left_join(yearIDs, by = "yearID")

emmaor_sum <- emmaor_df %>%
  group_by(year) %>%
  summarise(mu.phi.egg = mean(phi.egg),
            sd.phi.egg = sd(phi.egg),
            mu.phi.nestling = mean(phi.nestling),
            sd.phi.nestling = sd(phi.nestling),
            mu.phi.adult = mean(phi.adult),
            sd.phi.adult = sd(phi.adult),
            mu.pop.lambda = mean(pop.lambda),
            sd.pop.lambda = sd(pop.lambda),
            mu.prop.survival = mean(prop.survival),
            sd.prop.survival = sd(prop.survival),
            mu.prop.recruitment = mean(prop.recruitment),
            sd.prop.recruitment = sd(prop.recruitment)) %>%
  ungroup() %>%
  #get rid of years from analysis where nests weren't
  #surveyed and we estimatead -
  mutate(mu.phi.egg = case_when(year %in% c(2018, 2020) ~ NA_real_,
                                TRUE ~ mu.phi.egg),
         sd.phi.egg = case_when(year %in% c(2018, 2020) ~ NA_real_,
                                TRUE ~ sd.phi.egg),
         mu.phi.nestling = case_when(year %in% c(2018, 2020) ~ NA_real_,
                                TRUE ~ mu.phi.nestling),
         sd.phi.nestling = case_when(year %in% c(2018, 2020) ~ NA_real_,
                                TRUE ~ sd.phi.nestling))

saveRDS(emmaor_sum, here('data',
                         'ipm_outputs',
                         'EMMAOR_ipm_summary.RDS'))

ggplot(emmaor_sum, aes(x = mu.phi.egg, y = mu.pop.lambda)) +
  geom_point() +
  geom_errorbar(aes(xmin = mu.phi.egg-sd.phi.egg,
                    xmax = mu.phi.egg+sd.phi.egg)) +
  geom_errorbar(aes(ymin = mu.pop.lambda-sd.pop.lambda,
                    ymax = mu.pop.lambda+sd.pop.lambda))

ggplot(emmaor_sum, aes(x = mu.phi.nestling, y = mu.pop.lambda)) +
  geom_point() +
  geom_errorbar(aes(xmin = mu.phi.nestling-sd.phi.nestling,
                    xmax = mu.phi.nestling+sd.phi.nestling)) +
  geom_errorbar(aes(ymin = mu.pop.lambda-sd.pop.lambda,
                    ymax = mu.pop.lambda+sd.pop.lambda))


ggplot(emmaor_sum, aes(x = mu.phi.adult, y = mu.pop.lambda)) +
  geom_point() +
  geom_errorbar(aes(xmin = mu.phi.adult-sd.phi.adult,
                    xmax = mu.phi.adult+sd.phi.adult)) +
  geom_errorbar(aes(ymin = mu.pop.lambda-sd.pop.lambda,
                    ymax = mu.pop.lambda+sd.pop.lambda))

ggplot(emmaor_sum, aes(x = year, y = mu.pop.lambda)) +
  geom_hline(yintercept = 1, linetype = 2) +
  geom_point() +
  geom_errorbar(aes(ymin = mu.pop.lambda-sd.pop.lambda,
                    ymax = mu.pop.lambda+sd.pop.lambda))

mean.adult <- emmaor_sum %>%
  summarise(mean_adult = mean(mu.phi.adult)) %>%
  dplyr::select(mean_adult) %>%
  as_vector()

lci.adult <- emmaor_sum %>%
  summarise(lci_adult = quantile(mu.phi.adult, probs = 0.025, type = 8)) %>%
  dplyr::select(lci_adult) %>%
  as_vector()

uci.adult <- emmaor_sum %>%
  summarise(uci_adult = quantile(mu.phi.adult, probs = 0.975, type = 8)) %>%
  dplyr::select(uci_adult) %>%
  as_vector()

kozma.mean <- 0.85
kozma.lci <- 0.78
kozma.uci <- 0.93

ggplot(emmaor_sum, aes(x = year, y = mu.phi.adult)) +
  geom_rect(aes(ymin = kozma.lci, 
                ymax = kozma.uci,
                xmin = -Inf, 
                xmax = Inf),
            fill =  "#ef8a62", 
            alpha = 0.1) +
  geom_rect(aes(ymin = lci.adult, 
                ymax = uci.adult,
                xmin = -Inf, 
                xmax = Inf),
            fill =  "#67a9cf", 
            alpha = 0.1) +
  geom_hline(yintercept = kozma.mean, linetype = 2, color = "#ef8a62") +
  geom_hline(yintercept = mean.adult, linetype = 2, color = "#67a9cf") +
  geom_point() +
  geom_errorbar(aes(ymin = mu.phi.adult-sd.phi.adult,
                    ymax = mu.phi.adult+sd.phi.adult),
                width = 0.2)


# Get total means and sd for population growth ----------------------------

emmaor_lambda <- emmaor_df %>%
  ungroup() %>%
  summarise(mean_lambda = mean(pop.lambda, na.rm = T),
            sd_lambda = sd(pop.lambda, na.rm = T))

saveRDS(emmaor_lambda, here('data',
                            'ipm_outputs',
                            'EMMAOR_lambda_summary.RDS'))


# R^2 histograms ----------------------------------------------------------

r2_long <- r2 %>%
  pivot_longer(egg.r2:adult.r2,
               names_to = 'stage',
               values_to = 'r2') %>%
  mutate(stage = str_sub(stage, 1, (nchar(stage)-3))) %>%
  mutate(stage = factor(stage, levels = c('egg', 'nestling', 'adult'))) %>%
  mutate(r2 = abs(r2)) 

r2_mean <- r2_long %>%
  group_by(stage) %>%
  summarise(mean = mean(r2))


ggplot(r2_long) + 
  geom_histogram(aes(x = r2)) +
  facet_grid( stage ~.) +
  theme(legend.position = 'none',
        strip.background = element_rect(fill = "white")) +
  geom_vline( data = r2_mean,
              aes(xintercept = mean),
              linetype = 2)

saveRDS(r2_long, here('data',
                      'ipm_outputs',
                      'EMMAOR_ipm_correlations.RDS'))

