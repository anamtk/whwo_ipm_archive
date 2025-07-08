#GOF for full dynamic occupancy model
#Ana Miller-ter Kuile
#January 23, 2025

# Load packages -----------------------------------------------------------

package.list <- c("here", "tidyverse", 
                  "patchwork", 'pROC')

## Installing them if they aren't already on the computer
new.packages <- package.list[!(package.list %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## And loading them
for(i in package.list){library(i, character.only = T)}

theme_set(theme_bw())


# Load data ---------------------------------------------------------------

#raw data

ydf <- readRDS(here("monsoon",
                        "parameter_models",
                        "adult_occupancy",
                        'inputs',
                        'all_beta',
                        "adult_occupancy_data.RDS"))

y <- as.data.frame.table(ydf$y.occ) %>%
  mutate(point = as.numeric(as.factor(Var1)),
         rep = as.numeric(as.factor(Var2)),
         year = as.numeric(as.factor(Var3))) %>%
  dplyr::select(Freq, point, rep, year) %>%
  rename(y_obs = Freq)

#modeled results for y.rep

yrepdf <- readRDS(here("monsoon",
                       "parameter_models",
                       "adult_occupancy",
                       'outputs',
                       "adult_occupancy_gof.RDS"))

yrepdf2 <- as.data.frame.table(yrepdf$y.rep) %>%
  mutate(iter = as.numeric(as.factor(Var1)),
         point = as.numeric(as.factor(Var2)),
         rep = as.numeric(as.factor(Var3)),
         year = as.numeric(as.factor(Var4))
         ) %>%
  dplyr::select(Freq, point, rep, year, iter) %>%
  rename(y_rep = Freq)

p_df <- as.data.frame.table(yrepdf$p) %>%
  mutate(iter = as.numeric(as.factor(Var1)),
         point = as.numeric(as.factor(Var2)),
         rep = as.numeric(as.factor(Var3)),
         year = as.numeric(as.factor(Var4))
  ) %>%
  dplyr::select(Freq, point, rep, year, iter) %>%
  rename(p = Freq)

z_df <- as.data.frame.table(yrepdf$z) %>%
  mutate(iter = as.numeric(as.factor(Var1)),
         point = as.numeric(as.factor(Var2)),
         year = as.numeric(as.factor(Var3))
  ) %>%
  dplyr::select(Freq, point, year, iter) %>%
  rename(z = Freq)

# Combine -----------------------------------------------------------------

ys <- yrepdf2 %>%
  left_join(y, by = c("point", "rep", "year")) %>%
  filter(!is.na(y_obs))


# Summarize ---------------------------------------------------------------

y_sum <- ys %>%
  mutate(cat = case_when((y_rep == y_obs & y_obs == 1) ~ "match_yes",
                         (y_rep == y_obs & y_obs == 0) ~ "match_no",
                         (y_obs == 1 & y_obs != y_rep) ~ "mismatch_yes",
                         (y_obs == 0 & y_obs !=y_rep) ~ "mismatch_no",
                         TRUE ~ NA_character_))

y_sums <- y_sum %>%
  group_by(cat) %>%
  tally()

y_sums %>%
  pivot_wider(names_from = 'cat',
              values_from ='n') %>%
  summarise(no = match_no/(match_no+mismatch_no),
            yes = match_yes/(match_yes+mismatch_yes))
         

# AUC ---------------------------------------------------------------------

pzdf <- p_df %>%
  left_join(z_df, by = c("iter", "point", 'year')) %>%
  rowwise() %>%
  mutate(pz = p*z) %>%
  ungroup() %>%
  left_join(y, by = c('point','rep', 'year'))

pzdf %>%
  distinct(point, rep, year)
#stole function from the survival model project - so look
#there for more info on how to run this...
#needs pROC package
#mod_GOF in this case will need to be something like
#p*z from the sims list
#resp is the observed "y" data
AUC_JAGS <- function(iteration.num){
  
  pred <- pzdf %>%
    filter(iter == {{iteration.num}}) %>%
    dplyr::select(pz) %>%
    as_vector()
  
  resp <- pzdf %>%
    filter(iter == {{iteration.num}}) %>%
    dplyr::select(y_obs) %>%
    as_vector()

  ROC <- roc(response =resp, predictor =pred)
  
  AUC <- auc(ROC)
  
  return(AUC)
}

iters <- 1:max(pzdf$iter)

AUC_list <- lapply(iters, AUC_JAGS)

AUC_df <- as.data.frame(do.call(rbind,AUC_list) ) %>%
  rename(AUC = V1)

AUC_mean <- AUC_df %>%
  summarise(mean = mean(AUC))

# Plot and assess ---------------------------------------------------------

ggplot() +
  geom_histogram(data = AUC_df, aes(x = AUC)) +
  geom_vline(data = AUC_mean, aes(xintercept = mean),
             linetype = 2) +
  labs(y = "Number of posterior samples")


# Export ------------------------------------------------------------------


ggsave(here('pictures',
                'R',
                'SI',
                'Figure_adultoccupancy_gof.png'),
       height = 4.5,
       width = 6,
       units = "in")

