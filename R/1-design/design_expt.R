## prelims ----

#rm(list = ls())

library(tidyverse)
library(readxl)
library(here)
library(kableExtra)
library(patchwork)

source(here("R/0-functions/make a community.R"))

## define the experimental treatments ----
#b_opt_mean_treatment <- seq(17.5, 19.5, 1)
#b_opt_range_treatment <- c(5) ## used in packs 1-6
# seq(3, 7, 1)
alpha_ij_mean_treatment <- c(0)

rep_names <- paste0("rep-", 1:num_replicates)

## make experiment ----
expt <- expand.grid(b_opt_mean = b_opt_mean_treatment,
                    b_opt_range = b_opt_range_treatment,
                    alpha_ij_mean = alpha_ij_mean_treatment,
                    alpha_ij_sd = alpha_ij_sd_treatment,
                    rep_names = rep_names)
expt <- expt %>%
  mutate(community_id = paste0("Comm-", 1:nrow(expt)),
         case_id = paste(community_id, rep_names, sep = "-"))

## make communities ----
S <- 10
a_b <- 0.3 #3
s <- 10
a_d <- 0 # 0.01
z <- 0.05
#alpha_ij_mean <- 0 # 0.5
#alpha_ij_sd <- 0 #0.1
intrafactor <- 1
community_object <- expt %>%
  rowwise(community_id) %>%
  #group_by(b_opt_mean, b_opt_range, rep_names, community_id) %>%
  do(community_object = make_a_community(S = S,
                                         a_b = a_b,
                                         b_opt_mean = .$b_opt_mean,
                                         b_opt_range = .$b_opt_range,
                                         s = s,
                                         a_d = a_d,
                                         z = z,
                                         alpha_ij_mean = .$alpha_ij_mean,
                                         alpha_ij_sd = .$alpha_ij_sd,
                                         intrafactor = intrafactor))
expt <- cbind(expt, community_object)
saveRDS(expt, here("data", pack, "expt_communities.RDS"))


## Create the perturbation treatment ----

temperature_treatments <- tibble(
  time = seq(0, before_pulse + duration_pulse + after_pulse, 1),
  temperature_control1 = rep(temperature_control, (before_pulse + duration_pulse + after_pulse + 1)),
  temperature_pulse1 = c(rep(temperature_control, before_pulse + 1),
                         rep(temperature_pulse, duration_pulse),
                         rep(temperature_control, after_pulse))) %>%
  rename(temperature_control = temperature_control1,
         temperature_pulse = temperature_pulse1)

#temperature_treatments %>%
#  pivot_longer(names_to = "treatment_level", values_to = "temperature", 2:3) %>%
#  ggplot(aes(x = time, y = temperature, col = treatment_level)) +
#  geom_line()

saveRDS(temperature_treatments, here("data", pack, "temperature_treatments.RDS"))






