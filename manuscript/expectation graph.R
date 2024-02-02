## Quick graph results

rm(list = ls())
library(tidyverse)
library(readxl)
library(here)
library(kableExtra)
library(patchwork)
library(mgcv)
library(broom)
library(DBI)

source(here("R/0-functions/intrinsic_growth_rate.R"))
source(here("R/0-functions/get_no_interaction_Nequil.R"))

options(dplyr.summarise.inform = FALSE)


## Load data ----
pack <- "pack1"
expt <- readRDS(here("data", pack, "expt_communities.RDS")) |> 
  filter(alpha_ij_sd == 0, alpha_ij_mean == 0)

## get the igr effects already calculated for the pack
species_measures <- readRDS(here("data", pack, "species_measures.RDS")) |> 
  filter(alpha_ij_sd == 0, alpha_ij_mean == 0)

## Get control and perturbation temperatures
temperature_treatments <- readRDS(here("data", pack, "temperature_treatments.RDS"))
perturbation_temperature <- min(c(temperature_treatments$temperature_control,
                                  temperature_treatments$temperature_pulse))
control_temperature <- max(c(temperature_treatments$temperature_control,
                             temperature_treatments$temperature_pulse))


for(i in 1:length(expt$case_id)) {
  
  comm_pars_i <- expt$community_object[i][[1]]
  
  if(i == 1)
    species_pars <- tibble(case_id = rep(expt$case_id[i], length(comm_pars_i$b_opt_i)),
                           species_id = paste0("Spp-", 1:length(comm_pars_i$b_opt_i)),
                           b_opt_i = comm_pars_i$b_opt_i,
                           a_b_i = comm_pars_i$a_b_i,
                           s_i = comm_pars_i$s_i,
                           a_d_i = comm_pars_i$a_d_i,
                           z_i = comm_pars_i$z_i)
  if(i > 1)
    species_pars <- rbind(species_pars,
                          tibble(case_id = rep(expt$case_id[i], length(comm_pars_i$b_opt_i)),
                                 species_id = paste0("Spp-", 1:length(comm_pars_i$b_opt_i)),
                                 b_opt_i = comm_pars_i$b_opt_i,
                                 a_b_i = comm_pars_i$a_b_i,
                                 s_i = comm_pars_i$s_i,
                                 a_d_i = comm_pars_i$a_d_i,
                                 z_i = comm_pars_i$z_i))
  
}

species_Nequil_pert_effect <- species_pars |> 
  mutate(control_temperature = control_temperature,
         perturbed_temperature = perturbation_temperature,
         Nequil_control = get_Nequil(input_com_params = list(a_b = species_pars$a_b_i,
                                                             b_opt = species_pars$b_opt_i,
                                                             s = species_pars$s_i,
                                                             a_d = species_pars$a_d_i,
                                                             z = species_pars$z_i),
                                     control_temperature),
         Nequil_perturbation = get_Nequil(input_com_params = list(a_b = species_pars$a_b_i,
                                                    b_opt = species_pars$b_opt_i,
                                                    s = species_pars$s_i,
                                                    a_d = species_pars$a_d_i,
                                                    z = species_pars$z_i),
                            perturbation_temperature),
         Nequil_pert_effect = Nequil_perturbation - Nequil_control) |>
  select(case_id, species_id, Nequil_pert_effect, Nequil_perturbation, Nequil_control) |> 
  mutate(species_id = str_replace(species_id, pattern = "-", replacement = ""))


species_measures_1 <- species_measures |>
  #select(case_id, species_id, igr_pert_effect) |> 
  full_join(species_Nequil_pert_effect)

community_effects <- species_measures_1 |> 
  group_by(case_id) |> 
  summarise(sum_delta_Nequil = sum(Nequil_pert_effect),
            sum_delta_igr = sum(igr_pert_effect),
            sum_Nequil_control = sum(Nequil_control),
            sum_Nequil_perturbation = sum(Nequil_perturbation),
            sum_delta_Nequil2 = sum_Nequil_perturbation - sum_Nequil_control)

species_measures_1 |> 
  ggplot(aes(x = igr_pert_effect, y = Nequil_pert_effect)) +
  geom_line() +
  xlab("Difference in intrinsic growth rate\nbetween perturbed and control treatment") +
  ylab("Difference in equilibrium abundance between\nperturbed and control treatment") +
  ggtitle("Relationships between effect of perturbation\non species intrinsic growth rate and species equilibrium abundance")

species_measures_1 |> 
  ggplot(aes(x = Nequil_pert_effect, y = species_RR_AUC)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "darkgrey") +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "darkgrey") +
  geom_point() +
  ylab("Species AUC of response ratio") +
  xlab("Difference in equilibrium abundance between\nperturbed and control treatment")

community_effects |> 
  ggplot(aes(x = sum_delta_igr, y = sum_delta_Nequil)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "darkgrey") +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "darkgrey") +
  geom_line() +
  xlab("Community response trait\n(Sum of perturbation effect on species intrinsic growth rate)") +
  ylab("Community stability (0 = maximum stability)\n(Perturbation effect on total abundance at equilibrium)")
 # ggtitle("Relationship between community stability and a community response trait\n(sum of perturbation effects on species intrinsic growth rate")
ggsave(here("manuscript/simple_expectation.pdf"),
       width = 5, height = 5)
