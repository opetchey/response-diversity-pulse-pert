rm(list=ls())

library(tidyverse)
library(readxl)
library(MESS)
library(here)
library(cowplot)
library(GGally)
library(patchwork)

## source any required user defined functions
source(here("r/my_auc.R"))

## sub-sample rate
keep_every_t <- 20

## read data
temp <- readRDS(here("data/sim_results.RDS"))
community_pars <- temp$community_pars
dynamics <- temp$dynamics_long
species_igr_pert_effect <- readRDS(here("data/species_igr_pert_effect.RDS"))



## Community level stability ----

## Calculate and visualise the community level stability
## measures
## First for each time point in each case
comm_time_stab <- dynamics |>
  ## remove rows where biomass is 0 in both control and treatment
  #filter((Con.M + Dist.M) != 0) |>
  group_by(case_id, community_id, replicate_id, Time, Treatment) %>%
  summarise(tot_ab = sum(Abundance, na.rm = T)) %>%
  pivot_wider(names_from = Treatment, values_from = tot_ab) %>%
  mutate(comm_LRR = log(Perturbed / Control),
         comm_deltabm = (Perturbed - Control) /
           (Perturbed + Control))


comm_time_stab |>
  filter(case_id =="Comm-6-rep-1",
        # Time > 10400, Time < 10500,
         (Time %% keep_every_t) == 0) |> 
  ggplot(aes(x = Time, y = comm_deltabm)) +
  geom_line()

## And now across time points by auc
comm_stab <- comm_time_stab |>
  filter((Time %% keep_every_t) == 0) |> 
  group_by(case_id, community_id, replicate_id) |> 
  summarise(comm_tot_deltabm = my_auc_func(Time, comm_deltabm)) |> 
  mutate(OEV = sqrt(abs(comm_tot_deltabm)))
## I would be cautious about using splines without checking they are
## working as expected, here, and for the species level auc calculations.
## They might be not very well constrained at the two ends of the RD axis,
## and then make for some funking results.


## Now the species level stabilities ----

## calculate stabilities (for now I don't look at the proportional stab)
species_time_stab <- dynamics |>
  #filter(Time > 9999 & Time < 10051) |> 
  select(-Temperature) |> 
  ## remove rows where biomass is 0 in both control and treatment
  #filter((Con.M + Dist.M) != 0) |>
  pivot_wider(names_from = Treatment, values_from = Abundance) |> 
  mutate(spp_deltabm = (Perturbed - Control)/
           (Perturbed + Control))
species_stab <- species_time_stab |>
  filter((Time %% keep_every_t) == 0) |> 
  group_by(case_id, community_id, replicate_id, Species_ID) |> 
  summarise(species_tot_deltabm = my_auc_func(Time, spp_deltabm))
## AUC calculation is giving warnings when there are duplicate RD values
## Need to check how important this is.


## Calculate community level indicies based on
## species level traits
comm_indicies <- species_stab |> 
  group_by(community_id, replicate_id, case_id) |> 
  summarise(mean_spp_deltabm = mean(species_tot_deltabm),
            var_spp_deltabm = var(species_tot_deltabm))

## Calculate response diversity from response curve traits
igr_respdiv <- species_igr_pert_effect |> 
  group_by(case_id) |> 
  summarise(mean_igr_effect = mean(igr_pert_effect),
            var_igr_effect = var(igr_pert_effect))

## merge with comm stability measures 
comm_all <- full_join(comm_stab, comm_indicies) |> 
  full_join(igr_respdiv)

saveRDS(comm_all, here("data/community_measures.RDS"))



## merge and save species level measures
species_igr_pert_effect <-
  species_igr_pert_effect |> 
  mutate(species_id = str_replace(species_id, "-", ""))
species_stab <- species_stab |> 
  rename(species_id = Species_ID)
species_all <- full_join(species_igr_pert_effect, species_stab)
saveRDS(species_all, here("data/species_measures.RDS"))



