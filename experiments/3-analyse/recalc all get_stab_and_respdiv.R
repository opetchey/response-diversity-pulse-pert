## This code reads in the dynamics and species_igr traits from each pack and
## then calculates:
## measures of community stability (AUC), species stability (AUC),
## measure of community level trait summaries (e.g. trait mean, trait diversity)



rm(list=ls())

library(tidyverse)
library(readxl)
library(MESS)
library(here)
#library(cowplot)
#library(GGally)
library(patchwork)

## source any required user defined functions
source(here("R/my_auc.R"))
source(here("R/Ross_et_al_functions.R"))

## sub-sample rate
keep_every_t <- 10

packs_to_read_in <- c("pack1", "pack2", "pack3", "pack4", "pack5", "pack6")
for(i in 1:length(packs_to_read_in)) {
  
  
  ## read data
  temp <- readRDS(here(paste0("data/", packs_to_read_in[i], "/sim_results.RDS")))
  community_pars <- temp$community_pars
  dynamics <- temp$dynamics_long
  species_igr_pert_effect <- readRDS(here(paste0("data/", packs_to_read_in[i], "/species_igr_pert_effect.RDS")))
  
  
  
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
    summarise(comm_tot_deltabm_spline = my_auc_func_spline(Time, comm_deltabm),
              comm_tot_deltabm_raw = my_auc_func_raw(Time, comm_deltabm)) |> 
    mutate(OEV_spline = sqrt(abs(comm_tot_deltabm_spline)),
           OEV_raw = sqrt(abs(comm_tot_deltabm_raw)))
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
    summarise(species_tot_deltabm_spline = my_auc_func_spline(Time, spp_deltabm),
              species_tot_deltabm_raw = my_auc_func_raw(Time, spp_deltabm))
  ## AUC calculation is giving warnings when there are duplicate RD values
  ## Need to check how important this is.
  
  
  ## Calculate community level indicies based on
  ## species level traits
  comm_indicies <- species_stab |> 
    group_by(community_id, replicate_id, case_id) |> 
    summarise(mean_spp_deltabm_spline = mean(species_tot_deltabm_spline),
              var_spp_deltabm_spline = var(species_tot_deltabm_spline),
              mean_spp_deltabm_raw = mean(species_tot_deltabm_raw),
              var_spp_deltabm_raw = var(species_tot_deltabm_raw))
  
  ## Calculate response diversity from response curve traits
  igr_respdiv <- species_igr_pert_effect |> 
    group_by(case_id) |> 
    summarise(mean_igr_effect = mean(igr_pert_effect),
              var_igr_effect = var(igr_pert_effect),
              RD_diss = resp_div(igr_pert_effect, sign_sens = FALSE),
              RD_div = resp_div(igr_pert_effect, sign_sens = TRUE))
  
  ## merge with comm stability measures 
  comm_all <- full_join(comm_stab, comm_indicies) |> 
    full_join(igr_respdiv)
  
  saveRDS(comm_all, here(paste0("data/", packs_to_read_in[i], "/community_measures.RDS")))
  
  
  
  ## merge and save species level measures
  species_igr_pert_effect <-
    species_igr_pert_effect |> 
    mutate(species_id = str_replace(species_id, "-", ""))
  species_stab <- species_stab |> 
    rename(species_id = Species_ID)
  species_all <- full_join(species_igr_pert_effect, species_stab)
  saveRDS(species_all, here(paste0("data/", packs_to_read_in[i], "/species_measures.RDS")))
  
}

