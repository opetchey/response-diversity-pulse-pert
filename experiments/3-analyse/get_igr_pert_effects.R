rm(list = ls())

library(dplyr)

source(here("r/intrinsic_growth_rate.r"))

## read in experimental design
## Created by code in "design_expt.r" script in the experiments/1-design folder
expt <- readRDS(here("data/expt_communities.RDS"))
temperature_treatments <- readRDS(here("data/temperature_treatments.RDS"))

## Get control and perturbation temperatures
## Assume the perturbation is a decrease in temperature
perturbation_temperature <- min(c(temperature_treatments$temperature_control,
                         temperature_treatments$temperature_pulse))
control_temperature <- max(c(temperature_treatments$temperature_control,
                         temperature_treatments$temperature_pulse))
#min_max_temps <- tibble(temperature = c(min_temperature, max_temperature),
#                        y = c(0,0))
#buffer <- (max_temperature - min_temperature ) / 5
#temperatures <- seq(min_temperature - buffer, max_temperature + buffer, 0.1)

## Expand expt to make a species in row dataset

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

species_igr_pert_effect <- species_pars |> 
  mutate(control_temperature = control_temperature,
         perturbed_temperature = perturbation_temperature,
         control_igr = intrinsic_growth_rate2(a_b_i,
                                              b_opt_i,
                                              s_i,
                                              a_d_i,
                                              z_i,
                                              control_temperature),
         perturbation_igr = intrinsic_growth_rate2(a_b_i,
                                              b_opt_i,
                                              s_i,
                                              a_d_i,
                                              z_i,
                                              perturbation_temperature),
         igr_pert_effect = perturbation_igr - control_igr) |> 
  select(case_id, species_id, igr_pert_effect)


saveRDS(species_igr_pert_effect, here("data/species_igr_pert_effect.RDS"))
