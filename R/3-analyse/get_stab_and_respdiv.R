## Code in this file calculates:
## 
## 1. The stability of community biomass and various measures of community-level response diversity
## This community-level data is then saved in the file **community_measures.RDS**
##
## 2. The species-level stability and igr effect.
## This species-level data is saved in the file **species_measures.RDS**


## source any required user defined functions
source(here("R/0-functions/my_auc.R"))
source(here("R/0-functions/Ross_et_al_functions.R"))

## sub-sample rate
keep_every_t <- 1

#pack<-'pack2'
expt <- readRDS(here("data", pack, "expt_communities.RDS"))

conn_dynamics <- dbConnect(RSQLite::SQLite(), here("data", pack, "/dynamics.db"))
#dbListTables(conn_dynamics)
dynamics <- tbl(conn_dynamics, "dynamics")

tot_biomass <- dynamics |>
  #collect()
  ## remove rows where biomass is 0 in both control and treatment
  #filter((Con.M + Dist.M) != 0) |>
  filter((Time %% keep_every_t) == 0) |> 
  group_by(case_id, replicate_id, Time, Treatment) %>%
  summarise(tot_ab = sum(Abundance, na.rm = T)) %>%
  collect()


########



## read data
#temp <- readRDS(here("data", pack, "sim_results.RDS"))
#community_pars <- temp$community_pars
#dynamics <- temp$dynamics_long
species_igr_pert_effect <- readRDS(here("data", pack, "species_igr_pert_effect.RDS"))



## Community level stability ----

## Calculate and visualise the community level stability
## measures
## First for each time point in each case
comm_time_stab <- tot_biomass |>
  ## remove rows where biomass is 0 in both control and treatment
  #filter((Con.M + Dist.M) != 0) |>
  #group_by(case_id, community_id, replicate_id, Time, Treatment) %>%
  #summarise(tot_ab = sum(Abundance, na.rm = T)) %>%
  pivot_wider(names_from = Treatment, values_from = tot_ab) %>%
  mutate(comm_LRR = log(Perturbed / Control),
         comm_RR = (Perturbed - Control) /
           (Perturbed + Control)) 


## And now across time points by auc
comm_stab <- comm_time_stab |>
  #filter(case_id %in% comms_without_nas) |> 
  #filter((Time %% keep_every_t) == 0) |> 
  group_by(case_id, replicate_id) |> 
  summarise(comm_RR_AUC = my_auc_func_linear(Time, comm_RR),
            OEV = my_auc_func_linear(Time, abs(comm_RR)))

## Now the species level stabilities ----

tot_comm_ab <- dynamics |>
  #filter(case_id %in% comms_without_nas) |> 
  filter((Time %% keep_every_t) == 0) |> 
  ## remove rows where biomass is 0 in both control and treatment
  #filter((Con.M + Dist.M) != 0) |>
  group_by(case_id, replicate_id, Time, Treatment) %>%
  summarise(tot_ab = sum(Abundance, na.rm = T)) |> 
  collect()

## calculate stabilities (absolute abundance)

threshold <- other_pars$spp_RR_calc_threshold ## needed for database to have access
species_time_stab1 <- dynamics |>
  filter((Time %% keep_every_t) == 0) |> 
  pivot_wider(names_from = Treatment, values_from = Abundance) |> 
  ## threshold for calculating spp_RR ****IMPORTANT
  mutate(spp_RR = ifelse((Perturbed + Control) > threshold,
                         (Perturbed - Control) / (Perturbed + Control),
                         NA)) |> 
  select(-Control, -Perturbed) %>%
  collect() 


## calculate stabilities (relative abundance)
temp123 <- dynamics |>
  #filter(case_id %in% comms_without_nas) |> 
  filter((Time %% keep_every_t) == 0) |> 
  collect()

species_time_stab2 <- temp123 %>%
  full_join(tot_comm_ab) |> 
  mutate(pi = Abundance / tot_ab) |> 
  #filter(Time > 9999 & Time < 10051) |> 
  select(-Abundance, -tot_ab) |> 
  ## remove rows where biomass is 0 in both control and treatment
  #filter((Con.M + Dist.M) != 0) |>
  pivot_wider(names_from = Treatment, values_from = pi) |> 
  # group_by(case_id, community_id) |> 
  #mutate(con.tot = sum(Control),
  #        treat.tot = sum(Perturbed))
  mutate(delta_pi = Perturbed - Control) |> 
  select(-Control, -Perturbed) |> 
  collect()


species_time_stab <- full_join(species_time_stab1, species_time_stab2)



species_stab <- species_time_stab |>
  #filter(!(case_id %in% temp$case_id & Species_ID %in% temp$Species_ID)) |> 
  #filter((Time %% keep_every_t) == 0) |> 
  group_by(case_id, replicate_id, Species_ID) |> 
  summarise(species_RR_AUC = my_auc_func_linear(Time, spp_RR),
            species_delta_pi_AUC = my_auc_func_linear(Time, delta_pi))


## Calculate response diversity from species dynamics
comm_indicies <- species_stab |> 
  mutate(abs.RR = abs(species_RR_AUC))|>
  group_by(replicate_id, case_id) |> 
  summarise(mean_species_RR_AUC = mean(species_RR_AUC, na.rm = TRUE),
            var_species_RR_AUC = var(species_RR_AUC, na.rm = TRUE),
            RD_diss_species_RR_AUC = resp_div(species_RR_AUC, sign_sens = FALSE, na.rm = TRUE),
            RD_div_species_RR_AUC = resp_div(species_RR_AUC, sign_sens = TRUE, na.rm = TRUE),
            mean_species_abs_RR = mean(abs.RR),
            RD_diss_species_RR_abs = resp_div(abs.RR, sign_sens = FALSE, na.rm = TRUE),
            RD_div_species_RR_abs = resp_div(abs.RR, sign_sens = TRUE, na.rm = TRUE))






## Calculate response diversity from response curve traits
igr_respdiv <- species_igr_pert_effect |> 
  group_by(case_id) |> 
  summarise(mean_igr_effect = mean(igr_pert_effect),
            var_igr_effect = var(igr_pert_effect),
            RD_diss_igr_effect = resp_div(igr_pert_effect, sign_sens = FALSE),
            RD_div_igr_effect = resp_div(igr_pert_effect, sign_sens = TRUE))

## merge with comm stability measures 
comm_all <- full_join(comm_stab, comm_indicies) |> 
  full_join(igr_respdiv) |> 
  full_join(expt)

saveRDS(comm_all, here("data", pack, "community_measures.RDS"))



## merge and save species level measures
species_igr_pert_effect <-
  species_igr_pert_effect |> 
  mutate(species_id = str_replace(species_id, "-", ""))
species_stab <- species_stab |> 
  rename(species_id = Species_ID)
species_all <- full_join(species_igr_pert_effect, species_stab) |> 
  full_join(expt)
saveRDS(species_all, here("data", pack, "species_measures.RDS"))
