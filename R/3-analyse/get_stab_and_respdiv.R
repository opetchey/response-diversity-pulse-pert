
## sub-sample rate
keep_every_t <- 10

## read data
temp <- readRDS(here("data", pack, "sim_results.RDS"))
community_pars <- temp$community_pars
dynamics <- temp$dynamics_long
species_igr_pert_effect <- readRDS(here("data", pack, "species_igr_pert_effect.RDS"))



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
         comm_RR = (Perturbed - Control) /
           (Perturbed + Control))


# comm_time_stab |>
#   filter(case_id =="Comm-6-rep-1",
#          # Time > 10400, Time < 10500,
#          (Time %% keep_every_t) == 0) |> 
#   ggplot(aes(x = Time, y = comm_deltabm)) +
#   geom_line()

## And now across time points by auc
comm_stab <- comm_time_stab |>
  filter((Time %% keep_every_t) == 0) |> 
  group_by(case_id, community_id, replicate_id) |> 
  summarise(comm_RR_AUC = my_auc_func_linear(Time, comm_RR),
            OEV = my_auc_func_linear(Time, abs(comm_RR)))
## I would be cautious about using splines without checking they are
## working as expected, here, and for the species level auc calculations.
## They might be not very well constrained at the two ends of the RD axis,
## and then make for some funking results.


## Now the species level stabilities ----

tot_comm_ab <- dynamics |>
  ## remove rows where biomass is 0 in both control and treatment
  #filter((Con.M + Dist.M) != 0) |>
  group_by(case_id, community_id, replicate_id, Time, Treatment) %>%
  summarise(tot_ab = sum(Abundance, na.rm = T))

## calculate stabilities (absolute abundance)
species_time_stab1 <- dynamics |>
  #filter(Time > 9999 & Time < 10051) |> 
  select(-Temperature) |> 
  ## remove rows where biomass is 0 in both control and treatment
  #filter((Con.M + Dist.M) != 0) |>
  pivot_wider(names_from = Treatment, values_from = Abundance) |> 
 # group_by(case_id, community_id) |> 
  #mutate(con.tot = sum(Control),
 #        treat.tot = sum(Perturbed)) 
  mutate(spp_RR = (Perturbed - Control)/
           (Perturbed + Control)) |> 
  select(-Control, -Perturbed)

## calculate stabilities (relative abundance)
species_time_stab2 <- dynamics |>
  full_join(tot_comm_ab) |> 
  mutate(pi = Abundance / tot_ab) |> 
  #filter(Time > 9999 & Time < 10051) |> 
  select(-Temperature, -Abundance, -tot_ab) |> 
  ## remove rows where biomass is 0 in both control and treatment
  #filter((Con.M + Dist.M) != 0) |>
  pivot_wider(names_from = Treatment, values_from = pi) |> 
  # group_by(case_id, community_id) |> 
  #mutate(con.tot = sum(Control),
  #        treat.tot = sum(Perturbed))
  mutate(delta_pi = Perturbed - Control) |> 
  select(-Control, -Perturbed)


species_time_stab <- full_join(species_time_stab1, species_time_stab2)

# temp <- species_time_stab |> 
#   filter((Time %% keep_every_t) == 0,
#          case_id == "Comm-1-rep-1",
#          Species_ID == "Spp1") %>%
#   summarise(species_RR_AUC = my_auc_func_linear(Time, spp_RR),
#             species_delta_pi_AUC = my_auc_func_linear(Time, delta_pi))

species_stab <- species_time_stab |>
  filter((Time %% keep_every_t) == 0) |> 
  group_by(case_id, community_id, replicate_id, Species_ID) |> 
  summarise(species_RR_AUC = my_auc_func_linear(Time, spp_RR),
            species_delta_pi_AUC = my_auc_func_linear(Time, delta_pi))



## Calculate community level indicies based on
## species level traits
comm_indicies <- species_stab |> 
  group_by(community_id, replicate_id, case_id) |> 
  summarise(mean_species_RR_AUC = mean(species_RR_AUC),
            var_species_RR_AUC = var(species_RR_AUC),
            mean_species_delta_pi_AUC = mean(species_delta_pi_AUC),
            var_species_delta_pi_AUC = var(species_delta_pi_AUC))

## Calculate response diversity from response curve traits
igr_respdiv <- species_igr_pert_effect |> 
  group_by(case_id) |> 
  summarise(mean_igr_effect = mean(igr_pert_effect),
            var_igr_effect = var(igr_pert_effect),
            RD_diss_igr_effect = resp_div(igr_pert_effect, sign_sens = FALSE),
            RD_div_igr_effect = resp_div(igr_pert_effect, sign_sens = TRUE))

## merge with comm stability measures 
comm_all <- full_join(comm_stab, comm_indicies) |> 
  full_join(igr_respdiv)

saveRDS(comm_all, here("data", pack, "community_measures.RDS"))



## merge and save species level measures
species_igr_pert_effect <-
  species_igr_pert_effect |> 
  mutate(species_id = str_replace(species_id, "-", ""))
species_stab <- species_stab |> 
  rename(species_id = Species_ID)
species_all <- full_join(species_igr_pert_effect, species_stab)
saveRDS(species_all, here("data", pack, "species_measures.RDS"))
