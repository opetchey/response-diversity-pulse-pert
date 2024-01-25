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

pack<-'pack2'
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

## look at pattern of NA communities
# comms_with_nas <- comm_time_stab |> 
#   full_join(expt) |> 
#   group_by(case_id, alpha_ij_sd, replicate_id) |> 
#   summarise(num = n(),
#             num_na = sum(is.na(comm_LRR)))
# 
# comms_with_nas1 <- comms_with_nas |> 
#   group_by(alpha_ij_sd, replicate_id) |> 
#   summarise(sum(num_na < 100))
# 
# comms_with_nas |> 
#   ggplot(aes(x = alpha_ij_sd, y = num_na, col = replicate_id)) +
#   geom_jitter(position = position_jitter(width = 0.02, height = 1))


## find communities with many NA values
# comms_without_nas <- comm_time_stab |> 
#   group_by(case_id) |> 
#   summarise(num = n(),
#             num_na = sum(is.na(comm_LRR))) |> 
#   filter(num_na == 0) |> 
#   pull(case_id)

# comm_time_stab |>
#   filter(case_id =="Comm-6-rep-1",
#          # Time > 10400, Time < 10500,
#          (Time %% keep_every_t) == 0) |> 
#   ggplot(aes(x = Time, y = comm_deltabm)) +
#   geom_line()

## And now across time points by auc
comm_stab <- comm_time_stab |>
  #filter(case_id %in% comms_without_nas) |> 
  #filter((Time %% keep_every_t) == 0) |> 
  group_by(case_id, replicate_id) |> 
  summarise(comm_RR_AUC = my_auc_func_linear(Time, comm_RR),
            OEV = my_auc_func_linear(Time, abs(comm_RR)))
## I would be cautious about using splines without checking they are
## working as expected, here, and for the species level auc calculations.
## They might be not very well constrained at the two ends of the RD axis,
## and then make for some funking results.

#test <- comm_time_stab |>
#  filter(case_id == "Comm-10016-rep-4")
#my_auc_func_linear(test$Time, test$comm_RR)


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
species_time_stab1 <- dynamics |>
  #filter(case_id %in% comms_without_nas) |> 
  filter((Time %% keep_every_t) == 0) |> 
  #filter(Time > 9999 & Time < 10051) |> 
  #select(-Temperature) |> 
  ## remove rows where biomass is 0 in both control and treatment
  #filter((Con.M + Dist.M) != 0) |>
  pivot_wider(names_from = Treatment, values_from = Abundance) |> 
  # group_by(case_id, community_id) |> 
  #mutate(con.tot = sum(Control),
  #        treat.tot = sum(Perturbed)) 
  ## threshold for calculating spp_RR ****IMPORTANT
  mutate(spp_RR = ifelse((Perturbed + Control) > 1,
                         (Perturbed - Control) / (Perturbed + Control), #remove data point (setting it to NA) when species abundance is very low
                         NA)) |> 
  select(-Control, -Perturbed) %>%
  collect() 


dummy <- dynamics%>%
  filter(case_id %in% c('Comm-100-rep-1')) %>%
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

# temp <- species_time_stab |> 
#   filter((Time %% keep_every_t) == 0,
#          case_id == "Comm-1-rep-1",
#          Species_ID == "Spp1") %>%
#   summarise(species_RR_AUC = my_auc_func_linear(Time, spp_RR),
#             species_delta_pi_AUC = my_auc_func_linear(Time, delta_pi))


# comms_without_nas <- comm_time_stab |> 
#   group_by(case_id) |> 
#   summarise(num = n(),
#             num_na = sum(is.na(comm_LRR))) |> 
#   filter(num_na == 0) |> 
#   pull(case_id)
# 
# 
# temp <- species_time_stab |> 
#   group_by(case_id, Species_ID) |> 
#   summarise(num = n(),
#             num_na = sum(is.na(spp_RR))) |> 
#   filter(num_na != 0)
# 


species_stab <- species_time_stab |>
  #filter(!(case_id %in% temp$case_id & Species_ID %in% temp$Species_ID)) |> 
  #filter((Time %% keep_every_t) == 0) |> 
  group_by(case_id, replicate_id, Species_ID) |> 
  summarise(species_RR_AUC = my_auc_func_linear(Time, spp_RR),
            species_delta_pi_AUC = my_auc_func_linear(Time, delta_pi))


## Calculate response diversity from species dynamics
comm_indicies <- species_stab |> 
  group_by(replicate_id, case_id) |> 
  summarise(mean_species_RR_AUC = mean(species_RR_AUC, na.rm = TRUE),
            var_species_RR_AUC = var(species_RR_AUC, na.rm = TRUE),
            RD_diss_species_RR_AUC = resp_div(species_RR_AUC, sign_sens = FALSE, na.rm = TRUE),
            RD_div_species_RR_AUC = resp_div(species_RR_AUC, sign_sens = TRUE, na.rm = TRUE),
            mean_species_delta_pi_AUC = mean(species_delta_pi_AUC),
            var_species_delta_pi_AUC = var(species_delta_pi_AUC))





## Checking out what's going on with some specific cases, mostly concerning NAs
# case_to_check <- "Comm-1241-rep-1"
# case_to_check <- "Comm-10036-rep-4"
# case_to_check <- "Comm-1000-rep-1"
# case_to_check <- "Comm-1001-rep-1"
# case_to_check <- "Comm-1499-rep-1"
# case_to_check <- "Comm-14174-rep-5" ## all abundances are NA
# 
# species_stab |> 
#   filter(case_id == case_to_check)
# 
# check1 <- dynamics |>
#   filter((Time %% keep_every_t) == 0 & case_id == case_to_check) |> 
#   ##collect()
#   ## remove rows where biomass is 0 in both control and treatment
#   #filter((Con.M + Dist.M) != 0) |>
#   pivot_wider(names_from = Treatment, values_from = Abundance) |>
#   #mutate(spp_RR = (Perturbed - Control) / (Perturbed + Control)) |> 
#   collect()
# 
# temp123 |>
#   filter(case_id == case_to_check & Species_ID == "Spp3")
# 
# xxx1 <- species_time_stab |>
#   filter(case_id == case_to_check & Species_ID == "Spp3")
# x <- xxx1$Time
# y <- xxx1$spp_RR
# 
# species_stab |> 
#   filter(case_id == case_to_check)




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
