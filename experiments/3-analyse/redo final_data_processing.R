library(here)
library(tidyverse)
library(mgcv)
library(broom)

## merge data from all the data packs ----

packs_to_read_in <- c("pack1", "pack2", "pack3", "pack4", "pack5", "pack6")
for(i in 1:length(packs_to_read_in)) {
  if(i == 1) {
    expt <- readRDS(here(paste0("data/", 
                                packs_to_read_in[i],
                                "/expt_communities.RDS"))) %>%
      mutate(pack = packs_to_read_in[i])
    # temp <- readRDS(here(paste0("data/",
    #                             packs_to_read_in[i],
    #                             "/sim_results.RDS")))
    # community_pars <- temp$community_pars %>%
    #   mutate(pack = packs_to_read_in[i])
    # dynamics <- temp$dynamics_long %>%
    #   mutate(pack = packs_to_read_in[i])
    # 
    species_response_traits <- readRDS(here(paste0("data/",
                                                   packs_to_read_in[i],
                                                   "/species_measures.RDS"))) %>%
      mutate(pack = packs_to_read_in[i])
    comm_all <- readRDS(here(paste0("data/",
                                    packs_to_read_in[i],
                                    "/community_measures.RDS"))) %>%
      mutate(pack = packs_to_read_in[i])
    
    
  }
  
  if(i > 1) {
    t_expt <- readRDS(here(paste0("data/", 
                                  packs_to_read_in[i],
                                  "/expt_communities.RDS"))) %>%
      mutate(pack = packs_to_read_in[i])
    # temp <- readRDS(here(paste0("data/",
    #                             packs_to_read_in[i],
    #                             "/sim_results.RDS")))
    # t_community_pars <- temp$community_pars %>%
    #   mutate(pack = packs_to_read_in[i])
    # t_dynamics <- temp$dynamics_long %>%
    #   mutate(pack = packs_to_read_in[i])
    
    t_species_response_traits <- readRDS(here(paste0("data/",
                                                     packs_to_read_in[i],
                                                     "/species_measures.RDS"))) %>%
      mutate(pack = packs_to_read_in[i])
    t_comm_all <- readRDS(here(paste0("data/",
                                      packs_to_read_in[i],
                                      "/community_measures.RDS"))) %>%
      mutate(pack = packs_to_read_in[i])
    
    expt <- rbind(expt, t_expt)
    # community_pars <- rbind(community_pars, t_community_pars)
    # dynamics <- rbind(dynamics, t_dynamics)
    species_response_traits <- rbind(species_response_traits,
                                     t_species_response_traits)
    comm_all <- rbind(comm_all, t_comm_all)
    
  }
  
}

expt <- expt %>%
  mutate(case_id = paste(case_id, pack))
# community_pars <- community_pars %>%
#   mutate(case_id = paste(case_id, pack))
# dynamics <- dynamics %>%
#   mutate(case_id = paste(case_id, pack))
species_response_traits <- species_response_traits %>%
  mutate(case_id = paste(case_id, pack)) %>%
  full_join(expt, by = "case_id") %>%
  mutate(alpha_ij_sd = round(alpha_ij_sd, 2))
comm_all <- comm_all %>%
  mutate(case_id = paste(case_id, pack)) %>%
  full_join(expt) %>%
  mutate(alpha_ij_sd = round(alpha_ij_sd, 2))

#saveRDS(expt, here("data/merged/expt.RDS"))
#saveRDS(community_pars, here("data/merged/community_pars.RDS"))
#saveRDS(dynamics, here("data/merged/dynamics.RDS"))
saveRDS(species_response_traits, here("data/merged/species_response_traits.RDS"))
saveRDS(comm_all, here("data/merged/comm_all.RDS"))


## save dynamics as a database, since it is rather large
# library(DBI)
# library(RSQLite)
# conn <- dbConnect(RSQLite::SQLite(), here("data/merged/dynamics.db"))
# dbWriteTable(conn, "dynamics", dynamics)
# #dbListTables(conn)
# dbDisconnect(conn)

## Here the code for calculating the explanatory power ----

expl1 <- comm_all %>%
  ungroup() %>%
  nest_by(pack, alpha_ij_sd) %>%
  mutate(model = list(gam(comm_tot_deltabm_spline ~ s(mean_spp_deltabm_spline), data = data))) %>%
  summarise(rsq_realised = summary(model)$r.sq)

expl2 <- comm_all %>%
  ungroup() %>%
  nest_by(pack, alpha_ij_sd) %>%
  mutate(model = list(gam(comm_tot_deltabm_spline ~ s(mean_igr_effect), data = data))) %>%
  summarise(rsq_fundamental = summary(model)$r.sq)


#mod1 <- comm_all %>% 
#  filter(alpha_ij_sd == 0) %>%
#  gam(comm_tot_deltabm_spline ~ s(mean_igr_effect), data = .)
#summary(mod1)


expl_all <- full_join(expl1, expl2) %>%
  pivot_longer(names_to = "variable",
               values_to = "value",
               cols = 3:4)

saveRDS(expl_all, here("data/merged/expl_all.RDS"))


## and the dynamics of total community biomass ----

# comm_time_stab <- dynamics |>
#   ## remove rows where biomass is 0 in both control and treatment
#   #filter((Con.M + Dist.M) != 0) |>
#   group_by(case_id, community_id, replicate_id, Time, Treatment) %>%
#   summarise(tot_ab = sum(Abundance, na.rm = T)) %>%
#   pivot_wider(names_from = Treatment, values_from = tot_ab) %>%
#   mutate(comm_LRR = log(Perturbed / Control),
#          comm_deltabm = (Perturbed - Control) /
#            (Perturbed + Control))
# saveRDS(comm_time_stab, here("data/merged/comm_time_stab.RDS"))