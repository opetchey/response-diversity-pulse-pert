## merge data from all packs
## 
## library(here)
library(tidyverse)
library(mgcv)
library(broom)

## merge data from all the data packs ----

packs_to_read_in <- c("pack1")
for(i in 1:length(packs_to_read_in)) {
  print(i)
  if(i == 1) {
    expt <- readRDS(here(paste0("data/", 
                                packs_to_read_in[i],
                                "/expt_communities.RDS"))) %>%
      mutate(pack = packs_to_read_in[i])
    temp <- readRDS(here(paste0("data/",
                                packs_to_read_in[i],
                                "/sim_results.RDS")))
    community_pars <- temp$community_pars %>%
      mutate(pack = packs_to_read_in[i])
    dynamics <- temp$dynamics_long %>%
      mutate(pack = packs_to_read_in[i])
    
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
    temp <- readRDS(here(paste0("data/",
                                packs_to_read_in[i],
                                "/sim_results.RDS")))
    t_community_pars <- temp$community_pars %>%
      mutate(pack = packs_to_read_in[i])
    t_dynamics <- temp$dynamics_long %>%
      mutate(pack = packs_to_read_in[i])
    
    t_species_response_traits <- readRDS(here(paste0("data/",
                                                     packs_to_read_in[i],
                                                     "/species_measures.RDS"))) %>%
      mutate(pack = packs_to_read_in[i])
    t_comm_all <- readRDS(here(paste0("data/",
                                      packs_to_read_in[i],
                                      "/community_measures.RDS"))) %>%
      mutate(pack = packs_to_read_in[i])
    
    expt <- rbind(expt, t_expt)
    community_pars <- rbind(community_pars, t_community_pars)
    dynamics <- rbind(dynamics, t_dynamics)
    species_response_traits <- rbind(species_response_traits,
                                     t_species_response_traits)
    comm_all <- rbind(comm_all, t_comm_all)
    
  }
  
}

#expt <- expt %>%
  #mutate(case_id = paste(case_id, pack))
#community_pars <- community_pars %>%
#  mutate(case_id = paste(case_id, pack))
#dynamics <- dynamics #%>%
#  mutate(case_id = paste(case_id, pack))
species_response_traits <- species_response_traits %>%
  #mutate(case_id = paste(case_id, pack)) %>%
  #full_join(expt, by = join_by(case_id == case_id, pack == pack)) %>%
  full_join(expt) %>%
  mutate(alpha_ij_sd = round(alpha_ij_sd, 2))
comm_all <- comm_all %>%
  #mutate(case_id = paste(case_id, pack)) %>%
  #full_join(expt, by = join_by(case_id == case_id, pack == pack)) %>%
  full_join(expt) %>%
  mutate(alpha_ij_sd = round(alpha_ij_sd, 2))

saveRDS(expt, here("data/merged/expt.RDS"))
saveRDS(community_pars, here("data/merged/community_pars.RDS"))
#saveRDS(dynamics, here("data/merged/dynamics.RDS"))
saveRDS(species_response_traits, here("data/merged/species_response_traits.RDS"))
saveRDS(comm_all, here("data/merged/comm_all.RDS"))


## save dynamics as a database, since it is rather large
library(DBI)
library(RSQLite)
conn <- dbConnect(RSQLite::SQLite(), here("data/merged/dynamics.db"))
dbWriteTable(conn, "dynamics", dynamics)
#dbListTables(conn)
dbDisconnect(conn)

