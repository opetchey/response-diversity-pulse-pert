library(here)
library(tidyverse)
library(mgcv)
library(broom)

expt <- readRDS(here("data", pack, "expt_communities.RDS"))
community_pars <- readRDS(here("data", pack, "expt_communities.RDS"))
species_measures <- readRDS(here("data", pack, "species_measures.RDS"))
comm_all <- readRDS(here("data", pack, "community_measures.RDS"))

#library(DBI)
#library(RSQLite)
conn <- dbConnect(RSQLite::SQLite(), here("data", pack, "dynamics.db"))
dynamics <- tbl(conn, "dynamics")


# expt <- expt %>%
#   mutate(case_id = paste(case_id, pack))
# # community_pars <- community_pars %>%
# #   mutate(case_id = paste(case_id, pack))
# # dynamics <- dynamics %>%
# #   mutate(case_id = paste(case_id, pack))
# species_response_traits1 <- species_response_traits %>%
#   mutate(case_id = paste(case_id, pack)) %>%
#   full_join(expt, by = "case_id") %>%
#   mutate(alpha_ij_sd = round(alpha_ij_sd, 2))
# comm_all <- comm_all %>%
#   mutate(case_id = paste(case_id, pack)) %>%
#   full_join(expt) %>%
#   mutate(alpha_ij_sd = round(alpha_ij_sd, 2))


## save dynamics as a database, since it is rather large
# library(DBI)
# library(RSQLite)
# conn <- dbConnect(RSQLite::SQLite(), here("data/merged/dynamics.db"))
# dbWriteTable(conn, "dynamics", dynamics)
# #dbListTables(conn)
# dbDisconnect(conn)

## Here the code for calculating the explanatory power ----

expt_details <- expt |> 
  select(case_id, alpha_ij_sd)
# 
# nrow(na.omit(expt))
# nrow(na.omit(comm_all))
# 
# sumry <- comm_all |>
#  group_by(alpha_ij_sd, replicate_id) |> 
# summarise(num_non_nas = sum(!is.na(OEV)))

#comm_all %>%
# ggplot(., aes(x = RD_div_species_RR_AUC, y = comm_RR_AUC))+
# geom_point()+
# facet_wrap(~alpha_ij_sd)

#expl <- comm_all %>%
#   full_join(expt_details) |> 
#  ungroup() %>%
# filter(alpha_ij_sd == 0.5) #%>%

#mod3<- gam(comm_RR_AUC ~ s(RD_div_species_RR_AUC), data = expl)
#summary(mod3)$r.sq



expl <- comm_all %>%
  full_join(expt_details) |> 
  ungroup() %>%
  nest_by(alpha_ij_sd, replicate_id) %>%
  mutate(model1 = list(gam(comm_RR_AUC ~ s(mean_species_RR_AUC), data = data)),
         model2 = list(gam(comm_RR_AUC ~ s(RD_diss_species_RR_AUC), data = data)),
         model3 = list(gam(comm_RR_AUC ~ s(RD_div_species_RR_AUC), data = data)),
         model4 = list(gam(comm_RR_AUC ~ s(mean_igr_effect), data = data)),
         model5 = list(gam(comm_RR_AUC ~ s(RD_diss_igr_effect), data = data)),
         model6 = list(gam(comm_RR_AUC ~ s(RD_div_igr_effect), data = data)),
         model7 = list(gam(OEV ~ s(mean_species_RR_AUC), data = data)),
         model8 = list(gam(OEV ~ s(RD_diss_species_RR_AUC), data = data)),
         model9 = list(gam(OEV ~ s(RD_div_species_RR_AUC), data = data)),
         model10 = list(gam(OEV ~ s(mean_igr_effect), data = data)),
         model11 = list(gam(OEV ~ s(RD_diss_igr_effect), data = data)),
         model12 = list(gam(OEV ~ s(RD_div_igr_effect), data = data))) %>%
  summarise(rsq_rawcommstab_realised_mean = summary(model1)$r.sq,
            rsq_rawcommstab_realised_RD_diss = summary(model2)$r.sq,
            rsq_rawcommstab_realised_RD_div = summary(model3)$r.sq,
            rsq_rawcommstab_fundamental_mean = summary(model4)$r.sq,
            rsq_rawcommstab_fundamental_RD_diss = summary(model5)$r.sq,
            rsq_rawcommstab_fundamental_RD_div = summary(model6)$r.sq,
            rsq_OEV_realised_mean = summary(model7)$r.sq,
            rsq_OEV_realised_RD_diss = summary(model8)$r.sq,
            rsq_OEV_realised_RD_div = summary(model9)$r.sq,
            rsq_OEV_fundamental_mean = summary(model10)$r.sq,
            rsq_OEV_fundamental_RD_diss = summary(model11)$r.sq,
            rsq_OEV_fundamental_RD_div = summary(model12)$r.sq)



#mod1 <- comm_all %>% 
#  filter(alpha_ij_sd == 0) %>%
#  gam(comm_tot_deltabm_spline ~ s(mean_igr_effect), data = .)
#summary(mod1)


saveRDS(expl, here("data", pack, "expl_all.RDS"))


## and the dynamics of total community biomass ----

comm_time_stab <- dynamics |>
  ## remove rows where biomass is 0 in both control and treatment
  #filter((Con.M + Dist.M) != 0) |>
  group_by(pack, case_id, replicate_id, Time, Treatment) %>%
  summarise(tot_ab = sum(Abundance, na.rm = T)) %>%
  pivot_wider(names_from = Treatment, values_from = tot_ab) %>%
  mutate(comm_LRR = log(Perturbed / Control),
         comm_RR = (Perturbed - Control) /
           (Perturbed + Control))
temp <- collect(comm_time_stab)
temp <-  full_join(temp, expt) |> 
  select(-community_object)

saveRDS(temp, here("data", pack, "comm_time_stab.RDS"))

dbDisconnect(conn)
