library(here)
library(tidyverse)
library(mgcv)
library(broom)


expt <- readRDS(here("data/merged/expt.RDS"))
community_pars <- readRDS(here("data/merged/community_pars.RDS"))
#saveRDS(dynamics, here("data/merged/dynamics.RDS"))
species_response_traits <- readRDS(here("data/merged/species_response_traits.RDS"))
comm_all <- readRDS(here("data/merged/comm_all.RDS"))

#library(DBI)
#library(RSQLite)
conn <- dbConnect(RSQLite::SQLite(), here("data/merged/dynamics.db"))
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

expl <- comm_all %>%
  ungroup() %>%
  nest_by(pack, alpha_ij_sd) %>%
  mutate(model1 = list(gam(comm_tot_deltabm_spline ~ s(mean_spp_deltabm_spline), data = data)),
         model2 = list(gam(comm_tot_deltabm_spline ~ s(RD_diss_spp_deltabm_spline), data = data)),
         model3 = list(gam(comm_tot_deltabm_spline ~ s(RD_div_spp_deltabm_spline), data = data)),
         model4 = list(gam(comm_tot_deltabm_spline ~ s(mean_igr_effect), data = data)),
         model5 = list(gam(comm_tot_deltabm_spline ~ s(RD_diss_igr_effect), data = data)),
         model6 = list(gam(comm_tot_deltabm_spline ~ s(RD_div_igr_effect), data = data)),
         model7 = list(gam(OEV_spline ~ s(mean_spp_deltabm_spline), data = data)),
         model8 = list(gam(OEV_spline ~ s(RD_diss_spp_deltabm_spline), data = data)),
         model9 = list(gam(OEV_spline ~ s(RD_div_spp_deltabm_spline), data = data)),
         model10 = list(gam(OEV_spline ~ s(mean_igr_effect), data = data)),
         model11 = list(gam(OEV_spline ~ s(RD_diss_igr_effect), data = data)),
         model12 = list(gam(OEV_spline ~ s(RD_div_igr_effect), data = data))) %>%
  summarise(rsq_rawcommstab_realised_mean = summary(model1)$r.sq,
            rsq_rawcommstab_realised_RD_diss = summary(model2)$r.sq,
            rsq_rawcommstab_realised_RD_div = summary(model3)$r.sq,
            rsq_rawcommstab_fundamental_mean = summary(model4)$r.sq,
            rsq_rawcommstab_fundamental_RD_diss = summary(model5)$r.sq,
            rsq_rawcommstab_fundamental_RD_div = summary(model6)$r.sq,
            rsq_abscommstab_realised_mean = summary(model7)$r.sq,
            rsq_abscommstab_realised_RD_diss = summary(model8)$r.sq,
            rsq_abscommstab_realised_RD_div = summary(model9)$r.sq,
            rsq_abscommstab_fundamental_mean = summary(model10)$r.sq,
            rsq_abscommstab_fundamental_RD_diss = summary(model11)$r.sq,
            rsq_abscommstab_fundamental_RD_div = summary(model12)$r.sq)

#mod1 <- comm_all %>% 
#  filter(alpha_ij_sd == 0) %>%
#  gam(comm_tot_deltabm_spline ~ s(mean_igr_effect), data = .)
#summary(mod1)


saveRDS(expl, here("data/merged/expl_all.RDS"))


## and the dynamics of total community biomass ----

comm_time_stab <- dynamics |>
  ## remove rows where biomass is 0 in both control and treatment
  #filter((Con.M + Dist.M) != 0) |>
  group_by(pack, case_id, community_id, replicate_id, Time, Treatment) %>%
  summarise(tot_ab = sum(Abundance, na.rm = T)) %>%
  pivot_wider(names_from = Treatment, values_from = tot_ab) %>%
  mutate(comm_LRR = log(Perturbed / Control),
         comm_deltabm = (Perturbed - Control) /
           (Perturbed + Control))
temp <- collect(comm_time_stab)
temp <-  full_join(temp, expt) |> 
  select(-community_object)

saveRDS(temp, here("data/merged/comm_time_stab.RDS"))

dbDisconnect(conn)
