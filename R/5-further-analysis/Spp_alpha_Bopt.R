## Quick graph results

#rm(list = ls())
library(tidyverse)
library(readxl)
library(here)
library(kableExtra)
library(patchwork)
library(mgcv)
library(broom)
library(DBI)

source(here("R/0-functions/intrinsic_growth_rate.R"))

options(dplyr.summarise.inform = FALSE)


#### Load data ####
#pack <- "pack1"
comm_all <- readRDS(here("data", pack, "community_measures.RDS"))
species_measures <- readRDS(here("data", pack, "species_measures.RDS"))



### explore ###

ggplot(comm_all, aes ( x= b_opt_mean, y = comm_RR_AUC))+
  geom_hline(yintercept = 0)+
  geom_point(alpha = 0.3)+
  facet_wrap(~alpha_ij_sd)+
  theme_bw()

#ggsave(plot = last_plot(), file = here('output/topt.png'), width = 7, height = 8)

column_names <- paste0("Spp", 1:10)


#### T-opt ####

alphaij <- comm_all%>%
select(community_object, case_id, alpha_ij_sd, b_opt_mean,b_opt_range) %>%
 # filter(case_id %in%c ('Comm-10003-rep-4','Comm-10004-rep-4'))%>%
  unnest_wider(community_object)

unnest <- alphaij%>%
  select(case_id, b_opt_i,alpha_ij_sd, b_opt_mean,b_opt_range)%>%
  unnest_longer(b_opt_i, indices_to = "species_id") %>%
  mutate(
    species_column = rep(column_names, each = length(unique(case_id))))


#### Competiton - alpha ####

# Extract the matrix from the list and convert it to a data frame

flattened_df <- alphaij %>%
  select(case_id, alpha_ij)%>%
 # filter(case_id %in%c ('Comm-10003-rep-4','Comm-10004-rep-4'))%>%
  mutate(alpha_ij = lapply(alpha_ij, as.data.frame)) %>%  # Convert the matrix to data frame
  unnest(alpha_ij) %>% # Unnest to convert the list of data frames into separate rows
  rename(Spp1 = V1,
         Spp2 = V2,
         Spp3 = V3,
         Spp4 = V4,
         Spp5 = V5,
         Spp6 = V6,
         Spp7 = V7,
         Spp8 = V8,
         Spp9 = V9,
         Spp10 = V10)%>%
  mutate(species_column = rep(column_names, each = length(unique(case_id)))) %>%
  pivot_longer(c(Spp1,Spp2,Spp3,Spp4,Spp5,Spp6,Spp7,Spp8,Spp9,Spp10), names_to=c('Gegners' ), values_to='competition_value')

mean_alphaij <- flattened_df %>%
  filter(species_column != Gegners) %>%
  group_by(species_column, case_id) %>%
  summarise(mean_competitiveness = mean(competition_value))


#### all info ####
all_spp_info <- left_join(mean_alphaij, unnest) %>%
  select(-species_id)  %>%
  rename(species_id = species_column )

ggplot(all_spp_info, aes(y = mean_competitiveness, x = b_opt_i ))+
  geom_point(alpha = 0.3)+
  labs(x ='Topt', y = 'Mean Competitveness')+
  facet_wrap(~alpha_ij_sd)+
  theme_bw()

#ggsave(plot = last_plot(), file = here('output/Appendix_FigS_competitiveness_topt.pdf'), width = 7, height = 8)

#### now merge with AUCs and igr to see which species benefit and suffer ####
AUC_info <- left_join(all_spp_info, species_measures)

pA<-ggplot(AUC_info, aes(x = mean_competitiveness, y = species_RR_AUC ))+
  geom_point(alpha = 0.3)+
  geom_hline(yintercept = 0)+
  labs(x ='Competitiveness', y = 'Species realised responses')+
  facet_wrap(~alpha_ij_sd)+
  theme_bw()
pA

pB<-ggplot(AUC_info, aes(x = b_opt_i, y = species_RR_AUC ))+
  geom_point(alpha = 0.3)+
  geom_hline(yintercept = 0)+
  facet_wrap(~alpha_ij_sd)+
  labs(x ='Temperature Optimum', y = 'Species realised response')+
  theme_bw()
pB

pC<-ggplot(AUC_info, aes(x = mean_competitiveness, y = igr_pert_effect ))+
  geom_point(alpha = 0.3)+
  geom_hline(yintercept = 0)+
  labs(x ='Competitiveness', y = 'Species fundamental responses')+
  facet_wrap(~alpha_ij_sd)+
  theme_bw()
pC

pD<-ggplot(AUC_info, aes(x = b_opt_i, y = igr_pert_effect ))+
  geom_point(alpha = 0.3)+
  geom_hline(yintercept = 0)+
  labs(x ='Temperature Optimum', y = 'Species fundamental responses')+
  facet_wrap(~alpha_ij_sd)+
  theme_bw()
pD

cowplot::plot_grid(pA, pB, ncol = 2, labels = c('(a)', '(b)', '(c)', '(d)'))
ggsave(plot = last_plot(), file = here('output/Appendix_FigS_competitiveness_topt_realisedResp.png'), width = 14, height = 8)

cowplot::plot_grid(pC, pD, ncol = 2, labels = c('(a)', '(b)', '(c)', '(d)'))
ggsave(plot = last_plot(), file = here('output/Appendix_FigS_competitiveness_topt_igrEffect.png'), width = 14, height = 8)


