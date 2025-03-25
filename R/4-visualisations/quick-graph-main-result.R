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
library(ggpubr)

source(here("R/0-functions/intrinsic_growth_rate.R"))

options(dplyr.summarise.inform = FALSE)


## Load data ----
#pack <- "pack2"
expt <- readRDS(here("data", pack, "expt_communities.RDS"))
community_pars <- readRDS(here("data", pack, "expt_communities.RDS"))

conn_dynamics <- dbConnect(RSQLite::SQLite(), here("data", pack, "/dynamics.db"))
dynamics <- tbl(conn_dynamics, "dynamics")

species_measures <- readRDS(here("data", pack, "species_measures.RDS"))

comm_all <- readRDS(here("data", pack, "community_measures.RDS"))
expl_all <- readRDS(here("data", pack, "expl_all.RDS"))

comm_time_stab <- readRDS(here("data", pack, "comm_time_stab.RDS"))

other_pars <- readRDS(here("data", pack, "other_pars.RDS"))

expl_all_long <- expl_all %>%
  pivot_longer(names_to = "variable", values_to = "rsq", cols = 3:14) %>%
  separate(variable, into = c("junk1", "commstab_type", "trait_type", "summary_type", "summary_subtype")) %>%
  select(-junk1)





## report
other_pars
other_pars_text <- paste(names(other_pars),other_pars)

expl_all_long$summary_type[expl_all_long$summary_type == 'mean'] <- 'Mean'
expl_all_long$summary_type[expl_all_long$summary_type == 'RD'] <- 'Response'

expl_all_long$summary_subtype[is.na(expl_all_long$summary_subtype) ] <- 'response'
expl_all_long$summary_subtype[expl_all_long$summary_subtype== 'diss' ] <- 'dissimilarity'
expl_all_long$summary_subtype[expl_all_long$summary_subtype== 'div' ] <- 'divergence'

expl_all_long$commstab_type[expl_all_long$commstab_type == 'OEV']<- 'abs(OEV)'
expl_all_long$commstab_type[expl_all_long$commstab_type == 'rawcommstab']<- 'OEV'

p1 <- expl_all_long %>%
  filter(trait_type == "fundamental" & commstab_type!='abs(OEV)') %>%
  mutate(summary_type = paste(summary_type, summary_subtype)) %>%
  ggplot(aes(x = alpha_ij_sd, y = rsq, col = summary_type)) +
  facet_wrap(vars(commstab_type)) +
  geom_point() +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs")) +
  labs(x="Strength of interspecific interactions", y = "Explained variation in community stability",colour = "Type of measure", title = 'Fundamental response traits')+
  scale_y_continuous(breaks = seq(0, 1, 0.2))+
  theme_bw()


p2 <- expl_all_long %>%
  filter(trait_type == "realised"& commstab_type!='abs(OEV)') %>%
  mutate(summary_type = paste(summary_type, summary_subtype)) %>%
  ggplot(aes(x = alpha_ij_sd, y = rsq, col = summary_type)) +
  facet_wrap(vars(commstab_type)) +
  geom_point() +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs")) +
  labs(x="Strength of interspecific interactions",y= "Explained variation in community stability", colour = "Type of measure", title='Realised responses') +
  #ggtitle(paste("Realised niche-based community measures\n", other_pars_text)) +
  scale_y_continuous(breaks = seq(0, 1, 0.2))+
  theme_bw()

ggarrange(p1,p2, ncol = 2, labels = c('(a)', '(b)'), common.legend = T, legend = "bottom")
ggsave(plot = last_plot(), file = here('output', paste0(pack, "-quick-results-Fig4.pdf")), width = 9, height = 5)


### individual dynamics ###
dynamics1 <- dynamics %>%
  #filter(Treatment == "Control") %>%
  collect()
dynamics1$Treatment[dynamics1$Treatment == 'Perturbed']<-'Disturbed'

### subset for plot ###
dynamics2 <- dynamics1 %>%
  filter(case_id == "Comm-100-rep-1" )

dynamics2$fac_Species_ID <- dynamics2$Species_ID
dynamics2$fac_Species_ID<- factor(dynamics2$fac_Species_ID, levels = c("Spp1", "Spp2","Spp3", "Spp4", "Spp5", "Spp6", "Spp7","Spp8","Spp9", "Spp10" ))

dynamics2%>%
  ggplot(aes(x = Time, y = Abundance, linetype = Treatment)) +
  geom_line() +
  facet_wrap(~ fac_Species_ID)+
  theme_bw()
ggsave(plot = last_plot(), file = here('output/Appendix_FigS_SpeciesDynamics.pdf'), width = 8, height = 6)

  