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

expl_all_long$summary_subtype[is.na(expl_all_long$summary_subtype) ] <- 'response'
expl_all_long$summary_subtype[expl_all_long$summary_subtype== 'diss' ] <- 'dissimilarity'
expl_all_long$summary_subtype[expl_all_long$summary_subtype== 'div' ] <- 'divergence'

expl_all_long$commstab_type[expl_all_long$commstab_type == 'rawcommstab']<- 'Relative OEV'

p1 <- expl_all_long %>%
  filter(trait_type == "fundamental") %>%
  mutate(summary_type = paste(summary_type, summary_subtype)) %>%
  ggplot(aes(x = alpha_ij_sd, y = rsq, col = summary_type)) +
  facet_wrap(vars(commstab_type)) +
  geom_point() +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs")) +
  xlab("Strength of interspecific interactions") +
  ylab("Explained variation in community stability") +
  #scale_color_manual(labels = c("Fundamental trait", "Realised trait"), values = c("blue", "red", "black")) +
  labs(colour = "Type of measure", title = 'Fundamental species traits') +
 # ggtitle(paste("Fundamental niche-based community measures\n", other_pars_text)) +
  scale_y_continuous(breaks = seq(0, 1, 0.2))+
  theme_bw()


p2 <- expl_all_long %>%
  filter(trait_type == "realised") %>%
  mutate(summary_type = paste(summary_type, summary_subtype)) %>%
  ggplot(aes(x = alpha_ij_sd, y = rsq, col = summary_type)) +
  facet_wrap(vars(commstab_type)) +
  geom_point() +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs")) +
  xlab("Strength of interspecific interactions") +
  ylab("Explained variation in community stability") +
  #scale_color_manual(labels = c("Fundamental trait", "Realised trait"), values = c("blue", "red", "black")) +
  labs(colour = "Type of measure", title='Realised responses') +
  #ggtitle(paste("Realised niche-based community measures\n", other_pars_text)) +
  scale_y_continuous(breaks = seq(0, 1, 0.2))+
  theme_bw()

cowplot::plot_grid(p1,p2, ncol = 1, labels = c('a)', 'b)'))
ggsave(plot = last_plot(), file = here('output', paste0(pack, "-quick-results.pdf")), width = 8, height = 8)
#ggsave(here("reports", paste0(pack, "-quick-results.pdf")), width = 8, height = 8,
 #      p1 / p2)

### indivdiual dynamics ###
dynamics1 <- dynamics %>%
  #filter(Treatment == "Control") %>%
  collect()

dynamics1 %>%
  filter(case_id == "Comm-2667-rep-1") %>%
  ggplot(aes(x = Time, y = Abundance, col = Species_ID)) +
  geom_line() +
  facet_wrap(~ Treatment)


  