
rm(list = ls())
library(tidyverse)
library(readxl)
library(here)
library(kableExtra)
library(patchwork)


temp <- readRDS(here("data/sim_results.RDS"))
community_pars <- temp$community_pars
dynamics <- temp$dynamics_long


coi <- "Comm-1"
temp <- community_pars %>%
  filter(community_id == coi)
b_opts <- temp$B_opts[1][[1]]
b_opts <- tibble(Time = rep(9901, length(b_opts)),
                 b_opts = b_opts)
p1 <- dynamics %>%
  filter(community_id == coi) %>%
  ggplot(aes(x = Time, y = Abundance, col = Species_ID)) +
  geom_line() +
  facet_grid(cols = vars(Treatment))
p2 <- dynamics %>%
  filter(community_id == coi) %>%
  ggplot(aes(x = Time, y = Temperature)) +
  geom_line() +
  facet_grid(cols = vars(Treatment)) +
  geom_point(data = b_opts,
             aes(x = Time, y = b_opts))
p1 / p2

