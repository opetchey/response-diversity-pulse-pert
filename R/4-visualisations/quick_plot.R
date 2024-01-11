## quick look

library(here)
library(tidyverse)

dynamics <- readRDS(here("data/pack8/sim_results.RDS"))
                         
dynamics_oi <- dynamics$dynamics_long |> 
  dplyr::filter(alpha_ij_sd == 0,
         b_opt_mean == 18)


p_dynamics <- dynamics_oi |>
  select(case_id, Species_ID, Time, Treatment, Abundance) |> 
  ggplot(aes(x = Time, y = Abundance, linetype = Treatment)) +
  #geom_point() +
  geom_line() +
  facet_wrap(~ Species_ID, nrow = 3, scales = "free_y") 

