rm(list = ls())
library(here)

## source any required user defined functions
source(here("R/0-functions/my_auc.R"))
source(here("R/0-functions/Ross_et_al_functions.R"))


## there is currently no random number seed set.



################################################################################
## Pack-by-pack section ----

## simulations are run in "packs", with each pack creating
## a set of data files

## simulations can take a few hours.

## Common across packs
temperature_control <- 22
temperature_pulse <- 15
duration_pulse <- 50
before_pulse <- 500
after_pulse <- 200
ignore_first <- 490

## pack1, the real thing - this will take some time to run and analyse
pack <- "pack1"
b_opt_mean_treatment <- seq(15, 22, 0.5) # 18.5 halfway
b_opt_range_treatment <- seq(3, 7, 0.5) ## used in packs 1-6
alpha_ij_mean_treatment <- c(0)
alpha_ij_sd_treatment <- round(seq(0, 0.5, 0.025), 3)
spp_RR_calc_threshold <- 1
num_replicates <- 5
other_pars <- list()
other_pars$spp_RR_calc_threshold <- 1*10^-3
other_pars$interactions <- "only_comp"


## testing ##
## I used the following thresholds: 1, 0.5, 0.1, 0.01, 10^-6, 10^-10, 10^-20, 0
## it becomes quite clear that the results for the Realised response diversity are highly dependent on this value
## I either would chose a value near 1 as then a fair amount of species gets cut off or 
## a value near 0 to include also communities where species have a very low abundance (there are some with 10^-8)
## for a better comparison with the fundamental niche I would propably choose the latter (10^-20)

pack <- "pack6"
b_opt_mean_treatment <- seq(15, 22, 0.5) # 18.5 halfway
b_opt_range_treatment <- seq(3, 7, 0.5) ## used in packs 1-6
alpha_ij_mean_treatment <- c(0)
alpha_ij_sd_treatment <- round(seq(0, 0.5, 0.025), 3)
spp_RR_calc_threshold <- 1
num_replicates <- 5
other_pars <- list()
other_pars$spp_RR_calc_threshold <- 1*10^-10
other_pars$interactions <- "only_comp"


## data folder
dir.create(here("data", pack))

saveRDS(other_pars, here("data", pack, "other_pars.RDS"))

# Design experiment
source(here("R/1-design/design_expt.R"))

# Calculate perturbation effects on intrinsic growth rate
source(here("R/3-analyse/get_igr_pert_effects.R"))

# Run dynamical simulation of experiment
source(here("R/2-run/run_experiment.r"))

# Calculate species and community level measures (e.g. stability and diversity)
source(here("R/3-analyse/get_stab_and_respdiv.R"))

## Getting the explanatory powers
source(here("R/3-analyse/calcs_on_data.R"))


## plotting and making html report
source(here("R", "4-visualisations", "quick-graph-main-result.r"))
#quarto::quarto_render(input = here("reports", "main-report.qmd"),
#                      output_file = paste0("main-report-", pack, ".html"))

#source(here('R/5-from-charly/plots_simulations.R'))
###############################################################################
## Here is how to add new diversity measures. Its possibly still not so clear, so let me know if something seems amiss or unclear. ----
## 1. Add to the calculation in the file `R/3-analyse/get_stab_and_respdiv.R`.
## 2. Rerun from there above.
## 3. Add to the report file `reports/main-report.R`


# use "reports/main-report.qmd" to make a report containing various results.
