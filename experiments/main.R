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
pack <- "pack4"
b_opt_mean_treatment <- seq(15, 22, 0.5) # 18.5 halfway
b_opt_range_treatment <- seq(3, 7, 0.5) ## used in packs 1-6
alpha_ij_sd_treatment <- round(seq(0, 0.5, 0.025), 3)
alpha_ij_mean_treatment <- c(0)
num_replicates <- 5

## pack2 for testing
pack <- "pack2"
b_opt_mean_treatment <- seq(15, 22, 1) # 18.5 halfway
b_opt_range_treatment <- seq(3, 7, 2) ## used in packs 1-6
alpha_ij_sd_treatment <- round(seq(0, 0.5, 0.1), 3)
alpha_ij_mean_treatment <- c(0)
num_replicates <- 2

## pack3, only negative alpha
pack <- "pack3"
b_opt_mean_treatment <- seq(15, 22, 0.5) # 18.5 halfway
b_opt_range_treatment <- seq(3, 7, 0.5) ## used in packs 1-6
alpha_ij_sd_treatment <- round(seq(0, 0.45, 0.025), 3)
alpha_ij_mean_treatment <- c(0.5)
num_replicates <- 5

## data folder
dir.create(here("data", pack))

pack <- 'pack3'
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

## plotting ##
source(here('R/5-from-charly/plots_simulations.R'))
###############################################################################
## Here is how to add new diversity measures. Its possibly still not so clear, so let me know if something seems amiss or unclear. ----
## 1. Add to the calculation in the file `R/3-analyse/get_stab_and_respdiv.R`.
## 2. Rerun from there above.
## 3. Add to the report file `reports/main-report.R`


# use "reports/main-report.qmd" to make a report containing various results.
