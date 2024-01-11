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

## pack1, testing
pack <- "pack1"
b_opt_mean_treatment <- seq(15, 22, 0.5) # 18.5 halfway
b_opt_range_treatment <- seq(3, 7, 0.5) ## used in packs 1-6
alpha_ij_sd_treatment <- round(seq(0, 0.5, 0.025), 3)
num_replicates <- 5


## pack2, big
#alpha_ij_sd_treatment <- c(0, 0.01, 0.02, 0.05, 0.1)
#alpha_ij_sd_treatment <- c(0.2, 0.3, 0.4, 0.5)
#alpha_ij_sd_treatment <- seq(0, 0.5, 0.05)
#alpha_ij_sd_treatment <- seq(0, 0.5, 0.025)
#b_opt_range_treatment <- c(5) ## used in packs 1-6
#b_opt_mean_treatment <- seq(17.5, 19.5, 0.25)
#b_opt_range_treatment <- c(3,4,5,6,7) ## used in packs 1-6
#num_replicates <- 2


## data folder
dir.create(here("data", pack))

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


###############################################################################
## Here is how to add new diversity measures. Its possibly still not so clear, so let me know if something seems amiss or unclear. ----
## 1. Add to the calculation in the file `R/3-analyse/get_stab_and_respdiv.R`.
## 2. Rerun from there above.
## 3. Add to the report file `reports/main-report.R`


# use "reports/main-report.qmd" to make a report containing various results.
