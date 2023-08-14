library(here)

## there is currently no random number seed set.

## simulations were run in "packs", with each pack creating
## a set of data files

## the code immediately below is for working on a pack-by-pack basis.
## if you would like to redo any calculations across all packs, see lower down...

## Pack-by-pack section ----

## simulations can take a few hours.
# pack1
#alpha_ij_sd_treatment <- c(0, 0.01, 0.02, 0.05, 0.1)
# pack2
#alpha_ij_sd_treatment <- c(0.2, 0.3, 0.4, 0.5)
# pack 3 and pack4 and pack5
#alpha_ij_sd_treatment <- seq(0, 0.5, 0.05)
# pack6
#alpha_ij_sd_treatment <- seq(0, 0.5, 0.025)

# Design experiment
source(here("experiments/1-design/design_expt.R"))

# Calculate perturbation effects on intrinsic growth rate
source(here("experiments/3-analyse/get_igr_pert_effects.R"))

# Run dynamical simulation of experiment
source(here("experiments/2-run/run experiment.r"))

# Calculate species and community level measures (e.g. stability and diversity)
source(here("experiments/3-analyse/get_stab_and_respdiv.R"))

# Final data processing and analyses
source(here("experiments/3-analyse/final_data_processing.R"))

# use "reports/main-report.qmd" to make a report containing various results.


## Across-pack section ----
## This can be useful when wanting to do new calculations on all sim results
# Recalculate species and community level measures (e.g. stability and diversity) of each pack
# library(here)
source(here("experiments/3-analyse/recalc all get_stab_and_respdiv.R"))
# remerge the relevant files
source(here("experiments/3-analyse/redo final_data_processing.R"))


## Here is how to add new diversity measures. Its possibly still not so clear, so let me know if something seems amiss or unclear. ----
## 1. Add to the calculation in the file `experiments/3-analyse/recalc all get_stab_and_respdiv.R`.
## 2. Run all script in that file.
## 3. Run all scritp in file `experiments/3-analyse/redo final_data_processing.R`
## 4. Add to the report file `reports/main-report.R`