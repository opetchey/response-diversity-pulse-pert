library(here)

## there is currently no random number seed set.


################################################################################
## Pack-by-pack section ----

## simulations are run in "packs", with each pack creating
## a set of data files

## simulations can take a few hours.

## pack1, lower values of interaction strength
#pack <- "pack1"
#alpha_ij_sd_treatment <- c(0, 0.01, 0.02, 0.05, 0.1)

## pack2, higher interaction strengths
#pack <- "pack2"
#alpha_ij_sd_treatment <- c(0.2, 0.3, 0.4, 0.5)

## pack3-5 are the same, with finer resolution
#pack <- "pack3"
#pack <- "pack4"
#pack <- "pack5"
#alpha_ij_sd_treatment <- seq(0, 0.5, 0.05)

## pack6, even finer resolution
#pack <- "pack6"
#alpha_ij_sd_treatment <- seq(0, 0.5, 0.025)

## pack0 for testing
pack <- "pack0"
alpha_ij_sd_treatment <- seq(0, 0.1, 0.1)

## data folder
dir.create(here("data", pack))

# Design experiment
source(here("R/1-design/design_expt.R"))

# Calculate perturbation effects on intrinsic growth rate
source(here("R/3-analyse/get_igr_pert_effects.R"))

# Run dynamical simulation of experiment
source(here("R/2-run/run_experiment.r"))

# Calculate species and community level measures (e.g. stability and diversity)
source(here("R/3-analyse/get_pack_stab_and_respdiv.R"))

## end of Pack-by-pack section
################################################################################



################################################################################
# Merge pack data
source(here("R/3-analyse/merge_packs_data.R"))


################################################################################
# Calculations on merged data
source(here("R/3-analyse/calcs_on_merged_data.R"))



###############################################################################
## Recalculations across packs ----
## This can be useful when wanting to do new calculations on all sim results
# Recalculate species and community level measures (e.g. stability and diversity) of each pack
# library(here)
# next line recalculates stability and diversity of each packs
source(here("R/3-analyse/recalc_all_packs_stab_and_respdiv.R"))
# remerge the relevant files
source(here("R/3-analyse/merge_packs_data.R"))
# redo calculations on merged data
source(here("R/3-analyse/calcs_on_merged_data.R"))


###############################################################################
## Here is how to add new diversity measures. Its possibly still not so clear, so let me know if something seems amiss or unclear. ----
## 1. Add to the calculation in the file `R/3-analyse/get_pack_stab_and_respdiv.R`.
## 2. Do the "Recalculations across packs" section above.
## 3. Add to the report file `reports/main-report.R`


# use "reports/main-report.qmd" to make a report containing various results.
