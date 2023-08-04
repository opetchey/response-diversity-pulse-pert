library(here)

## there is currently no random number seed set

# Design experiment
# pack1
#alpha_ij_sd_treatment <- c(0, 0.01, 0.02, 0.05, 0.1)
# pack2
#alpha_ij_sd_treatment <- c(0.2, 0.3, 0.4, 0.5)
# pack 3 and pack4 and pack5
#alpha_ij_sd_treatment <- seq(0, 0.5, 0.05)
# pack6
alpha_ij_sd_treatment <- seq(0, 0.5, 0.025)
source(here("experiments/1-design/design_expt.R"))

# Calculate perturbation effects on intrinsic growth rate
source(here("experiments/3-analyse/get_igr_pert_effects.R"))

# Run dynamical simulation of experiment
source(here("experiments/2-run/run experiment.r"))

# Calculate species and community level measures (e.g. stability and diversity)
source(here("experiments/3-analyse/get_stab_and_respdiv.R"))

# Final data processing and analyses
source(here("experiments/3-analyse/final_data_processing.R"))



# use "reports/quick look.r" to have a quick look at some results
# use "reports/main.qmd" for more comprehensive report.

