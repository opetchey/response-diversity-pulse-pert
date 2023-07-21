# Design experiment
source(here("experiments/1-design/design_expt.r"))

# Calculate perturbation effects on intrinsic growth rate
source(here("experiments/3-analyse/get_igr_pert_effects.r"))

# Run dynamical simulation of experiment
source(here("experiments/2-run/run experiment.r"))

# Calculate species and community level measures (e.g. stability and diversity)
source(here("experiments/3-analyse/get_stab_and_respdiv.r"))


# use "reports/quick look.r" to have a quick look at some results
# use "reports/analyse.qmd" for more comprehensive report.