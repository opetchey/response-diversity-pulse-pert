## This code reads in the dynamics and species_igr traits from each pack and
## then calculates:
## measures of community stability (AUC), species stability (AUC),
## measure of community level trait summaries (e.g. trait mean, trait diversity)



rm(list=ls())

library(tidyverse)
library(readxl)
library(MESS)
library(here)
#library(cowplot)
#library(GGally)
library(patchwork)

## source any required user defined functions
source(here("R/0-functions/my_auc.R"))
source(here("R/0-functions/Ross_et_al_functions.R"))


packs_to_read_in <- c("pack1", "pack2", "pack3", "pack4", "pack5", "pack6")
for(i in 1:length(packs_to_read_in)) {
  
  pack <- packs_to_read_in[i]
  
  source(here("R/3-analyse/get_pack_stab_and_respdiv.R"))
  
}

