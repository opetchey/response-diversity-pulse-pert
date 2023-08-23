#rm(list = ls())

## Some preliminaries ----
options(scipen = 1, digits = 3) #set to two decimal 
library(here)
library(tidyverse)
library(gridExtra)
#source(here("R", "mtime.R"))
#seed <- 101

## source some functions ----
source(here("R/0-functions/simulator_lv.R"))
source(here("R/0-functions/make a community.R"))


#set.seed(seed)



## read in experimental design
## Created by code in "design_expt.r" script in the experiments/1-design folder
expt <- readRDS(here("data", pack, "expt_communities.RDS"))
temperature_treatments <- readRDS(here("data", pack, "temperature_treatments.RDS"))


## run experiment ----
Tcel_control<-temperature_treatments$temperature_control
Time<-temperature_treatments$time
nrun<-nrow(expt)
control_res<-data.frame(community_id = expt$community_id,
                        case_id = expt$case_id,
                B_opts = NA*numeric(nrun),
                Other_species_pars = NA*numeric(nrun),
                Replicate_ID = expt$rep_names,
                Treatment = "Control",
                Time=NA,
                Temperature =NA,
                Species_ID=NA,
                Abundance = NA)
control_res$Time<-I(list(Time))
control_res$Temperature<-I(list(Tcel_control))
Tcel_controlm<-matrix(Tcel_control,nrow=1)


Tcel_pert<-temperature_treatments$temperature_pulse
Time<-temperature_treatments$time
nrun<-nrow(expt)
perturbed_res<-data.frame(community_id=expt$community_id,
                          case_id = expt$case_id,
                          B_opts = NA*numeric(nrun),
                          Other_species_pars = NA*numeric(nrun),
                          Replicate_ID = expt$rep_names,Treatment = "Perturbed",
                          Time=NA,
                          Temperature =NA,
                          Species_ID=NA,
                          Abundance = NA)

perturbed_res$Time<-I(list(Time))
perturbed_res$Temperature<-I(list(Tcel_pert))

Tcel_pertm<-matrix(Tcel_pert,nrow=1)

#plot(t(Tcel_pertm))

for(i in 1:nrun){

  S <- expt$community_object[[i]]$S
  
  initial_abundances <- (rdirichlet(1, rep(1, S))*1000)[1,]
  
  input <- expt$community_object[[i]]
  
  ## for testing
  input_com_params = input 
  TcelSeries = Tcel_controlm
  
  spts <- simulator_lv(input_com_params = input,
                       TcelSeries = Tcel_controlm,
                       initial_abundances = initial_abundances)
  
  #plot(t(Tcel_controlm))
  #unique(t(Tcel_controlm))
  #matplot(spts[6400:7000,1])
  
  oth<-input
  oth[["b_opt_i"]]<-NULL
  oth$bopt_mean<-expt$b_opt_mean[i]
  oth$bopt_range<-expt$b_opt_range[i]
  control_res$Other_species_pars[i]<-I(list(oth))
  
  control_res$B_opts[i]<-I(list(input$b_opt_i))
  control_res$Abundance[i]<-I(list(spts))
  
  
  input<-expt$community_object[[i]]
  spts<-simulator_lv(input_com_params = input,
                     TcelSeries=Tcel_pertm,
                     initial_abundances = initial_abundances)
  
  perturbed_res$Other_species_pars[i]<-I(list(oth))
  perturbed_res$B_opts[i]<-I(list(input$b_opt_i))
  perturbed_res$Abundance[i]<-I(list(spts))
  
}
control_res$Species_ID<-I(list(colnames(spts)))
perturbed_res$Species_ID<-I(list(colnames(spts)))
allres<-rbind(control_res,
              perturbed_res)
allres<-as_tibble(allres)

allres <- expt %>%
  select(community_id, b_opt_mean, b_opt_range,
         alpha_ij_mean, alpha_ij_sd) %>%
  full_join(allres, multiple = "all")




community_pars <- allres %>%
  select(community_id, case_id, b_opt_mean, b_opt_range,
         B_opts,
         alpha_ij_mean, alpha_ij_sd,
         Other_species_pars, Replicate_ID) %>%
  unique()
dynamics <- allres %>%
  select(community_id, Replicate_ID, case_id, b_opt_mean, b_opt_range,
         alpha_ij_mean, alpha_ij_sd,
         Treatment, Time,
         Temperature, Species_ID, Abundance)


for(i in 1:nrow(dynamics)) {
  #i <- 1
  temp1 <- tibble(community_id = rep(dynamics$community_id[i], length(dynamics$Time[i][[1]])),
                  replicate_id = rep(dynamics$Replicate_ID[i], length(dynamics$Time[i][[1]])),
                  case_id = rep(dynamics$case_id[i], length(dynamics$Time[i][[1]])),
                  b_opt_mean = rep(dynamics$b_opt_mean[i], length(dynamics$Time[i][[1]])),
                  b_opt_range = rep(dynamics$b_opt_range[i], length(dynamics$Time[i][[1]])),
                  alpha_ij_mean = rep(dynamics$alpha_ij_mean[i], length(dynamics$Time[i][[1]])),
                  alpha_ij_sd = rep(dynamics$alpha_ij_sd[i], length(dynamics$Time[i][[1]])),
                  Treatment = rep(dynamics$Treatment[i], length(dynamics$Time[i][[1]])),
                  Time = dynamics$Time[i][[1]],
                  Temperature = dynamics$Temperature[i][[1]])
  temp2 <- dynamics$Abundance[i][[1]]
  temp <- cbind(temp1, temp2) %>%
    pivot_longer(names_to = "Species_ID", values_to = "Abundance",
                 cols = (ncol(temp1) + 1):(ncol(temp1) + ncol(temp2)))
  
  if(i == 1)
    dynamics_long <- temp
  if(i > 1)
    dynamics_long <- rbind(dynamics_long, temp)
  
}
dynamics_long <- dynamics_long %>%
  filter(Time > ignore_first)


## save the results
all_results <- list(community_pars = community_pars,
                    dynamics_long = dynamics_long)
saveRDS(all_results, here("data", pack, "sim_results.RDS"))

