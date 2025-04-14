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


## set up data base to save results into
library(DBI)
library(RSQLite)
#file.remove(here("data", pack, "/dynamics.db"))
#file.remove(here("data", pack, "/temperatures.db"))
conn_dynamics <- dbConnect(RSQLite::SQLite(), here("data", pack, "/dynamics.db"))
#conn_temperatures <- dbConnect(RSQLite::SQLite(), here("data", pack, "/temperatures.db"))
#dbWriteTable(conn_dynamics, "dynamics", dynamics)
#dbListTables(conn)
#dbDisconnect(conn)




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

i <- 104

for(i in 1:nrun){
  print(i)
  S <- expt$community_object[[i]]$S
  
  initial_abundances <- (rdirichlet(1, rep(1, S))*1000)[1,]
  
  input <- expt$community_object[[i]]
  
  ## for testing
  # input_com_params = input 
  #TcelSeries = Tcel_controlm
  
  spts <- simulator_lv(input_com_params = input,
                       TcelSeries = Tcel_controlm,
                       initial_abundances = initial_abundances)
  
  keep <- (ignore_first+1):(before_pulse + after_pulse)
  spts <- spts[keep,]
  
  
  spts_control <- spts |> 
    as_tibble() |> 
    mutate(case_id = expt$case_id[i],
           replicate_id = expt$rep_names[i],
           Time = (ignore_first+1):(before_pulse + after_pulse)) |> 
    pivot_longer(names_to = "Species_ID", values_to = "Abundance",
                 cols = starts_with("Spp")) |> 
    mutate(Treatment = "Control")
  
  
  ##simulations  perturbed
  spts<-simulator_lv(input_com_params = input,
                     TcelSeries=Tcel_pertm,
                     initial_abundances = initial_abundances)
  spts <- spts[keep,]
  
  
  spts_perturbed <- spts |> 
    as_tibble() |> 
    mutate(case_id = expt$case_id[i],
           replicate_id = expt$rep_names[i],
           Time = (ignore_first+1):(before_pulse + after_pulse)) |> 
    pivot_longer(names_to = "Species_ID", values_to = "Abundance",
                 cols = starts_with("Spp")) |> 
    mutate(Treatment = "Perturbed")
  
  results <- bind_rows(spts_control,
                       spts_perturbed) |> 
    mutate(b_opt_mean = expt$b_opt_mean[i],
           b_opt_range = expt$b_opt_range[i],
           alpha_ij_sd = expt$alpha_ij_sd[i],
           alpha_ij_mean = expt$alpha_ij_mean[i],
           rep_names = expt$rep_names[i])
  

  
  if(i == 1) {
    dbWriteTable(conn_dynamics, "dynamics", results, overwrite = TRUE)
    #dbWriteTable(conn_temperatures, "temperatures", temperature_series_expt_only, overwrite = TRUE)
  }
  if(i > 1) {
    dbWriteTable(conn_dynamics, "dynamics", results, append = TRUE)
    #dbWriteTable(conn_temperatures, "temperatures", temperature_series_expt_only, append = TRUE)
  }
  
  
}



