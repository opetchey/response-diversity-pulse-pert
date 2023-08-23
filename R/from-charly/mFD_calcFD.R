####

library(mFD)
library(tidyverse)

### import data ###

packs_to_read_in <- c("pack1", "pack2", "pack4", "pack6")
for(i in 1:length(packs_to_read_in)) {
  
  ## read data
  speciesData <- readRDS(here(paste0("data/", packs_to_read_in[i], "/species_measures.RDS")))
  communityData <- readRDS(here(paste0("data/", packs_to_read_in[i], "/community_measures.RDS")))
}

str(speciesData)

#### mFD ####
# Trait data
TraitData <- speciesData %>%
  group_by(case_id) %>%
  mutate(presence = abs((igr_pert_effect))) %>%
  filter(presence >0)

sp_tr <- TraitData %>%
  ungroup()%>%
    mutate(population = paste(species_id, case_id, sep = '_')) %>%
  select(population, species_AUC.pi, species_AUC.RR)%>%
  column_to_rownames(var = 'population') 
sp_tr[is.na(sp_tr)] <-0

# community matrix
asb_sp_w <- TraitData %>%
  mutate(population = paste(species_id, case_id, sep = '_')) %>%
  select(population, case_id, presence) %>%
  spread(key = population, value = presence) %>%
  column_to_rownames(var = 'case_id') 
asb_sp_w[is.na(asb_sp_w)] <-0
asb_sp_w[(asb_sp_w>0)] <-1
str(asb_sp_w)
asb_sp_w<-as.matrix(asb_sp_w)

# 2. compute functional space
fspace <-mFD::tr.cont.fspace(
  sp_tr        = sp_tr, 
  pca          = FALSE, 
  nb_dim       = 2, 
  scaling      = "scale_center",
  compute_corr = "none")

fspace
summary(fspace)
fspace$"sp_faxes_coord"


# Plot functional space
big_plot <- mFD::funct.space.plot(
  sp_faxes_coord  = fspace$"sp_faxes_coord",
  faxes           = c('species_AUC.RR','species_AUC.pi'),
  name_file       = NULL,
  faxes_nm        = NULL,
  range_faxes     = c(NA, NA),
  color_bg        = "grey95",
  color_pool      = "darkgreen",
  fill_pool       = "white",
  shape_pool      = 21,
  size_pool       = 1,
  plot_ch         = TRUE,
  color_ch        = "black",
  fill_ch         = "white",
  alpha_ch        = 0.5,
  plot_vertices   = TRUE,
  color_vert      = "blueviolet",
  fill_vert       = "blueviolet",
  shape_vert      = 23,
  size_vert       = 1,
  plot_sp_nm      = NULL,
  nm_size         = 3,
  nm_color        = "black",
  nm_fontface     = "plain",
  check_input     = TRUE)

big_plot$patchwork

alpha_fd_indices <- alpha.fd.multidim( #add  mFD to standardise by global 
  sp_faxes_coord  = fspace$"sp_faxes_coord",
  asb_sp_w         = asb_sp_w,
  ind_vect         = c("fdis", "fmpd", "fnnd", "feve", "fric",  
                       "fspe", "fide"),
  scaling          = TRUE,
  check_input      = TRUE,
  details_returned = TRUE)


# export functional diversity indices
fd_ind_values <- alpha_fd_indices$"functional_diversity_indices"
fd_ind_values

# look at details
details_list <- alpha_fd_indices$"details"


## plot exemplary two caseID hypervolumina within the maximum volumina 
plots_alpha <- mFD::alpha.multidim.plot(
  output_alpha_fd_multidim = alpha_fd_indices,
  plot_asb_nm              = c("Comm-98-rep-1",'Comm-191-rep-1'),
  ind_nm                   = c("fdis", "fide", "fnnd", "feve", "fric", 
                               "fspe"),
  faxes                    = NULL,
  faxes_nm                 = NULL,
  range_faxes              = c(NA, NA),
  color_bg                 = "grey95",
  shape_sp                 = c(pool = 3, asb1 = 21, asb2 = 21),
  size_sp                  = c(pool = 0.7, asb1 = 1, asb2 = 1),
  color_sp                 = c(pool = "grey50", asb1 = "#1F968BFF", asb2 = "#DCE319FF"),
  color_vert               = c(pool = "grey50", asb1 = "#1F968BFF", asb2 = "#DCE319FF"),
  fill_sp                  = c(pool = NA, asb1 = "#1F968BFF", asb2 = "#DCE319FF"),
  fill_vert                = c(pool = NA, asb1 = "#1F968BFF", asb2 = "#DCE319FF"),
  color_ch                 = c(pool = NA, asb1 = "#1F968BFF", asb2 = "#DCE319FF"),
  fill_ch                  = c(pool = "white", asb1 = "#1F968BFF", asb2 = "#DCE319FF"),
  alpha_ch                 = c(pool = 1, asb1 = 0.3, asb2 = 0.3),
  shape_centroid_fdis      = c(asb1 = 22,  asb2 = 24),
  shape_centroid_fdiv      = c(asb1 = 22,  asb2 = 24),
  shape_centroid_fspe      = 23,
  color_centroid_fspe      = "black",
  size_sp_nm               = 3, 
  color_sp_nm              = "black",
  plot_sp_nm               = NULL,
  fontface_sp_nm           = "plain",
  save_file                = FALSE,
  check_input              = TRUE) 

plots_alpha$"fric"$"patchwork"

FD_ind_values <- fd_ind_values %>%
  tibble::rownames_to_column() %>%
  rename(case_id = rowname) 

#### Merge Fdiv and Functional community stability ####

#new df
Fdiv <- FD_ind_values%>%
  distinct(case_id,fdis,feve,fric)%>%
  left_join(., communityData, by = c('case_id')) %>%
  select(case_id, fdis, feve, fric, community_id, alpha_ij_sd, 
         OEV, OEV_sqrt, var_spp_deltabm,var_spp_deltabm_pi, comm_tot_deltabm,
         var_igr_effect, mean_igr_effect, RD_diss, RD_div)
which(is.na(Fdiv$OEV))

#save RD
saveRDS(Fdiv, here('data/merged/Fdiv_responseDiversity.RDS'))

#### plots ####

## overall ##
p1<-ggplot(Fdiv, aes(x = RD_diss, y = comm_tot_deltabm, alpha = alpha_ij_sd))+
  geom_point()+
  theme_bw()

p2<-ggplot(Fdiv, aes(x = RD_div, y = comm_tot_deltabm, alpha = alpha_ij_sd))+
  geom_point()+
  theme_bw()

p6<-ggplot(Fdiv, aes(x = mean_igr_effect, y = comm_tot_deltabm, alpha = alpha_ij_sd))+
  geom_point()+
  theme_bw()

p3<-ggplot(Fdiv, aes(x = fdis, y = comm_tot_deltabm, alpha = alpha_ij_sd))+
  geom_point()+
  theme_bw()

p4<-ggplot(Fdiv, aes(x = feve, y = comm_tot_deltabm, alpha = alpha_ij_sd))+
  geom_point()+
  theme_bw()

p5<-ggplot(Fdiv, aes(x = fric, y = comm_tot_deltabm, alpha = alpha_ij_sd))+
  geom_point()+
  theme_bw()

p1+p2+ggtitle('Ross et al. RD')+p6
ggsave(plot = last_plot(), file = 'RD_Ross_etal_totRR.png', width = 10, height = 4)

p3+p4+ggtitle('mFD RD')+p5
ggsave(plot = last_plot(), file = 'RD_mFD_totRR.png', width = 10, height = 4)

### OEV ###
p1<-ggplot(Fdiv, aes(x = RD_diss, y = OEV, alpha = alpha_ij_sd))+
  geom_point()+
  theme_bw()

p2<-ggplot(Fdiv, aes(x = RD_div, y = OEV, alpha = alpha_ij_sd))+
  geom_point()+
  theme_bw()

p6<-ggplot(Fdiv, aes(x = mean_igr_effect, y = OEV, alpha = alpha_ij_sd))+
  geom_point()+
  theme_bw()

p3<-ggplot(Fdiv, aes(x = fdis, y = OEV, alpha = alpha_ij_sd))+
  geom_point()+
  theme_bw()

p4<-ggplot(Fdiv, aes(x = feve, y = OEV, alpha = alpha_ij_sd))+
  geom_point()+
  theme_bw()

p5<-ggplot(Fdiv, aes(x = fric, y = OEV, alpha = alpha_ij_sd))+
  geom_point()+
  theme_bw()

p1+p2+ggtitle('Ross et al. RD')+p6
ggsave(plot = last_plot(), file = 'RD_Ross_etal_OEV.png', width = 10, height = 4)

p3+p4+ggtitle('mFD RD')+p5
ggsave(plot = last_plot(), file = 'RD_mFD_OEV.png', width = 10, height = 4)


## facets ##
p1+facet_wrap(~alpha_ij_sd)
p2+facet_wrap(~alpha_ij_sd)
p6+facet_wrap(~alpha_ij_sd)

p3+facet_wrap(~alpha_ij_sd)
p4+facet_wrap(~alpha_ij_sd)
p5+facet_wrap(~alpha_ij_sd)

