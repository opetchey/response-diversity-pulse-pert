#### import data ####

library(tidyverse)
library(here)
library(cowplot)


  ### read data ###
pack <- "pack3"
expt <- readRDS(here("data", pack, "expt_communities.RDS"))
speciesData <- readRDS(here("data", pack, "species_measures.RDS"))
communityData <- readRDS(here("data", pack, "community_measures.RDS"))


str(speciesData)
str(communityData)

### species contributions ###
sample(speciesData$community_id, 10)
speciesData %>%
  #filter(community_id %in% c("Comm-5630" , "Comm-7528",  "Comm-2058" , "Comm-4940" , "Comm-13927" ,"Comm-12409" ,"Comm-5936" ,  "Comm-13770" ,"Comm-10428", "Comm-4303" ))%>%
  ggplot(., aes ( x = species_RR_AUC, y = species_delta_pi_AUC, alpha = alpha_ij_sd))+
  geom_hline(yintercept=0)+
  geom_vline(xintercept = 0)+
  geom_point()+
  labs(x = 'Absolute Contribution', y = 'Relative Contribution')+
  facet_wrap(~alpha_ij_sd, ncol = 5)+ # scale_y_continuous(limits = c(-0.00002,0.00004), breaks = c(-0.00002,-0.00001,0,0.00001, 0.00002,0.00004))+
  theme_bw()+
  theme(axis.title.y=element_text(size=14, face="plain", colour="black",vjust=0.3),axis.text.y=element_text(size=10,face="bold",colour="black",angle=0,hjust=0.4))+
  theme(axis.title.x=element_text(size=14,face="plain",colour="black",vjust=0),axis.text.x=element_text(size=10,face="bold",colour="black"))
ggsave(plot = last_plot(), file = here('output/ModelContributions_subset.png'), width = 15, height = 15)

str(communityData)

### Fundamental response traits and communtiy instability ###

p1<-communityData %>%
  ggplot(., aes ( x = var_igr_effect, y = comm_RR_AUC, alpha = alpha_ij_sd))+
  geom_point()+
  facet_wrap(~alpha_ij_sd, ncol = 5)+
  theme_bw()+
  theme(axis.title.y=element_text(size=14, face="plain", colour="black",vjust=0.3),axis.text.y=element_text(size=10,face="plain",colour="black",angle=0,hjust=0.4))+
  theme(axis.title.x=element_text(size=14,face="plain",colour="black",vjust=0),axis.text.x=element_text(size=10,face="plain",colour="black"))+
  theme(legend.position = 'none')

p2<-communityData %>%
  ggplot(., aes ( x = mean_igr_effect, y = comm_RR_AUC, alpha = alpha_ij_sd))+
  geom_point()+
  facet_wrap(~alpha_ij_sd, ncol = 5)+
  theme_bw()+
  theme(axis.title.y=element_text(size=14, face="plain", colour="black",vjust=0.3),axis.text.y=element_text(size=10,face="plain",colour="black",angle=0,hjust=0.4))+
  theme(axis.title.x=element_text(size=14,face="plain",colour="black",vjust=0),axis.text.x=element_text(size=10,face="plain",colour="black"))+
  theme(legend.position = 'none')

p3<-communityData %>%
  ggplot(., aes ( x = RD_diss_igr_effect, y = comm_RR_AUC, alpha = alpha_ij_sd))+
  geom_point()+
  facet_wrap(~alpha_ij_sd, ncol = 5)+
  theme_bw()+
  theme(axis.title.y=element_text(size=14, face="plain", colour="black",vjust=0.3),axis.text.y=element_text(size=10,face="plain",colour="black",angle=0,hjust=0.4))+
  theme(axis.title.x=element_text(size=14,face="plain",colour="black",vjust=0),axis.text.x=element_text(size=10,face="plain",colour="black"))+
  theme(legend.position = 'none')

p4<-communityData %>%
  ggplot(., aes ( x = RD_div_igr_effect, y = comm_RR_AUC, alpha = alpha_ij_sd))+
  geom_point()+
  facet_wrap(~alpha_ij_sd, ncol = 5)+
  theme_bw()+
  theme(axis.title.y=element_text(size=14, face="plain", colour="black",vjust=0.3),axis.text.y=element_text(size=10,face="plain",colour="black",angle=0,hjust=0.4))+
  theme(axis.title.x=element_text(size=14,face="plain",colour="black",vjust=0),axis.text.x=element_text(size=10,face="plain",colour="black"))+
  theme(legend.position = 'bottom')

legend_a<-get_legend(p4)
cowplot:: plot_grid(p1,p2,p3,p4+theme(legend.position = 'none'), labels = c('a)', 'b)', 'c)', 'd)'), ncol = 2)
ggsave(plot = last_plot(), file = here('output/IGR_Traits_Instab.png'), width = 15, height = 15)


#### Realised response traits and community stability ####

p5<-communityData %>%
  ggplot(., aes ( x = mean_species_RR_AUC, y = comm_RR_AUC, alpha = alpha_ij_sd))+
  geom_point()+
  facet_wrap(~alpha_ij_sd, ncol = 5)+
  theme_bw()+
  theme(axis.title.y=element_text(size=14, face="plain", colour="black",vjust=0.3),axis.text.y=element_text(size=10,face="plain",colour="black",angle=0,hjust=0.4))+
  theme(axis.title.x=element_text(size=14,face="plain",colour="black",vjust=0),axis.text.x=element_text(size=10,face="plain",colour="black"))+
  theme(legend.position = 'none')

p6<-communityData %>%
  ggplot(., aes ( x = var_species_RR_AUC, y = comm_RR_AUC, alpha = alpha_ij_sd))+
  geom_point()+
  facet_wrap(~alpha_ij_sd, ncol = 5)+
  theme_bw()+
  theme(axis.title.y=element_text(size=14, face="plain", colour="black",vjust=0.3),axis.text.y=element_text(size=10,face="plain",colour="black",angle=0,hjust=0.4))+
  theme(axis.title.x=element_text(size=14,face="plain",colour="black",vjust=0),axis.text.x=element_text(size=10,face="plain",colour="black"))+
  theme(legend.position = 'none')

p7<-communityData %>%
  ggplot(., aes ( x = RD_diss_igr_effect, y = comm_RR_AUC, alpha = alpha_ij_sd))+
  geom_point()+
  facet_wrap(~alpha_ij_sd, ncol = 5)+
  theme_bw()+
  theme(axis.title.y=element_text(size=14, face="plain", colour="black",vjust=0.3),axis.text.y=element_text(size=10,face="plain",colour="black",angle=0,hjust=0.4))+
  theme(axis.title.x=element_text(size=14,face="plain",colour="black",vjust=0),axis.text.x=element_text(size=10,face="plain",colour="black"))+
  theme(legend.position = 'none')

p8<-communityData %>%
  ggplot(., aes ( x = RD_div_species_RR_AUC, y = comm_RR_AUC, alpha = alpha_ij_sd))+
  geom_point()+
  facet_wrap(~alpha_ij_sd, ncol = 5)+
  theme_bw()+
  theme(axis.title.y=element_text(size=14, face="plain", colour="black",vjust=0.3),axis.text.y=element_text(size=10,face="plain",colour="black",angle=0,hjust=0.4))+
  theme(axis.title.x=element_text(size=14,face="plain",colour="black",vjust=0),axis.text.x=element_text(size=10,face="plain",colour="black"))+
  theme(legend.position = 'none')

cowplot:: plot_grid(p5,p6,p7,p8, labels = c('a)', 'b)', 'c)', 'd)', 'e)', 'f)', 'g)', 'h)'), rel_heights = c(2,2), ncol = 2)
ggsave(plot = last_plot(), file = here('output/Realised_Traits_Instab.png'), width = 15, height = 15)


cowplot:: plot_grid(p1,p2,p3,p4+theme(legend.position = 'none'), p5,p6,p7,p8, labels = c('a)', 'b)', 'c)', 'd)', 'e)', 'f)', 'g)', 'h)'), rel_heights = c(2,2,0.3), ncol = 4)
ggsave(plot = last_plot(), file = here('output/IGR_Realised_Traits_Instab.png'), width = 20, height = 12)


#### Fundamental response traits correlations ####
p1<-communityData %>%
  ggplot(., aes ( x = var_igr_effect, y = RD_diss_igr_effect, alpha =alpha_ij_sd ))+
  geom_point()+
  facet_wrap(~alpha_ij_sd, ncol = 5)+
  theme_bw()+
  theme(axis.title.y=element_text(size=14, face="plain", colour="black",vjust=0.3),axis.text.y=element_text(size=10,face="plain",colour="black",angle=0,hjust=0.4))+
  theme(axis.title.x=element_text(size=14,face="plain",colour="black",vjust=0),axis.text.x=element_text(size=10,face="plain",colour="black"))+
  theme(legend.position = 'none')


p2<-communityData %>%
  ggplot(., aes ( x = var_igr_effect, y = RD_div_igr_effect, alpha =alpha_ij_sd ))+
  geom_point()+
  facet_wrap(~alpha_ij_sd, ncol = 5)+
  theme_bw()+
  theme(axis.title.y=element_text(size=14, face="plain", colour="black",vjust=0.3),axis.text.y=element_text(size=10,face="plain",colour="black",angle=0,hjust=0.4))+
  theme(axis.title.x=element_text(size=14,face="plain",colour="black",vjust=0),axis.text.x=element_text(size=10,face="plain",colour="black"))+
  theme(legend.position = 'none')


p3<-communityData %>%
  ggplot(., aes ( x = mean_igr_effect, y = RD_diss_igr_effect, alpha =alpha_ij_sd ))+
  geom_point()+
  facet_wrap(~alpha_ij_sd, ncol = 5)+
  theme_bw()+
  theme(axis.title.y=element_text(size=14, face="plain", colour="black",vjust=0.3),axis.text.y=element_text(size=10,face="plain",colour="black",angle=0,hjust=0.4))+
  theme(axis.title.x=element_text(size=14,face="plain",colour="black",vjust=0),axis.text.x=element_text(size=10,face="plain",colour="black"))+
  theme(legend.position = 'none')


p4<-communityData %>%
  ggplot(., aes ( x = mean_igr_effect, y = RD_div_igr_effect, alpha =alpha_ij_sd ))+
  geom_point()+
  facet_wrap(~alpha_ij_sd, ncol = 5)+
  theme_bw()+
  theme(axis.title.y=element_text(size=14, face="plain", colour="black",vjust=0.3),axis.text.y=element_text(size=10,face="plain",colour="black",angle=0,hjust=0.4))+
  theme(axis.title.x=element_text(size=14,face="plain",colour="black",vjust=0),axis.text.x=element_text(size=10,face="plain",colour="black"))+
  theme(legend.position = 'bottom')

legend_p4 <- get_legend(p4)
cowplot:: plot_grid(p1,p2,p3,p4+theme(legend.position = 'none'),ncol = 2,legend_p4, labels = c('a)', 'b)', 'c)', 'd)'), rel_heights = c(2,2,0.3))
ggsave(plot = last_plot(), file = here('output/IGR_Traits.png'), width = 15, height = 15)

#### Realised Response traits ####
p5<-communityData %>%
  ggplot(., aes ( x = var_spp_deltabm, y = realised_RD_diss, alpha =alpha_ij_sd ))+
  geom_point()+
  # facet_wrap(~alpha_ij_sd, ncol = 6)+
  theme_bw()+
  theme(axis.title.y=element_text(size=14, face="plain", colour="black",vjust=0.3),axis.text.y=element_text(size=10,face="plain",colour="black",angle=0,hjust=0.4))+
  theme(axis.title.x=element_text(size=14,face="plain",colour="black",vjust=0),axis.text.x=element_text(size=10,face="plain",colour="black"))+
  theme(legend.position = 'none')


p6<-communityData %>%
  ggplot(., aes ( x = var_spp_deltabm, y = realised_RD_div, alpha =alpha_ij_sd ))+
  geom_point()+
  # facet_wrap(~alpha_ij_sd, ncol = 6)+
  theme_bw()+
  theme(axis.title.y=element_text(size=14, face="plain", colour="black",vjust=0.3),axis.text.y=element_text(size=10,face="plain",colour="black",angle=0,hjust=0.4))+
  theme(axis.title.x=element_text(size=14,face="plain",colour="black",vjust=0),axis.text.x=element_text(size=10,face="plain",colour="black"))+
  theme(legend.position = 'none')


p7<-communityData %>%
  ggplot(., aes ( x = mean_spp_deltabm, y = realised_RD_diss, alpha =alpha_ij_sd ))+
  geom_point()+
  #facet_wrap(~alpha_ij_sd, ncol = 6)+
  theme_bw()+
  theme(axis.title.y=element_text(size=14, face="plain", colour="black",vjust=0.3),axis.text.y=element_text(size=10,face="plain",colour="black",angle=0,hjust=0.4))+
  theme(axis.title.x=element_text(size=14,face="plain",colour="black",vjust=0),axis.text.x=element_text(size=10,face="plain",colour="black"))+
  theme(legend.position = 'none')


p8<-communityData %>%
  ggplot(., aes ( x = mean_spp_deltabm, y = realised_RD_div, alpha =alpha_ij_sd ))+
  geom_point()+
  # facet_wrap(~alpha_ij_sd, ncol = 6)+
  theme_bw()+
  theme(axis.title.y=element_text(size=14, face="plain", colour="black",vjust=0.3),axis.text.y=element_text(size=10,face="plain",colour="black",angle=0,hjust=0.4))+
  theme(axis.title.x=element_text(size=14,face="plain",colour="black",vjust=0),axis.text.x=element_text(size=10,face="plain",colour="black"))+
  theme(legend.position = 'bottom')

legend_p4 <- get_legend(p4)
cowplot:: plot_grid(p1,p2,p3,p4+theme(legend.position = 'none'),p5,p6,p7,p8+theme(legend.position = 'none'),ncol = 4,legend_p4, labels = c('a)', 'b)', 'c)', 'd)', 'e)', 'f)', 'g)', 'h)'), rel_heights = c(2,2,0.3))
ggsave(plot = last_plot(), file = 'IGR_Realised_Traits.png', width = 12, height = 8)

#### Fundamental and Realised Response traits ####
p1<-communityData %>%
  ggplot(., aes ( x = var_species_RR_AUC, y = var_igr_effect ))+
  geom_point(alpha = 0.4)+
  facet_wrap(~alpha_ij_sd, ncol = 5)+
  theme_bw()+
  theme(axis.title.y=element_text(size=14, face="plain", colour="black",vjust=0.3),axis.text.y=element_text(size=10,face="plain",colour="black",angle=0,hjust=0.4))+
  theme(axis.title.x=element_text(size=14,face="plain",colour="black",vjust=0),axis.text.x=element_text(size=10,face="plain",colour="black"))+
  theme(legend.position = 'none')


p2<-communityData %>%
  ggplot(., aes ( x = RD_div_igr_effect, y = RD_div_species_RR_AUC ))+
  geom_point(alpha = 0.4)+
  facet_wrap(~alpha_ij_sd, ncol = 5)+
  theme_bw()+
  theme(axis.title.y=element_text(size=14, face="plain", colour="black",vjust=0.3),axis.text.y=element_text(size=10,face="plain",colour="black",angle=0,hjust=0.4))+
  theme(axis.title.x=element_text(size=14,face="plain",colour="black",vjust=0),axis.text.x=element_text(size=10,face="plain",colour="black"))+
  theme(legend.position = 'none')


p3<-communityData %>%
  ggplot(., aes ( x = mean_species_RR_AUC, y = mean_igr_effect))+
  geom_point(alpha = 0.4)+
  facet_wrap(~alpha_ij_sd, ncol = 5)+
  theme_bw()+
  theme(axis.title.y=element_text(size=14, face="plain", colour="black",vjust=0.3),axis.text.y=element_text(size=10,face="plain",colour="black",angle=0,hjust=0.4))+
  theme(axis.title.x=element_text(size=14,face="plain",colour="black",vjust=0),axis.text.x=element_text(size=10,face="plain",colour="black"))+
  theme(legend.position = 'none')


p4<-communityData %>%
  ggplot(., aes ( x = RD_div_igr_effect, y = RD_div_species_RR_AUC ))+
  geom_point(alpha = 0.4)+
  facet_wrap(~alpha_ij_sd, ncol = 5)+
  theme_bw()+
  theme(axis.title.y=element_text(size=14, face="plain", colour="black",vjust=0.3),axis.text.y=element_text(size=10,face="plain",colour="black",angle=0,hjust=0.4))+
  theme(axis.title.x=element_text(size=14,face="plain",colour="black",vjust=0),axis.text.x=element_text(size=10,face="plain",colour="black"))+
  theme(legend.position = 'bottom')

legend_p4 <- get_legend(p4)
cowplot:: plot_grid(p1,p2,p3,p4+theme(legend.position = 'none'),ncol = 2,legend_p4, labels = c('a)', 'b)', 'c)', 'd)'), rel_heights = c(2,2,0.3))
ggsave(plot = last_plot(), file = here('output/Realised_Fundamental_Traits.png'), width = 14, height = 15)


### Relative contributions ###
names(communityData)

p9<-communityData %>%
  ggplot(., aes ( x = var_spp_deltabm_pi, y = comm_tot_deltabm, alpha = alpha_ij_sd))+
  geom_point()+
  # facet_wrap(~alpha_ij_sd, ncol = 6)+
  theme_bw()+
  theme(axis.title.y=element_text(size=14, face="plain", colour="black",vjust=0.3),axis.text.y=element_text(size=10,face="plain",colour="black",angle=0,hjust=0.4))+
  theme(axis.title.x=element_text(size=14,face="plain",colour="black",vjust=0),axis.text.x=element_text(size=10,face="plain",colour="black"))+
  theme(legend.position = 'none')

p10<-communityData %>%
  ggplot(., aes ( x = mean_spp_deltabm_pi, y = comm_tot_deltabm, alpha = alpha_ij_sd))+
  geom_point()+
  # facet_wrap(~alpha_ij_sd, ncol = 6)+
  theme_bw()+
  theme(axis.title.y=element_text(size=14, face="plain", colour="black",vjust=0.3),axis.text.y=element_text(size=10,face="plain",colour="black",angle=0,hjust=0.4))+
  theme(axis.title.x=element_text(size=14,face="plain",colour="black",vjust=0),axis.text.x=element_text(size=10,face="plain",colour="black"))+
  theme(legend.position = 'none')

p11<-communityData %>%
  ggplot(., aes ( x = realised_RD_diss_pi, y = comm_tot_deltabm, alpha = alpha_ij_sd))+
  geom_point()+
  # facet_wrap(~alpha_ij_sd, ncol = 6)+
  theme_bw()+
  theme(axis.title.y=element_text(size=14, face="plain", colour="black",vjust=0.3),axis.text.y=element_text(size=10,face="plain",colour="black",angle=0,hjust=0.4))+
  theme(axis.title.x=element_text(size=14,face="plain",colour="black",vjust=0),axis.text.x=element_text(size=10,face="plain",colour="black"))+
  theme(legend.position = 'none')

p12<-communityData %>%
  ggplot(., aes ( x = realised_RD_div_pi, y = comm_tot_deltabm, alpha = alpha_ij_sd))+
  geom_point()+
  # facet_wrap(~alpha_ij_sd, ncol = 6)+
  theme_bw()+
  theme(axis.title.y=element_text(size=14, face="plain", colour="black",vjust=0.3),axis.text.y=element_text(size=10,face="plain",colour="black",angle=0,hjust=0.4))+
  theme(axis.title.x=element_text(size=14,face="plain",colour="black",vjust=0),axis.text.x=element_text(size=10,face="plain",colour="black"))+
  theme(legend.position = 'bottom')
legend_b<-get_legend(p12)
cowplot:: plot_grid(p9,p10,p11,p12+theme(legend.position = 'none'), legend_b, labels = c('a)', 'b)', 'c)', 'd)'), rel_heights = c(2,2,0.3), ncol = 2)
ggsave(plot = last_plot(), file = 'Realised_Traits_Pi_Instab.png', width = 9, height = 9)

#### Facets: Realised response traits correlations ####
p1<-communityData %>%
  ggplot(., aes ( x = var_spp_deltabm, y = realised_RD_diss))+
  geom_point()+
  facet_wrap(~alpha_ij_sd, ncol = 6)+
  theme_bw()+
  theme(axis.title.y=element_text(size=14, face="plain", colour="black",vjust=0.3),axis.text.y=element_text(size=10,face="bold",colour="black",angle=0,hjust=0.4))+
  theme(axis.title.x=element_text(size=14,face="plain",colour="black",vjust=0),axis.text.x=element_text(size=10,face="bold",colour="black"))


p2<-communityData %>%
  ggplot(., aes ( x = var_spp_deltabm, y = realised_RD_div))+
  geom_point()+
  facet_wrap(~alpha_ij_sd, ncol = 6)+
  theme_bw()+
  theme(axis.title.y=element_text(size=14, face="plain", colour="black",vjust=0.3),axis.text.y=element_text(size=10,face="bold",colour="black",angle=0,hjust=0.4))+
  theme(axis.title.x=element_text(size=14,face="plain",colour="black",vjust=0),axis.text.x=element_text(size=10,face="bold",colour="black"))


p3<-communityData %>%
  ggplot(., aes ( x = mean_spp_deltabm, y = realised_RD_diss))+
  geom_point()+
  facet_wrap(~alpha_ij_sd, ncol = 6)+
  theme_bw()+
  theme(axis.title.y=element_text(size=14, face="plain", colour="black",vjust=0.3),axis.text.y=element_text(size=10,face="bold",colour="black",angle=0,hjust=0.4))+
  theme(axis.title.x=element_text(size=14,face="plain",colour="black",vjust=0),axis.text.x=element_text(size=10,face="bold",colour="black"))


p4<-communityData %>%
  ggplot(., aes ( x = mean_spp_deltabm, y = realised_RD_div))+
  geom_point()+
  facet_wrap(~alpha_ij_sd, ncol = 6)+
  theme_bw()+
  theme(axis.title.y=element_text(size=14, face="plain", colour="black",vjust=0.3),axis.text.y=element_text(size=10,face="bold",colour="black",angle=0,hjust=0.4))+
  theme(axis.title.x=element_text(size=14,face="plain",colour="black",vjust=0),axis.text.x=element_text(size=10,face="bold",colour="black"))

cowplot:: plot_grid(p1,p2,p3,p4, labels = c('a)', 'b)', 'c)', 'd)'))
ggsave(plot = last_plot(), file = 'Realised_Traits.png', width = 15, height = 12)

