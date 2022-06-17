# prep ----
pacman::p_load(dplyr, 
               readxl,
               writexl,
               tidyr, 
               vegan,
               ggplot2, 
               ggrepel,
               ggdendro,
               patchwork,
               pairwiseAdonis,
               cluster,
               Polychrome, 
               extrafont)
# source
source("source/msjh.R")
source("source/map_func.R")

# load data 
load("data/2022_OCA_midterm_size.RData")
load("data/color_code.RData")
load("data/den_bio_wide.RData")

# density nmds ----
all_den_nmds <-
  dw[-(1:2)]^0.25 %>%
  metaMDS(trymax = 200, autotransform = FALSE)

all_den_sites <- all_den_nmds$points %>% as.data.frame() %>% cbind(dw[1:2])
all_den_species <- all_den_nmds$species %>% as.data.frame()

den_nmds_ggplot <-
  ggplot(all_den_sites, (aes(x = MDS1, y = MDS2, color = Location)))+
  geom_point(size = 2) +
  geom_text_repel(aes(label = Station_zh), seed = 1, size = 2.5)+
  annotate(geom = "text",
           label = paste0("Stress: ", round(all_den_nmds$stress, 2)),
           x = Inf, y = Inf,
           hjust = 1.1,
           vjust = 1.1)+
  scale_color_manual(values = loc_color)+
  theme_bw() +
  coord_fixed()

# biomass nmds ----
all_bio_nmds <-
  bw[-(1:2)]^0.25 %>%
  metaMDS(trymax = 200, autotransform = FALSE)

all_bio_sites <- all_bio_nmds$points %>% as.data.frame() %>% cbind(bw[1:2])
all_bio_species <- all_bio_nmds$species %>% as.data.frame()

bio_nmds_ggplot <-
  ggplot(all_bio_sites, (aes(x = MDS1, y = MDS2, color = Location)))+
  geom_point(size = 2) +
  geom_text_repel(aes(label = Station_zh), seed = 1, size = 2.5)+
  annotate(geom = "text",
           label = paste0("Stress: ", round(all_bio_nmds$stress, 2)),
           x = Inf, y = Inf,
           hjust = 1.1,
           vjust = 1.1)+
  scale_color_manual(values = loc_color)+
  theme_bw() +
  coord_fixed()

all_nmds <- 
  den_nmds_ggplot + theme(legend.position = "none") +
  bio_nmds_ggplot + 
  plot_annotation(tag_levels = "a",
                  tag_prefix = "(",
                  tag_suffix = ")")
plot_save(all_nmds, "nmds", w = 8, h =5)
