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

# density pca ----
all_den_rda <-
  dw[-(1:2)] %>%
  decostand(method = "hellinger") %>% 
  rda()

den_rda_pr <- round((all_den_rda$CA$eig / all_den_rda$tot.chi) * 100, 2)

barplot(all_den_rda$CA$eig)

all_den_sites <- scores(all_den_rda, scales = 1)$sites %>% as.data.frame %>% cbind(dw[1:2])
all_den_species <- scores(all_den_rda, scales = 1)$species %>% as.data.frame()
all_den_species$ri <- (all_den_species$PC1^2 + all_den_species$PC2^2)^0.5

den_rda_ggplot <-
  ggplot()+
  geom_point(data = all_den_sites, 
             aes(x = PC1, y = PC2, color = Location), size = 2) +
  geom_text_repel(data = all_den_sites, 
                  aes(x = PC1, y = PC2, color = Location, label = Station_zh), 
                  seed = 1, size = 2.5)+
  # geom_text(data = all_den_species,
  #            aes(x = PC1,
  #                y = PC2,
  #                label = rownames(all_den_species),
  #                alpha = ri))+
  xlab(paste0("PC1 (", den_rda_pr[1], "% explained)"))+
  ylab(paste0("PC2 (", den_rda_pr[2], "% explained"))+
  scale_color_manual(values = loc_color)+
  # scale_alpha_continuous(colors = c(0.3, 1))+
  theme_bw() +
  coord_fixed()

# biomass pca ----

# density pca ----
all_bio_rda <-
  bw[-(1:2)] %>%
  decostand(method = "hellinger") %>% 
  rda()

bio_rda_pr <- round((all_bio_rda$CA$eig / all_bio_rda$tot.chi) * 100, 2)

barplot(all_bio_rda$CA$eig)

all_bio_sites <- scores(all_bio_rda, scales = 1)$sites %>% as.data.frame %>% cbind(bw[1:2])
all_bio_species <- scores(all_bio_rda, scales = 1)$species %>% as.data.frame()

bio_rda_ggplot <-
  ggplot(all_bio_sites, (aes(x = PC1, y = PC2, color = Location)))+
  geom_point(size = 2) +
  geom_text_repel(aes(label = Station_zh), seed = 1, size = 2.5)+
  # geom_label(data = all_bio_species,
  #            aes(x = PC1, y = PC2, label = rownames(all_bio_species)),
  #            alpha = 0.6,
  #            color = "black")+
  xlab(paste0("PC1 (", bio_rda_pr[1], "% explained)"))+
  ylab(paste0("PC2 (", bio_rda_pr[2], "% explained"))+
  scale_color_manual(values = loc_color)+
  theme_bw() +
  coord_fixed()

# save
all_rda <- 
  den_rda_ggplot + theme(legend.position = "none") +
  bio_rda_ggplot + 
  plot_annotation(tag_levels = "a",
                  tag_prefix = "(",
                  tag_suffix = ")")
plot_save(all_rda, "hel_pca", w = 8, h =5)
