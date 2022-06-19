# plot combine

load("data/hel_cluster.RData")
load("data/hel_pca.RData")

# density
den1 <- (den_rda_ggplot+ theme(legend.position = "none")) / (den_rda_ggplot_species + theme(legend.position = "none"))
den2 <- den_dendro_ggplot


plot_save(den1 - den2+ 
            plot_annotation(tag_levels = "a",
                            tag_prefix = "(",
                            tag_suffix = ")"),
          "den_cluster_pca", h = 4, w = 5, scale = 2)

# biomass ----
bio1 <- (bio_rda_ggplot+ theme(legend.position = "none")) / (bio_rda_ggplot_species + theme(legend.position = "none"))
bio2 <- bio_dendro_ggplot


plot_save(bio1 - bio2 + 
            plot_annotation(tag_levels = "a",
                            tag_prefix = "(",
                            tag_suffix = ")"),
          "bio_cluster_pca", h = 4, w = 4.5, scale = 2)


