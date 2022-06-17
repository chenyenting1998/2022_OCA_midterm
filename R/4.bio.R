# prep ----
pacman::p_load(dplyr, 
               readxl,
               tidyr, 
               ggplot2, 
               Polychrome, 
               ggrepel, 
               extrafont)
# source
source("source/msjh.R")

# load data 
load("data/2022_OCA_midterm_size.RData")
load("data/color_code.RData")
load("data/taxa_rank.RData")

# constants
gc_area <- (6.6 / 2 * 0.01) ^2 * pi # gravity core area
sw <- 1.13 # specific weight

# function
plot_save <- function(object, name, scale = 1.5, h = 4, w = 8){
  L <- paste0("fig/", name, ".png")
  ggsave(filename = L,
         plot = object,
         scale = scale,
         height = h,
         width = w)
}

# Boxplot (abundance and biomass) ----
ss <- # standing stock
  size %>% 
  mutate(Location = gsub("2021.", "", Location)) %>% 
  group_by(Location, Station_zh, Tube, Section) %>% 
  summarize(Density = n()/gc_area, 
            Biomass = sum(Size * sw) / gc_area / 1000) %>% 
  pivot_longer(cols = c("Density", "Biomass"),
               names_to = "Variable",
               values_to = "Value")
set.seed(1)
ss_name <- 
  c("Density" = "Density~(ind.~m^-2)",
    "Biomass" = "Biomass~(g~wet~weight~m^-2)")

boxplot_ggplot <- 
  ss %>% 
  ggplot(aes(x = Location, y = Value, color = Location))+
  geom_boxplot(outlier.shape = NA) +
  geom_point(position = "jitter")+
  facet_wrap(~Variable, 
             scales = "free_y",
             labeller = as_labeller(ss_name, label_parsed))+
  scale_color_manual(values = loc_color)+
  theme_bw()


plot_save(boxplot_ggplot, "standing_stock_boxplot", scale = 1)

# Density composition ------
comp <- # standing stock
  size %>% 
  mutate(Location = gsub("2021.", "", Location)) %>% 
  group_by(Location, Station_zh, Tube, Section, Taxon) %>% 
  summarize(Density = n()/gc_area, 
            Biomass = sum(Size * sw) / gc_area / 1000) %>% 
  pivot_longer(cols = c("Density", "Biomass"),
               names_to = "Variable",
               values_to = "Value") 
comp$Taxa_den <- rank_den$Taxa[match(comp$Taxon, rank_den$Taxon)]
comp$Taxa_bio <- rank_bio$Taxa[match(comp$Taxon, rank_bio$Taxon)]

den_comp_ggplot <- 
  comp[comp$Variable == "Density",] %>% 
  ggplot(aes(x = Station_zh, y = Value, fill = Taxa_den))+
  geom_bar(stat = "identity")+
  facet_grid(~Location, scales = "free_x", space = "free")+
  scale_fill_manual(values = taxa_den_color)+
  scale_y_continuous(expand = c(0,0.1), 
                     limits = c(0,max(ss[ss$Variable == "Density",]$Value) * 1.05))+
  ylab(Density~(ind.~m^-2))+
  guides(guide_legend(title = "Taxa"))+
  theme_bw()+
  theme(axis.text.x = element_text(family = msjh, 
                                   angle = 90,
                                   hjust = 1,
                                   vjust = 0.5))

plot_save(den_comp_ggplot, "composition_density")
# Biomass composition ------
bio_comp_ggplot <- 
  comp[comp$Variable == "Biomass",] %>% 
  ggplot(aes(x = Station_zh, y = Value, fill = Taxa_bio))+
  geom_bar(stat = "identity")+
  facet_grid(~Location, scales = "free_x", space = "free")+
  scale_fill_manual(values = taxa_bio_color)+
  scale_y_continuous(expand = c(0,0.1), 
                     limits = c(0,max(ss[ss$Variable == "Biomass",]$Value) * 1.05))+
  ylab(Biomass~(g~wet~weight~m^-2))+
  theme_bw()+
  guides(guide_legend(title = "Taxa"))+
  theme(axis.text.x = element_text(family = msjh, 
                                   angle = 90,
                                   hjust = 1,
                                   vjust = 0.5))

plot_save(bio_comp_ggplot, "composition_biomass")
