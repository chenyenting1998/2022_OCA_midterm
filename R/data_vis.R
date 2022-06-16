pacman::p_load(dplyr, readxl, tidyr, ggplot2, GRSPRSThesisData, Polychrome, ggrepel, extrafont)

load("data/2022_OCA_midterm_size.RData")

# exclude unwanted data
size <- 
  full_join(Penghu_North_Taoyun_East_size, East_Liuqiu_size) %>% 
  filter(Condition %in% c("C", "FH")) %>% 
  filter(!Taxon %in% c("Unknown"))

# 
rare <- 
  size %>% 
  group_by(Taxon) %>% 
  summarize(Count = n()) %>% 
  mutate(percent = round(Count/sum(Count) * 100, 2)) %>% 
  arrange(desc(percent))

# label rare taxa as others
rare$Taxa <- if_else(rare$percent < 1, "Others", "Dominant")
# use the original label as donimants
rare[rare$Taxa == "Dominant",]$Taxa <- rare[rare$Taxa == "Dominant",]$Taxon

# reorder and factorize dominant taxa
taxa_order <- rare$Taxa[c(12,1,6,3,4,5,7,11,9,10,8,2)]
rare$Taxa <- factor(rare$Taxa, taxa_order)

taxa_color <- kelly.colors(13)[-1]

# Sampling location (sampled and analyzed)----
# Boxplot (abundance and biomass) ----
# Abundance composition ------
# Biomass composition ------