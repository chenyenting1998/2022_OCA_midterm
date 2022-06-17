pacman::p_load(dplyr, writexl, readxl)
# color code
loc_color <-
  c("North" = "purple", 
    "East" = "green", 
    "Liuqiu" = "orange", 
    "TY" = "darkgrey", 
    "Penghu" = "black", 
    "N. Penghu" = "red", 
    "S. Penghu" = "blue")
prog_color <-
  c("Analyzed" = "cyan", "Sampled" = "pink")


# density rare ----
rank_den <- 
  size %>% 
  group_by(Taxon) %>% 
  summarize(Count = n()) %>% 
  mutate(percent = round(Count/sum(Count) * 100, 2)) %>% 
  arrange(desc(percent))

# label rare taxa as others
rank_den$Taxa <- if_else(rank_den$percent < 1, "Others", "Dominant")
# use the original label as donimants
rank_den[rank_den$Taxa == "Dominant",]$Taxa <- rank_den[rank_den$Taxa == "Dominant",]$Taxon

# reorder and factorize dominant taxa
den_taxa_order <- 
  c("Others", "Polychaeta", "Oligochaeta", "Bivalvia",
    "Amphipoda", "Harpacticoida", "Isopoda", "Tanaidacea",
    "Nemertea", "Platyhelminthes", "Nematoda")

rank_den$Taxa <- factor(rank_den$Taxa, den_taxa_order)
taxa_den_color <- kelly.colors(12)[-1] %>% as.vector

# biomass rare----
rank_bio <- 
  size %>% 
  group_by(Taxon) %>% 
  summarize(Biomass = sum(WM)) %>% 
  mutate(percent = round(Biomass/sum(Biomass) * 100, 2)) %>% 
  arrange(desc(percent))

# label rare taxa as others
rank_bio$Taxa <- if_else(rank_bio$percent < 1, "Others", "Dominant")
# use the original label as donimants
rank_bio[rank_bio$Taxa == "Dominant",]$Taxa <- rank_bio[rank_bio$Taxa == "Dominant",]$Taxon

# reorder and factorize dominant taxa
bio_taxa_order <- c("Others", "Polychaeta", "Sipuncula", "Bivalvia", 
                    "Polyplacophora", "Decapoda", "Amphipoda", "Tanaidacea",
                    "Ophiuroidea", "Holothuroidea", "Platyhelminthes", "Nemertea", "Bryozoa")

rank_bio$Taxa <- factor(rank_bio$Taxa, bio_taxa_order)

taxa_bio_color <- kelly.colors(14)[-1]%>% as.vector

# save ----
save(loc_color, prog_color, taxa_bio_color, taxa_den_color, file = "data/color_code.RData")
save(rank_den, rank_bio, file = "data/taxa_rank.RData")