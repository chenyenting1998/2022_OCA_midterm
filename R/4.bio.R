# prep ----
pacman::p_load(dplyr, 
               readxl,
               writexl,
               tidyr, 
               ggplot2, 
               Polychrome, 
               ggrepel, 
               extrafont,
               GRSPRSThesisData)
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
plot_save <- function(object, name, scale = 1.5, h = 3.5, w = 8){
  L <- paste0("fig/", name, ".png")
  ggsave(filename = L,
         plot = object,
         scale = scale,
         height = h,
         width = w)
}
size %>% filter(Station_zh %in% ("潮境")) %>% View

ss %>% 
  ungroup %>% 
  group_by(Location, Variable) %>% 
  summarise(mean(Value)) %>% 
  arrange(Variable)

# Boxplot (abundance and biomass) ----
ss <- # standing stock
  size %>% 
  filter(Condition %in% c("C", "FH")) %>% 
  filter(!Taxon %in% c("Unknown")) %>% 
  mutate(Location = gsub("2021.", "", Location)) %>% 
  group_by(Location, Station_zh) %>% 
  summarize(Density = n()/gc_area, 
            Biomass = sum(Size * sw) / gc_area / 1000) %>% 
  pivot_longer(cols = c("Density", "Biomass"),
               names_to = "Variable",
               values_to = "Value")
set.seed(1)
ss_name <- 
  c("Density" = "Density~(ind.~m^-2)",
    "Biomass" = "Biomass~(g~wet~weight~m^-2)")
fct_loc <- c("Penghu", "North", "Taoyuan", "Liuqiu", "East")

boxplot_ggplot <- 
  ss %>% 
  mutate(Location = factor(Location, fct_loc)) %>% 
  ggplot(aes(x = Location, y = Value, color = Location))+
  geom_boxplot(outlier.shape = NA) +
  geom_point(position = "jitter")+
  facet_wrap(~Variable, 
             scales = "free_y",
             labeller = as_labeller(ss_name, label_parsed))+
  scale_color_manual(values = loc_color)+
  theme_bw()


plot_save(boxplot_ggplot, "standing_stock_boxplot", scale = 1)

# Kruskal wallis
# remove ty due to few samples
ss_pnle <- ss[ss$Location != "Taoyuan",]
library(FSA)
kw_density <- 
  kruskal.test(Value~Location, 
               data = ss_pnle[ss_pnle$Variable == "Density",])

kw_density <- 
  data.frame("chi-squared" = kw_density$statistic,
             "df" = kw_density$parameter,
             "p.value" = kw_density$p.value,
             row.names = "Density")
d_density <-
  dunnTest(Value~Location, 
           data = ss_pnle[ss_pnle$Variable == "Density",],
           method = "bonferroni")

kw_biomass <- 
  kruskal.test(Value~Location, 
               data = ss_pnle[ss_pnle$Variable == "Biomass",])

kw_biomass <- 
  data.frame("chi-squared" = kw_biomass$statistic,
             "df" = kw_biomass$parameter,
             "p.value" = kw_biomass$p.value,
             row.names = "Biomass")
d_biomass <-
  dunnTest(Value~Location,
           data = ss_pnle[ss_pnle$Variable == "Biomass",],
           method = "bonferroni")

write_xlsx(list(KW_density = kw_density,
                Dunn_density = as.data.frame(d_density$res),
                KW_biomass = kw_biomass,
                Dunn_biomass = as.data.frame(d_biomass$res)),
           path = "tab/nonparm_test_standing_stock.xlsx")

# Density composition ------
comp <- # standing stock
  size %>% 
  filter(Condition %in% c("C", "FH")) %>% 
  filter(!Taxon %in% c("Unknown")) %>% 
  mutate(Location = gsub("2021.", "", Location)) %>% 
  group_by(Location, Station_zh, Taxon) %>% 
  summarize(Density = n()/gc_area, 
            Biomass = sum(Size * sw) / gc_area / 1000) %>% 
  pivot_longer(cols = c("Density", "Biomass"),
               names_to = "Variable",
               values_to = "Value") 
comp$Taxa_den <- rank_den$Taxa[match(comp$Taxon, rank_den$Taxon)]
comp$Taxa_bio <- rank_bio$Taxa[match(comp$Taxon, rank_bio$Taxon)]

den_comp_ind_ggplot <- 
  comp[comp$Variable == "Density",] %>% 
  mutate(Location = factor(Location, fct_loc)) %>% 
  ggplot(aes(x = Station_zh, y = Value, fill = Taxa_den))+
  geom_bar(stat = "identity")+
  facet_grid(~Location, scales = "free_x", space = "free")+
  scale_fill_manual(values = taxa_den_color)+
  scale_y_continuous(expand = c(0,0.1), 
                     limits = c(0,max(ss[ss$Variable == "Density",]$Value) * 1.05))+
  xlab("Station")+
  ylab(Density~(ind.~m^-2))+
  guides(fill = guide_legend(title = "Taxa"))+
  theme_bw()+
  theme(axis.text.x = element_text(family = msjh, 
                                   angle = 90,
                                   hjust = 1,
                                   vjust = 0.5))

plot_save(den_comp_ind_ggplot, "composition_density_ind.")

den_comp_ggplot_percent <- 
  comp[comp$Variable == "Density",] %>% 
  mutate(Location = factor(Location, fct_loc)) %>% 
  ggplot(aes(x = Station_zh, y = Value, fill = Taxa_den))+
  geom_bar(stat = "identity", position = "fill")+
  facet_grid(~Location, scales = "free_x", space = "free")+
  scale_fill_manual(values = taxa_den_color)+
  scale_y_continuous(expand = c(0,0),
                     label = scales::percent) +
  xlab("Station")+
  ylab("Density (%)")+
  guides(fill = guide_legend(title = "Taxa"))+
  theme_bw()+
  theme(axis.text.x = element_text(family = msjh, 
                                   angle = 90,
                                   hjust = 1,
                                   vjust = 0.5))

plot_save(den_comp_ggplot_percent, "composition_density_percent")


# Biomass composition ------
bio_comp_g_ggplot <- 
  comp[comp$Variable == "Biomass",] %>% 
  mutate(Location = factor(Location, fct_loc)) %>% 
  ggplot(aes(x = Station_zh, y = Value, fill = Taxa_bio))+
  geom_bar(stat = "identity")+
  facet_grid(~Location, scales = "free_x", space = "free")+
  scale_fill_manual(values = taxa_bio_color)+
  scale_y_continuous(expand = c(0,0.1), 
                     limits = c(0,max(ss[ss$Variable == "Biomass",]$Value) * 1.05))+
  xlab("Station")+
  ylab(Biomass~(g~wet~weight~m^-2))+
  theme_bw()+
  guides(fill = guide_legend(title = "Taxa"))+
  theme(axis.text.x = element_text(family = msjh, 
                                   angle = 90,
                                   hjust = 1,
                                   vjust = 0.5))

plot_save(bio_comp_g_ggplot, "composition_biomass_g")


bio_comp_percent_ggplot <- 
  comp[comp$Variable == "Biomass",] %>% 
  mutate(Location = factor(Location, fct_loc)) %>% 
  ggplot(aes(x = Station_zh, y = Value, fill = Taxa_bio))+
  geom_bar(stat = "identity", position = "fill")+
  facet_grid(~Location, scales = "free_x", space = "free")+
  scale_fill_manual(values = taxa_bio_color)+
  scale_y_continuous(expand = c(0, 0),
                     label = scales::percent) +
  xlab("Station")+
  ylab("Biomass (%)")+
  theme_bw()+
  guides(fill = guide_legend(title = "Taxa"))+
  theme(axis.text.x = element_text(family = msjh, 
                                   angle = 90,
                                   hjust = 1,
                                   vjust = 0.5))

plot_save(bio_comp_percent_ggplot, "composition_biomass_percent")

# Phylum rank ----
phylum_rank <- 
  c("Annelida", "Arthopoda", "Echinodermata", "Mollusca", "Cnidaria",
    "Nematoda", "Nemertea", "Entoprocta", "Bryozoa", "Phoronida",
    "Chaetognatha", "Hemichordata", "Chordata", "Platyhelminthes",
    "Tardigrada","Unknown")

table <- function(v = "Density", location = "Penghu"){
  x <-
    comp %>%
    add_coarse_taxon(coarse_taxa_add, output = "Phylum") %>%
    ungroup %>%
    filter(Location %in% location) %>%
    filter(Variable %in% v) %>%
    select(Phylum, Station_zh, Taxon, Value) %>%
    pivot_wider(names_from = "Station_zh",
                values_from = "Value",
                values_fill = NA_integer_) %>%
    group_by(Phylum, Taxon) %>%
    slice(match(phylum_rank, Phylum))
  
  s <- strsplit(v, "")[[1]]
  s[1] <- tolower(s[1])
  V <- paste0(s, collapse = "")
  
  a <- rbind(x[1:2], 
             data.frame(Phylum = paste0("Total ", V), Taxon = ""))
  
  sum_narm <- function(x) sum(x, na.rm = TRUE)
  b <- rbind(x[-(1:2)], 
             apply(x[-(1:2)], 2, sum_narm))
  cbind(a,b)
}

#density table ----
den_table_ph <- table("Density", "Penghu")
den_table_n <- table("Density", "North")
  
den_table_tle <- 
  full_join(table("Density", "Taoyuan"), 
            table("Density", "Liuqiu")) %>%
  full_join(table("Density", "East")) %>% 
  slice(match(c(phylum_rank, "Total density"), c(Phylum, "Total density")))

# biomass talbe ----
bio_table_ph <- table("Biomass", "Penghu")
bio_table_n <- table("Biomass", "North")
bio_table_tle <- 
  full_join(table("Biomass", "Taoyuan"), 
            table("Biomass", "Liuqiu")) %>%
  full_join(table("Biomass", "East")) %>% 
  slice(match(c(phylum_rank, "Total biomass"), c(Phylum, "Total biomass")))

write_xlsx(list(Density_Penghu = den_table_ph, 
                Density_North = den_table_n, 
                Density_Taoyuan_Liuqiu_East = den_table_tle,
                Biomass_Penghu = bio_table_ph,
                Biomass_North = bio_table_n,
                Biomass_Taoyuan_Liuqiu_East = bio_table_tle), 
           "tab/2022_OCA_midterm_density_biomass_table.xlsx")
