pacman::p_load(readxl, writexl, ggplot2, dplyr, patchwork, ncdf4)

# load files
load("data/2022_OCA_midterm_station.RData")
load("data/color_code.RData")
# read source
source("source/map_func.R")
source("source/msjh.R")

# load GEBCO 2020 ----
map_nc_path <- dir(path = "GEBCO_2020", pattern = "2020.nc", full.names = T)
map_nc <- nc_open(map_nc_path)

# extract lon. and lat.
Lon <- ncvar_get(map_nc, "lon")
Lat <- ncvar_get(map_nc, "lat")
Ele <- ncvar_get(map_nc, "elevation")

# Extract tw map
map <- 
  expand.grid(Lon = Lon,
              Lat = Lat) %>%
  cbind(Ele = as.vector(Ele)) %>% 
  as.data.frame() %>% 
  filter(Lon > 119 & Lon < 123 & Lat > 21.5 & Lat < 26)

# remove 
rm(list = c("map_nc", "Lon", "Lat", "Ele"))

# plot tw ----
tw <- 
  plot_map(map) + 
  add_tw_st(st, unique(st$Location))+
  scale_color_manual(values = loc_color[1:5])

# plot location ----
# `location`_corner
e_c <- data.frame(Lon = c(121.2, 122.1), Lat = c(23.0, 24.7))
l_c <- data.frame(Lon = c(120.325, 120.415), Lat = c(22.3, 22.38))
n_c <- data.frame(Lon = c(121.45, 122.3), Lat = c(24.7, 25.7))
p_c <- data.frame(Lon = c(119.2, 119.8), Lat = c(23.1, 23.9))
t_c <- data.frame(Lon = c(121.1, 121.3), Lat = c(25.06, 25.21))

# subset map
e_map <- map %>% filter(Lon > min(e_c$Lon) & Lon < max(e_c$Lon) & Lat > min(e_c$Lat) & Lat < max(e_c$Lat))
l_map <- map %>% filter(Lon > min(l_c$Lon) & Lon < max(l_c$Lon) & Lat > min(l_c$Lat) & Lat < max(l_c$Lat))
n_map <- map %>% filter(Lon > min(n_c$Lon) & Lon < max(n_c$Lon) & Lat > min(n_c$Lat) & Lat < max(n_c$Lat))
p_map <- map %>% filter(Lon > min(p_c$Lon) & Lon < max(p_c$Lon) & Lat > min(p_c$Lat) & Lat < max(p_c$Lat))
t_map <- map %>% filter(Lon > min(t_c$Lon) & Lon < max(t_c$Lon) & Lat > min(t_c$Lat) & Lat < max(t_c$Lat))

# plot progress
e <- plot_map(e_map) + add_st(st, loc = "East") + add_st_lab(st, loc = "East", s = 3) 
n <- plot_map(n_map) + add_st(st, loc = "North") + add_st_lab(st, loc = "North", s = 100)
l <- plot_map(l_map) + add_st(st, loc = "Liuqiu") + add_st_lab(st, loc = "Liuqiu") + ht
p <- plot_map(p_map) + add_st(st, loc = "Penghu") + add_st_lab(st, loc = "Penghu", s = 6) + ht
t <- plot_map(t_map) + add_st(st, loc = "TY") + add_st_lab(st, loc = "TY") + ht

plot_save(p + n + plot_annotation(tag_levels = "a",
                                  tag_prefix = "(",
                                  tag_suffix = ")"), 
          "Penghu_North", scale = 1.7)
sp1 <- wrap_plots(t, l, ncol = 1)
plot_save(sp1 - e + plot_annotation(tag_levels = "a",
                                    tag_prefix = "(",
                                    tag_suffix = ")")
          , "TY_Liuqiu_East", scale = 1.7)
plot_save(tw, "tw")

e_map_ggplot <- plot_map(e_map)
n_map_ggplot <- plot_map(n_map)
l_map_ggplot <- plot_map(l_map)
p_map_ggplot <- plot_map(p_map)
t_map_ggplot <- plot_map(t_map)

save(e_map_ggplot, n_map_ggplot, l_map_ggplot, p_map_ggplot, t_map_ggplot, 
     file = "data/map_ggplot.RData")
