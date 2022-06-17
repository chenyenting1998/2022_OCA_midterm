plot_map <- function(data){
  ggplot()+
    geom_raster(data = data[data$Ele > 0,],
                aes(x = Lon, y = Lat),
                color = "black") +
    theme_bw() +
    xlab(Longitude~(degree*E)) +
    ylab(Latitude~(degree*N)) +
    scale_x_continuous(expand = c(0,0), limit = c(min(data$Lon), max(data$Lon)))+
    scale_y_continuous(expand = c(0,0), limit = c(min(data$Lat), max(data$Lat)))+
    coord_fixed()
}

add_tw_st <- function(data, loc){
  geom_point(data = data[data$Location %in% loc,],
             aes(x = Lon, y = Lat, color = Location),
             size = 2, 
             shape = 21, 
             stroke = 1.5)
}

add_st <- function(data, loc){
  geom_point(data = data[data$Location %in% loc,],
             aes(x = Lon, y = Lat, color = Progress, fill = Progress),
             alpha = 0.5,
             shape = 21,
             size = 1.5)
}
add_st_lab <- function(data, loc, s = 1){
  ggrepel::geom_label_repel(data = data[data$Location %in% loc,],
                            aes(x = Lon, y = Lat, label = Station_zh, color = Progress),
                            family = msjh,
                            size = 3,
                            label.padding = 0.15,
                            seed = s,
                            force = 60,
                            force_pull = 30,
                            max.iter = 30000,
                            max.overlaps = 100,
                            min.segment.length = 0)
}

plot_save <- function(object, name, scale = 1.5, h = 4, w = 6){
  L <- paste0("fig/", name, ".png")
  ggsave(filename = L,
         plot = object,
         scale = scale,
         height = h,
         width = w)
}
ht <- list(theme(legend.position = "none"))
