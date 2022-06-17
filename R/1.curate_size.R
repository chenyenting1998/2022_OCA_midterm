pacman::p_load(dplyr, writexl, readxl, GRSPRSThesisData)

# locate file
xlsx_path <- grep("station", 
                  dir("xlsx", ".xlsx", full.names = TRUE), 
                  invert = TRUE, 
                  value = TRUE)
# size_raw ----
# read amnd curate new files
ch <-
  read_xlsx(xlsx_path[1]) %>% 
  mutate(Condition = toupper(Condition))

cyt <-
  read_xlsx(xlsx_path[2]) %>% 
  mutate(Condition = toupper(Condition))

tcc <-
  read_xlsx(xlsx_path[3],
            col_types = c(rep("guess",11), "numeric", "numeric", rep("guess", 5))) %>% 
  mutate(Condition = toupper(Condition)) %>% 
  mutate(Cruise = if_else(Cruise == "2021.小琉球",
                          "2021.Liuqiu",
                          "2021.East")) %>% 
  mutate(Cruise = if_else(Station == "青灣", # should be penghu
                          "2021.Penghu",
                          Cruise))
  
# combine files
raw_data <- 
  full_join(ch, cyt) %>% 
  full_join(tcc)

# set up data
# object naming regime: stack up all the "cruises"
East_Liuqiu_size <- 
  raw_data %>% 
  select(-C) %>% 
  assign_method() %>% 
  calculate_biovolume() %>% 
  define_ophiuroid_size(protocol_ophiuroid = "all_arms",
                        grouping_variables = colnames(raw_data)[1:6]) %>% 
  mutate(Station_zh = Station,
         WM = Size * 1.13,
         Type = Method) %>% 
  select(-Method)

# read old file
Penghu_North_Taoyun_East_size <- read_xlsx(xlsx_path[4])# %>% mutate(WM = size_raw * 1.13)

# create size_raw raw
size_raw <- full_join(East_Liuqiu_size, Penghu_North_Taoyun_East_size)
colnames(size_raw)[1] <- "Location"

# st----

# load station ----
st <- 
  read_xlsx("xlsx/station.xlsx") %>% 
  filter(macrofauna == 1) %>% 
  select(Location, Station_zh, Station, Lat, Lon) %>% 
  # mutate(Location = paste0("2021.", Location)) %>% 
  mutate(Lon = as.numeric(Lon))

st_liuqiu <- 
  read_xlsx("xlsx/station_liuqiu.xlsx") %>% 
  mutate(Location = "Liuqiu") %>% 
  select(Location, Station_zh, Station, Lat, Lon)

st_taoyuan <-
  read_xlsx("xlsx/station_taoyuan.xlsx") %>% 
  mutate(Location = "TY") %>% 
  select(Location, Station_zh, Station, Lat, Lon)

st <- 
  full_join(st, st_liuqiu) %>% 
  full_join(st_taoyuan)

analyzed <-
  size_raw %>% 
  select(Location, Station_zh) %>% 
  distinct

# attach progress
st$Progress <- if_else(st$Station_zh %in% analyzed$Station_zh,
                       "Analyzed",
                       "Sampled")

# one station rename
size_raw$Station_zh <- if_else(size_raw$Station_zh == "加母子灣", "加母子", size_raw$Station_zh)

# attach station code
size_raw$Station <- st$Station[match(size_raw$Station_zh, st$Station_zh)]

size <- 
  size_raw %>% 
  arrange(Location, Station) %>% 
  relocate(Station_zh, .after = Station) %>% 
  relocate(C, .after = b) %>%
  relocate(WM, .after = Size) 

# save rdata
save(size, file = "data/2022_OCA_midterm_size.RData")
save(st, file = "data/2022_OCA_midterm_station.RData")
size_desc <- 
  data.frame(
    Variables = c("Section", "Condition", "Type", "L", "W", "C", "Size", "WM"),
    Description = c( "Core section (cm)", "Body condition (C = Complete; FH = fragmented with head intact; FT = fragmented with tail intact; F = fragmented)", "Biovolume calculation method (LWR = Length-width relationship')", "length (mm)", "Width (mm)", "Conversion factor", "mm^3", "mg wet weight"))

# write
write_xlsx(list(Macrofauna = size, Description = size_desc), "tab/2022_OCA_midterm_macro_size.xlsx")
write_xlsx(st, "tab/2022_OCA_midterm_station.xlsx")
