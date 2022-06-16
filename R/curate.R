pacman::p_load(dplyr, writexl, readxl, GRSPRSThesisData)

# locate file
xlsx_path <- dir("xlsx", ".xlsx", full.names = TRUE)

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

raw_data %>% 
  select(Cruise, Station) %>%
  distinct() %>% 
  arrange(Cruise)
  

# set up data
# object naming regime: stack up all the "cruises"
East_Liuqiu_size <- 
  raw_data %>% 
  select(-C) %>% 
  assign_method() %>% 
  calculate_biovolume() %>% 
  define_ophiuroid_size(protocol_ophiuroid = "all_arms",
                        grouping_variables = colnames(raw_data)[1:6]) %>% 
  mutate(Station_zh = Station) #%>% 
  # mutate(Station = )

# read old file
Penghu_North_Taoyun_East_size <- read_xlsx(xlsx_path[4])

save(East_Liuqiu_size, Penghu_North_Taoyun_East_size, file = "data/2022_OCA_midterm_size.RData")

