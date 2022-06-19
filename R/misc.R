st %>% 
  group_by(Progress) %>% 
  count


unique(size$Taxon)[(order(unique(size$Taxon)))]
rank_bio %>% View

ss %>% 
  group_by(Location, Variable) %>% 
  summarise(mean(Value)) %>% View
