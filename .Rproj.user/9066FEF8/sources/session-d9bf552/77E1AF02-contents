library(pacman)
p_load(tidyverse, jsonlite)

data <-
  read_json("./data/oracle-cards.json",TRUE) %>% 
  type_line_parse()

data$legalities <- as.data.frame(data$legalities == "legal")


# Shop ----

shop <- 
  data %>% 
  legal_filter() %>% 
  not_land() %>% 
  slice_sample(n = 30)  %>% 
  name_clean()

shop %>% clean_display()

write.table(shop,file="./out/shop_names.txt", row.names = FALSE, col.names = FALSE, quote = FALSE)

# Lands ----

lands <- 
  data %>% 
  legal_filter() %>% 
  land() %>%
  slice_sample(n = 30)  %>% 
  name_clean()

lands %>% clean_display()

write.table(lands,file="./out/lands_names.txt", row.names = FALSE, col.names = FALSE, quote = FALSE)

# Packs ----

pack <- 
  data %>% 
  legal_filter() %>% 
  slice_pack(common = 10, uncommon = 3, rare = 1, random = 1, set = "5d293ad8-a749-4725-bd5c-c4e1db828bd0") %>% 
  name_clean()

pack %>% clean_display()

write.table(pack,file="./out/pack.txt", row.names = FALSE, col.names = FALSE, quote = FALSE)

# 100 pack ----
pack_100 <- 
  data %>% 
  legal_filter() %>% 
  slice_pack(common = 45, uncommon = 20, rare = 15, random = 20) %>% 
  name_clean()

pack_100 %>% clean_display()

write.table(pack_100,file="./out/pack_100.txt", row.names = FALSE, col.names = FALSE, quote = FALSE)

# Legendary Creature Pack ----

comm <- 
  data %>% 
  legal_filter() %>% 
  generic_find(supertypes,"Legendary") %>%  
  generic_find(types,"Creature") %>%  
  color_exactly(c("W","B")) %>% 
  slice_sample(n = 15)  %>% 
  name_clean()

comm %>% clean_display()

write.table(comm,file="./out/comm.txt", row.names = FALSE, col.names = FALSE, quote = FALSE)