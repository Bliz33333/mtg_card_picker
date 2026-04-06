library(pacman)
p_load(tidyverse, jsonlite, lubridate)

data <-
  read_json("./data/oracle-cards.json",TRUE) %>% 
  type_line_parse()

data$legalities <- as.data.frame(data$legalities == "legal")

set.seed(as.numeric(Sys.time()))


# Shop ----

shop <- 
  data %>% 
  legal_filter() %>% 
  generic_find(subtypes,"Attraction",not = TRUE) %>%
  filter(edhrec_rank < 27000) %>%
  not_land() %>% 
  slice_sample(n = 30)  %>% 
  name_clean()

shop %>% clean_display()

write.table(shop,file="./out/shop_names.txt", row.names = FALSE, col.names = FALSE, quote = FALSE)

# Lands ----

lands <- 
  data %>% 
  legal_filter() %>% 
  filter(edhrec_rank < 27000) %>%
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
  not_land() %>% 
  generic_find(subtypes,"Attraction",not = TRUE) %>% 
  color_at_most(c("B","R")) %>% 
  slice_pack(common = 45, uncommon = 20, rare = 15, random = 20) %>%
  # slice_pack(common = 2, uncommon = 2, rare = 0, random = 0) %>%
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

#----

shop_filt <- 
  data %>% 
  legal_filter() %>% 
  generic_find(subtypes,"Attraction",not = TRUE) %>%
  filter(edhrec_rank < 15000) %>%
  not_land() %>% 
  slice_pack(common = 10, uncommon = 9, rare = 9, mythic=2) %>%
  slice_sample(n = 30)  %>% 
  name_clean()

shop_filt %>% clean_display()

write.table(shop_filt,file="./out/shop_filt_names.txt", row.names = FALSE, col.names = FALSE, quote = FALSE)

# tab <- 
#   data %>% 
#   legal_filter() %>% 
#   # filter(edhrec_rank < 15000) %>% 
#   not_land() %>% 
#   select(rarity) %>% 
#   unlist() %>% 
#   table
# tab <- tab/sum(tab)
# tab[c(2,6,4,3)]
