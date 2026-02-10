library(pacman)
p_load(tidyverse, jsonlite)

# data <- read_json("./data/oracle-cards.json")

data <-
  read_json("./data/oracle-cards.json",TRUE) %>% 
  type_line_parse()

data$legalities <- as.data.frame(data$legalities == "legal")


# Testing ----

data %>% 
  generic_find(subtypes, c("Chicago","Incarnation")) %>% 
  nrow()


# Shop ----

shop <- 
  data %>% 
  filter(legalities$vintage | legalities$timeless | legalities$commander | legalities$brawl | legalities$alchemy) %>% 
  generic_find(types,"Land",TRUE) %>%
  slice_sample(n = 30)  %>% 
  select(name) %>%
  mutate(name = str_remove_all(name,"\"")) %>%
  unlist() %>%
  str_split_i(" // ",1)

shop

write.table(shop,file="./out/shop_names.txt", row.names = FALSE, col.names = FALSE, quote = FALSE)

# Lands ----

lands <- 
  data %>% 
  filter(legalities$vintage | legalities$timeless | legalities$commander | legalities$brawl | legalities$alchemy) %>% 
  generic_find(types,"Land") %>%
  slice_sample(n = 30)  %>% 
  select(name) %>%
  mutate(name = str_remove_all(name,"\"")) %>%
  unlist() %>%
  str_split_i(" // ",1)

lands

write.table(lands,file="./out/lands_names.txt", row.names = FALSE, col.names = FALSE, quote = FALSE)

# Packs ----

common <-
  data_simpl %>% 
  filter(set_id == "5d293ad8-a749-4725-bd5c-c4e1db828bd0") %>% 
  filter(rarity == "common") %>% 
  slice_sample(n=10)

uncommon <-
  data_simpl %>% 
  filter(set_id == "5d293ad8-a749-4725-bd5c-c4e1db828bd0") %>% 
  filter(rarity == "uncommon") %>% 
  slice_sample(n=3)

rare <-
  data_simpl %>% 
  filter(set_id == "5d293ad8-a749-4725-bd5c-c4e1db828bd0") %>% 
  filter(rarity == "rare") %>% 
  slice_sample(n=1)

random <-
  data_simpl %>% 
  filter(set_id == "5d293ad8-a749-4725-bd5c-c4e1db828bd0") %>% 
  slice_sample(n=1)

pack <- rbind(common, uncommon, rare, random)

pack <-
  pack %>% 
  select(name) %>%
  mutate(name = str_remove_all(name,"\"")) %>%
  unlist() %>%
  str_split_i(" // ",1)

pack

write.table(pack,file="./out/pack.txt", row.names = FALSE, col.names = FALSE, quote = FALSE)

# 100 pack ----
common <-
  data %>% 
  filter(rarity == "common") %>% 
  filter(legalities$vintage | legalities$timeless | legalities$commander | legalities$brawl | legalities$alchemy) %>% 
  generic_find(types,"Land",TRUE) %>% 
  filter(!sapply(.$color_identity, function(x) "W" %in% x)) %>%
  # filter(!sapply(.$color_identity, function(x) c("U") %in% x)) %>%
  filter(!sapply(.$color_identity, function(x) "B" %in% x)) %>%
  filter(!sapply(.$color_identity, function(x) "R" %in% x)) %>%
  # filter(!sapply(.$color_identity, function(x) "G" %in% x)) %>%
  slice_sample(n=45)
  
uncommon <-
  data %>% 
  filter(rarity == "uncommon") %>% 
  filter(legalities$vintage | legalities$timeless | legalities$commander | legalities$brawl | legalities$alchemy) %>% 
  generic_find(types,"Land",TRUE) %>% 
  filter(!sapply(.$color_identity, function(x) "W" %in% x)) %>%
  # filter(!sapply(.$color_identity, function(x) c("U") %in% x)) %>%
  filter(!sapply(.$color_identity, function(x) "B" %in% x)) %>%
  filter(!sapply(.$color_identity, function(x) "R" %in% x)) %>%
  # filter(!sapply(.$color_identity, function(x) "G" %in% x)) %>%
  slice_sample(n=20)

rare <-
  data %>% 
  filter(rarity == "rare") %>% 
  filter(legalities$vintage | legalities$timeless | legalities$commander | legalities$brawl | legalities$alchemy) %>% 
  generic_find(types,"Land",TRUE) %>% 
  filter(!sapply(.$color_identity, function(x) "W" %in% x)) %>%
  # filter(!sapply(.$color_identity, function(x) c("U") %in% x)) %>%
  filter(!sapply(.$color_identity, function(x) "B" %in% x)) %>%
  filter(!sapply(.$color_identity, function(x) "R" %in% x)) %>%
  # filter(!sapply(.$color_identity, function(x) "G" %in% x)) %>%
  slice_sample(n=15)

random <-
  data %>% 
  filter(legalities$vintage | legalities$timeless | legalities$commander | legalities$brawl | legalities$alchemy) %>% 
  generic_find(types,"Land",TRUE) %>% 
  filter(!sapply(.$color_identity, function(x) "W" %in% x)) %>%
  # filter(!sapply(.$color_identity, function(x) c("U") %in% x)) %>%
  filter(!sapply(.$color_identity, function(x) "B" %in% x)) %>%
  filter(!sapply(.$color_identity, function(x) "R" %in% x)) %>%
  # filter(!sapply(.$color_identity, function(x) "G" %in% x)) %>%
  slice_sample(n=20)

pack_100 <- rbind(common, uncommon, rare, random)

pack_100 <-
  pack_100 %>% 
  select(name) %>%
  mutate(name = str_remove_all(name,"\"")) %>%
  unlist() %>%
  str_split_i(" // ",1)

pack_100

write.table(pack_100,file="./out/pack_100.txt", row.names = FALSE, col.names = FALSE, quote = FALSE)

# Legendary Creature Pack ----
# comm <- 
#   data %>% 
#   filter(legalities$vintage | legalities$timeless | legalities$commander | legalities$brawl | legalities$alchemy) %>% 
#   generic_find(supertypes,"Legendary") %>%  
#   generic_find(types,"Creature") %>%  
#   filter(sapply(.$color_identity, function(x) "W" %in% x)) %>%
#   filter(!sapply(.$color_identity, function(x) c("U") %in% x)) %>%
#   filter(sapply(.$color_identity, function(x) "B" %in% x)) %>%
#   filter(!sapply(.$color_identity, function(x) "R" %in% x)) %>%
#   filter(!sapply(.$color_identity, function(x) "G" %in% x)) %>%
#   slice_sample(n = 15)  %>% 
#   select(name) %>%
#   mutate(name = str_remove_all(name,"\"")) %>%
#   unlist() %>%
#   str_split_i(" // ",1)

comm <- 
  data %>% 
  legal_filter() 
  generic_find(supertypes,"Legendary") %>%  
  generic_find(types,"Creature") %>%  
  color_exactly(c("W","B")) %>% 
  slice_sample(n = 15)  %>% 
  name_clean()

comm

write.table(comm,file="./out/comm.txt", row.names = FALSE, col.names = FALSE, quote = FALSE)