library(pacman)
p_load(tidyverse, jsonlite, lubridate)

data <-
  read_json("./data/oracle-cards.json",TRUE) %>% 
  type_line_parse()

data$legalities <- as.data.frame(data$legalities == "legal")

#----
rank_cutoff <- 1750
   
pauper_viable <-
  data %>% 
  legal_filter() %>%
  mutate(log_cost = log(as.numeric(prices$usd))) %>% 
  filter(log_cost <= log(35/50)) %>% 
  filter(rarity == "common") %>% 
  # not_land() %>% 
  arrange(log_cost) %>% 
  filter(edhrec_rank < rank_cutoff) 
  # generic_find(subtypes,"Elf")

color_breakdown <- pauper_viable$color_identity %>% lapply(paste0, collapse = "") %>% unlist() %>% table()
rank_cutoff
color_breakdown
print("Golgari")
sum(color_breakdown[names(color_breakdown) %in% c("","G","B","BG")])
golg_cards <- 
  pauper_viable %>% 
  color_at_most(c("G","B")) %>% 
  select(name) %>% 
  name_clean()
golg_cards %>% clean_display()
print("Simic")
sum(color_breakdown[names(color_breakdown) %in% c("","G","U","GU")])
simic_cards <- 
  pauper_viable %>% 
  color_at_most(c("G","U")) %>% 
  select(name) %>% 
  name_clean()
simic_cards %>% clean_display()
print("Dimir")
sum(color_breakdown[names(color_breakdown) %in% c("","U","B","BU")])
dimir_cards <- 
  pauper_viable %>% 
  color_at_most(c("U","B")) %>% 
  select(name) %>% 
  name_clean()
dimir_cards %>% clean_display()
print("Boros")
sum(color_breakdown[names(color_breakdown) %in% c("","W","R","RW")])
boros_cards <- 
  pauper_viable %>% 
  color_at_most(c("R","W")) %>% 
  select(name) %>% 
  name_clean()
boros_cards %>% clean_display()


plot(pauper_viable$edhrec_rank, pauper_viable$log_cost)

test <- paste0(pauper_viable$color_identity[[2]], collapse = "")
pauper_viable$color_identity[[2]]