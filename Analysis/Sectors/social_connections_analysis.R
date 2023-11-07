library(tidyverse)

# read and write data from/to xlsx file
#library("readxl")
#library("writexl")


# load data
collectors_sectors <- read_csv("/Users/svanginhoven/Dropbox/Mac/Documents/GPI projects/american_collectors/social_connections/data/Collectors_and_Subsectors.csv", col_names = TRUE, col_types = cols(.default = "c"))
collectors_companies <- read_csv("/Users/svanginhoven/Dropbox/Mac/Documents/GPI projects/american_collectors/social_connections/data/Companies.csv", col_names = TRUE, col_types = cols(.default = "c"))
gephi_list <- read_csv("/Users/svanginhoven/Dropbox/Mac/Documents/GPI projects/american_collectors/social_connections/data/Gephi_Unique.csv", col_names = TRUE, col_types = cols(.default = "c")) 

# nodes
gephi_nodes1 <- gephi_list %>%
  select(label = Name,
         ulan = ULAN_ID) %>%
  distinct(label, ulan) %>%
  mutate(role = "collector")
gephi_nodes2 <- gephi_list %>%
  select(label = Sector) %>%
  distinct(label) %>%
  mutate(role = ifelse(str_detect(label, "^Sector"), "sector", "group"))

gephi_nodes <- bind_rows(gephi_nodes1, gephi_nodes2)
gephi_nodes <- gephi_nodes %>%
  mutate(id = row_number())

# edges
gephi_edges <- gephi_list %>%
  select(source_label = Name,
         target_label = Sector)

gephi_edges <- gephi_edges %>%
  left_join(gephi_nodes, by = c("source_label" = "label"))
gephi_edges <- gephi_edges %>%
  mutate(source = id,
         source_role = role) %>%
  select(-id,
         -role)
gephi_edges <- gephi_edges %>%
  left_join(gephi_nodes, by = c("target_label" = "label"))
gephi_edges <- gephi_edges %>%
  mutate(target = id,
         target_role = role) %>%
  select(-id,
         -role)

write_csv(gephi_nodes, "/Users/svanginhoven/Dropbox/Mac/Documents/GPI projects/american_collectors/social_connections/data/gephi_nodes.csv")
write_csv(gephi_edges, "/Users/svanginhoven/Dropbox/Mac/Documents/GPI projects/american_collectors/social_connections/data/gephi_edges.csv")

# edges table version 2

source_sector <- collectors_companies %>%
  select(source = Name,
         Sector) %>%
  distinct(source, Sector)
target_sector <- collectors_companies %>%
  select(target = Name,
         Sector) %>%
  distinct(target, Sector)

gephi_edges_sector <- source_sector %>%
  left_join(target_sector, by = "Sector")
gephi_edges_sector <- gephi_edges_sector %>%
  filter(source != target)


# money trust to nodes
money_trust <- gephi_list %>%
  filter(Sector == "Money Trust Investigation") %>%
  select(label = Name,
         trust_investigation = Sector)
gephi_nodes <- gephi_nodes %>%
  left_join(money_trust, by = "label")

# leave only collectors and social clubs
gephi_nodes_new <- gephi_nodes %>%
  filter(role == "collector")

# treat clubs as nodes?
gephi_edges_clubs <- gephi_list %>%
  filter((Sector == "Lotos Club") | (Sector == "Union League Club")) %>%
  select(source = Name,
         target = Sector) %>%
  mutate(Sector = "social club")

gephi_edges_new <- gephi_edges_sector

# bring ids to edges
gephi_edges_new <- gephi_edges_new %>%
  select(source_label = source,
         target_label = target,
         sector = Sector)
gephi_edges_new <- gephi_edges_new %>%
  left_join(gephi_nodes_new, by = c("source_label" = "label"))
gephi_edges_new <- gephi_edges_new %>%
  mutate(source = id,
         source_role = role) %>%
  select(-id,
         -role)
gephi_edges_new <- gephi_edges_new %>%
  left_join(gephi_nodes_new, by = c("target_label" = "label"))
gephi_edges_new <- gephi_edges_new %>%
  mutate(target = id,
         target_role = role) %>%
  select(-id,
         -role)
gephi_edges_new <- gephi_edges_new %>%
  select(-trust_investigation.x,
         -trust_investigation.y,
         -ulan.x,
         -ulan.y)

write_csv(gephi_nodes_new, "/Users/svanginhoven/Dropbox/Mac/Documents/GPI projects/american_collectors/social_connections/data/gephi_nodes_new.csv")
write_csv(gephi_edges_new, "/Users/svanginhoven/Dropbox/Mac/Documents/GPI projects/american_collectors/social_connections/data/gephi_edges_new.csv")


# collector by sector with weight
collectors_sectors_rows <- read_csv("/Users/svanginhoven/Dropbox/Mac/Documents/GPI projects/american_collectors/social_connections/data/1912_Analysis_Companies.csv", col_names = TRUE, col_types = cols(.default = "c"))

gephi_edges_collectos_sectors_weights <- collectors_sectors_rows %>%
  group_by(Name, Sector) %>%
  summarize(weight = n(),
            .groups = "drop") %>%
  mutate(source_name = Name,
         target_name = Sector) %>%
  select(source_name, target_name, weight)

#write_csv(gephi_edges_collectos_sectors_weights, "/Users/svanginhoven/Dropbox/Mac/Documents/GPI projects/american_collectors/social_connections/data/gephi_edges_collectos_sectors_weights.csv")

nodes1 <- gephi_edges_collectos_sectors_weights %>%
  select(label = source_name) %>%
  distinct(label) %>%
  mutate(role = "collector")
nodes2 <- gephi_edges_collectos_sectors_weights %>%
  select(label = target_name) %>%
  distinct(label) %>%
  mutate(role = "sector")
nodes <- bind_rows(nodes1, nodes2)
nodes <- nodes %>%
  mutate(id = row_number())

gephi_edges <- gephi_edges_collectos_sectors_weights %>%
  left_join(select(nodes, label, id), by = c("source_name" = "label")) %>%
  mutate(source = id) %>%
  select(-id)
gephi_edges <- gephi_edges %>%
  left_join(select(nodes, label, id), by = c("target_name" = "label")) %>%
  mutate(target = id) %>%
  select(-id)

  