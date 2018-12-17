#==============================================================================
#==============================================================================
# Author: Zachary M. Smith
# Maintained: Zachary M. Smith
# Created: 07/01/2017
# Updated: 07/01/2017
# Purpose: Create master taxonomic attribute table for phytoplankton.
#==============================================================================
#==============================================================================
# Load dplyr for data manipulation.
library(dplyr)
#------------------------------------------------------------------------------
# Connect to ITIS postgreSQL database.
conn <- dplyr::src_postgres(dbname = "ITIS",
                            user = "guest",
                            password = "guest")
#------------------------------------------------------------------------------
src_tbls(conn)
#------------------------------------------------------------------------------
vern.df <- tbl(conn, "vernaculars") %>% 
  data.frame() %>% 
  mutate(tsn = as.character(tsn),
         vernacular_name = tolower(vernacular_name)) %>%
  filter(language == "English") %>% 
  select(tsn, vernacular_name) %>% 
  group_by(tsn) %>% 
  summarize(vernacular_name = paste(unlist(vernacular_name), collapse = "; "))
#------------------------------------------------------------------------------
hier.df <- tbl(conn, "hierarchy") %>% 
  data.frame()
#------------------------------------------------------------------------------
fish.list <- list()
fish.list["petromyzontiformes"] <- "-159696-"
fish.list["amiiformes"] <- "-161100-"
fish.list["anguilliformes"] <- "-161123-"
fish.list["clupeiformes"] <- "-161694-"
fish.list["osteoglossiformes"] <- "-161886-"
fish.list["percopsiformes"] <- "-164386-"
fish.list["gadiformes"] <- "-164665-"
fish.list["atheriniformes"] <- "-165429-"
fish.list["gasterosteiformes"] <- "-166361-"
fish.list["polypteriformes"] <- "-161053-"
fish.list["acipenseriformes"] <- "-161063-"
fish.list["lepisosteiformes"] <- "-650224-"
fish.list["cypriniformes"] <- "-162846-"
fish.list["siluriformes"] <- "-163992-"
fish.list["esociformes"] <- "-553131-"
fish.list["salmoniformes"] <- "-161929-"
fish.list["scorpaeniformes"] <- "-166702-"
fish.list["perciformes"] <- "-167640-"
fish.list["cyprinodontiformes"] <- "-553130-"
#------------------------------------------------------------------------------
fish.df <- hier.df %>% 
  filter(grepl(paste(fish.list, collapse = "|"), hierarchy_string))
#------------------------------------------------------------------------------
syn.df <- tbl(conn, "synonym_links") %>% 
  select(1:2) %>% 
  data.frame()
#------------------------------------------------------------------------------
tsn.vec <- strsplit(fish.df$hierarchy_string, "-") %>% 
  unlist() %>% 
  unique()
#------------------------------------------------------------------------------
kingdoms <- tbl(conn, "kingdoms") %>% 
  select(1:2) %>% 
  data.frame()
#------------------------------------------------------------------------------
taxa.units <- tbl(conn, "taxonomic_units") %>% 
  select(tsn, name_usage, kingdom_id, rank_id, complete_name) %>% 
#  filter(tsn %in% tsn.vec) %>% 
  distinct() %>% 
  data.frame() %>% 
  mutate(complete_name = trimws(complete_name),
         complete_name = gsub(" ", "_", complete_name))
#------------------------------------------------------------------------------  
taxa.types <- tbl(conn, "taxon_unit_types") %>% 
  select(kingdom_id, rank_id, rank_name) %>% 
  distinct() %>% 
  arrange(rank_id) %>% 
  data.frame() %>% 
  left_join(kingdoms, by = "kingdom_id")
#------------------------------------------------------------------------------
taxa.df <- left_join(taxa.units, taxa.types, by = c("kingdom_id", "rank_id")) %>% 
  select(-kingdom_id, -rank_id, -kingdom_name)
#------------------------------------------------------------------------------
split.list <- strsplit(fish.df$hierarchy_string, "-")

split.unlist <- unlist(split.list) %>% 
  unique() %>% 
  as.data.frame(stringsAsFactors = FALSE) %>% 
  rename(tsn = ".") %>% 
  mutate(tsn = as.integer(tsn))

taxa.df <- semi_join(taxa.df, split.unlist, by = "tsn")
x <- 1
taxa.list <- lapply(seq_along(split.list), function(x) {
  print(paste0(x, "/", length(split.list)))
  sub.df <- data.frame(tsn = as.integer(split.list[[x]]))
  join.df <- left_join(sub.df, taxa.df, by = "tsn") %>% 
    mutate(rank_name = trimws(rank_name))
    
  final.df <- taxa.types %>% 
    filter(kingdom_id == 5) %>% 
    mutate(rank_name = trimws(rank_name)) %>% 
    left_join(join.df, by = "rank_name") %>% 
    mutate(rank_name = factor(rank_name, levels = rank_name)) %>% 
    select(rank_name, complete_name) %>% 
    tidyr::spread(rank_name, complete_name) %>% 
    dplyr::mutate(tsn = join.df[nrow(join.df), "tsn"],
                  name_usage = join.df[nrow(join.df), "name_usage"],
                  rank_name = join.df[nrow(join.df), "rank_name"])
    
  return(final.df)
})

taxa.final <- bind_rows(taxa.list) %>% 
  distinct() %>% 
  mutate(tsn = as.character(tsn)) %>% 
  left_join(vern.df, by = "tsn")
getwd()
data.table::fwrite(taxa.final, "data/fish_heirarchy.csv")

