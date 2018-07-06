#==============================================================================
#==============================================================================
# Author: Zachary M. Smith
# Maintained: Zachary M. Smith
# Created: 07/11/2017
# Updated: 07/11/2017
# Purpose: Calculate fish metrics for NYSDEC.
#==============================================================================
#==============================================================================
# Load dplyr for data manipulation.
library(dplyr)
library(tidyr)
library(Benthos)
#------------------------------------------------------------------------------
# Main Directory
main.dir <- "data"
#------------------------------------------------------------------------------
# Import fish data.
fish.data <- file.path(main.dir, "all_fish_data.csv") %>% 
  read.csv(stringsAsFactors = FALSE)
# Change column names to uppercase.
names(fish.data) <- toupper(names(fish.data))
#------------------------------------------------------------------------------
# Import environmental data.
env.data <- file.path(main.dir, "historicalenvdata_final.csv") %>% 
  read.csv(stringsAsFactors = FALSE)
# Change column names to uppercase.
names(env.data) <- toupper(names(env.data))
#------------------------------------------------------------------------------
# Convert the fish data to a long data format.
fish.long <- fish.data[, c(1, 2, 11:ncol(fish.data))] %>% 
  tidyr::gather(SITE, COUNT, 3:ncol(.)) %>% 
  dplyr::select(SITE, FAMILY, SPECIES, COUNT) %>% 
  filter(COUNT > 0)
#------------------------------------------------------------------------------
# Extract the taxa attributes from the original fish data.
att.df <- fish.data[, 1:10]
#------------------------------------------------------------------------------
# Create a new data frame to store metrics.
metrics.df <- fish.long %>% 
  select(SITE) %>% 
  distinct()
#------------------------------------------------------------------------------
metrics.df <- metrics.df %>% 
  mutate(richness = Benthos::taxon_richness(fish.long, "all"))
# Richness/Diversity Metrics
metrics.df$RICHNESS <- vegan::specnumber(fish.wide[, 2:ncol(fish.wide)])
metrics.df2 <- merge(env.data[, c("SITE", "WIDTH")], metrics.df, by = "SITE")
metrics.df$WEIGHTED_RICHNESS <- ifelse(metrics.df2$WIDTH <= 4, metrics.df2$RICHNESS + 2, 
                                       ifelse(metrics.df2$WIDTH > 4 & metrics.df2$WIDTH <= 9, metrics.df2$RICHNESS , 
                                              ifelse(metrics.df2$WIDTH >9 & metrics.df2$WIDTH <= 19, metrics.df2$RICHNESS - 2,
                                                     ifelse(metrics.df2$WIDTH > 19, metrics.df2$RICHNESS - 4, NA))))
metrics.df$ABUNDANCE <- apply(fish.wide[, 2:ncol(fish.wide)], 1, sum)
metrics.df$SHANNON <- vegan::diversity(fish.wide[, 2:ncol(fish.wide)], "shannon")
metrics.df$SIMPSONS <- vegan::diversity(fish.wide[, 2:ncol(fish.wide)], "simpson")
#------------------------------------------------------------------------------
# Tolerance Metrics
metrics.df$PCT_INTOLERANT <- pct_attribute(fish.wide, master, "TOLERANCE", "I", "SPECIES")
metrics.df$RICH_INTOLERANT <- rich_attribute(fish.wide, master, "TOLERANCE", "I", "SPECIES")
metrics.df$PCT_FACULTATIVE <- pct_attribute(fish.wide, master, "TOLERANCE", "M", "SPECIES")
metrics.df$RICH_FACULTATIVE <- rich_attribute(fish.wide, master, "TOLERANCE", "M", "SPECIES")
metrics.df$PCT_TOLERANT <- pct_attribute(fish.wide, master, "TOLERANCE", "T", "SPECIES")
metrics.df$RICH_TOLERANT <- rich_attribute(fish.wide, master, "TOLERANCE", "T", "SPECIES")

metrics.df$PCT_DOM1 <- pct_dom(fish.wide, 1)
metrics.df$PCT_DOM2 <- pct_dom(fish.wide, 2)
metrics.df$PCT_DOM3 <- pct_dom(fish.wide, 3)
metrics.df$PCT_DOM4 <- pct_dom(fish.wide, 4)
metrics.df$PCT_DOM5 <- pct_dom(fish.wide, 5)
#------------------------------------------------------------------------------
# Trophic Metrics
metrics.df$PCT_BENTHIC_HERB <- pct_attribute(fish.wide, master, "TROPHIC_CLASS","BH", "SPECIES")
metrics.df$RICH_BENTHIC_HERB <- rich_attribute(fish.wide, master, "TROPHIC_CLASS","BH", "SPECIES")
metrics.df$PCT_BENTHIC_INSECT <- pct_attribute(fish.wide, master, "TROPHIC_CLASS", "BI", "SPECIES")
metrics.df$RICH_BENTHIC_INSECT <- rich_attribute(fish.wide, master, "TROPHIC_CLASS", "BI", "SPECIES")
metrics.df$PCT_GENERALIST <- pct_attribute(fish.wide, master, "TROPHIC_CLASS", "GF", "SPECIES")
metrics.df$RICH_GENERALIST <- rich_attribute(fish.wide, master, "TROPHIC_CLASS", "GF", "SPECIES")
metrics.df$PCT_NF <- pct_attribute(fish.wide, master, "TROPHIC_CLASS", "NF", "SPECIES")
metrics.df$RICH_NF <- rich_attribute(fish.wide, master, "TROPHIC_CLASS", "NF", "SPECIES")
metrics.df$PCT_PF <- pct_attribute(fish.wide, master, "TROPHIC_CLASS", "PF", "SPECIES")
metrics.df$RICH_PF <- rich_attribute(fish.wide, master, "TROPHIC_CLASS", "PF", "SPECIES")
metrics.df$PCT_PLANKTIVOROUS <- pct_attribute(fish.wide, master, "TROPHIC_CLASS", "PI", "SPECIES")
metrics.df$RICH_PLANKTIVOROUS <- rich_attribute(fish.wide, master, "TROPHIC_CLASS", "PI", "SPECIES")
metrics.df$PCT_TOP_CARNIVOR <- pct_attribute(fish.wide, master, "TROPHIC_CLASS", "TC", "SPECIES")
metrics.df$RICH_TOP_CARNIVOR <- rich_attribute(fish.wide, master, "TROPHIC_CLASS", "TC", "SPECIES")
metrics.df$PCT_WC_INSECT <- pct_attribute(fish.wide, master, "TROPHIC_CLASS", "WC", "SPECIES")
metrics.df$RICH_WC_INSECT <- rich_attribute(fish.wide, master, "TROPHIC_CLASS", "WC", "SPECIES")

apply(metrics.df[, c("PCT_NF", "PCT_PF", "PCT_BENTHIC_HERB", "PCT_BENTHIC_INSECT",
                     "PCT_GENERALIST", "PCT_PLANKTIVOROUS", "PCT_TOP_CARNIVOR", "PCT_WC_INSECT")], 1, sum)

metrics.df$PCT_INSECTIVOURS <- apply(metrics.df[, c("PCT_BENTHIC_INSECT", "PCT_WC_INSECT",
                                                    "PCT_PLANKTIVOROUS")], 1, sum)
metrics.df$RICH_INSECTIVOURS <- apply(metrics.df[, c("RICH_BENTHIC_INSECT", "RICH_WC_INSECT",
                                                     "RICH_PLANKTIVOROUS")], 1, sum)
#------------------------------------------------------------------------------
# Habit Metrics
metrics.df$PCT_COLD_WATER <- pct_attribute(fish.wide, master, "WATER_TYPE", "C", "SPECIES")
metrics.df$RICH_COLD_WATER <- rich_attribute(fish.wide, master, "WATER_TYPE", "C", "SPECIES")
metrics.df$PCT_FAC_WATER <- pct_attribute(fish.wide, master, "WATER_TYPE", "F", "SPECIES")
metrics.df$RICH_FAC_WATER <- rich_attribute(fish.wide, master, "WATER_TYPE", "F", "SPECIES")
metrics.df$PCT_WARM_WATER <- pct_attribute(fish.wide, master, "WATER_TYPE", "W", "SPECIES")
metrics.df$RICH_WARM_WATER <- rich_attribute(fish.wide, master, "WATER_TYPE", "W", "SPECIES")

apply(metrics.df[, c("PCT_COLD_WATER", "PCT_FAC_WATER", "PCT_WARM_WATER")], 1, sum)
#------------------------------------------------------------------------------
# Native and Non-Native
metrics.df$PCT_NON_NATIVE <- pct_attribute(fish.wide, master, "NATIVE", "YES", "SPECIES")
metrics.df$RICH_NON_NATIVE <- rich_attribute(fish.wide, master, "NATIVE", "YES", "SPECIES")

metrics.df$PCT_NON_NATIVE + pct_attribute(fish.wide, master, "NATIVE", "NO", "SPECIES")
#------------------------------------------------------------------------------
pct_taxa <- seq_taxa(fish.long, master, job = "PCT")
metrics.df <- merge(metrics.df, pct_taxa, by = "SITE", all.x = T)
#------------------------------------------------------------------------------
num_taxa <- seq_taxa(fish.long, master, job = "NUM")
metrics.df <- merge(metrics.df, num_taxa, by = "SITE", all.x = T)
#------------------------------------------------------------------------------
metrics.df$PCT_DARTER <- apply(metrics.df[, grepl("PCT", names(metrics.df)) &
                                            grepl("DARTER", names(metrics.df))], 1, sum)
metrics.df$NUM_DARTER <- apply(metrics.df[, grepl("NUM", names(metrics.df)) &
                                            grepl("DARTER", names(metrics.df))], 1, sum)
metrics.df$PCT_SUNFISH <- apply(metrics.df[, grepl("PCT", names(metrics.df)) &
                                             grepl("SUNFISH", names(metrics.df))], 1, sum)
metrics.df$NUM_SUNFISH <- apply(metrics.df[, grepl("NUM", names(metrics.df)) &
                                             grepl("SUNFISH", names(metrics.df))], 1, sum)
metrics.df$PCT_SCULPIN <- apply(metrics.df[, grepl("PCT", names(metrics.df)) &
                                             grepl("SCULPIN", names(metrics.df))], 1, sum)
metrics.df$NUM_SCULPIN <- apply(metrics.df[, grepl("NUM", names(metrics.df)) &
                                             grepl("SCULPIN", names(metrics.df))], 1, sum)
metrics.df$PCT_TROUT <- apply(metrics.df[, grepl("PCT", names(metrics.df)) &
                                           grepl("TROUT", names(metrics.df))], 1, sum) - metrics.df$PCT_TROUT.PERCH
metrics.df$NUM_TROUT <- apply(metrics.df[, grepl("NUM", names(metrics.df)) &
                                           grepl("TROUT", names(metrics.df))], 1, sum) - metrics.df$NUM_TROUT.PERCH
metrics.df$PCT_DACE <- apply(metrics.df[, grepl("PCT", names(metrics.df)) &
                                          grepl("DACE", names(metrics.df))], 1, sum)
metrics.df$NUM_DACE <- apply(metrics.df[, grepl("NUM", names(metrics.df)) &
                                          grepl("DACE", names(metrics.df))], 1, sum)
metrics.df$PCT_LAMPREY <- apply(metrics.df[, grepl("PCT", names(metrics.df)) &
                                             grepl("LAMPREY", names(metrics.df))], 1, sum)
metrics.df$NUM_LAMPREY <- apply(metrics.df[, grepl("NUM", names(metrics.df)) &
                                             grepl("LAMPREY", names(metrics.df))], 1, sum)
metrics.df$PCT_SUNFISH_TROUT <- apply(metrics.df[, c("PCT_SUNFISH", "PCT_TROUT")], 1, sum)
metrics.df$NUM_SUNFISH_TROUT <- apply(metrics.df[, c("NUM_SUNFISH", "NUM_TROUT")], 1, sum)
metrics.df$PCT_DARTER_SCULPIN <- apply(metrics.df[, c("PCT_DARTER", "PCT_SCULPIN")], 1, sum)
metrics.df$NUM_DARTER_SCULPIN <- apply(metrics.df[, c("NUM_DARTER", "NUM_SCULPIN")], 1, sum)
#------------------------------------------------------------------------------
metrics.final <- merge(env.data[, c("SITE", "CATEGORY", "IBI_CLASS")], metrics.df, by = "SITE", all.y = T)
write.csv(metrics.final, "fish_metrics_11_02_16.csv")
ref.all <- metrics.final[metrics.final$CATEGORY %in% "REF", ]
deg.all <- metrics.final[metrics.final$CATEGORY %in% "TEST", ]
barbour.all <- barbour(metrics.final, ref.all, deg.all)
write.csv(barbour.all, "all_barbour_11_02_16.csv")
plot_me(metrics.final, "all", "11_02_16")
#------------------------------------------------------------------------------
metrics.miss <- metrics.final[metrics.final$IBI_CLASS %in% "Mississippi", ]
ref.miss <- metrics.miss[metrics.miss$CATEGORY %in% "REF", ]
deg.miss <- metrics.miss[metrics.miss$CATEGORY %in% "TEST", ]
barbour.miss <- barbour(metrics.miss, ref.miss, deg.miss)
write.csv(barbour.miss, "mississippi_barbour_11_02_16.csv")
plot_me(metrics.miss, "mississippi", "11_02_16")
#------------------------------------------------------------------------------
metrics.mid <- metrics.final[metrics.final$IBI_CLASS %in% "Mid-Order", ]
ref.mid <- metrics.mid[metrics.mid$CATEGORY %in% "REF", ]
deg.mid <- metrics.mid[metrics.mid$CATEGORY %in% "TEST", ]
barbour.mid <- barbour(metrics.mid, ref.mid, deg.mid)
write.csv(barbour.mid, "mid_order_barbour_11_02_16.csv")
plot_me(metrics.mid, "mid_order", "11_02_16")
#------------------------------------------------------------------------------
metrics.head <- metrics.final[metrics.final$IBI_CLASS %in% "Headwater", ]
ref.head <- metrics.head[metrics.head$CATEGORY %in% "REF", ]
deg.head <- metrics.head[metrics.head$CATEGORY %in% "TEST", ]
barbour.head <- barbour(metrics.head, ref.head, deg.head)
write.csv(barbour.head, "head_barbour_11_02_16.csv")
plot_me(metrics.head, "headwater", "11_02_16")
#------------------------------------------------------------------------------


