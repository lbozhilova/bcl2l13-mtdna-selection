##########################################
##### Maximal tolerated heteroplasmy #####
##########################################

# Last edited: 20/07/22 by LVB

# Description: Analysing maximal tolerated heteroplasmy levels.

#----- Packages
require("tidyverse")

#----- Data
load("data/parsed/01-parsed-data.RData")

#----- Top 5 heteroplasmy values per genotype group
highest_obs <- df$Frequency %>% 
  split(df$Genotype_mother) %>% 
  sapply(function(x) sort(x, decreasing = TRUE)[1:5])

highest_obs # Aside from the 86% Bcl2l13 WT, nothing above 80%

#----- Fraction of pup heteroplasmies above 75%
high_df <- df %>% 
  split(df$Genotype_mother) %>% 
  lapply(function(gt_df) data.frame(Genotype_mother = gt_df$Genotype_mother[1],
                                    no_pups = nrow(gt_df),
                                    no_high = sum(gt_df$Frequency >= .75))) %>% 
  bind_rows()
high_df$prop = round(100 * high_df$no_high / high_df$no_pups, 2)

high_df # Comparable prop. of high-heteroplasmy pups

#------ Save
save(highest_obs, high_df, file = "data/parsed/02-high-summary.RData")
