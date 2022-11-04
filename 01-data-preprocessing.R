##############################
##### Data preprocessing #####
##############################

# Last edited: 04/11/22 by LVB

# Description: Data preprocessing and summary statistics.

#----- Packages
require("tidyverse")

#----- Code
source("00-plot-setup.R")

#----- Load data
mother_df <- read_table("data/raw/tRNAAla_mothers.txt")[, -7] # remove empty column

offspring_df <- read_table("data/raw/tRNAAla_offspring.txt")
offspring_df <- filter(offspring_df, !is.na(Linear)) # remove empty rows

pyroseq_df <- read_table("data/raw/tRNAAla_pyro.txt")[, -3] # remove empty column

df <- merge(offspring_df, mother_df, by = "Dam")
colnames(df)[c(5, 13)] <- c("DoB", "DoB_mother")
df <- merge(df, pyroseq_df, by = "Linear")

rm(mother_df, offspring_df, pyroseq_df); gc()

#----- Process data
# Heteroplasmy
# Note: If using different data, make sure to check:
# - are all heteroplasmy fractions recorded
# - are they between 0 and 100
# - are there any homoplasmies that need perturbing for the log-odds calculation.
df$Frequency <- .01 * df$Frequency
df$Het_mothers <- .01 * df$Het_mothers
df$Shift <- qlogis(df$Frequency) - qlogis(df$Het_mothers)

# Mother's genotype
df$Genotype_mother <- factor(df$Genotype_mother, levels = gt_nms)

# WT vs knockout
df$is_ko <- !(df$Genotype_mother %in% gt_nms[1:2])

#------ Summary statistics
# Table 1
tab <- df %>% 
  split(df$Genotype_mother) %>%
  lapply(function(gt_df) data.frame(
    Genotype_mother = gt_df$Genotype_mother[1],
    no_mothers = (gt_df$ID_mother %>% unique %>% length),
    no_pups = nrow(gt_df),
    mean_het = (mean(100 * gt_df$Frequency) %>% round(2)),
    sd_het = (sd(100 * gt_df$Frequency) %>% round(2)),
    mean_shift = (mean(gt_df$Shift) %>% round(2)),
    sd_shift = (sd(gt_df$Shift) %>% round(2)),
    mean_hdiff = (mean(100 * (gt_df$Frequency-gt_df$Het_mothers)) %>% round(2)),
    sd_hdiff = (sd(100 * (gt_df$Frequency-gt_df$Het_mothers))) %>% round(2))) %>% 
  bind_rows()
write_csv(tab, "data/parsed/01-table.csv")

# Mother heteroplasmy range
mother_h_range <- df$Het_mothers %>% range
mother_h_range

# Number of pups per mother
no_pups <- split(df$Linear, df$ID_mother) %>% sapply(length)
no_pups <- c("mean" = round(mean(no_pups), 2),
             "sd" = round(sd(no_pups), 2))

#----- Save
save(df, file = "data/parsed/01-parsed-data.RData")
save(tab, mother_h_range, no_pups, file = "data/parsed/01-summary-stats.RData")
