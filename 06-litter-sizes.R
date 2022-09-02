##################################################
##### Are litter sizes different by genotype #####
##################################################

# Last edited: 01/08/22 by LVB

# Description: Do we see a genotype effect on litter sizes?

#----- Packages
require("tidyverse")

#----- Code
# source("00-plot-setup.R")

#----- Data
load("data/parsed/01-parsed-data.RData")

# Get litters
litter_lt <- split(df, list(df$ID_mother, df$DoB), drop = T)

# For every litter, get the litter size, the genotype, and the heteroplasmy of
# the mother.
lit_df <- lapply(litter_lt,
                 function(df) data.frame(litter_size = nrow(df),
                                         Genotype_mother = df$Genotype_mother[1],
                                         Het_mothers = df$Het_mothers[1],
                                         mean_shift = mean(df$Shift),
                                         is_ko = df$is_ko[1])) %>% 
  bind_rows()

ggplot(lit_df, aes(x = factor(litter_size))) +
  facet_wrap(~Genotype_mother) +
  geom_bar()

ggplot(lit_df, aes(x = Genotype_mother, y = litter_size)) +
  geom_violin() +geom_boxplot(width = .1)

ggplot(lit_df, aes(x = Het_mothers, y = litter_size)) +
  facet_wrap(~Genotype_mother) +
  geom_point() +
  geom_smooth()

split(lit_df$litter_size, lit_df$Genotype_mother) %>% 
  sapply(summary)
