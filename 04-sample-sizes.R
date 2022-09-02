################################
##### Sample size analysis #####
################################

# Last edited: 20/07/22 by LVB

# Description: Does the Bcl2l13:del/del group come up as significant because of
# its larger sample size?

#----- Packages
require("tidyverse")

#----- Code
source("00-plot-setup.R")
set.seed(200722) # set random seed to guarantee reproducibility

#----- Data
load("data/parsed/01-parsed-data.RData")

#----- Subsampling offspring
# Extract shift
bcl_shift <- filter(df, Genotype_mother == "Bcl2l13:del/del")$Shift
wt_shift <- filter(df, Genotype_mother %in% gt_nms[1:2])$Shift

# Calculate p-values
test_shift_n <- function(n) {
  bcl_sub <- sample(bcl_shift, n, replace = TRUE)
  wt_sub <- sample(wt_shift, n, replace = TRUE)
  ks.test(bcl_sub, wt_sub)$p.value
}
ns <- seq(50, 235, 5) %>% rep(each = 500)
ps <- sapply(ns, test_shift_n)
detailed_sample_df <- data.frame(sample_size = ns, pval = ps)

sample_df <- split(ps, ns) %>% 
  lapply(function(x) data.frame(psig = c("p < 0.01", "p < 0.05"),
                                freq = c(sum(x < 0.01), sum(x < 0.05)))) %>%
  bind_rows()
sample_df$sample_size <- seq(50, 235, 5) %>% rep(each = 2)         

# Check how often significance is detected at n = 120
minsize_frac <- (filter(detailed_sample_df, sample_size == 120)$pval <.05/6) %>% sum
minsize_frac # 408

#----- Save
save(sample_df, detailed_sample_df, minsize_frac, file = "data/parsed/04-sample-size-results.RData")
