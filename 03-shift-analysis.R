#################################
##### Distribution analysis #####
#################################

# Last edited: 09/11/22 by LVB

# Description: Do autophagy gene knockouts have an effect on mother-to-pup
# heteroplasmy shift?

#----- Packages
require("tidyverse")

#----- Code
source("00-plot-setup.R")

#----- Data
load("data/parsed/01-parsed-data.RData")

#----- Compare Bcl2l13 del/del controls to the remaining groups
shifts <- split(df$Shift, df$Genotype_mother)
shifts$Ulk1_noNNT <- filter(df, Genotype_mother == "Ulk1:del/del" & NNT_locus == "wt")$Shift

bcl_pvals <- names(shifts)[-2] %>% 
  sapply(function(gt) ks.test(shifts[["Bcl2l13:wt/wt"]], shifts[[gt]])$p.value) %>% 
  round(3)
bcl_pvals

# Bonferroni correction for significance
(bcl_pvals < .05 / 5) %>% which %>% names # Bcl2l13:del/del & Ulk1:del/del

#----- Do Wilcoxon test for shift being zero
w.zero.pvals <- shifts %>% sapply(function(x) wilcox.test(x)$p.value) 
w.zero.pvals < .05/6
w.zero.pvals %>% round(3)

#----- Bootstrap with pseudo-controls
# Generate pseudocontrols
ctr_mother_ids <- filter(df, Genotype_mother %in% gt_nms[1:2])$ID_mother %>% unique
ctr_shift <- ctr_mother_ids %>% 
  combn(6) %>% 
  apply(2, function(mots) filter(df, ID_mother %in% mots)$Shift)

# Test each of the genotype groups against each of the 210 pseudo-controls
test_shift <- function(gt){
  shift <- shifts[[gt]]
  data.frame(Genotype_mother = gt,
             ctr_idx = 1:210,
             pval = sapply(ctr_shift, function(ctr) ks.test(ctr, shift)$p.value))
}
test_df <- names(shifts) %>% 
  lapply(test_shift) %>% 
  bind_rows()
test_df$Genotype_mother <- factor(test_df$Genotype_mother, levels = c(gt_nms, "Ulk1_noNNT"))

# Bonferroni correction for bootstrap tests
psig <- .05 / 6
test_df$is_sig <- test_df$pval < psig

no_sig <- split(test_df$is_sig, test_df$Genotype_mother) %>% sapply(sum)
no_sig
(100 * no_sig / 210) %>% round(2)

# Median p-values
median_pvals <- split(test_df$pval, test_df$Genotype_mother) %>% 
  sapply(median) %>% 
  round(3)
median_pvals
(median_pvals < psig) %>% which %>% names # "Bcl2l13:del/del" "Ulk1:del/del" "Ulk1_noNNT" 

# Discretise p-values to < 0.01, < 0.05
test_df$p_gr <- case_when(test_df$pval < 0.01 ~ "p < 0.01", 
                          test_df$pval < 0.05 ~ "p < 0.05", 
                          TRUE ~ "p > 0.05")
test_df$p_gr <- factor(test_df$p_gr, levels = c("p < 0.01", 
                                                "p < 0.05", 
                                                "p > 0.05"))

#----- Save
save(test_df, bcl_pvals, median_pvals, no_sig, psig, file = "data/parsed/03-shift-test-results.RData")
