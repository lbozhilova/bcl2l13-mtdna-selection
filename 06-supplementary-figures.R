#################################
##### Supplementary figures #####
#################################

# Last edited: 04/11/22 by LVB

# Description: Figure generation for SI, by reviewer request

#----- Packages
require("tidyverse")
require("ggbeeswarm")

#----- Code
source("00-plot-setup.R")

#----- Data
load("data/parsed/01-parsed-data.RData")

tab <- read_csv("data/parsed/01-table.csv")
tab$Genotype_mother <- factor(tab$Genotype_mother, levels = levels(df$Genotype_mother))

#----- Figure S1: Shift by mother
s1 <- ggplot(df, aes(x = ID_mother, y = Shift, fill = Genotype_mother)) +
  theme_lvb + theme(legend.position = "none") +
  facet_wrap(~Genotype_mother, labeller = gt_lblr, scales = "free") +
  geom_hline(data = tab, aes(yintercept = mean_shift, colour = Genotype_mother)) +
  geom_hline(data = tab, aes(yintercept = mean_shift + 1.5 * sd_shift, colour = Genotype_mother), linetype = "dotted") +
  geom_hline(data = tab, aes(yintercept = mean_shift - 1.5 * sd_shift, colour = Genotype_mother), linetype = "dotted") +
  geom_violin() +
  geom_boxplot(width = .2) +
  scale_x_discrete("", labels = NULL) +
  scale_y_continuous("Heteroplasmy\nshift", limits = c(-1.25, 1.25), breaks = seq(-1, 1, .5)) +
  scale_fill_manual(values = gt_cols) +
  scale_colour_manual(values = gt_cols)
plot_save(s1, "figures/06-figs1.jpg", ar = 3/2)

#----- Figure S2: Normalised variance
vartab <- df %>% 
  split(df$ID_mother) %>% 
  lapply(function(id_df) data.frame(
    Genotype_mother = id_df$Genotype_mother[1],
    ID_mother = id_df$ID_mother[1],
    no_pups = nrow(id_df),
    mean_het = mean(id_df$Frequency),
    norm_var = var(id_df$Frequency) / (mean(id_df$Frequency) * (1 - mean(id_df$Frequency))))) %>% 
  bind_rows

s2 <- ggplot(vartab, aes(x = Genotype_mother, y = norm_var, colour = Genotype_mother)) +
  theme_lvb + theme(legend.position = "none") +
  geom_beeswarm() +
  geom_vline(xintercept = 2.5, colour = "grey20") +
  scale_x_discrete("", labels = gt_labs) +
  scale_y_continuous("Normalised heteroplasmy\nvariance", limits = c(0, 0.0425)) +
  scale_colour_manual(values = gt_cols)
plot_save(s2, "figures/06-figs2.jpg", ar = 3/1.1)
