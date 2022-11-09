#################################
##### Supplementary figures #####
#################################

# Last edited: 09/11/22 by LVB

# Description: Figure generation for SI, by reviewer request

#----- Packages
require("tidyverse")
require("ggbeeswarm")

#----- Code
source("00-plot-setup.R")

#----- Data
load("data/parsed/01-parsed-data.RData")
load("data/parsed/03-shift-test-results.RData")

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
s1
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
s2
plot_save(s2, "figures/06-figs2.jpg", ar = 3/1.1)

#----- Figure S3: NNT mutation difference
# S3A: NNT vs not NNT: heteroplasmy shift
nnt_df <- filter(df, Genotype_mother == "Ulk1:del/del") # 178
nnt_df <- rbind(nnt_df, nnt_df)
nnt_df$NNT_locus[1:178] <- "all"
nnt_df$NNT_locus <- factor(nnt_df$NNT_locus, levels = c("wt", "all", "mut"))
nnt_labs <- c("wt" = "WT", "all" = "all", "mut" = "NNT")

test_df <- filter(test_df, !(Genotype_mother %in% gt_nms)) 

s3a <- ggplot(nnt_df, aes(x = NNT_locus, y = Shift)) +
  theme_lvb + theme(legend.position = "none") +
  geom_violin(fill = gt_cols["Ulk1:del/del"]) +
  geom_boxplot(width = .2, fill = gt_cols["Ulk1:del/del"]) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  scale_x_discrete("", labels = nnt_labs) +
  scale_y_continuous("Heteroplasmy\nshift", limits = c(-1.25, 1.25), breaks = seq(-1, 1, .5))
s3a

s3b <- ggplot(test_df, aes(x = p_gr)) +
  theme_lvb + theme(legend.position = "none",
                    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  geom_bar(fill = gt_cols["Ulk1:del/del"]) +
  scale_x_discrete("", labels = c("p < 0.01", "p < 0.05", "p \u2265 0.05")) +
  scale_y_continuous("Count", limits = c(0, 210)) +
  scale_fill_manual(values = gt_cols)
s3b

s3 <- plot_arrange(s3a, s3b)
s3

plot_save(s3, "figures/06-figs3.jpg", ar = 2)
