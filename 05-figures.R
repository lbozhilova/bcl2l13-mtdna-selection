#############################
##### Figure generation #####
#############################

# Last edited: 31/08/22 by LVB

# Description: Figure generation.

#----- Packages
require("tidyverse")
require("ggbeeswarm")

#----- Code
source("00-plot-setup.R")

#----- Data
load("data/parsed/01-parsed-data.RData")
load("data/parsed/03-shift-test-results.RData")
load("data/parsed/04-sample-size-results.RData")

#----- Figure 1
# Fig 1A: Schematic
# Fig 1B: Western blots

#----- Figure 2
# Mother-to-pup heteroplasmy and highest observed values. 
# TODO: A rather hacky solution... fix.
pup_cts <- split(df$Shift, df$Genotype_mother) %>% lengths
pup_cts
gt_labs2 <- c(bquote(italic("Parkin"^"+/+")~"(122 pups)"), bquote(italic("Bcl2l13"^"+/+")~"(152 pups)"),
             bquote(italic("Parkin"^"-/-")~"(159 pups)"), bquote(italic("Bcl2l13"^"-/-")~"(236 pups)"),
             bquote(italic("Ulk1"^"-/-")~"(148 pups)"), bquote(italic("Ulk2"^"-/-")~"(142 pups)"))
gt_lblr2 <- function (variable, value) {
  return(gt_labs2[value])
}
p <- ggplot(filter(df, is_ko), aes(x = 100 * Het_mothers, y = 100 * Frequency, colour = Genotype_mother)) +
  theme_lvb + theme(legend.position = "none") +
  facet_wrap(~Genotype_mother, labeller = gt_lblr2) +
  geom_point(data = filter(df, !is_ko)[, -10], colour = "grey40", alpha = .25, size = 1.5) +
  geom_point(alpha = .25, size = 1.5) +
  geom_abline(intercept = 0, slope = 1, linetype = "dotted", colour = "grey20") +
  geom_hline(aes(yintercept = max(100 * Frequency), colour = Genotype_mother), linetype = "dashed") +
  geom_hline(yintercept = 86, colour = "grey20", linetype = "dashed") +
  scale_x_continuous("Mother heteroplasmy (%)", limits = c(0, 100), breaks = seq(0, 100, 20)) +
  scale_y_continuous("Pup heteroplasmy (%)", limits = c(0, 100), breaks = seq(0, 100, 20)) +
  scale_colour_manual(values = gt_cols)
p
plot_save(p, "figures/05-fig2.jpg", ar = 4/3, size = 1)

#----- Figure 3
# 3A Beeswarms of mother heteroplasmy 
mdf <- df[!duplicated(df$ID_mother), ]
a <- ggplot(mdf, aes(x = Genotype_mother, y = 100 * Het_mothers, colour = Genotype_mother)) +
  theme_lvb + theme(legend.position = "none") +
  geom_beeswarm() +
  geom_vline(xintercept = 2.5, colour = "grey20") +
  scale_x_discrete("", labels = gt_labs) +
  scale_y_continuous("Mother\nheteroplasmy (%)", limits = c(0, 100), breaks = seq(0, 100, 20)) +
  scale_colour_manual(values = gt_cols)
a

# 3B Violin plots of offspring heteroplasmy
b <- ggplot(df, aes(x = Genotype_mother, y = 100 * Frequency, fill = Genotype_mother)) +
  theme_lvb + theme(legend.position = "none") +
  geom_violin() +
  geom_boxplot(width = .2) +
  geom_vline(xintercept = 2.5, colour = "grey20") +
  scale_x_discrete("", labels = gt_labs) +
  scale_y_continuous("Pup\nheteroplasmy (%)", limits = c(0, 100), breaks = seq(0, 100, 20)) +
  scale_fill_manual(values = gt_cols)
b

# 3C Violin plots of heteroplasmy shift
c <- ggplot(df, aes(x = Genotype_mother, y = Shift, fill = Genotype_mother)) +
  theme_lvb + theme(legend.position = "none") +
  geom_violin() +
  geom_boxplot(width = .2) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = 2.5, colour = "grey20") +
  scale_x_discrete("", labels = gt_labs) +
  scale_y_continuous("Heteroplasmy\nshift", limits = c(-1.25, 1.25), breaks = seq(-1, 1, .5)) +
  scale_fill_manual(values = gt_cols)
c

p <- plot_arrange(a, b, c, ncol = 1)
p
plot_save(p, "figures/05-fig3.jpg",  ar = 1.1)

#----- Figure 4
# 4A P-values from pseudo-controls
a <- ggplot(test_df, aes(x = p_gr, fill = Genotype_mother)) +
  theme_lvb + theme(legend.position = "none",
                    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  facet_wrap(~Genotype_mother, labeller = gt_lblr) +
  geom_bar() +
  scale_x_discrete("", labels = c("p < 0.01", "p < 0.05", "p \u2265 0.05")) +
  scale_y_continuous("Count") +
  scale_fill_manual(values = gt_cols)
a

# 4B
b <- ggplot(sample_df, aes(x = sample_size, y = freq / 500, colour = psig, shape = psig)) +
  theme_lvb + 
  geom_point() +
  geom_line() +
  scale_x_continuous("Sample size", limits = c(50, 235), breaks = c(seq(50, 200, 50), 235)) +
  scale_y_continuous("Frequency", limits = c(0, 1), breaks =  seq(0, 1, .1)) +
  scale_colour_manual(values = psig_cols)
b 

p <- plot_arrange(a, b, ncol = 2)
p
plot_save(p, file = "figures/05-fig4.jpg", ar = 2)
