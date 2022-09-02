######################
##### Plot setup #####
######################

# Last edited: 25/08/22 by LVB

# Description: Theme, custom colours and labels, and plot saving functions for
# generating the figures in the paper.

#----- Packages
require("tidyverse")
require("MetBrewer")
require("cowplot")

#----- Themes
theme_lvb <- theme_minimal(base_size = 11) +
  theme(
    text = element_text(color = "gray20"),
    # Legend
    legend.position = "top",
    legend.direction = "horizontal",
    legend.justification = 0.1,
    legend.title = element_blank(),
    # Axes
    axis.text = element_text(face = "italic"),
    axis.title.x = element_text(vjust = -1),        
    axis.title.y = element_text(vjust = 2),
    axis.ticks.x = element_line(color = "gray70", size = 0.2),
    axis.ticks.y = element_line(color = "gray70", size = 0.2),
    axis.line = element_line(color = "gray40", size = 0.3),
    axis.line.y = element_line(color = "gray40", size = 0.3),
    # Panel
    panel.grid.major = element_line(color = "gray70", size = 0.2),
    panel.grid.major.x = element_line(color = "gray70", size = 0.2),
    panel.border = element_rect(color = "gray70", fill = NA, size = 0.5),
    # Facet
    strip.text.x = element_text(face = "italic"))

#----- Colours and labels
# Genotype
gt_nms <- c("Parkin:wt/wt", "Bcl2l13:wt/wt", 
            "Parkin:del/del","Bcl2l13:del/del",
            "Ulk1:del/del", "Ulk2:del/del") 
gt_cols <- met.brewer("Archambault", 6)[c(3, 6, 4, 5, 1, 2)]
names(gt_cols) <- gt_nms

gt_labs <- c(bquote(italic("Parkin"^"+/+")), bquote(italic("Bcl2l13"^"+/+")),
             bquote(italic("Parkin"^"-/-")), bquote(italic("Bcl2l13"^"-/-")),
             bquote(italic("Ulk1"^"-/-")), bquote(italic("Ulk2"^"-/-")))
names(gt_labs) <- gt_nms

gt_lblr <- function (variable, value) {
  return(gt_labs[value])
}

# P-values
psig_nms <- c("p < 0.01", "p < 0.05")

psig_cols <- met.brewer("Demuth", 2)
names(psig_cols) <- psig_nms

#----- Save figures
plot_arrange <- function(...) plot_grid(..., label_size = 11, labels = "AUTO")

plot_save <- function(p, filename, size = 1, ar = 1, dev = "jpeg"){
  allowed_devs <- c("eps", "ps", "tex", "pdf", "jpeg", 
                    "tiff", "png", "bmp", "svg")
  if (!(dev %in% allowed_devs))
    stop("Invalid device.")
  if (dev != "jpeg" & !str_detect(filename, paste0("\\.", dev)))
    filename <- paste0(filename, ".", dev)
  if (dev == "jpeg" & !str_detect(filename, "\\.jpg|\\.jpeg"))
    filename <- paste0(filename, ".jpg")
  w <- round(180 * size)
  h <- w/ar
  ggsave(filename = filename,
         plot = p,
         width = w,
         height = h,
         units = "mm",
         device = dev)
}
