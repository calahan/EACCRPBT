library(Calahanlab)
#library(tiff)
# In general all graphics processing is done with TIFF files. PNGs are friendlier
# to Scrivener, so for the purposes of generating readable manuscripts, we use PNGs
# there.

source("Code/R/Settings.R")

in_fn <- paste0(fig_dir, "Figure ", 1:5, "/Figure ", 1:5, ".tiff")
out_fn <- paste0(fig_dir, "Figure ", 1:5, "/Figure ", 1:5, ".png")

for(i in 1:5) {
    TIFF2PNG(in_fn[i], out_fn[i], 8, 300)
}