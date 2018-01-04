# Copyright 2017-2018 by Steven Dean Calahan
#
# This file is part of the publication "Expanding Algal Cultivation Can Reverse
# Planetary Boundary Transgressions" (EACCRPBT).
#
# The software component of EACCRPBT is free software: you can redistribute it
# and/or modify it under the terms of the GNU General Public License as published
# by the Free Software Foundation, either version 3 of the License, or (at your
# option) any later version.

# The software component of EACCRPBT is distributed in the hope that it will be
# useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public
# License for more details.
#
# You should have received a copy of the GNU General Public License
# along with the software component of EACCRPBT. If not, see <http://www.gnu.org/licenses/>.
#
library(Calahanlab)
library(ggplot2)
library(tiff)
source("Code/R/Settings.R")

fig_dir <- paste0(fig_dir, "Figure 1/")

# Load geospatial data sets.
bbox_df <- readOGR(bbox_fn, bbox)
cont_df <- readOGR(cont_fn, cont)
lake_df <- readOGR(lake_fn, lake)
river_df <- readOGR(river_fn, river)

# Prepare ATS locations data.
loc_df <- gdata::read.xls(ss_fn, 2) # Warning or notification appears to be OK (Wide character in print at .../Library/R/3.3/library/gdata/perl/xls2csv.pl line 327.)
map_loc_df <- loc_df[loc_df$status=="good",]
map_loc_df <- data.frame(long=map_loc_df$long, lat=map_loc_df$lat, type=map_loc_df$type)
map_loc_df$size <- 0.1
map_loc_df[map_loc_df$type== "prod",]$size <- 0.2
coordinates(map_loc_df) <- c("long", "lat") # map_loc_df is now a SpatialPointsDataFrame
proj4string(map_loc_df) <- CRS("+proj=longlat +datum=WGS84")

# Transform and fortify (if necessary) the data.
proj <- CRS(fig_CRS)
ftbbox_df <- fortify(spTransform(bbox_df, proj))
ftcont_df <- fortify(spTransform(cont_df, proj))
ftlake_df <- fortify(spTransform(lake_df, proj))
ftriver_df <- fortify(spTransform(river_df, proj))
fmap_loc_df <- as.data.frame(spTransform(map_loc_df, proj))

# Create map image, trim white border.
fig_fn <- paste0(fig_dir, "E.tiff")
fig_rline <- 0.01               # river line size
fig_bline <- 0.1                # basin line size
plot <- ggplot(data = ftbbox_df, aes(x = long, y = lat)) +
    geom_polygon(fill = fig_wcol) +
    geom_polygon(data = ftcont_df, aes(x = long, y = lat, group = group), fill = fig_ccol) +
    geom_polygon(data = ftlake_df, aes(x = long, y = lat, group = group), fill = fig_wcol) +
    geom_path(data = ftriver_df, aes(x = long, y = lat, group=group), color = fig_wcol, size = fig_rline) +
    geom_point(data = fmap_loc_df, aes(x = long, y = lat, size = size), color = fig_scol, alpha = 0.55) +
    scale_size(range = c(0.75, 2.25)) +
    theme_opts
ggsave(fig_fn, plot=plot, width=orig_map_wid, height=orig_map_hgt, dpi=fig_rdpi)
RemoveWhiteEdges(fig_fn, fig_fn, fig_rdpi)

# Assemble figure
#
fig_fn <- paste0(fig_dir, "Figure 1.tiff")
in_fn <- paste0(fig_dir, c("A.tiff", "B.tiff", "C.tiff", "D.tiff", "E.tiff"))
per_row <- c(1,2,2)
fig_gap <- 1/16
dpi <- 300
labels <- LETTERS[1:5]
lable_cols <- c("white", rep("black", 4))
xoff <- 30
yoff <- -35
cex <- 1.0
AssemblePanels(fig_fn, per_row, in_fn, fig_wid, fig_gap, fig_rdpi, labels, lable_cols, xoff, yoff, cex)
AddArrows2TIFF(fig_fn, fig_fn,
               c(219, 1231, 607, 1337, 1377, 1425, 1501, 1558),   # x's
               c(2062, 2152, 996, 1840, 1850, 1856, 1864, 1940),  # y's
               c(25, 25, 25, 15, 15, 15, 15, 15),                 # base widths
               c(40, 40, 40, 24, 24, 24, 24, 24),                 # base heights
               c(40, 40, 40, 24, 24, 24, 24, 24),                 # head widths
               c(40, 40, 40, 24, 24, 24, 24, 24),                 # head heights
               c(rep(1.57, 8)),                                   # angles
               c(rep("white", 7), "black"),                       # line colors
               c(rep(5, 8)))                                      # line widths
