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

# Directory names
work_dir <- PathString(list("Code", "Working"))                 # To store results of Precompute.R
cont_dir <- PathString(list("Data", "NaturalEarth"))            # Continents shapefile
lake_dir <- PathString(list("Data", "GRDC"))                    # Lakes shapefile
river_dir <- PathString(list("Data", "GRDC"))                   # Rivers shapefile
basin_dir <- PathString(list("Data", "GRDC"))                   # Basin polygons shapefile
nut_dir <- PathString(list("Data", "EarthStat", "FertilizerBalance_Ascii"))    # ESRI grid data of nutrients
ss_dir <- PathString(list("Data", "Spreadsheets"))              # ATS locations
fig_dir <- PathString(list("Visual Elements", "Figures"))       # Figures
tbl_dir <- PathString(list("Visual Elements", "Tables"))        # Tables

# File names
area <- "area"
bbox <- "ne_110m_wgs84_bounding_box"
cont <- "ne_110m_land"
lake <- "GRDC_lakes_join_rivers"
river <- "GRDC_687_rivers"
basin <- "GRDC_405_basins_from_mouth"
Nsums <- "Nsums"
Psums <- "Psums"
NPlim <- "NPlim"

# Fully qualified path names
area_fn <- paste0(work_dir, area)
bbox_fn <- paste0(cont_dir, bbox)
cont_fn <- paste0(cont_dir, cont)
lake_fn <- paste0(lake_dir, lake)
river_fn <- paste0(river_dir, river)
basin_fn <- paste0(basin_dir, basin)
Nsums_fn <- paste0(work_dir, Nsums)
Psums_fn <- paste0(work_dir, Psums)
NPlim_fn <- paste0(work_dir, NPlim)

# Figure settings
fig_rdpi <- 300                 # raster art dots per inch
fig_ldpi <- 600                 # line art dots per inch
fig_wcol <- "cornflowerblue"    # water color
fig_ccol <- "darkgray"          # continent color
fig_scol <- "springgreen"       # spot color
fig_bline <- 0.5                # basin polygon line size
fig_rline <- 0.2                # river line size
fig_pt_sz <- 0.01               # size of geom_point
fig_pt_sh <- 16                 # shape of geom_point
fig_bcol <- "black"             # basin polygon edge color
fig_CRS <- "+proj=robin"        # Robinson map projection
fig_wid <- 8                    # figure width, inches
fig_hgt <- 8*(sqrt(5)-1)/2      # use when height doesn't depend on width
fig_gap <- 1/16                 # figure panel gap, inches
fig_band_col_ct <- 250          # figure geom_pt color count per band
NP_pal <- c("#3050F8", "#FF8000") # CPK Jmol coloring N:blue P:orange

# Manually determined dimensions for Robinson projection; RemoveWhiteEdges()
# results in 8" width
orig_map_wid <- 8.986639
orig_map_hgt <- 4.636736

# ggplot2 theme
theme_opts <- list(theme(axis.line = element_blank(),
                         axis.text.x = element_blank(),
                         axis.text.y = element_blank(),
                         axis.ticks = element_blank(),
                         axis.title.x = element_blank(),
                         axis.title.y = element_blank(),
                         legend.position = "none",
                         panel.background = element_blank()
))

# Assign economic and ecological variables from XL sheet EACCRPBT.xlsx:R Variables"
ss_fn <- paste0(ss_dir, "EACCRPBT.xlsx")
Rvars_ws <- gdata::read.xls(ss_fn, 1)
ret <- mapply(assign, as.character(Rvars_ws$var), Rvars_ws$val, MoreArgs = list(envir = .GlobalEnv))
