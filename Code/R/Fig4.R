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
library(rgdal)
source("Code/R/EACCRPBT.R")
source("Code/R/Settings.R")

fig_dir <- paste0(fig_dir, "Figure 4/")
fig_fn <- paste0(fig_dir, "Figure 4.tiff")
A_fn <- paste0(fig_dir, "A.tiff")
B_fn <- paste0(fig_dir, "B.tiff")
panel_fns <- paste0(fig_dir, c("A.tiff", "B.tiff"))

# Load geospatial data sets.
bbox_df <- readOGR(bbox_fn, bbox)
cont_df <- readOGR(cont_fn, cont)
lake_df <- readOGR(lake_fn, lake)
river_df <- readOGR(river_fn, river)
basin_df <- readOGR(basin_fn, basin)

nutN_df <- LoadNutrientData(nut_dir, "nitrogen")
nutP_df <- LoadNutrientData(nut_dir, "phosphorus")

NPlim_df <- read.table(NPlim_fn)
Nsums_df <- read.table(Nsums_fn)
Psums_df <- read.table(Psums_fn)

# Transform and fortify the geospatial data.
ftbbox_df <- fortify(spTransform(bbox_df, CRS(fig_CRS)))
ftcont_df <- fortify(spTransform(cont_df, CRS(fig_CRS)))
ftlake_df <- fortify(spTransform(lake_df, CRS(fig_CRS)))
ftriver_df <- fortify(spTransform(river_df, CRS(fig_CRS)))
ftbasin_df <- fortify(spTransform(basin_df, CRS(fig_CRS)))
ftbasin_df$id <- as.numeric(ftbasin_df$id) + 1 # indexing from 1 rather than 0

# Useful values
P2N <- P_prp/N_prp
basin_ct <- nrow(Nsums_df)

# Trapezoid calculations
# Gather coordinates,
trap_df <- data.frame(long=NPlim_df$long, lat=NPlim_df$lat, val=NPlim_df$ATSarea)
trap_df <- trap_df[!is.na(trap_df$val),]
ttrap_df <- TransformNutrientData(trap_df, fig_CRS)

ttrap_val <- ttrap_df[which(!is.na(ttrap_df$val)),]$val
s_ttrap_val <- sort(ttrap_val, decreasing=TRUE)
ntrap_90 <- WhichFewResponsible(s_ttrap_val, 0.9)
ntrap_50 <- WhichFewResponsible(s_ttrap_val, 0.5)

sum_rats <- Psums_df$val/Nsums_df$val
sum_rats[is.nan(sum_rats) | is.infinite(sum_rats)] <- NA
lims <- vector("character", length=basin_ct)
lims[1:basin_ct] <- NA
lims[sum_rats >= P2N] <- "N"
lims[sum_rats < P2N] <- "P"
plot_df <- merge(ftbasin_df, data.frame(id=1:basin_ct, lim=lims, val=sum_rats), by="id")

area_sums <- read.table(area_fn) # ATS area needed per basin
s_areas <- sort(unique(area_sums$val), decreasing=TRUE)
areas_90 <- WhichFewResponsible(s_areas, 0.9)
areas_50 <- WhichFewResponsible(s_areas, 0.5)

# Create data frame for plotting ranked area per basin
polys <- basin_df@polygons
data <- basin_df@data
id <- vector(mode="numeric", length=basin_ct)
basin_area <- vector(mode="numeric", length=basin_ct)
long <- vector(mode="numeric", length=basin_ct)
lat <- vector(mode="numeric", length=basin_ct)
for(i in 1:basin_ct) {
    id[i] <- as.numeric(polys[[i]]@ID) + 1
    basin_area[i] <- data[i,]$AREA_CALC
    long[i] <- polys[[i]]@labpt[1]
    lat[i] <- polys[[i]]@labpt[2]
}
ATSarea_df <- data.frame(id=id, basin_area=basin_area, ATs_area=area_sums$val, long=long, lat=lat, name=area_sums$name, nut=lims)
ftarea_sums <- merge(ftbasin_df, area_sums, by="id")

# 4A - ATS Area needed, trapezoids
fig_fn <- A_fn
plot <- ggplot(data=ftbbox_df, aes(x=long, y=lat)) +
    geom_polygon(fill=fig_wcol) +
    geom_polygon(data=ftcont_df, aes(x=long, y=lat, group=group), fill=fig_ccol) +
    geom_point(data=ttrap_df, aes(x=long, y=lat, color=val), shape=fig_pt_sh, size=fig_pt_sz) +
    geom_polygon(data=plot_df[plot_df$hole==FALSE,], aes(x=long, y=lat, group=group), color=fig_bcol, fill=NA, size=fig_bline) +
#    scale_color_gradientn(colors=c("red", "yellow", "green"), values=c(max(s_ttrap_val), s_ttrap_val[trap_10], s_ttrap_val[trap_01], min(s_ttrap_val))/max(s_ttrap_val), na.value=NA) +
    scale_color_gradientn(colors=c("red", "yellow", "green"), values=c(max(s_ttrap_val), s_ttrap_val[ntrap_50], s_ttrap_val[ntrap_90], min(s_ttrap_val))/max(s_ttrap_val), na.value=NA) +
    geom_path(data=ftriver_df, aes(x=long, y=lat, group=group), color=fig_wcol, size=fig_rline) +
    geom_polygon(data=ftlake_df, aes(x=long, y=lat, group=group), fill=fig_wcol) +
    theme_opts
ggsave(fig_fn, plot=plot, width=3*orig_map_wid, height=3*orig_map_hgt, dpi=fig_rdpi)
RemoveWhiteEdges(fig_fn, fig_fn, fig_rdpi)
ResaveTIFF(fig_fn, fig_fn, fig_rdpi, fig_rdpi, 8)

# 4B - ATS Area needed, basins.
fig_fn <- B_fn
plot <- ggplot(data=ftbbox_df, aes(x=long, y=lat)) +
    geom_polygon(fill=fig_wcol) +
    geom_polygon(data=ftcont_df, aes(x=long, y=lat, group=group), fill=fig_ccol) +
    geom_polygon(data=ftarea_sums[ftarea_sums$hole==FALSE,], aes(x=long, y=lat, group=group, fill=val), color=fig_bcol, size=fig_bline) +
    scale_fill_gradientn(colors=c("red", "yellow", "green"), values=c(max(s_areas), s_areas[areas_50], s_areas[areas_90], min(s_areas))/max(s_areas), na.value=NA) +
    geom_path(data=ftriver_df, aes(x=long, y=lat, group=group), color=fig_wcol, size=fig_rline) +
    geom_polygon(data=ftlake_df, aes(x=long, y=lat, group=group), fill=fig_wcol) +
    theme_opts
ggsave(fig_fn, plot=plot, width=3*orig_map_wid, height=3*orig_map_hgt, dpi=fig_rdpi)
RemoveWhiteEdges(fig_fn, fig_fn, fig_rdpi)
ResaveTIFF(fig_fn, fig_fn, fig_rdpi, fig_rdpi, 8)

# Assemble panels into figure
fig_fn <- paste0(fig_dir, "Figure 4.tiff")
#per_row <- c(2,2)
per_row <- c(1,1)
fig_gap <- 1/16
dpi <- 300
labels <- LETTERS[1:2]
label_cols <- rep("black", 2)
xoff <- 30
yoff <- -35
cex <- 1.0
AssemblePanels(fig_fn, per_row, panel_fns, fig_wid, fig_gap, fig_rdpi, labels, label_cols, xoff, yoff, cex)
