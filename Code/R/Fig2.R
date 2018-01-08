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

# Folders and files
fig_dir <- paste0(fig_dir, "Figure 2/")

A_fn <- paste0(fig_dir, "A.tiff")
B_fn <- paste0(fig_dir, "B.tiff")
C_fn <- paste0(fig_dir, "C.tiff")
D_fn <- paste0(fig_dir, "D.tiff")
fig_fn <- paste0(fig_dir, "Figure 2.tiff")
panel_fns <- paste0(fig_dir, c("A.tiff", "B.tiff", "C.tiff", "D.tiff"))

# Load and transform geographical data
bbox_df <- readOGR(bbox_fn, bbox)
cont_df <- readOGR(cont_fn, cont)
lake_df <- readOGR(lake_fn, lake)
river_df <- readOGR(river_fn, river)
basin_df <- readOGR(basin_fn, basin)
ftbbox_df <- fortify(spTransform(bbox_df, CRS(fig_CRS)))
ftcont_df <- fortify(spTransform(cont_df, CRS(fig_CRS)))
ftlake_df <- fortify(spTransform(lake_df, CRS(fig_CRS)))
ftriver_df <- fortify(spTransform(river_df, CRS(fig_CRS)))
ftbasin_df <- fortify(spTransform(basin_df, CRS(fig_CRS)))
ftbasin_df$id <- as.numeric(ftbasin_df$id) + 1 # indexed from 0 here, but 1 elsewhere, so add 1 to correct

# Load and transform nutrient data
nutN_df <- LoadNutrientData(nut_dir, "nitrogen")
tnutN_df <- TransformNutrientData(nutN_df, fig_CRS)
nutP_df <- LoadNutrientData(nut_dir, "phosphorus")
tnutP_df <- TransformNutrientData(nutP_df, fig_CRS)

# Load basin sums computed by Precompute.R, combined N & P values
Nsums_df <- read.table(Nsums_fn)
Psums_df <- read.table(Psums_fn)
#NPsums_df <- read.table(NPsums_fn)

# Statistics about the data.
#cells_ct <- nrow(nutN_df)
Nval <- nutN_df[which(!is.na(nutN_df$val)),]$val
Pval <- nutP_df[which(!is.na(nutP_df$val)),]$val
Nsumsval <- Nsums_df$val
Psumsval <- Psums_df$val

s_Nval <- sort(Nval, decreasing=TRUE)
s_Pval <- sort(Pval, decreasing=TRUE)
s_Pval_pos <- s_Pval[which(s_Pval >= 0)] # Negative values indicate deficit, not excess
s_Pval_sh <- s_Pval - min(s_Pval) # Shifted so that all values are >= 0
s_Nsumsval <- sort(Nsumsval, decreasing=TRUE)
s_Psumsval <- sort(Psumsval, decreasing=TRUE)

N_wfr_50 <- WhichFewResponsible(s_Nval, 0.5)
P_wfr_50 <- WhichFewResponsible(s_Pval_pos, 0.5)
N_wfr_90 <- WhichFewResponsible(s_Nval, 0.9)
P_wfr_90 <- WhichFewResponsible(s_Pval_pos, 0.9)
Nsums_wfr_90 <- WhichFewResponsible(s_Nsumsval, 0.9)
Nsums_wfr_50 <- WhichFewResponsible(s_Nsumsval, 0.5)
Psums_wfr_90 <- WhichFewResponsible(s_Psumsval, 0.9)
Psums_wfr_50 <- WhichFewResponsible(s_Psumsval, 0.5)

N_wfr_50_pct <- 100 * N_wfr_50/length(s_Nval)
P_wfr_50_pct <- 100 * P_wfr_50/(length(s_Pval_pos))

# Figure 2A - Nitrogen excess
plot <- ggplot(data=ftbbox_df, aes(x=long, y=lat)) +
    geom_polygon(fill=fig_wcol) +
    geom_polygon(data=ftcont_df, aes(x=long, y=lat, group=group), fill=fig_ccol) +
    geom_point(data=tnutN_df, aes(x=long, y=lat, color=val), shape=fig_pt_sh, size=fig_pt_sz) +
    scale_color_gradientn(colors=c("red", "yellow", "green"), values=c(max(s_Nval), s_Nval[N_wfr_50], s_Nval[N_wfr_90], min(s_Nval))/max(s_Nval), na.value=NA) +
    geom_polygon(data=ftbasin_df[ftbasin_df$hole==FALSE,], aes(x=long, y=lat, group=group), fill=NA, color=fig_bcol, size=fig_bline) +
    geom_path(data=ftriver_df, aes(x=long, y=lat, group=group), color=fig_wcol, size=fig_rline) +
    geom_polygon(data=ftlake_df, aes(x=long, y=lat, group=group), fill=fig_wcol) +
    theme_opts
ggsave(A_fn, plot=plot, width=3*orig_map_wid, height=3*orig_map_hgt, dpi=fig_rdpi)
RemoveWhiteEdges(A_fn, A_fn, fig_rdpi)
ResaveTIFF(A_fn, A_fn, fig_rdpi, fig_rdpi, 8)

# Figure 2B - Phosphorus excess
plot <- ggplot(data=ftbbox_df, aes(x=long, y=lat)) +
    geom_polygon(fill=fig_wcol) +
    geom_polygon(data=ftcont_df, aes(x=long, y=lat, group=group), fill=fig_ccol) +
    geom_point(data=tnutP_df, aes(x=long, y=lat, color=val), shape=fig_pt_sh, size=fig_pt_sz) +
    scale_color_gradientn(colors=c("red", "yellow", "green", "midnightblue"), values=c(max(s_Pval_sh), s_Pval_sh[P_wfr_50], s_Pval_sh[P_wfr_90], s_Pval_sh[min(which(s_Pval <= 0))], min(s_Pval_sh))/max(s_Pval_sh), na.value=NA) +
    geom_polygon(data=ftbasin_df[ftbasin_df$hole==FALSE,], aes(x=long, y=lat, group=group), fill=NA, color=fig_bcol, size=fig_bline) +
    geom_path(data=ftriver_df, aes(x=long, y=lat, group=group), color=fig_wcol, size=fig_rline) +
    geom_polygon(data=ftlake_df, aes(x=long, y=lat, group=group), fill=fig_wcol) +
    theme_opts
ggsave(B_fn, plot=plot, width=3*orig_map_wid, height=3*orig_map_hgt, dpi=fig_rdpi)
RemoveWhiteEdges(B_fn, B_fn, fig_rdpi)
ResaveTIFF(B_fn, B_fn, fig_rdpi, fig_rdpi, 8)

# Assemble panels into figure
per_row <- c(1,1)
fig_gap <- 1/16
dpi <- 300
labels <- LETTERS[1:2]
label_cols <- rep("black", 2)
xoff <- 30
yoff <- -35
cex <- 1.0
AssemblePanels(fig_fn, per_row, panel_fns, fig_wid, fig_gap, fig_rdpi, labels, label_cols, xoff, yoff, cex)

# Save information to be used to create ¶Nutrient Excesses
totalcells <- nrow(nutN_df)
totalNxs <- sum(s_Nval)
totalPxs <- sum(s_Pval_pos)
Ncells <- sum(!is.na(nutN_df$val) & !is.na(nutP_df$val))
Pcells <- sum(!is.na(nutP_df$val))
cellminN <- min(nutN_df$val, na.rm=TRUE)
cellmaxN <- max(nutN_df$val, na.rm=TRUE)
cellminP <-  min(nutP_df$val, na.rm=TRUE)
cellmaxP <- max(nutP_df$val, na.rm=TRUE)
cellhalfN <- N_wfr_50_pct
cellhalfP <- P_wfr_50_pct
basinNpct <- sum(Nsumsval)/totalNxs
basinPpct <- sum(Psumsval)/totalPxs
basinNmin <- min(Nsumsval)
basinNmax <- max(Nsumsval)
basinPmin <- min(Psumsval)
basinPmax <- max(Psumsval)
basinNhalf <- Nsums_wfr_50
basinNhalf <- Nsums_wfr_50
maxNname <- as.character(Nsums_df[which(Nsums_df$val == max(Nsums_df$val)),]$name)
maxNval <-max(Nsums_df$val)
maxPname <- as.character(Psums_df[which(Psums_df$val == max(Psums_df$val)),]$name)
maxPval <-max(Psums_df$val)

fn <- paste0(work_dir, "pNutrientExcesses_vals")
write.table(data.frame(name=c("totalcells",
                              "totalNxs",
                              "totalPxs",
                              "Ncells",
                              "Pcells",
                              "cellminN",
                              "cellmaxN",
                              "cellminP",
                              "cellmaxP",
                              "cellhalfN",
                              "cellhalfP",
                              "basinNpct",
                              "basinPpct",
                              "maxNval",
                              "maxPval"),
                       val=c(totalcells,
                             totalNxs,
                             totalPxs,
                             Ncells,
                             Pcells,
                             cellminN,
                             cellmaxN,
                             cellminP,
                             cellmaxP,
                             cellhalfN,
                             cellhalfP,
                             basinNpct,
                             basinPpct,
                             maxNval,
                             maxPval
                       )),
            fn)

fn <- paste0(work_dir, "pNutrientExcesses_txt")
write.table(data.frame(name=c("maxNname",
                              "maxPname"),
                         val=c(maxNname,
                               maxPname
                       )),
            fn)

# t <- read.table(textvars_fn)
# t
