library(Calahanlab)
library(ggplot2)
library(rgdal)
source("Code/R/EACCRPBT.R")
source("Code/R/Settings.R")

# Folders and files
this_fig_dir <- paste0(fig_dir, "Figure 2/")

A_fn <- paste0(this_fig_dir, "A.tiff")
B_fn <- paste0(this_fig_dir, "B.tiff")
C_fn <- paste0(this_fig_dir, "C.tiff")
D_fn <- paste0(this_fig_dir, "D.tiff")
fig_fn <- paste0(this_fig_dir, "Figure 2.tiff")
panel_fns <- paste0(this_fig_dir, c("A.tiff", "B.tiff", "C.tiff", "D.tiff"))

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
ftbasin_df$id <- as.numeric(ftbasin_df$id) + 1 # indexed from 0 here but 1 elsewhere

# Load and transform nutrient data
nutN_df <- LoadNutrientData(nut_dir, "nitrogen")
tnutN_df <- TransformNutrientData(nutN_df, fig_CRS)
nutP_df <- LoadNutrientData(nut_dir, "phosphorus")
tnutP_df <- TransformNutrientData(nutP_df, fig_CRS)
nutNP_df <- NutrientDataProduct(nutN_df, nutP_df)
tnutNP_df <- TransformNutrientData(nutNP_df, fig_CRS)

# Load basin sums computed by Precompute.R, combined N & P values
Nsums_df <- read.table(Nsums_fn)
Psums_df <- read.table(Psums_fn)
NPsums_df <- read.table(NPsums_fn)
NPcomb_df <- NutrientDataProduct(Nsums_df, Psums_df)
t <- merge(ftbasin_df, NPsums_df, by="id")

# Statistics about the data.
#cells_ct <- nrow(nutN_df)
Nval <- nutN_df[which(!is.na(nutN_df$val)),]$val
Pval <- nutP_df[which(!is.na(nutP_df$val)),]$val
NPval <- nutNP_df[which(!is.na(nutNP_df$val)),]$val
Nsumsval <- Nsums_df$val
Psumsval <- Psums_df$val
NPsumsval <- NPsums_df$val

s_Nval <- sort(Nval, decreasing=TRUE)
s_Pval <- sort(Pval, decreasing=TRUE)
s_Pval_pos <- s_Pval[which(s_Pval >= 0)] # Negative values indicate deficit, not excess
s_Pval_sh <- s_Pval - min(s_Pval) # Shifted so that all values are >= 0
s_NPval <- sort(NPval, decreasing=TRUE)
s_Nsumsval <- sort(Nsumsval, decreasing=TRUE)
s_Psumsval <- sort(Psumsval, decreasing=TRUE)
s_NPsumsval <-sort(NPsumsval, decreasing=TRUE)

N_wfr_50 <- WhichFewResponsible(s_Nval, 0.5)
P_wfr_50 <- WhichFewResponsible(s_Pval_pos, 0.5)
NP_wfr_50 <- WhichFewResponsible(s_NPval, 0.5)
N_wfr_90 <- WhichFewResponsible(s_Nval, 0.9)
P_wfr_90 <- WhichFewResponsible(s_Pval_pos, 0.9)
NP_wfr_90 <- WhichFewResponsible(s_NPval, 0.9)
Nsums_wfr_90 <- WhichFewResponsible(s_Nsumsval, 0.9)
Nsums_wfr_50 <- WhichFewResponsible(s_Nsumsval, 0.5)
Psums_wfr_90 <- WhichFewResponsible(s_Psumsval, 0.9)
Psums_wfr_50 <- WhichFewResponsible(s_Psumsval, 0.5)
NPsums_wfr_90 <- WhichFewResponsible(s_NPsumsval, 0.9)
NPsums_wfr_50 <- WhichFewResponsible(s_NPsumsval, 0.5)

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

# Figure 2C - Combined excess
plot <- ggplot(data=ftbbox_df, aes(x=long, y=lat)) +
    geom_polygon(fill=fig_wcol) +
    geom_polygon(data=ftcont_df, aes(x=long, y=lat, group=group), fill=fig_ccol) +
    geom_point(data=tnutNP_df, aes(x=long, y=lat, color=val), shape=fig_pt_sh, size=fig_pt_sz) +
    scale_color_gradientn(colors=c("red", "yellow", "green"), values=c(max(s_NPval), s_Nval[NP_wfr_50], s_Nval[NP_wfr_90], min(s_NPval))/max(s_NPval), na.value=NA) +
    geom_polygon(data=ftbasin_df[ftbasin_df$hole==FALSE,], aes(x=long, y=lat, group=group), fill=NA, color=fig_bcol, size=fig_bline) +
    geom_path(data=ftriver_df, aes(x=long, y=lat, group=group), color=fig_wcol, size=fig_rline) +
    geom_polygon(data=ftlake_df, aes(x=long, y=lat, group=group), fill=fig_wcol) +
    theme_opts
ggsave(C_fn, plot=plot, width=3*orig_map_wid, height=3*orig_map_hgt, dpi=fig_rdpi)
RemoveWhiteEdges(C_fn, C_fn, fig_rdpi)
ResaveTIFF(C_fn, C_fn, fig_rdpi, fig_rdpi, 8)

# Figure 2D - Basin sums
plot <- ggplot(data=ftbbox_df, aes(x=long, y=lat)) +
    geom_polygon(fill=fig_wcol) +
    geom_polygon(data=ftcont_df, aes(x=long, y=lat, group=group), fill=fig_ccol) +
    geom_polygon(data=t[t$hole==FALSE,], aes(x=long, y=lat, group=group, fill=val), color=fig_bcol, size=fig_bline) +
    scale_fill_gradientn(colors=c("red", "yellow", "green"), values=c(max(s_NPsumsval), s_NPsumsval[NPsums_wfr_90], s_NPsumsval[NPsums_wfr_50], min(s_NPsumsval))/max(s_NPsumsval), na.value=NA) +
    geom_path(data=ftriver_df, aes(x=long, y=lat, group=group), color=fig_wcol, size=fig_rline) +
    geom_polygon(data=ftlake_df, aes(x=long, y=lat, group=group), fill=fig_wcol) +
    theme_opts
ggsave(D_fn, plot=plot, width=3*orig_map_wid, height=3*orig_map_hgt, dpi=fig_rdpi)
RemoveWhiteEdges(D_fn, D_fn, fig_rdpi)
ResaveTIFF(D_fn, D_fn, fig_rdpi, fig_rdpi, 8)

# Assemble panels into figure
per_row <- c(2,2)
fig_gap <- 1/16
dpi <- 300
labels <- LETTERS[1:4]
label_cols <- rep("black", 4)
xoff <- 30
yoff <- -35
cex <- 1.0
AssemblePanels(fig_fn, per_row, panel_fns, fig_wid, fig_gap, fig_rdpi, labels, label_cols, xoff, yoff, cex)
