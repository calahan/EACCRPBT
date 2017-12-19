library(Calahanlab)
library(ggplot2)
library(rgdal)
source("Code/R/EACCRPBT.R")
source("Code/R/Settings.R")

fig_dir <- paste0(fig_dir, "Figure 4/")
fig_fn <- paste0(fig_dir, "Figure 4.tiff")
A_fn <- paste0(fig_dir, "A.tiff")
B_fn <- paste0(fig_dir, "B.tiff")
C_fn <- paste0(fig_dir, "C.tiff")
D_fn <- paste0(fig_dir, "D.tiff")
panel_fns <- paste0(fig_dir, c("A.tiff", "B.tiff", "C.tiff", "D.tiff"))

# Load geospatial data sets.
bbox_df <- readOGR(bbox_fn, bbox)
cont_df <- readOGR(cont_fn, cont)
lake_df <- readOGR(lake_fn, lake)
river_df <- readOGR(river_fn, river)
basin_df <- readOGR(basin_fn, basin)
NP_lim_df <- read.table(NP_lim_fn)
nutN_df <- LoadNutrientData(nut_dir, "nitrogen")
nutP_df <- LoadNutrientData(nut_dir, "phosphorus")
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
trap_df <- data.frame(long=NP_lim_df$long, lat=NP_lim_df$lat, val=NP_lim_df$arearat)
trap_df <- trap_df[!is.na(trap_df$val),]
ttrap_df <- TransformNutrientData(trap_df, fig_CRS)
ttrap_val <- ttrap_df[which(!is.na(ttrap_df$val)),]$val
s_ttrap_val <- sort(ttrap_val, decreasing=TRUE)
trap_10 <- max(which(s_ttrap_val > 0.1))
trap_01 <- max(which(s_ttrap_val > 0.01))
ntrap_90 <- WhichFewResponsible(s_ttrap_val, 0.9)
ntrap_50 <- WhichFewResponsible(s_ttrap_val, 0.5)

sum_rats <- Psums_df$val/Nsums_df$val
sum_rats[is.nan(sum_rats) | is.infinite(sum_rats)] <- NA
lims <- vector("character", length=basin_ct)
lims[1:basin_ct] <- NA
lims[sum_rats >= P2N] <- "N"
lims[sum_rats < P2N] <- "P"
plot_df <- merge(ftbasin_df, data.frame(id=1:basin_ct, lim=lims, val=sum_rats), by="id")

area_sums <- read.table(paste0(work_dir, "areas")) # [todo]
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

# 4A - N or P Limited, trapezoids.
fig_fn <- A_fn
plot <- ggplot(data=ftbbox_df, aes(x=long, y=lat)) +
    geom_polygon(fill=fig_wcol) +
    geom_polygon(data=ftcont_df, aes(x=long, y=lat, group=group), fill=fig_ccol) +
    geom_point(data=NP_lim_df, aes(x=long, y=lat, color=nut), shape=fig_pt_sh, size=fig_pt_sz) +
    scale_color_manual(values=NP_pal) +
    geom_polygon(data=ftbasin_df, aes(x=long, y=lat, group=group), fill=NA, color=fig_bcol, size=fig_bline) +
    geom_path(data=ftriver_df, aes(x=long, y=lat, group=group), color=fig_wcol, size=fig_rline) +
    geom_polygon(data=ftlake_df, aes(x=long, y=lat, group=group), fill=fig_wcol) +
    theme_opts
ggsave(fig_fn, plot=plot, width=3*orig_map_wid, height=3*orig_map_hgt, dpi=fig_rdpi)
RemoveWhiteEdges(fig_fn, fig_fn, fig_rdpi)
ResaveTIFF(fig_fn, fig_fn, fig_rdpi, fig_rdpi, 8)

# 4B - N or P Limited, basins.
fig_fn <- B_fn
plot <- ggplot(data=ftbbox_df, aes(x=long, y=lat)) +
    geom_polygon(fill=fig_wcol) +
    geom_polygon(data=ftcont_df, aes(x=long, y=lat, group=group), fill=fig_ccol) +
    geom_polygon(data=plot_df[plot_df$hole==FALSE,], aes(x=long, y=lat, group=group, fill=lim), color=fig_bcol, size=fig_bline) +
    scale_fill_manual(values=NP_pal) +
    geom_path(data=ftriver_df, aes(x=long, y=lat, group=group), color=fig_wcol, size=fig_rline) +
    geom_polygon(data=ftlake_df, aes(x=long, y=lat, group=group), fill=fig_wcol) +
    theme_opts
ggsave(fig_fn, plot=plot, width=3*orig_map_wid, height=3*orig_map_hgt, dpi=fig_rdpi)
RemoveWhiteEdges(fig_fn, fig_fn, fig_rdpi)
ResaveTIFF(fig_fn, fig_fn, fig_rdpi, fig_rdpi, 8)

# 4C - ATS Area needed, trapezoids
fig_fn <- C_fn
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

# 4D - ATS Area needed, basins.
fig_fn <- D_fn
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
per_row <- c(2,2)
fig_gap <- 1/16
dpi <- 300
labels <- LETTERS[1:4]
label_cols <- rep("black", 4)
xoff <- 30
yoff <- -35
cex <- 1.0
AssemblePanels(fig_fn, per_row, panel_fns, fig_wid, fig_gap, fig_rdpi, labels, label_cols, xoff, yoff, cex)

#[todo] Preserve his code somewhere, for later
# Create data frame for plotting squares of ATS surface area, with coordinates in degrees
# sq_wid_deg <- sqrt(area_sums$val * 1000) # [todo]playing with multipliers to see what appears on map
# ATSarea_df$sq.wid <- sq_wid_deg * DegreesPerMeter(ATSarea_df$lat)
# ats_poly <- list()
# for(i in 1:basin_ct) {
#     if(ATSarea_df[i,]$ATs_area > 0) {
#         long <- ATSarea_df[i,]$long
#         lat <- ATSarea_df[i,]$lat
#         wid <- ATSarea_df[i,]$sq.wid/2
#         ats_poly[[i]] <- SpatialPolygons(list(Polygons(list(Polygon(matrix(c(long + wid, lat + wid, # upper left
#                                 long - wid, lat + wid, # upper right
#                                 long - wid, lat - wid, # lower right
#                                 long + wid, lat - wid), # lower left
#                               ncol=2,
#                               byrow=TRUE))), i)))
#         proj4string(ats_poly[[i]]) <- CRS("+proj=longlat +datum=WGS84")
#     } else {
#         ats_poly[[i]] <- NA
#     }
# }
# ats_poly <- ats_poly[which(!is.na(ats_poly))]
# ats_poly_sp <- SpatialPolygons(lapply(ats_poly, function(x){x@polygons[[1]]}))
# ats_poly_df <- SpatialPolygonsDataFrame(Sr=ats_poly_sp, data=data.frame(i=1:length(ats_poly_sp)),FALSE)
# proj4string(ats_poly_df) <- CRS("+proj=longlat +datum=WGS84")
# ftats_poly <- fortify(spTransform(ats_poly_df, CRS(fig_CRS)))

# How much area for each nutrient [todo]there seems to be multiplication of data frames, perhaps a few unifying ones are in order (i.e. normalization)
