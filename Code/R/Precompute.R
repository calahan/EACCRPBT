library(Calahanlab)

# setwd("Code/R")
source("Code/R/EACCRPBT.R")
source("Code/R/Settings.R")

#[todo] look into read.asciigrid, ESRI ascii grid loader

# Directories and filenames
nut_dir <- "Data/EarthStat/FertilizerBalance_Ascii/"
basin_dir <- "Data/GRDC/"
basin <- "GRDC_405_basins_from_mouth"
basin_fn <- paste0(basin_dir, basin)
# work_dir <- "../Working/"
Nsums_fn <- paste0(work_dir, "Nsums")
Psums_fn <- paste0(work_dir, "Psums")
NPsums_fn <- paste0(work_dir, "NPsums")

# External datasets
nutN_df <- LoadNutrientData(nut_dir, "nitrogen")
nutP_df <- LoadNutrientData(nut_dir, "phosphorus")
nutNP_df <- NutrientDataProduct(nutN_df, nutP_df)
basin_df <- readOGR(basin_fn, basin)

# Sum nutrient data over basins
Nsums_df <- SumNutrientsByBasin(nutN_df, basin_df)
Psums_df <- SumNutrientsByBasin(nutP_df, basin_df)
NPsums_df <- SumNutrientsByBasin(nutNP_df, basin_df)
write.table(Nsums_df, Nsums_fn)
write.table(Psums_df, Psums_fn)
write.table(NPsums_df, NPsums_fn)

# ATS Area needed, taking N or P limitation into account (Fig 3)
P2N <- P_prp/N_prp
lim_df <- NutrientLimits(nutP_df, nutN_df, P_prp, N_prp)
NP_lim_df <- TransformNutrientData(lim_df, fig_CRS, c("val", "rat", "nut", "area", "ATSarea", "biomass", "prod", "arearat"))
NP_lim_fn <- paste0(work_dir, "NP_lim")
write.table(NP_lim_df, NP_lim_fn)

area_df <- data.frame(long=lim_df$long, lat=lim_df$lat, val=lim_df$ATSarea) # ATSarea in ha
area_sums_df <- SumNutrientsByBasin(area_df, basin_df)
area_fn <- paste0(work_dir, "areas")
write.table(area_sums_df, area_fn)

#
# # Economic model assumptions
# The table of results, which includes the input parameters to the economic model,
# takes a long time to compute, so only do so if the saved data do not exist.
tblA3_df_fn <- paste0(work_dir, "tblA3")
if(!file.exists(tblA3_df_fn)) {
    # Useful values
    P2N <- P_prp/N_prp
    lim_df <- NutrientLimits(nutP_df, nutN_df, P_prp, N_prp) # Returns $val in tons (note: slower to pre-compute and load)
    basin_ct <- nrow(Nsums_df)

    # Trapezoid calculations
    trap_df <- data.frame(long=lim_df$long, lat=lim_df$lat, val=lim_df$arearat)
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
    ATSarea_df <- data.frame(id=id, basin_area=basin_area, ATS_area=area_sums$val, long=long, lat=lat, name=area_sums$name, nut=lims)

    # Numbers for Results Table
    tblA3_ttl <- "Table A3"

    # tblA3_fn <- paste0(tbl_dir, tblA3_ttl, ".docx")
    sig_figs <- 2
    head <- list(NULL, c("name", "val", "unit"))
    max_lat_nut <- max(max(abs(nutN_df[which(!is.na(nutN_df$val)),]$lat), na.rm=TRUE),
                       max(abs(nutP_df[which(!is.na(nutP_df$val)),]$lat), na.rm=TRUE))
    max_lat_prod <- mod_m * abs(max_lat_nut) + mod_b
    trap_N_ct <- sum(lim_df$nut=="N", na.rm=TRUE)
    trap_P_ct <- sum(lim_df$nut=="P", na.rm=TRUE)
    basin_P_ct <- sum(lims=="P", na.rm=TRUE)
    basin_N_ct <- sum(lims=="N", na.rm=TRUE)
    N_xs <- sum(nutN_df$val, na.rm=TRUE)
    P_xs <- sum(nutP_df$val, na.rm=TRUE)
    N_area <- sum(lim_df[which(lim_df$nut=="N"),]$ATSarea)
    P_area <- sum(lim_df[which(lim_df$nut=="P"),]$ATSarea)
    tot_area <- N_area + P_area # ha
    N_bas_area <- sum(ATSarea_df[which(ATSarea_df$nut=="N"),]$ATS_area)
    P_bas_area <- sum(ATSarea_df[which(ATSarea_df$nut=="P"),]$ATS_area)
    tot_bas_area <- N_bas_area + P_bas_area
    biomass <- sum(NP_lim_df$biomass, na.rm=TRUE) # t yr-1
    biomass_rat_world <- biomass/world_npp
    biomass_rat_ag <- biomass/ag_npp
    mean_prod <- biomass/tot_area # t ha-1 yr-1

    tblA3_df <- data.frame(var=c("N_xs",                           # excess N
                                 "P_xs",                           # excess P
                                 "trap_N_ct",                      # number of N-limited trapezoids
                                 "basin_N_ct",                     # number of N-limited basins
                                 "trap_P_ct",                      # number of P-limited trapezoids
                                 "basin_P_ct",                     # number of P-limited basins
                                 "prod_hi",                        # areal biomass productivity at equator
                                 "prod_lo",                        # areal biomass productivity at specified latitude
                                 "mod_lat_hi",                     # equator latitude
                                 "mod_lat_eq",                     # specified latitude
                                 "max_lat_nut",                    # max latitude with N or P
                                 "max_lat_prod",                   # prod at that latitude
                                 "N_area",                         # area needed for N
                                 "P_area",                         # area needed for P
                                 "tot_area",                       # total area needed
                                 "N_bas_area",                     # basin area needed for N
                                 "P_bas_area",                     # basin area needed for P
                                 "tot_bas_area",                   # total basin area needed
                                 "biomass",                        # total basin area needed
                                 "world_npp",                      # algae to world biomass NPP
                                 "ag_npp",                         # algae to world ag NPP
                                 "mean_prod"),                     # global prod over area required
                           val=c(N_xs,
                                 P_xs,
                                 trap_N_ct,
                                 basin_N_ct,
                                 trap_P_ct,
                                 basin_P_ct,
                                 prod_hi*1000,
                                 prod_lo*1000,
                                 mod_lat_hi,
                                 mod_lat_eq,
                                 max_lat_nut,
                                 max_lat_prod*1000,
                                 N_area,
                                 P_area,
                                 tot_area,
                                 N_bas_area,
                                 P_bas_area,
                                 tot_bas_area,
                                 biomass*1000,
                                 world_npp,
                                 ag_npp,
                                 mean_prod),
                           unit=c("kg yr-1",
                                  "kg yr-1",
                                  "ct",
                                  "ct",
                                  "ct",
                                  "ct",
                                  "kg ha-1 yr-1",
                                  "kg ha-1 yr-1",
                                  "deg",
                                  "deg",
                                  "deg",
                                  "kg ha-1 yr-1",
                                  "ha",
                                  "ha",
                                  "ha",
                                  "ha",
                                  "ha",
                                  "ha",
                                  "kg yr-1",
                                  "rat",
                                  "rat",
                                  "t ha-1 yr-1")
    )
    write.table(tblA3_df, tblA3_df_fn)
}
# else {
#     tblA3_df <- read.table(tblA3_df_fn, stringsAsFactors = FALSE)
# }
