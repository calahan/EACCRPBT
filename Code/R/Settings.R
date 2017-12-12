library(ggplot2)

# Directory names
work_dir <- "Code/Working/"                 # PanDoc friendly for intermediate results
cont_dir <-  "Data/NaturalEarth/"           # Continents shapefile
lake_dir <-  "Data/GRDC/"                   # Lakes shapefile
river_dir <-  "Data/GRDC/"                  # Rivers shapefile
basin_dir <- "Data/GRDC/"                   # Basin polygons shapefile
nut_dir <- "Data/EarthStat/FertilizerBalance_Ascii/"    # ESRI grid data of nutrients
ss_dir <- "Data/Spreadsheets/"              # ATS locations
fig_dir <- "Visual Elements/Figures/"       # Figures
tbl_dir <- "Visual Elements/Tables/"        # Tables

# File names
areas <- "areas"
bbox <- "ne_110m_wgs84_bounding_box"
cont <- "ne_110m_land"
lake <- "GRDC_lakes_join_rivers"
river <- "GRDC_687_rivers"
basin <- "GRDC_405_basins_from_mouth"
Nsums <- "Nsums"
Psums <- "Psums"
NPsums <- "NPSums"
NP_lim <- "NPlim"

# Fully qualified path names
area_fn <- paste0(work_dir, areas)
bbox_fn <- paste0(cont_dir, bbox)
cont_fn <- paste0(cont_dir, cont)
lake_fn <- paste0(lake_dir, lake)
river_fn <- paste0(river_dir, river)
basin_fn <- paste0(basin_dir, basin)
Nsums_fn <- paste0(work_dir, Nsums)
Psums_fn <- paste0(work_dir, Psums)
NPsums_fn <- paste0(work_dir, NPsums)
NP_lim_fn <- paste0(work_dir, NP_lim)

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
#pop_df <- gdata::read.xls(ss_fn, 9)
#Population <- pop_df$population
