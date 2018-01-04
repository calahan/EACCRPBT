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
#source("EACCRPBT.R")
source("Code/R/Settings.R")

#[todo] include something like this:
# Create data frame for plotting ranked area per basin
# polys <- basin_df@polygons
# data <- basin_df@data
# id <- vector(mode="numeric", length=basin_ct)
# basin_area <- vector(mode="numeric", length=basin_ct)
# long <- vector(mode="numeric", length=basin_ct)
# lat <- vector(mode="numeric", length=basin_ct)
# for(i in 1:basin_ct) {
#     id[i] <- as.numeric(polys[[i]]@ID) + 1
#     basin_area[i] <- data[i,]$AREA_CALC
#     long[i] <- polys[[i]]@labpt[1]
#     lat[i] <- polys[[i]]@labpt[2]
# }
# ATSarea_df <- data.frame(id=id, basin_area=basin_area, ATs_area=area_sums$val, long=long, lat=lat, name=area_sums$name, nut=lims)
# ftarea_sums <- merge(ftbasin_df, area_sums, by="id")
#
# # Last minute code mods, not nicely integrated yet. Hard deadline.
# #tbl2_ttl <- "Table 2"
# tblA1_ttl <- "Table A1"
# tblA1_fn <- paste0(tbl.dir, "Table A1.docx")
# tblA1df_fn <- paste0(work.dir, "tbl2a")
# tblA1_df <- read.table(tblA1df_fn)
# t <- merge(tblA1_df, ATSarea_df, by="id")
# tbl2_df <- data.frame(id=t$id, drainage=t$drainage, Nexcess=t$Nexcess, Pexcess=t$Pexcess, NPexcess=t$NPexcess, area=t$area, ATs_area=t$ATs_area)
# tbl2.np_df <- tbl2_df[order(tbl2_df$NPexcess, decreasing=TRUE),]
# tbl2.dr_df <- tbl2_df[order(tbl2_df$drainage),]
# tbl2.dr_df$Algaefraction <- tbl2.dr_df$ATs_area/tbl2.dr_df$area
# ftbl2.dr_df <- data.frame(BasinName=tbl2.dr_df$drainage,
#                           CombinedNPexcess=format(tbl2.dr_df$NPexcess, scientific=TRUE, digits=2),
#                           Nexcess=format(tbl2.dr_df$Nexcess, scientific=TRUE, digits=2),
#                           Pexcess=format(tbl2.dr_df$Pexcess, scientific=TRUE, digits=2),
#                           Basinarea=format(tbl2.dr_df$area, scientific=TRUE, digits=2),
#                           Algaearea=format(tbl2.dr_df$ATs_area, scientific=TRUE, digits=2),
#                           Algaefraction=format(tbl2.dr_df$Algaefraction, scientific=TRUE, digits=2)

# Table 2 (highest priority basins), and Table A2 (all basins)
n <- 15 # number of top basins to include in highest priority

# Folders and files
tbl2_title <- "Table 2"
tblA2_title <- "Table A2"
tbl2_fn <- paste0(tbl_dir, tbl2_title, ".docx")
tblA2_fn <- paste0(tbl_dir, tblA2_title, ".docx")
areas_fn <- paste0(work_dir, "areas")

# Read and order precomputed data
areas_df <- read.table(areas_fn)
areas_df <- areas_df[areas_df$val > 0,]
areas_df <- areas_df[order(areas_df$val, decreasing=TRUE),]
area_tot <- sum(areas_df$val)

tbl2_df <- data.frame(Basin=areas_df[1:n,]$name, ATSArea=areas_df[1:n,]$val, ATSProp=areas_df[1:n,]$val/area_tot)
tblA2_df <- data.frame(Basin=areas_df$name, ATSArea=areas_df$val, ATSProp=areas_df$val/area_tot)

WordTable(tbl2_fn, tbl2_df, 3, tbl2_title)
WordTable(tblA2_fn, tblA2_df, 3, tblA2_title)