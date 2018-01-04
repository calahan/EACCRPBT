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
library(rtf)
source("Code/R/Settings.R")

# Table 2 (highest priority basins), and Table A2 (all basins)
n <- 15 # number of top basins to include in highest priority

# Folders and files
tbl2_title <- "Table 2"
tblA2_title <- "Table A2"
tbl2_fn <- paste0(tbl_dir, tbl2_title, ".doc")
tblA2_fn <- paste0(tbl_dir, tblA2_title, ".doc")
areas_fn <- paste0(work_dir, "areas")

# Read and order precomputed data
areas_df <- read.table(areas_fn)
areas_df <- areas_df[areas_df$val > 0,]
areas_df <- areas_df[order(areas_df$val, decreasing=TRUE),]
area_tot <- sum(areas_df$val)
area_cumsum <- cumsum(areas_df$val)/area_tot

tbl2_df <- data.frame(Basin=areas_df[1:n,]$name,
                      ATSArea=Sci2RTF(areas_df[1:n,]$val, digits=2),
                      ATSProp=Sci2RTF(areas_df[1:n,]$val/area_tot, digits=2),
                      ATSCum=Sci2RTF(area_cumsum[1:n], digits=2))
colnames(tbl2_df) <- c("Basin Name", "ATS Area (ha)", "ATS Area (prop.)", "ATS Area (cum. prop.)")

tblA2_df <- data.frame(Basin=areas_df$name,
                       ATSArea=Sci2RTF(areas_df$val, digits=2),
                       ATSProp=Sci2RTF(areas_df$val/area_tot, digits=2),
                       ATSCum=Sci2RTF(area_cumsum, digits=2))
colnames(tblA2_df) <- c("Basin Name", "ATS Area (ha)", "ATS Area (prop.)", "ATS Area (cum. prop.)")

tbl2_rtf <- RTF(tbl2_fn, width=8.5, height=11, font.size=12, omi=c(1,1,1,1))
addHeader(tbl2_rtf, "Table 2")
addTable(tbl2_rtf, tbl2_df)
done(tbl2_rtf)

tblA2_rtf <- RTF(tblA2_fn, width=8.5, height=11, font.size=12, omi=c(1,1,1,1))
addHeader(tblA2_rtf, "Table A2")
addTable(tblA2_rtf, tblA2_df)
done(tblA2_rtf)
