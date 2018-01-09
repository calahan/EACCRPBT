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

# Table 2 (highest priority basins), and Table A1 (all basins)
# [todo] Note there still needs to be some manual editing of the generated RTF files.
# [todo] Specifically look at rounding and replacing × with ◊.

n <- 15 # number of top basins to include in highest priority

# Folders and files
tbl2_title <- "Table 2"
tblA1_title <- "Table A1"
tbl2_fn <- paste0(tbl_dir, tbl2_title, ".doc")
tblA1_fn <- paste0(tbl_dir, tblA1_title, ".doc")
areas_fn <- paste0(work_dir, "areas")

# Read and order precomputed data
areas_df <- read.table(areas_fn)
areas_df <- areas_df[areas_df$val > 0,]
areas_df <- areas_df[order(areas_df$val, decreasing=TRUE),]
area_tot <- sum(areas_df$val)
area_cumsum <- cumsum(areas_df$val)/area_tot

# Using format() on a vector, with numbers beyond its summarizing (e.g. digits=2 for 0.0001) defaults to ugly. Pre-clean to avoid.
area_prop <- areas_df$val/area_tot
area_prop_cl <- format(100 * area_prop[area_prop > .001], digits=2)
area_prop_cl <- c(area_prop_cl, rep("< 1", length(area_prop) - length(area_prop_cl)))

tbl2_df <- data.frame(Basin=areas_df[1:n,]$name,
                      ATSArea=Sci2RTF(areas_df[1:n,]$val, digits=2),
                      ATSProp=format(100 * areas_df[1:n,]$val/area_tot, digits=2, scientific=FALSE),
                      ATSCum=format(100 * area_cumsum[1:n], digits=2))
colnames(tbl2_df) <- c("Basin Name", "ATS Area (ha)", "ATS Area (%, vs. basin)", "ATS Area (%, vs. total)")

tblA1_df <- data.frame(Basin=areas_df$name,
                       ATSArea=Sci2RTF(areas_df$val, digits=2),
                       ATSProp=area_prop_cl,
                       ATSCum=format(100 * area_cumsum, digits=2))
colnames(tblA1_df) <- c("Basin Name", "ATS Area (ha)", "ATS Area (%, vs. basin)", "ATS Area (%, vs. total)")

tbl2_rtf <- RTF(tbl2_fn, width=8.5, height=11, font.size=12, omi=c(1,1,1,1))
addHeader(tbl2_rtf, "Table 2")
addTable(tbl2_rtf, tbl2_df)
done(tbl2_rtf)

tblA1_rtf <- RTF(tblA1_fn, width=8.5, height=11, font.size=12, omi=c(1,1,1,1))
addHeader(tblA1_rtf, "Table A1")
addTable(tblA1_rtf, tblA1_df)
done(tblA1_rtf)
