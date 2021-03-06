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
source("Code/R/EACCRPBT.R")

# External datasets
nutN_df <- LoadNutrientData(nut_dir, "nitrogen")
nutP_df <- LoadNutrientData(nut_dir, "phosphorus")
basin_df <- readOGR(basin_fn, basin)

# Calculate nutrient data per trapezoid
lim_df <- NutrientLimits(nutP_df, nutN_df, P_prp, N_prp) # $val in tons

# Sum nutrient data over basins
Nsums_df <- SumNutrientsByBasin(nutN_df, basin_df)
Psums_df <- SumNutrientsByBasin(nutP_df[!is.na(nutP_df$val) & nutP_df$val > 0,], basin_df) # ignore P < 0
write.table(Nsums_df, Nsums_fn)
write.table(Psums_df, Psums_fn)

# ATS Area needed, taking N or P limitation into account
P2N <- P_prp/N_prp
NPlim_df <- NutrientLimits(nutP_df, nutN_df, P_prp, N_prp)
write.table(NPlim_df, NPlim_fn)
area_df <- data.frame(long=lim_df$long, lat=lim_df$lat, val=lim_df$ATSarea) # ATSarea in ha
area_sums_df <- SumNutrientsByBasin(area_df, basin_df)
write.table(area_sums_df, area_fn)

# SciNotString <- function(num, digits) {
#     exp_floor <- floor(log10(num))
#     significand <- format(num/10^exp_floor, digits=digits)
#     return(paste0(significand, " × 10^", exp_floor))
# }
