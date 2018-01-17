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
source("Code/R/Settings.R")

# Assign R variables from pre-computed data frames. Done in two parts to handle
# numerical values separate from character values. The variables vars and ret are
# not used later but can be used when debugging manually.

# total land used to produce food: 49116227 km^2, 4.9116227 e+7 km^2
# 1 km^2 = 1e^6 m^2 = 100 ha
# total land used to produce food: 4.9116 e+9 ha
#
# area_needed * 2.47105 = 4.6032360e+7
#
# Biomass: 0.35 g carbs, 0.40 g protein
# Carbs: 19 kJ g-1; Protein: 17 kJ g-1
# 1 g biomass: 0.35 * 19 + 0.4 * 17 = 13.5 kJ g-1
# Equivalent energy: biomass * 13.5 kJ g-1; 1.35e+4 J g-1; 1.35e+10 J t-1
# Jyr <- 1.35e+10 * biomass
# Jsec <- Jyr/(365*24*60*60)
# format(Jsec, scientific=TRUE, digits=2) = 5.0e+11 W; 500 GW; 0.5 TW
# World = 18 TW
# 0.5/18 = 0.23%
# Mtoe = 11630 kWh
# 2015 Mtoe = 13647 ==> 13647 * 11630 ==> 158714610 kWh ==> 158714610/(365 * 24 * 1000)

NPlim_df <- read.table(NPlim_fn)
basin_areas <- read.table(area_fn)
area_needed <- sum(NPlim_df$ATSarea, na.rm=TRUE)

kg2t <- 0.001
kg2T <- 0.00110231
tha2Tac <- 2.723868587
ha2ac <- 2.47105
t2T <- 1.10231

vars <- read.table(paste0(work_dir, "pNutrientExcesses_vals"))
ret <- mapply(assign, as.character(vars$name), vars$val, MoreArgs = list(envir = .GlobalEnv))

vars <- read.table(paste0(work_dir, "pNutrientExcesses_txt"))
ret <- mapply(assign, as.character(vars$name), vars$val, MoreArgs = list(envir = .GlobalEnv))

vars <- read.table(paste0(work_dir, "econ"))
ret <- mapply(assign, as.character(vars$var), vars$val, MoreArgs = list(envir = .GlobalEnv))

# Methods/Parameters
p1 <- paste0("We assume biomass mass ratios of ",
             format(100 * C_prp, digits=2),
             "%, ",
             format(100 * N_prp, digits=2),
             "%, and ",
             format(100 * P_prp, digits=2),
             "% for C, N and P respectively, derived from the stoichiometric ratios of algal biomass {Ebeling et al., 2006, #88379; Hillebrand and Sommer, 1999, #12928}. CapEx and OpEx estimates are ultimately derived from actual construction and operation figures {Hoffman et al., 2017, #34706}. Values for economic parameters were chosen to represent best and worst cases for CapEx ($",
             SciNotString(cap_lo, 2),
             " ha-1 ($",
             SciNotString(cap_lo/ha2ac, 2),
             " ac-1) and $",
             SciNotString(cap_hi, 2),
             " $ ha-1 ($",
             SciNotString(cap_hi/ha2ac, 2),
             " ac-1 respectively)), OpEx ($",
             SciNotString(op_lo, 2),
             " ha-1 ($",
             SciNotString(op_lo/ha2ac, 2),
             " ac-1) and $",
             SciNotString(op_hi, 2),
             " ha-1 yr-1 ($",
             SciNotString(op_hi/ha2ac, 2),
             " ac-1) respectively)) and facility lifetime (",
             op_life_short,
             " yr for low CapEx and ",
             op_life_long,
             " yr for high Capex, respectively). Best and worst case OpEx values were chosen to represent fully encumbered lower and higher end middle-class salaries in the US and additional operational costs of 10% of CapEx (Table 3).",
             " The economic model creates an R data frame by populating the first row (year 1) with initial conditions then applies the model’s spending growth formula (",
             format(100 * inc_prp, digits = 0),
             "% yr-1), to that row to produce the next row (year 2), repeating the procedure for year 3 to year ",
             build_yr_hi,
             " . Constant spending is then applied similarly for the following ",
             final_yr - build_yr_hi,
             " yr. To determine the initial spending rate, we chose parameters such that once the system is fully built out, the minimum algal biomass is always sufficient to completely recycle the relevant nutrients."
)

# Results/Nutrient Excess
p2 <- paste0("Nutrient data {Potter et al., 2010, #43603} are organized on a 0.5’ × 0.5’ grid of latitude and longitude, with ",
            format(totalcells, big.mark=","),
            " total grid cells, ",
            format(Ncells, big.mark=","),
            " having N and P data and ",
            format(Pcells-Ncells, big.mark=","),
            " having P data only (Fig. 2). ",
            "Values represent kg of excess nutrient per grid cell, and are positive for N but positive or negative for P. We consider only positive values, as nutrient deficits are not transferrable among grid cells or basins. Excess N per grid cell ranges from ",
            0,
            " kg to ",
            SciNotString(cellmaxN, 2),
            " kg. Excess P per grid cell ranges from ",
            0,
            " kg to ",
            SciNotString(cellmaxP, 2),
            " kg. Half of the excess N and P is accounted for by ",
            format(cellhalfN, digits=2, zero.print=TRUE),
            "% and ",
            format(cellhalfP, digits=2, zero.print=TRUE),
            ".0% of their respective grid cells. The 405 major basins curated by the Global Runoff Data Center {Global Runoff Data Centre, 2007, #36285} are responsible for ",
            format(100*basinNpct, digits=2),
            "% of global nutrient excess, for both N and P (Tables 1, A1). Excess N per curated basin ranges from 0 to ",
            SciNotString(maxNval, 2),
            " kg yr-1. Excess P per basin ranges from 0 kg yr-1 to ",
            SciNotString(maxPval, 2),
            " kg yr-1. Half of the nutrient excess is contributed by five (N) or six (P) basins (Table 1). The Yangze contributes the most N, while the Ganges contributes the most P."
)

# Results/Area Required
basin_area <- sum(basin_areas$val)
biomass <- sum(NPlim_df$biomass, na.rm=TRUE)
p3 <- paste0("Globally, excess N and P applied to the 140 crops curated in the EarthStat datasets {West et al., 2014, #62583} total ",
            SciNotString(kg2t * totalNxs, 2),
            " t (",
            SciNotString(kg2T * totalNxs, 2),
            " T) and ",
            SciNotString(kg2t * totalPxs, 2),
            " t (",
            SciNotString(kg2T * totalPxs, 2),
            " T), respectively. We abbreviate metric tons with 't' and US tons with 'T'. Related to the Redfield ratios, these excesses imply N-limitation in ",
            format(sum(NPlim_df$nut=="N", na.rm=TRUE), big.mark=","),
            " grid cells and P-limitation in ",
            format(sum(NPlim_df$nut=="P", na.rm=TRUE), big.mark=","),
            " grid cells (Figure 4A). As complete recycling of a limiting nutrient in a given grid cell or basin would leave an excess of the non-limiting nutrient, we compute the algal cultivation area required to recycle P in N-limited grid cells, and to recycle N in P-limited grid cells, summing these areas over each basin. We assume equatorial productivity ",
            SciNotString(prod_lo, 2),
            "t ha-1 yr-1 (",
            SciNotString(tha2Tac * prod_lo, 2),
            " T ac-1 yr-1), and at ",
            mod_lat_hi,
            "°, at ",
            SciNotString(prod_hi, 2),
            " t ha-1 yr-1 (",
            SciNotString(tha2Tac * prod_hi, 2),
            " T ac-1 yr-1). Globally, ",
            SciNotString(area_needed, 2),
            " ha (",
            SciNotString(ha2ac * area_needed, 2),
            " ac) of algal cultivation area is required for complete N and P recycling. For nutrient recycling in the major basins curated in the GRDC data set, a total of ",
            SciNotString(basin_area, 2),
            " ha (",
            SciNotString(ha2ac * basin_area, 2),
            " ac) is required. The total biomass produced, ",
            SciNotString(biomass,2),
            " t yr-1 (",
            SciNotString(t2T * biomass, 2),
            " T yr-1), ",
            format(100 * biomass/world_npp, digits=2),
            "% of world net primary productivity, or approximately ",
            format(100 * biomass/ag_npp, digits=2),
            "% of world agriculture productivity."
)

p4 <- paste0("Our economic model for N and P recycling assumes continued application of excess agricultural nutrients at the current rate. ",
             "Our worst and best case scenarios bracket a range of potential capital and operational expenses, determined by floway cost and lifetime (CapEx), and operator salary (OpEx). ",
             "We adopt a simple investment scenario starting with an initial annual spending rate (best case: $",
             SciNotString(best_startNP, 2),
             " yr-1; worst case: ",
             SciNotString(worst_startNP, 2),
             " yr-1), that increases by ",
             SciNotString(100 * inc_prp, 2),
             "% each year for ",
             SciNotString(build_yr_hi, 2),
             " years, then remains at that level (best case: $",
             SciNotString(best_endNP, 2),
             " yr-1; worst case: $",
             SciNotString(worst_endNP, 2),
             " yr-1) for the next ",
             SciNotString(final_yr - build_yr_hi, 2),
             " years, setting the initial spending at a level that results in complete nutrient recycling after year ",
             SciNotString(build_yr_hi, 2),
             " (Table 3, Figure 5). This exponential spending increase, combined with the limited lifetime of an ATS facility produces the “ringing” phenomenon apparent in the plots of biomass production, while the sudden halt to investment growth at year 100 results in an apparent discontinuity apparent as a change in slope of biomass production.",
             " We apply the same tactic to estimate the effort needed to recycle net anthropogenic C, finding the initial investment rate (best case: $",
             SciNotString(best_startC, 2),
             " yr-1; worst case: $",
             SciNotString(worst_startC, 2),
             " yr-1) and final investment rate (best case: $",
             SciNotString(best_endC, 2),
             " yr-1; worst case: $",
             SciNotString(worst_endC, 2),
             " yr-1) to be an order of magnitude greater than for N and P recycling."
)