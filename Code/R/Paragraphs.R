library(Calahanlab)
source("Code/R/Settings.R")

vars <- read.table(paste0(work_dir, "pNutrientExcesses_vals"))
ret <- mapply(assign, as.character(vars$name), vars$val, MoreArgs = list(envir = .GlobalEnv))

vars <- read.table(paste0(work_dir, "pNutrientExcesses_txt"))
ret <- mapply(assign, as.character(vars), vars$val, MoreArgs = list(envir = .GlobalEnv))

p <- paste0("Nutrient data {Potter et al., 2010, #43603} are organized on a 0.5’ × 0.5’ grid of latitude and longitude, with ",
            format(totalcells, big.mark=","),
            " total grid cells, ",
            format(Ncells, big.mark=","),
            " having N and P data and ",
            format(Pcells-Ncells, big.mark=","),
            " having P data only (Fig. 2). ",
            "Values represent kg of excess nutrient per grid cell, and are positive for N but positive or negative for P. We consider only positive values, as nutrient deficits are not transferrable among grid cells or basins. Excess N and P total ",
            SciNotString(totalNxs, 2),
            " kg and ",
            SciNotString(totalPxs, 2),
            " kg, respectively. Excess N per grid cell ranges from ",
            0,
            " kg to ",
            SciNotString(cellmaxN, 2),
            " kg. Excess P per cell ranges from ",
            0,
            " kg to ",
            SciNotString(cellmaxP, 2),
            " kg {West et al., 2014, #62583}. Half of the excess N and P is accounted for by ",
            format(cellhalfN, digits=2, zero.print=TRUE),
            "% and ",
            format(cellhalfP, digits=2, zero.print=TRUE),
            ".0% of their respective grid cells. The 405 major basins curated by the Global Runoff Data Center are responsible for ",
            format(100*basinNpct, digits=2),
            "% of global nutrient excess, for both N and P (Table A1). Excess N per curated basin ranges from 0 to ",
            SciNotString(maxNval, 2),
            " kg yr-1. Excess P per basin ranges from 0 kg yr-1 to ",
            SciNotString(maxPval, 2),
            " kg yr-1. Half of the nutrient excess is contributed by five (N) or six (P) basins (Table 1). The Yangze contributes the most N, while the Ganges contributes the most P."
)


