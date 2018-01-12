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
library(rgdal)
library(tiff)
library(Calahanlab)
source("Code/R/Settings.R")

#
BasinNames <- function(df) {
# Cleaned up basin names for the basins specified by the indexes (ixs). Called by
# LoadNutrientData
      proc_names <- as.character(df$DRAINAGE)

      # Some name-specific replacements that afford more general ones. ISO_3166-1_alpha-2 two-letter country codes are used
      proc_names <- gsub("AMAZONAS", "AMAZON", proc_names)
      proc_names <- gsub("ARAL DRAINAGE", "ARAL SEA", proc_names)
      proc_names <- gsub("AUX MELEZES", "LARCH", proc_names)
      proc_names <- gsub("BALEINE, GRANDE RIVIERE DE LA", "WHALE", proc_names)
      proc_names <- gsub("BEI JIANG", "BEI", proc_names)
      proc_names <- gsub("BRAHMANI RIVER \\(BHAHMANI\\)", "BRAHMANI", proc_names)
      proc_names <- gsub("CHURCHILL RIVER", "CHURCHILL (CA-SK)", proc_names)
      proc_names <- gsub("CHURCHILL, FLEUVE \\(LABRADOR\\)", "CHURCHILL (CA-NL)", proc_names)
      proc_names <- gsub("COLORADO \\(ARGENTINIA\\)", "COLORADO (RA)", proc_names)
      proc_names <- gsub("COLORADO RIVER \\(CARIBBEAN SEA\\)", "COLORADO (CARIBBEAN)", proc_names)
      proc_names <- gsub("COLORADO RIVER \\(PACIFIC OCEAN\\)", "COLORADO (PACIFIC)", proc_names)
      proc_names <- gsub("DARYACHEH-YE ORUMIEH", "LAKE URMIA", proc_names)
      proc_names <- gsub("EEL RIVER \\(CALIF.\\)", "EEL", proc_names)
      proc_names <- gsub("ESCAUT \\(SCHELDE\\)", "SCHELDT", proc_names)
      proc_names <- gsub("FEUILLES \\(RIVIERE AUX\\)", "LEAF", proc_names)
      proc_names <- gsub("FITZROY", "FITZROY (AU-QLD)", proc_names)
      proc_names <- gsub("FITZROY \\(AU-QLD\\) RIVER", "FITZROY (AU-WA)", proc_names)
      proc_names <- gsub("GONO \\(GO\\)", "GONO", proc_names)
      proc_names <- gsub("GRANDE RIVIERE DE LA BALEINE", "GREAT WHALE", proc_names)
      proc_names <- gsub("HAN-GANG \\(HAN RIVER\\)", "HAN", proc_names)
      proc_names <- gsub("HAYES RIVER \\(TRIB. ARCTIC OCEAN\\)", "HAYES (ARCTIC OCEAN)", proc_names)
      proc_names <- gsub("HAYES RIVER \\(TRIB. HUDSON BAY\\)", "HAYES (HUDSON BAY)", proc_names)
      proc_names <- gsub("HONG\\(RED RIVER\\)", "HONG", proc_names)
      proc_names <- gsub("HUANG HE \\(YELLOW RIVER\\)", "HUANG", proc_names)
      proc_names <- gsub("MAHANADI RIVER \\(MAHAHADI\\)", "MAHANADI", proc_names)
      proc_names <- gsub("MANICOUAGAN \\(RIVIERE\\)", "MANICOUAGAN", proc_names)
      proc_names <- gsub("MITCHELL RIVER \\(N. AU\\)", "MITCHELL", proc_names)
      proc_names <- gsub("MOOSE RIVER \\(TRIB. HUDSON BAY\\)", "MOOSE", proc_names)
      proc_names <- gsub("NATASHQUAN \\(RIVIERE\\)", "NATASHQUAN", proc_names)
      proc_names <- gsub("NEGRO \\(ARGENTINIA\\)", "NEGRO (RA)", proc_names)
      proc_names <- gsub("NEGRO \\(URUGUAY\\)", "NEGRO (UY)", proc_names)
      proc_names <- gsub("NIZHNY VYG \\(SOROKA\\)", "NIZHNY VYG", proc_names)
      proc_names <- gsub("NORTHERN DVINA\\(SEVERNAYA DVINA\\)", "NORTHERN DVINA", proc_names)
      proc_names <- gsub("SAGUENAY \\(RIVIERE\\)", "SAGUENAY", proc_names)
      proc_names <- gsub("SAN JUAN$", "SAN JUAN (NI)", proc_names)
      proc_names <- gsub("SAN JUAN \\(COLUMBIA - PACIFIC\\)", "SAN JUAN (CO)", proc_names)
      proc_names <- gsub("SAGUENAY \\(RIVIERE\\)", "SAGUENAY", proc_names)
      proc_names <- gsub("SEVERN RIVER \\(TRIB. HUDSON BAY\\)", "SEVERN", proc_names)
      proc_names <- gsub("SOLO \\(BENGAWAN SOLO\\)", "SOLO", proc_names)
      proc_names <- gsub("SVARTA, SKAGAFIROI", "SVARTA", proc_names)
      proc_names <- gsub("TANA$", "TANA \\(KE\\)", proc_names)
      proc_names <- gsub("TRANH \\(NR THU BON\\)", "TRANH", proc_names)
      proc_names <- gsub("TRINITY RIVER \\(TEXAS\\)", "TRINITY", proc_names)
      proc_names <- gsub("WESTERN DVINA \\(DAUGAVA\\)", "WESTERN DVINA", proc_names)
      proc_names <- gsub("YANGTZE RIVER \\(CHANG JIANG\\)", "YANGTZE", proc_names)

      # eliminate river words
      proc_names <- gsub("( RIVER|RIO |RIBEIRA DO |GRANDE DE | HE)", "", proc_names)

      # add space to "ST."
      proc_names <- gsub("\\.", ". ", proc_names)

      # deal with capitilization
      proc_names <- CapWords(tolower(proc_names))
      proc_names <- gsub("Macarthur", "MacArthur", proc_names)
      proc_names <- gsub("Mackenzie", "MacKenzie", proc_names)
      proc_names <- gsub("\\(hudson", "(Hudson", proc_names)
      proc_names <- gsub("\\(ca-sk\\)", "(CA-SK)", proc_names)
      proc_names <- gsub("\\(ca-nl\\)", "(CA-NL)", proc_names)
      proc_names <- gsub("\\(ra\\)", "(RA)", proc_names)
      proc_names <- gsub("\\(caribbean\\)", "(Caribbean)", proc_names)
      proc_names <- gsub("\\(pacific\\)", "(Pacific)", proc_names)
      proc_names <- gsub("\\(au-qld\\)", "(AU-QLD)", proc_names)
      proc_names <- gsub("\\(au-wa\\)", "(AU-WA)", proc_names)
      proc_names <- gsub("\\(ni\\)", "(NI)", proc_names)
      proc_names <- gsub("\\(co\\)", "(CO)", proc_names)
      proc_names <- gsub("\\(ke\\)", "(KE)", proc_names)
      proc_names <- gsub("\\(no, Fi\\)", "(NO, FI)", proc_names)
      proc_names <- gsub("Vaenern-goeta", "Vaenern-Goeta", proc_names)
      proc_names <- gsub("Vaza-barris", "Vaza-Barris", proc_names)

      return(as.vector(proc_names))
}
#
CoordArea <- function(lat, longres=1/12, latres=1/12, rad=6378) {
# Return approximate area of spherical quadrilateral with given dimensions centered
# at lat. Dimensions: Km^2
# longres: degrees along longitude; default is 5 min or 0.08333333 deg
# longres: degrees along latitude; default is 5 min or 0.08333333 deg
# [todo] could do a trapezoid but why not just go all the way to spherical coords
    deg2rad <- 180/pi
    circ <- 2 * pi * rad
    lat_len <- latres * (circ / 360)
    long_len <- lat_len * cos(lat/deg2rad)
    return(lat_len * long_len)
}
#
DegreesPerMeter <- function(lat, rad=6378) {
   return(360 / (2 * pi * (rad * 1000) * cos(lat * 2 * pi / 360)))
}
#
EconDataFrame <- function(dollars_yr1, dollars_inc, years_start, years_total, prod, capex, opex, lifetime, worker_ha, worker_sal) {
# Generate economic data frames based on the following simple assumptions:
#   1) There is an initial spending rate
#   2) That rate increases up by a constant percentage annually
#   3) Spending increases stop after a certain number of years
#   4) CAPEX and OPEX
#
# dollars_yr1:  initial spending ($ yr-1)
# dollars_inc:  proportion to increase spending each year
# years_start:  how long before stopping spending growth
# years_total:  total years to calculate
# prod:         productivity (t ha-1 yr-1)
# capex:        capital expense ($ ha-1)
# opex:         operational expense ($ ha-1 yr-1)
# lifetime:     lifetime of a floway (yr)
# worker_ha:    number of ha a FTE can operate
# worker_sal:   fully encumbered FTE salary
#
      df <- data.frame(Year=rep(NA, years_total), Spending=NA, NewArea=NA, TotalArea=NA, CapEx=NA, OpEx=NA, CumSpend=NA, AlgalMass=NA, CRemoved=NA, NRemoved=NA, PRemoved=NA, CBaseline=NA, CNet=NA)
      df$Year <- seq(1, years_total)

      # Initialize and compute year 1
      df[1,]$Spending <- dollars_yr1
      df[1,]$OpEx <- 0
      df[1,]$CapEx <- df[1,]$Spending
      df[1,]$NewArea <- df[1,]$Spending / capex
      df[1,]$TotalArea <- df[1,]$NewArea
      df[1,]$CumSpend <- df[1,]$Spending
      df[1,]$AlgalMass <- df[1,]$TotalArea * prod
      df[1,]$CRemoved <- df[1,]$AlgalMass * C_prp
      df[1,]$NRemoved <- df[1,]$AlgalMass * N_prp
      df[1,]$PRemoved <- df[1,]$AlgalMass * P_prp
      df[1,]$CBaseline <- CO2_bl
      df[1,]$CNet <- df[1,]$CBaseline - df[1,]$CRemoved

      # Compute year 2 through full build-out
      for(i in 2:years_start) {
            df[i,]$Spending <- df[i-1,]$Spending + dollars_inc * df[i-1,]$Spending
            df[i,]$OpEx <- df[i-1,]$TotalArea * opex
            df[i,]$CapEx <- df[i,]$Spending - df[i,]$OpEx
            df[i,]$NewArea <- df[i,]$CapEx / capex
            df[i,]$TotalArea <- sum(df[1:i,]$NewArea)
            df[i,]$CumSpend <- sum(df[1:i,]$Spending)
            df[i,]$AlgalMass <- df[i,]$TotalArea * prod
            df[i,]$CRemoved <- df[i-1,]$CRemoved + df[i,]$AlgalMass * C_prp
            df[i,]$NRemoved <- df[i,]$AlgalMass * N_prp
            df[i,]$PRemoved <- df[i,]$AlgalMass * P_prp
            df[i,]$CBaseline <- df[i-1,]$CBaseline + CO2_gr
            df[i,]$CNet <- df[i,]$CBaseline - df[i,]$CRemoved
     }

      # Compute constant spending after full build-out
      for(i in (years_start + 1):years_total) {
            df[i,]$Spending <- df[years_start,]$Spending
            df[i,]$OpEx <- df[i-1,]$TotalArea * opex
            df[i,]$CapEx <- df[i,]$Spending - df[i,]$OpEx
            df[i,]$NewArea <- df[i,]$CapEx / capex
            df[i,]$TotalArea <- sum(df[(i-lifetime):i,]$NewArea) # note the formula difference
            df[i,]$CumSpend <- sum(df[1:i,]$Spending)
            df[i,]$AlgalMass <- df[i,]$TotalArea * prod
            df[i,]$CRemoved <- df[i-1,]$CRemoved + df[i,]$AlgalMass * C_prp
            df[i,]$NRemoved <- df[i,]$AlgalMass * N_prp
            df[i,]$PRemoved <- df[i,]$AlgalMass * P_prp
            df[i,]$CBaseline <- df[i-1,]$CBaseline + CO2_gr
            df[i,]$CNet <- df[i,]$CBaseline - df[i,]$CRemoved
      }
      return(df)
}
#
# Plot an economic model panel
# To plot series with different colnames, create new data frames with same colname
#[todo] This could be made more general
EconPanel <- function(df, names, lines, colname, divisor, xaxis, yaxis, legtitle, years, fn, theme_overrides, cols, wid_adj=1) {
    # data frame count
    df_ct <- length(df)

    # construct column of names
    name_col <- unlist(lapply(names, function(x) return(c(rep(x[1], years)))))

    # construct figure data frame
    col_extract <- paste0("x$", colname)
    fig_df <- data.frame(year=rep(seq(1,years), df_ct),
                         yvals=unlist(lapply(df, function(x) return(eval(parse(text=col_extract)))))/divisor,
                         group=as.factor(name_col)
                         )

    # impose order on legend display
    fig_df$group <- factor(fig_df$group, names)

    # plot and save
    fig <- ggplot(data=fig_df, aes(x=year, y=yvals, group=group, col=group, linetype=group)) +
        #geom_line(aes(linetype=group, color=group)) +
        geom_line() +
        scale_linetype_manual(values=lines) +
        scale_color_manual(values=cols) +
        theme_opts +
        theme_overrides +
        labs(x=xaxis, y=yaxis, linetype=legtitle, color=legtitle)

    ggsave(fn, plot=fig, width=(wid_adj*fig_wid/2)-fig_gap, height=(fig_hgt/2)-fig_gap, dpi=fig_ldpi)
}
#
LoadNutrientData <- function(dir, nutrient) {
# Load EarthStat nutrient data
#
# Spatial Reference: GCS_WGS_1984
# Datum: D_WGS_1984
#
# [todo] look into read.asciigrid, ESRI ascii grid loader
# Notes: Saving the data frame with write.table then reloading doesn't necessarily
# work as a perf enhancment; loading the resulting file is slower than building
# from scratch.
    fn_frag <- "balanceonlandscape_140crops.txt"
    nut_fn <- paste0(dir, nutrient, fn_frag)
    nut_data <- ReadESRIAscii(nut_fn)
    nut_data[nut_data==0] <- NA

  # create a data frame from the gridded nutrient data
  nut_df <- data.frame(long=as.numeric(rep(dimnames(nut_data)[[2]],
                                           each=length(dimnames(nut_data)[[1]]))),
                       lat=as.numeric(rep(dimnames(nut_data)[[1]],
                                          length(dimnames(nut_data)[[2]]))))
  nut_df$val <- as.vector(nut_data)
  return(nut_df)
}
#
LatLongTrapezoidArea <- function(lat, dim, rad=6.371e+6) {
# Return approximate area of lat/long isosceles trapezoid, in m^2
#       ___
#      / | \
#     /--|--\---wid (defined by latitude & dim)
#    /___|___\
#        |
#       hgt (defined by dim)
#
    circ <- 2 * pi * rad
    hgt <- circ * dim / 360
    wid_top <- hgt * cos((lat + dim/2) * 2 * pi / 360)
    wid_bot <- hgt * cos((lat - dim/2) * 2 * pi / 360)
    return(hgt * (wid_top + wid_bot) / 2)
}
#
MetersPerDegree <- function(lat, rad=6378) {
# For a given latitude, how many meters constitute a degree along that latitude.
# lat   latitude in degrees
# rad   radius in km
    return(2 * pi * ((rad * 1000) * cos(lat * 2 * pi / 360)) / 360)
}
#
RankNutrientData <- function(nut1_df, nut2_df) {
# Heuristic: Locations with excess of both nut1 and nut2 rank highest. Where nut_1$val
# and nut_2$val are postive, the ranking metric is the square root of their product.
# Locations
# Defined only where both nutrients exist
    t1 <- !is.na(nut1_df$val) & nut1_df$val > 0
    t2 <- !is.na(nut2_df$val) & nut2_df$val > 0
    t3 <- t1 & t2
    ret_df <- nut1_df
    ret_df$val <- NA
    t1_min <- min(nut1_df$val, na.rm=TRUE)
    t2_min <- min(nut2_df$val, na.rm=TRUE)
    ret_df[t3,]$val <- sqrt((nut1_df[t3,]$val - t1_min + 1) * (nut2_df[t3,]$val - t2_min + 1))
    return(ret_df)
}
#
NutrientLimits <- function(a_df, b_df, a_prop, b_prop) {
# [todo] This is only expected to work if a_df is P and b_df is N, a2b is P:N. Want a more general 2 nutrient analysis
# [todo] Very much hard coded here
# For analysing nutrient data based on coordinate locations, not basins.
# Create a data frame summarizing which nutrient is limiting, the area of each coordinate,
# The amount of biomass needed, and the amount of ATS area needed to supply that biomass
# Returns tons.
    a2b <- a_prop/b_prop
    rats <- a_df$val/b_df$val
    a_lim <- rats < a2b
    b_lim <- rats >= a2b
    a_ix <- which(a_lim)
    b_ix <- which(b_lim)
    a_negs <- a_df$val < 0
    b_negs <- b_df$val < 0 # not used currently
    lim_df <- data.frame(long=a_df$long, lat=a_df$lat, val=NA, rat=NA, nut=NA, area=NA, ATSarea=NA, biomass=NA)
    lim_df[a_ix,]$nut <- "P"
    lim_df[b_ix,]$nut <- "N"
    lim_df[a_ix,]$val <- b_df[a_ix,]$val/1000           # going from kg to t, want the value of the non-limiting nutrient
    lim_df[b_ix,]$val <- a_df[b_ix,]$val/1000           # going from kg to t, want the value of the non-limiting nutrient
    lim_df$area <- LatLongTrapezoidArea(lim_df$lat, 1/12)/(100*100) # function returns m^2, want ha
    lim_df[a_ix,]$biomass <- lim_df[a_ix,]$val/b_prop   # value is for non-limiting nutrient, in tons
    lim_df[b_ix,]$biomass <- lim_df[b_ix,]$val/a_prop   # value is for non-limiting nutrient, in tons
    lim_df$prod <- mod_m * abs(lim_df$lat) + mod_b      # in t ha-1 yr-1 (mod_m from Spreadsheet)
    lim_df$ATSarea <- lim_df$biomass/lim_df$prod        # in ha
    lim_df$arearat <- lim_df$ATSarea/lim_df$area        #

    return(lim_df)
}
#
RankedBandedPalette <- function(val, col_ct, col, prop) {
# Create a palette that smoothly goes from color to color, where size of each color
# band is defined by what proportion of the whole each band covers. Negative values
# only get one band, the first in the list. The proportions only apply to the >0
# values. Note that because of the way WhichFewResponsible() works, the band counts
# must be reversed at the very end
    if(min(val) < 0) {
        val <- val - min(val)
    }
    wfr <- unlist(lapply(prop, function(x) {WhichFewResponsible(val, x)} ))
    val_ln <- length(val)
    band_ct <- vector("numeric", length(col)-1)
    band_ct[1] <- wfr[1]
    for(i in 2:(length(band_ct)-1)) {
        band_ct[i] <- wfr[i] - wfr[i-1]
    }
    band_ct[length(band_ct)] <- val_ln - wfr[length(wfr)]
    band_ct <- round((band_ct/val_ln * col_ct), 0)
    bcl <- length(band_ct)
    band_ct[bcl] <- band_ct[bcl] + (col_ct - sum(band_ct))
    return(BandedPalette(col, rev(band_ct)))
}
#
ReadESRIAscii <- function(filename) {
# Note that the original nitrogen file has -1.#IND instead of 0.
#      thefile <- paste0(path, '/', filename, '\n')
      cat(paste0(filename, '\n'))
      ESRIHeader <- read.table(filename, nrows=6)
      ESRIHeader <- data.frame(ESRIHeader$V2, row.names=ESRIHeader$V1)
      names(ESRIHeader)[1]<-'value'

      #ESRIData <- t(apply(as.matrix(read.table(filename, skip=6, nrows=ESRIHeader['nrows',], na.strings=c(ESRIHeader['NODATA_value',]))), 2, rev))
      # For ggplot, must flip across horizontal axis, for raster, must not.
      #ESRIData <- as.matrix(read.table(filename, skip=6, nrows=ESRIHeader['nrows',], na.strings=c(ESRIHeader['NODATA_value',])))
      ESRIData <- apply(as.matrix(read.table(filename,
                                             skip=6,
                                             nrows=ESRIHeader['nrows',],
                                             na.strings=c(ESRIHeader['NODATA_value', ],
                                                          '-1.#IND'))),
                        2,
                        rev)

      # name rows and columns with the text of their degree location
      # is that a fencepost error at the end? It seems as if there is a missing column
      incr <- ESRIHeader['cellsize', ]

      start <- ESRIHeader['xllcorner', ]
      end <- start + ((ESRIHeader['ncols', ] - 1) * incr)
      colnames(ESRIData) <- as.character(seq(start, end, incr))

      start <- ESRIHeader['yllcorner',]
      end <- start + ((ESRIHeader['nrows',] - 1) * incr)
      rownames(ESRIData) <- as.character(seq(start, end, incr))

      return(ESRIData)
}
# Minimum algal biomass productivity once spending is steady
SSMinAlgalMass <- function(df, verbose=FALSE) {
      whichmax <- which(df$AlgalMass==max(df$AlgalMass))
      #cat(paste0("SSMinAlgalMass which: ", whichmax, "\n"))
      return(min(df[whichmax:nrow(df),]$AlgalMass))
}
#
# Minimum carbon removal rate once spending is steady
SSMinCarbon <- function(df, verbose=FALSE) {
      whichmax <- which(df$AlgalMass==max(df$AlgalMass))
      #cat(paste0("SSMinAlgalMass which: ", whichmax, "\n"))
      return(min(df[whichmax:nrow(df),]$AlgalMass))
}
#
# nut_df <- nutN_df
#
SumNutrientsByBasin <- function(nut_df, basin_df) {
# Returned list is ordered by nutrient balance
# nut_df: nutrient data frame; b_df: basin data frame
    sums <- vector('numeric', length=nrow(basin_df))
    for(poly in basin_df@polygons) {
        i <- as.numeric(poly@ID) + 1 # polygons slot ID is indexed from "0"
        cat(paste0(i, ':'))
        polyx <- poly@Polygons[[1]]@coords[,1]
        polyy <- poly@Polygons[[1]]@coords[,2]

        # bounding box for the polygon
        bb_ul_x <- min(polyx)
        bb_ul_y <- min(polyy)
        bb_lr_x <- max(polyx)
        bb_lr_y <- max(polyy)

        # subset coords from b_df
        coords <- nut_df[nut_df$long >= bb_ul_x &
                         nut_df$long <= bb_lr_x &
                         nut_df$lat >= bb_ul_y &
                         nut_df$lat <= bb_lr_y, ]
        if(sum(coords$val, na.rm = TRUE) == 0) {
            sums[[i]] <- 0
        } else {
            pip <- as.logical(point.in.polygon(coords$long,
                                               coords$lat,
                                               polyx,
                                               polyy))
            sums[[i]] <- sum(coords[pip, ]$val, na.rm = TRUE)
        }
    cat(paste0(sums[[i]], '\n'))
    }

    #[todo] there must be an R way to multi-access polygons[[]] but I haven't found it yet
    basin_ct <- nrow(basin_df)
    long <- vector("numeric", basin_ct)
    lat <- vector("numeric", basin_ct)
    for(i in 1:nrow(basin_df)) {
        long[i] <- basin_df@polygons[[i]]@labpt[1]
        lat[i] <- basin_df@polygons[[i]]@labpt[2]
    }
    ret_df <- data.frame(id=basin_df$BASIN_ID, val=sums)
    ret_df$name <- BasinNames(basin_df)
    return(ret_df)
}
#
TransformNutrientData <- function(nut_df, proj, cols=c("val")) {
# Transform nutrient data to Robinson.
    df <- data.frame(spTransform(
        SpatialPointsDataFrame(subset(nut_df, select=c(long, lat)),
                               subset(nut_df, select=cols),
                               proj4string = CRS('+init=epsg:4326')), # hard-coded just to have something
        CRS(proj)))
    return(df)
}
# ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
# ## Code under development goes here until it hasn't had any bugs for a while,
# ## then it gets alphabetized or placed into Calahanlab.R
# ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
# Vals2Levels <- function(low, high, actual, max_level) {
# # Convert boundary values to levels, with ceiling at max_levels
# # Level "0" is rendered at set level 0.5 * max_levels so there is something to show
#     zu_delta <- high - low # delta between low and high zones of uncertainty
#     zu_zero <- low - zu_delta # origin of green zone (effective zero, can be < 0)
#     sh_act <- actual - zu_zero # shift actual values down by the minimum level
#     sh_hi <- high - zu_zero # shift high values down by the minimum level
#     dup_levs <- function(t){return(c(t[1], t[1], t[2], t[2], t[3], t[3], t[4], t[4], t[5], t[6], t[7], t[7], t[8], t[8], t[9], t[10], t[11], t[11]))}
#     new_vals <- mapply(min, round((2/3) * max_level * sh_act / sh_hi, 0), 100)
# # cat("zu_delta, zu_zero\n")
# # cat(zu_delta)
# # cat("\n")
# # cat(zu_zero)
# # cat("\n")
# # cat(new_vals)
# # cat("\n")
# # cat("sh_act, sh_hi, new_vals\n")
# # cat(sh_act)
# # cat("\n")
# # cat(sh_hi)
# # cat("\n")
# # cat(new_vals)
# # cat("\n")
#     ret_levs <- dup_levs(mapply(min, round((2/3) * max_level * sh_act / sh_hi, 0), 100))
#     ret_levs[which(ret_levs < 10)] <- 10
#     return(ret_levs)
#     #return(round((2/3) * max_level * sh_act / sh_hi, 0))
# }
#
# SimpleBoundEvolution <- function(Vin, Vzl, Vzh, year, CO2_base, CO2_rate) {
# # Vin
# # Vzl
# # Vzh
# # year
# # CO2_base
# # CO2_rate
# # Basal calculation of evolution of boundary status over time
#     Vret <- vector(mode="numeric", length=11)
#
#     # Non-quantified boundary statuses (NE, AL, BIF, indices 1, 3, 9)
#     for(i in c(1, 3, 9)) {
#         Vret[i] <- NA
#     }
#
#     # Fiat values (OD, LC, indices=2, 7, 8)
#     # OD assumed to linearly progress to pre-industrial in 50 yr
#     if(year >= 50) {
#         Vret[2] <- Vzl[2] # pre-industrial after 50 yr
#     } else {
#         Vret[2] <- Vzh[2] - (year / 50) * (Vzh[2] - Vzl[2]) # linear decline over 1st 50 yr
#     }
#     # FU increase 55% from baseline per 30 yr, or 1.8% per year
#     Vret[7] <- Vin[7] + (0.55/30) * year * Vin[7]
#     # LC losses assumed to be 13% from baseline per 30 yrs, or 0.43% per year
#     Vret[8] <- Vin[8] + (0.13/30) * year * Vin[8]
#
#     # Population-based values (BFN, BFP, FU, BIE, indices 5, 6, 10)
#     pop_rat <- Population[year]/Population[1]
#     for(i in c(5, 6, 10)) {
#         Vret[i] <- pop_rat * Vin[i]
#     }
#
#     # CO2-based values (OA, CC, indices 4, 11)
#     CO2_rat <- (CO2_base + year * CO2_rate) / CO2_base
#     for(i in c(4, 11)) {
#         Vret[i] <- CO2_rat * Vin[i]
#     }
#
#     return(Vret)
# }
# #
# ComplexBoundEvolution <- function(Vin, Vzl, Vzh, year, CO2_base, CO2_rate, n_rem, p_rem) {
# # Calculation of evolution of boundary status over time with N and P removal
#     Vret <- vector(mode="numeric", length=11)
#
#     # Non-quantified boundary statuses (NE, AL, BIF, indices 1, 3, 9)
#     for(i in c(1, 3, 9)) {
#         Vret[i] <- NA
#     }
#
#     # Fiat values (OD, LC, indices=2, 7, 8)
#     # OD assumed to linearly progress to pre-industrial in 50 yr
#     if(year >= 50) {
#         Vret[2] <- Vzl[2] # pre-industrial after 50 yr
#     } else {
#         Vret[2] <- Vzh[2] - (year / 50) * (Vzh[2] - Vzl[2]) # linear decline over 1st 50 yr
#     }
#     # FU increase 55% from baseline per 30 yr, or 1.8% per year
#     Vret[7] <- Vin[7] + (0.55/30) * year * Vin[7]
#     # LC losses assumed to be 13% from baseline per 30 yrs, or 0.43% per year
#     Vret[8] <- Vin[8] + (0.13/30) * year * Vin[8]
#
#     # Population-based values (BFN, BFP, BIE, indices 5, 6, 10)
#     pop_rat <- Population[year]/Population[1]
#     for(i in c(5, 6, 10)) {
#         Vret[i] <- pop_rat * Vin[i]
#     }
#
#     # Apply effect of nutrient removal (BFN, BFP, indices 5, 6)
#     Vret[5] <- Vret[5] - n_rem
#     Vret[6] <- Vret[6] - p_rem
#
#     # CO2-based values (OA, CC, indices 4, 11)
#     CO2_rat <- (CO2_base + year * CO2_rate) / CO2_base
#     for(i in c(4, 11)) {
#         Vret[i] <- CO2_rat * Vin[i]
#     }
#
#     return(Vret)
# }
#
SimpleBoundFB <- function(Vin, Vtemp, Vbf, Vbr) {
# Simple Boundary Feedback
# Given initial values and first order change, calculate effect of changes on BI then effect of BI on first order

    Vret <- Vtemp # Statuses as affectd by first-order change
    delta_prop <- (Vtemp - Vin) / Vin # Proportional change of each status (vector)
    delta_BI_fwd <- sum(delta_prop * Vbf, na.rm = TRUE) # total proportional change on BI (scalar)
    Vret[10] <- Vret[10] + delta_BI_fwd * Vret[10] # Effect of status changes on BI (index = 10)
    delta_BI_rev <- delta_BI_fwd * Vbr # proportional changes from change in BI on statuses (vector)
    Vret <- Vret + delta_BI_rev * Vret # apply the feedback
    return(Vret)
}
#
ComplexBoundFB <- function(Vin, Vtemp, Vbf, Vbr, Vnpf) {
# Boundary Feedback with more interactions
# Given initial values and first order change, calculate effect of changes on BI then effect of BI on first order

    Vret <- Vtemp # Statuses as affectd by first-order change
    delta_prop <- (Vtemp - Vin) / Vin # Proportional change of each status (vector)
    delta_BI_fwd <- sum(delta_prop * Vbf, na.rm = TRUE) # total proportional change on BI (scalar)
    Vret[10] <- Vret[10] + delta_BI_fwd * Vret[10] # Effect of status changes on BI (index = 10)
    Vret[4]  <- Vret[4] + delta_prop[5] * Vnpf[4] * Vret[4] # Effect of NP removal on OA (index 4)
    Vret[11] <- Vret[11] + delta_prop[11] * Vnpf[11] * Vret[11] #Effect of NP removal on CC (index 11)
    delta_BI_rev <- delta_BI_fwd * Vbr # proportional changes from change in BI on statuses (vector)
    Vret <- Vret + delta_BI_rev * Vret # apply the feedback
    return(Vret)
}
#
RadialBarPlot <- function(plot_data, cols, hole_size, fact_ct, grey_ct, fig_fn, fig_wid, fig_hgt, dpi_raster) {
# expand a vector of numerics to create data for a stacked bar chart, then create
# the chart
#[note]Generates an ignorable warning: "Stacking not well defined when ymin != 0"
#[todo]Hard coded & specific to Figure 3
#
# plot_data     a vector of numerics
# cols          color palette
# fact_ct       total number of factors for data display
# grey_ct       number of factors for grey (NA) display
# fig_fn        output filename
# fig_wid       inches
# fig_hgt       inches
# dpi_raster       dots per inch
#
      plot_df <- expand(plot_data, hole_size, fact_ct, grey_ct)
      rplot <- ggplot() +
            geom_bar(data=plot_df, width = 1, stat='identity', aes(factor(factor), value, fill = factor(value))) +
            coord_polar(start=(2*pi)/18, direction=1) +
            scale_fill_manual(values=cols) +
            geom_vline(aes(xintercept=c(0.5, 2.5, 4.5, 6.5, 8.5, 10.5, 12.5, 14.5, 16.5),
                           yintercept=c(93, 93, 93, 93, 93, 93, 93, 93, 93)),
                       color="lightgrey",
                       size=0.2) +
            theme(legend.position='none',
                  axis.title.x=element_blank(),
                  axis.title.y=element_blank(),
                  axis.text = element_blank(),
                  axis.ticks = element_blank(),
                  panel.background = element_rect(fill = 'white'))
      ggsave(fig_fn, plot=rplot, width=fig_wid, height=fig_hgt, dpi=dpi_raster)
}
#
expand <- function(plot_data, hole_size, bar_height, grey_ct) {
# Helper function for RadialBarPlot
# Converts vector plot_data into a set of factors for a stacked radial bar chart.
# plot_data: Height of each bar; NA indicates use of upper palette
# hole_size: Number of levels to be used for the hole (value == 1)
# bar_height: Number of levels in the data. Total bar height == hole_size + bar_height
#[todo] perhaps there is a standard data-reshaping method for doing this
#
      edf <- data.frame()
      for(i in 1:length(plot_data)) {
            if(!is.na(plot_data[[i]])) {
                  edf <- rbind(edf, data.frame(
                      factor=rep(i, plot_data[[i]] + hole_size),
                      value=c(1:hole_size, (hole_size+1):(plot_data[[i]]+hole_size)))
                  )
            } else {
                  edf <- rbind(edf, data.frame(
                      factor=rep(i, grey_ct + hole_size),
                      value=c(1:hole_size, (hole_size+bar_height+1):(bar_height+grey_ct+hole_size)))
                  )
            }
      }
      return(edf)
}
#
ExpandSubLabels <- function(labs) {
# Expand the encoded sublabels into a full string
    ret_val <- vector(mode="character", length=2*length(labs))
    for(i in 1:length(labs)) {
        if(is.na(labs[i])) {
            ret_val[2 * i - 1] <- ""
            ret_val[2 * i] <- ""
        } else {
            sub_lab <- strsplit(labs[i], ";")
            ret_val[2 * i - 1] <- sub_lab[[1]][1]
            ret_val[2 * i] <- sub_lab[[1]][2]
        }
    }
    return(ret_val)
}

#[todo] Preserving this code for later
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
