library(rgdal)
library(tiff)

#
BasinNames <- function(df) {
# Cleaned up basin names for the basins specified by the indexes (ixs). Called by
# LoadNutrientData
      proc.names <- as.character(df$DRAINAGE)

      # Some name-specific replacements that afford more general ones. ISO_3166-1_alpha-2 two-letter country codes are used
      proc.names <- gsub("AMAZONAS", "AMAZON", proc.names)
      proc.names <- gsub("ARAL DRAINAGE", "ARAL SEA", proc.names)
      proc.names <- gsub("AUX MELEZES", "LARCH", proc.names)
      proc.names <- gsub("BALEINE, GRANDE RIVIERE DE LA", "WHALE", proc.names)
      proc.names <- gsub("BEI JIANG", "BEI", proc.names)
      proc.names <- gsub("BRAHMANI RIVER \\(BHAHMANI\\)", "BRAHMANI", proc.names)
      proc.names <- gsub("CHURCHILL RIVER", "CHURCHILL (CA-SK)", proc.names)
      proc.names <- gsub("CHURCHILL, FLEUVE \\(LABRADOR\\)", "CHURCHILL (CA-NL)", proc.names)
      proc.names <- gsub("COLORADO \\(ARGENTINIA\\)", "COLORADO (RA)", proc.names)
      proc.names <- gsub("COLORADO RIVER \\(CARIBBEAN SEA\\)", "COLORADO (CARIBBEAN)", proc.names)
      proc.names <- gsub("COLORADO RIVER \\(PACIFIC OCEAN\\)", "COLORADO (PACIFIC)", proc.names)
      proc.names <- gsub("DARYACHEH-YE ORUMIEH", "LAKE URMIA", proc.names)
      proc.names <- gsub("EEL RIVER \\(CALIF.\\)", "EEL", proc.names)
      proc.names <- gsub("ESCAUT \\(SCHELDE\\)", "SCHELDT", proc.names)
      proc.names <- gsub("FEUILLES \\(RIVIERE AUX\\)", "LEAF", proc.names)
      proc.names <- gsub("FITZROY", "FITZROY (AU-QLD)", proc.names)
      proc.names <- gsub("FITZROY \\(AU-QLD\\) RIVER", "FITZROY (AU-WA)", proc.names)
      proc.names <- gsub("GONO \\(GO\\)", "GONO", proc.names)
      proc.names <- gsub("GRANDE RIVIERE DE LA BALEINE", "GREAT WHALE", proc.names)
      proc.names <- gsub("HAN-GANG \\(HAN RIVER\\)", "HAN", proc.names)
      proc.names <- gsub("HAYES RIVER \\(TRIB. ARCTIC OCEAN\\)", "HAYES (ARCTIC OCEAN)", proc.names)
      proc.names <- gsub("HAYES RIVER \\(TRIB. HUDSON BAY\\)", "HAYES (HUDSON BAY)", proc.names)
      proc.names <- gsub("HONG\\(RED RIVER\\)", "HONG", proc.names)
      proc.names <- gsub("HUANG HE \\(YELLOW RIVER\\)", "HUANG", proc.names)
      proc.names <- gsub("MAHANADI RIVER \\(MAHAHADI\\)", "MAHANADI", proc.names)
      proc.names <- gsub("MANICOUAGAN \\(RIVIERE\\)", "MANICOUAGAN", proc.names)
      proc.names <- gsub("MITCHELL RIVER \\(N. AU\\)", "MITCHELL", proc.names)
      proc.names <- gsub("MOOSE RIVER \\(TRIB. HUDSON BAY\\)", "MOOSE", proc.names)
      proc.names <- gsub("NATASHQUAN \\(RIVIERE\\)", "NATASHQUAN", proc.names)
      proc.names <- gsub("NEGRO \\(ARGENTINIA\\)", "NEGRO (RA)", proc.names)
      proc.names <- gsub("NEGRO \\(URUGUAY\\)", "NEGRO (UY)", proc.names)
      proc.names <- gsub("NIZHNY VYG \\(SOROKA\\)", "NIZHNY VYG", proc.names)
      proc.names <- gsub("NORTHERN DVINA\\(SEVERNAYA DVINA\\)", "NORTHERN DVINA", proc.names)
      proc.names <- gsub("SAGUENAY \\(RIVIERE\\)", "SAGUENAY", proc.names)
      proc.names <- gsub("SAN JUAN$", "SAN JUAN (NI)", proc.names)
      proc.names <- gsub("SAN JUAN \\(COLUMBIA - PACIFIC\\)", "SAN JUAN (CO)", proc.names)
      proc.names <- gsub("SAGUENAY \\(RIVIERE\\)", "SAGUENAY", proc.names)
      proc.names <- gsub("SEVERN RIVER \\(TRIB. HUDSON BAY\\)", "SEVERN", proc.names)
      proc.names <- gsub("SOLO \\(BENGAWAN SOLO\\)", "SOLO", proc.names)
      proc.names <- gsub("SVARTA, SKAGAFIROI", "SVARTA", proc.names)
      proc.names <- gsub("TANA$", "TANA \\(KE\\)", proc.names)
      proc.names <- gsub("TRANH \\(NR THU BON\\)", "TRANH", proc.names)
      proc.names <- gsub("TRINITY RIVER \\(TEXAS\\)", "TRINITY", proc.names)
      proc.names <- gsub("WESTERN DVINA \\(DAUGAVA\\)", "WESTERN DVINA", proc.names)
      proc.names <- gsub("YANGTZE RIVER \\(CHANG JIANG\\)", "YANGTZE", proc.names)

      # eliminate river words
      proc.names <- gsub("( RIVER|RIO |RIBEIRA DO |GRANDE DE | HE)", "", proc.names)

      # add space to "ST."
      proc.names <- gsub("\\.", ". ", proc.names)

      # deal with capitilization
      proc.names <- CapWords(tolower(proc.names))
      proc.names <- gsub("Macarthur", "MacArthur", proc.names)
      proc.names <- gsub("Mackenzie", "MacKenzie", proc.names)
      proc.names <- gsub("\\(ca-sk\\)", "(CA-SK)", proc.names)
      proc.names <- gsub("\\(ca-nl\\)", "(CA-NL)", proc.names)
      proc.names <- gsub("\\(ra\\)", "(RA)", proc.names)
      proc.names <- gsub("\\(caribbean\\)", "(Caribbean)", proc.names)
      proc.names <- gsub("\\(pacific\\)", "(Pacific)", proc.names)
      proc.names <- gsub("\\(au-qld\\)", "(AU-QLD)", proc.names)
      proc.names <- gsub("\\(au-wa\\)", "(AU-WA)", proc.names)
      proc.names <- gsub("\\(ni\\)", "(NI)", proc.names)
      proc.names <- gsub("\\(co\\)", "(CO)", proc.names)
      proc.names <- gsub("\\(ke\\)", "(KE)", proc.names)
      proc.names <- gsub("\\(no, Fi\\)", "(NO, FI)", proc.names)
      proc.names <- gsub("Vaenern-goeta", "Vaenern-Goeta", proc.names)
      proc.names <- gsub("Vaza-barris", "Vaza-Barris", proc.names)

      return(as.vector(proc.names))
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
    lat.len <- latres * (circ / 360)
    long.len <- lat.len * cos(lat/deg2rad)
    return(lat.len * long.len)
}
#
DegreesPerMeter <- function(lat, rad=6378) {
   return(360 / (2 * pi * (rad * 1000) * cos(lat * 2 * pi / 360)))
}
#
EconDataFrame <- function(dollars.yr1, dollars.inc, years.start, years.total, prod, capex, opex, lifetime, worker.ha, worker.sal) {
# Generate economic data frames based on the following simple assumptions:
#   1) There is an initial spending rate
#   2) That rate increases up by a constant percentage annually
#   3) Spending increases stop after a certain number of years
#   4) CAPEX and OPEX
#
# dollars.yr1:  initial spending ($ yr-1)
# dollars.inc:  proportion to increase spending each year
# years.start:  how long before stopping spending growth
# years.total:  total years to calculate
# prod:         productivity (t ha-1 yr-1)
# capex:        capital expense ($ ha-1)
# opex:         operational expense ($ ha-1 yr-1)
# lifetime:     lifetime of a floway (yr)
# worker.ha:    number of ha a FTE can operate
# worker.sal:   fully encumbered FTE salary
#
      df <- data.frame(Year=rep(NA, years.total), Spending=NA, NewArea=NA, TotalArea=NA, CapEx=NA, OpEx=NA, CumSpend=NA, AlgalMass=NA, CRemoved=NA, NRemoved=NA, PRemoved=NA, CBaseline=NA, CNet=NA)
      df$Year <- seq(1, years.total)

      # Initialize and compute year 1
      df[1,]$Spending <- dollars.yr1
      df[1,]$OpEx <- 0
      df[1,]$CapEx <- df[1,]$Spending
      df[1,]$NewArea <- df[1,]$Spending / capex
      df[1,]$TotalArea <- df[1,]$NewArea
      df[1,]$CumSpend <- df[1,]$Spending
      df[1,]$AlgalMass <- df[1,]$TotalArea * prod
      df[1,]$CRemoved <- df[1,]$AlgalMass * C_prp
      df[1,]$NRemoved <- df[1,]$AlgalMass * N_prp
      df[1,]$PRemoved <- df[1,]$AlgalMass * P_prp
      df[1,]$CBaseline <- CO2.bl
      df[1,]$CNet <- df[1,]$CBaseline - df[1,]$CRemoved

      # Compute year 2 through full build-out
      for(i in 2:years.start) {
            df[i,]$Spending <- df[i-1,]$Spending + dollars.inc * df[i-1,]$Spending
            df[i,]$OpEx <- df[i-1,]$TotalArea * opex
            df[i,]$CapEx <- df[i,]$Spending - df[i,]$OpEx
            df[i,]$NewArea <- df[i,]$CapEx / capex
            df[i,]$TotalArea <- sum(df[1:i,]$NewArea)
            df[i,]$CumSpend <- sum(df[1:i,]$Spending)
            df[i,]$AlgalMass <- df[i,]$TotalArea * prod
            df[i,]$CRemoved <- df[i-1,]$CRemoved + df[i,]$AlgalMass * C_prp
            df[i,]$NRemoved <- df[i,]$AlgalMass * N_prp
            df[i,]$PRemoved <- df[i,]$AlgalMass * P_prp
            df[i,]$CBaseline <- df[i-1,]$CBaseline + CO2.gr
            df[i,]$CNet <- df[i,]$CBaseline - df[i,]$CRemoved
     }

      # Compute constant spending after full build-out
      for(i in (years.start + 1):years.total) {
            df[i,]$Spending <- df[years.start,]$Spending
            df[i,]$OpEx <- df[i-1,]$TotalArea * opex
            df[i,]$CapEx <- df[i,]$Spending - df[i,]$OpEx
            df[i,]$NewArea <- df[i,]$CapEx / capex
            df[i,]$TotalArea <- sum(df[(i-lifetime):i,]$NewArea) # note the formula difference
            df[i,]$CumSpend <- sum(df[1:i,]$Spending)
            df[i,]$AlgalMass <- df[i,]$TotalArea * prod
            df[i,]$CRemoved <- df[i-1,]$CRemoved + df[i,]$AlgalMass * C_prp
            df[i,]$NRemoved <- df[i,]$AlgalMass * N_prp
            df[i,]$PRemoved <- df[i,]$AlgalMass * P_prp
            df[i,]$CBaseline <- df[i-1,]$CBaseline + CO2.gr
            df[i,]$CNet <- df[i,]$CBaseline - df[i,]$CRemoved
      }
      return(df)
}
#
# Plot an economic model panel
# To plot series with different colnames, create new data frames with same colname
#[todo] This could be made more general
EconPanel <- function(df, names, lines, colname, divisor, xaxis, yaxis, legtitle, years, fn, theme.overrides, cols, wid.adj=1) {
    # data frame count
    df.ct <- length(df)

    # construct column of names
    name.col <- unlist(lapply(names, function(x) return(c(rep(x[1], years)))))

    # construct figure data frame
    col.extract <- paste0("x$", colname)
    fig.df <- data.frame(year=rep(seq(1,years), df.ct),
                         yvals=unlist(lapply(df, function(x) return(eval(parse(text=col.extract)))))/divisor,
                         group=as.factor(name.col)
                         )

    # impose order on legend display
    fig.df$group <- factor(fig.df$group, names)

    # plot and save
    fig <- ggplot(data=fig.df, aes(x=year, y=yvals, group=group, col=group, linetype=group)) +
        #geom_line(aes(linetype=group, color=group)) +
        geom_line() +
        scale_linetype_manual(values=lines) +
        scale_color_manual(values=cols) +
        theme.opts +
        theme.overrides +
        labs(x=xaxis, y=yaxis, linetype=legtitle, color=legtitle)

    ggsave(fn, plot=fig, width=(wid.adj*fig.wid/2)-fig.gap, height=(fig.hgt/2)-fig.gap, dpi=fig.ldpi)
}
#
LoadNutrientData <- function(dir, nutrient) {
# Load EarthStat nutrient data
#
# Spatial Reference: GCS_WGS_1984
# Datum: D_WGS_1984
#
# Notes: Saving the data frame with write.table then reloading doesn't necessarily
# work as a perf enhancment; loading the resulting file is slower than building
# from scratch.
    fn.frag <- "balanceonlandscape_140crops.txt"
    nut.fn <- paste0(dir, nutrient, fn.frag)
    nut.data <- ReadESRIAscii(nut.fn)
    nut.data[nut.data==0] <- NA

  # create a data frame from the gridded nutrient data
  nut.df <- data.frame(long=as.numeric(rep(dimnames(nut.data)[[2]],
                                           each=length(dimnames(nut.data)[[1]]))),
                       lat=as.numeric(rep(dimnames(nut.data)[[1]],
                                          length(dimnames(nut.data)[[2]]))))
  nut.df$val <- as.vector(nut.data)
  return(nut.df)
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
    wid.top <- hgt * cos((lat + dim/2) * 2 * pi / 360)
    wid.bot <- hgt * cos((lat - dim/2) * 2 * pi / 360)
    return(hgt * (wid.top + wid.bot) / 2)
}
#
MetersPerDegree <- function(lat, rad=6378) {
# For a given latitude, how many meters constitute a degree along that latitude.
# lat   latitude in degrees
# rad   radius in km
    return(2 * pi * ((rad * 1000) * cos(lat * 2 * pi / 360)) / 360)
}
#
NutrientDataProduct <- function(nut1.df, nut2.df) {
# Actually the sqrt of the product
    # Defined only where both nutrients exist
    t1 <- !is.na(nut1.df$val)
    t2 <- !is.na(nut2.df$val)
    t3 <- t1 & t2
    ret.df <- nut1.df
    ret.df$val <- NA
    t1.min <- min(nut1.df$val, na.rm=TRUE)
    t2.min <- min(nut2.df$val, na.rm=TRUE)
    ret.df[t3,]$val <- sqrt((nut1.df[t3,]$val - t1.min + 1) * (nut2.df[t3,]$val - t2.min + 1))
    return(ret.df)
}
#
NutrientLimits <- function(a.df, b.df, a.prop, b.prop) {
# [todo] This is only expected to work if a.df is P and b.df is N, a2b is P:N. Want a more general 2 nutrient analysis
# [todo] Very much hard coded here
# For analysing nutrient data based on coordinate locations, not basins.
# Create a data frame summarizing which nutrient is limiting, the area of each coordinate,
# The amount of biomass needed, and the amount of ATS area needed to supply that biomass
# Returns tons.
    a2b <- a.prop/b.prop
    rats <- a.df$val/b.df$val
    a.lim <- rats < a2b
    b.lim <- rats >= a2b
    a.ix <- which(a.lim)
    b.ix <- which(b.lim)
    a.negs <- a.df$val < 0
    b.negs <- b.df$val < 0 # not used currently
    lim.df <- data.frame(long=a.df$long, lat=a.df$lat, val=NA, rat=NA, nut=NA, area=NA, ATSarea=NA, biomass=NA)
    lim.df[a.ix,]$nut <- "P"
    lim.df[b.ix,]$nut <- "N"
    lim.df[a.ix,]$rat <- rats[which(a.lim)]             # [todo]is this used?
    lim.df[b.ix,]$rat <- rats[which(b.lim)]             # [todo]is this used?
    lim.df[a.ix,]$val <- b.df[a.ix,]$val/1000           # going from kg to t, want the value of the non-limiting nutrient
    lim.df[b.ix,]$val <- a.df[b.ix,]$val/1000           # going from kg to t, want the value of the non-limiting nutrient
    lim.df$area <- LatLongTrapezoidArea(lim.df$lat, 1/12)/(100*100) # function returns m^2, want ha
    lim.df[a.ix,]$biomass <- lim.df[a.ix,]$val/b.prop   # value is for non-limiting nutrient, in tons
    lim.df[b.ix,]$biomass <- lim.df[b.ix,]$val/a.prop   # value is for non-limiting nutrient, in tons
    lim.df$prod <- mod_m * abs(lim.df$lat) + mod_b      # in t ha-1 yr-1 (mod_m from Spreadsheet)
    lim.df$ATSarea <- lim.df$biomass/lim.df$prod        # in ha
    lim.df$arearat <- lim.df$ATSarea/lim.df$area        #

    return(lim.df)
}
#
RankedBandedPalette <- function(val, col.ct, col, prop) {
# Create a palette that smoothly goes from color to color, where size of each color
# band is defined by what proportion of the whole each band covers. Negative values
# only get one band, the first in the list. The proportions only apply to the >0
# values. Note that because of the way WhichFewResponsible() works, the band counts
# must be reversed at the very end
    if(min(val) < 0) {
        val <- val - min(val)
    }
    wfr <- unlist(lapply(prop, function(x) {WhichFewResponsible(val, x)} ))
    val.ln <- length(val)
    band.ct <- vector("numeric", length(col)-1)
    band.ct[1] <- wfr[1]
    for(i in 2:(length(band.ct)-1)) {
        band.ct[i] <- wfr[i] - wfr[i-1]
    }
    band.ct[length(band.ct)] <- val.ln - wfr[length(wfr)]
    band.ct <- round((band.ct/val.ln * col.ct), 0)
    bcl <- length(band.ct)
    band.ct[bcl] <- band.ct[bcl] + (col.ct - sum(band.ct))
    return(BandedPalette(col, rev(band.ct)))
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
# nut.df <- nutN.df
#
SumNutrientsByBasin <- function(nut.df, basin.df) {
# Returned list is ordered by nutrient balance
# nut.df: nutrient data frame; b.df: basin data frame
    sums <- vector('numeric', length=nrow(basin.df))
    for(poly in basin.df@polygons) {
        i <- as.numeric(poly@ID) + 1 # polygons slot ID is indexed from "0"
        cat(paste0(i, ':'))
        polyx <- poly@Polygons[[1]]@coords[,1]
        polyy <- poly@Polygons[[1]]@coords[,2]

        # bounding box for the polygon
        bb.ul.x <- min(polyx)
        bb.ul.y <- min(polyy)
        bb.lr.x <- max(polyx)
        bb.lr.y <- max(polyy)

        # subset coords from b.df
        coords <- nut.df[nut.df$long >= bb.ul.x &
                         nut.df$long <= bb.lr.x &
                         nut.df$lat >= bb.ul.y &
                         nut.df$lat <= bb.lr.y, ]
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
    basin.ct <- nrow(basin.df)
    long <- vector("numeric", basin.ct)
    lat <- vector("numeric", basin.ct)
    for(i in 1:nrow(basin.df)) {
        long[i] <- basin.df@polygons[[i]]@labpt[1]
        lat[i] <- basin.df@polygons[[i]]@labpt[2]
    }
    ret.df <- data.frame(id=basin.df$BASIN_ID, val=sums)
    ret.df$name <- BasinNames(basin.df)
    return(ret.df)
}
#
TransformNutrientData <- function(nut.df, proj, cols=c("val")) {
# Transform nutrient data to Robinson. [todo] Hard coded initial proj4string
    df <- data.frame(spTransform(
        SpatialPointsDataFrame(subset(nut.df, select=c(long, lat)),
                               subset(nut.df, select=cols),
                               proj4string = CRS('+init=epsg:4326')),
        CRS(proj)))
    return(df)
}
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
## Code under development goes here until it hasn't had any bugs for a while,
## then it gets alphabetized or placed into Calahanlab.R
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
Vals2Levels <- function(low, high, actual, max.level) {
# Convert boundary values to levels, with ceiling at max.levels
# Level "0" is rendered at set level 0.5 * max.levels so there is something to show
    zu.delta <- high - low # delta between low and high zones of uncertainty
    zu.zero <- low - zu.delta # origin of green zone (effective zero, can be < 0)
    sh.act <- actual - zu.zero # shift actual values down by the minimum level
    sh.hi <- high - zu.zero # shift high values down by the minimum level
    dup.levs <- function(t){return(c(t[1], t[1], t[2], t[2], t[3], t[3], t[4], t[4], t[5], t[6], t[7], t[7], t[8], t[8], t[9], t[10], t[11], t[11]))}
    new.vals <- mapply(min, round((2/3) * max.level * sh.act / sh.hi, 0), 100)
# cat("zu.delta, zu.zero\n")
# cat(zu.delta)
# cat("\n")
# cat(zu.zero)
# cat("\n")
# cat(new.vals)
# cat("\n")
# cat("sh.act, sh.hi, new.vals\n")
# cat(sh.act)
# cat("\n")
# cat(sh.hi)
# cat("\n")
# cat(new.vals)
# cat("\n")
    ret.levs <- dup.levs(mapply(min, round((2/3) * max.level * sh.act / sh.hi, 0), 100))
    ret.levs[which(ret.levs < 10)] <- 10
    return(ret.levs)
    #return(round((2/3) * max.level * sh.act / sh.hi, 0))
}
#
SimpleBoundEvolution <- function(Vin, Vzl, Vzh, year, CO2.base, CO2.rate) {
# Vin
# Vzl
# Vzh
# year
# CO2.base
# CO2.rate
# Basal calculation of evolution of boundary status over time
    Vret <- vector(mode="numeric", length=11)

    # Non-quantified boundary statuses (NE, AL, BIF, indices 1, 3, 9)
    for(i in c(1, 3, 9)) {
        Vret[i] <- NA
    }

    # Fiat values (OD, LC, indices=2, 7, 8)
    # OD assumed to linearly progress to pre-industrial in 50 yr
    if(year >= 50) {
        Vret[2] <- Vzl[2] # pre-industrial after 50 yr
    } else {
        Vret[2] <- Vzh[2] - (year / 50) * (Vzh[2] - Vzl[2]) # linear decline over 1st 50 yr
    }
    # FU increase 55% from baseline per 30 yr, or 1.8% per year
    Vret[7] <- Vin[7] + (0.55/30) * year * Vin[7]
    # LC losses assumed to be 13% from baseline per 30 yrs, or 0.43% per year
    Vret[8] <- Vin[8] + (0.13/30) * year * Vin[8]

    # Population-based values (BFN, BFP, FU, BIE, indices 5, 6, 10)
    pop.rat <- Population[year]/Population[1]
    for(i in c(5, 6, 10)) {
        Vret[i] <- pop.rat * Vin[i]
    }

    # CO2-based values (OA, CC, indices 4, 11)
    CO2.rat <- (CO2.base + year * CO2.rate) / CO2.base
    for(i in c(4, 11)) {
        Vret[i] <- CO2.rat * Vin[i]
    }

    return(Vret)
}
#
ComplexBoundEvolution <- function(Vin, Vzl, Vzh, year, CO2.base, CO2.rate, n.rem, p.rem) {
# Calculation of evolution of boundary status over time with N and P removal
    Vret <- vector(mode="numeric", length=11)

    # Non-quantified boundary statuses (NE, AL, BIF, indices 1, 3, 9)
    for(i in c(1, 3, 9)) {
        Vret[i] <- NA
    }

    # Fiat values (OD, LC, indices=2, 7, 8)
    # OD assumed to linearly progress to pre-industrial in 50 yr
    if(year >= 50) {
        Vret[2] <- Vzl[2] # pre-industrial after 50 yr
    } else {
        Vret[2] <- Vzh[2] - (year / 50) * (Vzh[2] - Vzl[2]) # linear decline over 1st 50 yr
    }
    # FU increase 55% from baseline per 30 yr, or 1.8% per year
    Vret[7] <- Vin[7] + (0.55/30) * year * Vin[7]
    # LC losses assumed to be 13% from baseline per 30 yrs, or 0.43% per year
    Vret[8] <- Vin[8] + (0.13/30) * year * Vin[8]

    # Population-based values (BFN, BFP, BIE, indices 5, 6, 10)
    pop.rat <- Population[year]/Population[1]
    for(i in c(5, 6, 10)) {
        Vret[i] <- pop.rat * Vin[i]
    }

    # Apply effect of nutrient removal (BFN, BFP, indices 5, 6)
    Vret[5] <- Vret[5] - n.rem
    Vret[6] <- Vret[6] - p.rem

    # CO2-based values (OA, CC, indices 4, 11)
    CO2.rat <- (CO2.base + year * CO2.rate) / CO2.base
    for(i in c(4, 11)) {
        Vret[i] <- CO2.rat * Vin[i]
    }

    return(Vret)
}
#
SimpleBoundFB <- function(Vin, Vtemp, Vbf, Vbr) {
# Simple Boundary Feedback
# Given initial values and first order change, calculate effect of changes on BI then effect of BI on first order

    Vret <- Vtemp # Statuses as affectd by first-order change
    delta.prop <- (Vtemp - Vin) / Vin # Proportional change of each status (vector)
    delta.BI.fwd <- sum(delta.prop * Vbf, na.rm = TRUE) # total proportional change on BI (scalar)
    Vret[10] <- Vret[10] + delta.BI.fwd * Vret[10] # Effect of status changes on BI (index = 10)
    delta.BI.rev <- delta.BI.fwd * Vbr # proportional changes from change in BI on statuses (vector)
    Vret <- Vret + delta.BI.rev * Vret # apply the feedback
    return(Vret)
}
#
ComplexBoundFB <- function(Vin, Vtemp, Vbf, Vbr, Vnpf) {
# Boundary Feedback with more interactions
# Given initial values and first order change, calculate effect of changes on BI then effect of BI on first order

    Vret <- Vtemp # Statuses as affectd by first-order change
    delta.prop <- (Vtemp - Vin) / Vin # Proportional change of each status (vector)
    delta.BI.fwd <- sum(delta.prop * Vbf, na.rm = TRUE) # total proportional change on BI (scalar)
    Vret[10] <- Vret[10] + delta.BI.fwd * Vret[10] # Effect of status changes on BI (index = 10)
    Vret[4]  <- Vret[4] + delta.prop[5] * Vnpf[4] * Vret[4] # Effect of NP removal on OA (index 4)
    Vret[11] <- Vret[11] + delta.prop[11] * Vnpf[11] * Vret[11] #Effect of NP removal on CC (index 11)
    delta.BI.rev <- delta.BI.fwd * Vbr # proportional changes from change in BI on statuses (vector)
    Vret <- Vret + delta.BI.rev * Vret # apply the feedback
    return(Vret)
}
#
RadialBarPlot <- function(plot.data, cols, hole.size, fact.ct, grey.ct, fig.fn, fig.wid, fig.hgt, dpi.raster) {
# expand a vector of numerics to create data for a stacked bar chart, then create
# the chart
#[note]Generates an ignorable warning: "Stacking not well defined when ymin != 0"
#[todo]Hard coded & specific to Figure 3
#
# plot.data     a vector of numerics
# cols          color palette
# fact.ct       total number of factors for data display
# grey.ct       number of factors for grey (NA) display
# fig.fn        output filename
# fig.wid       inches
# fig.hgt       inches
# dpi.raster       dots per inch
#
      plot.df <- expand(plot.data, hole.size, fact.ct, grey.ct)
      rplot <- ggplot() +
            geom_bar(data=plot.df, width = 1, stat='identity', aes(factor(factor), value, fill = factor(value))) +
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
      ggsave(fig.fn, plot=rplot, width=fig.wid, height=fig.hgt, dpi=dpi.raster)
}
#
expand <- function(plot.data, hole.size, bar.height, grey.ct) {
# Helper function for RadialBarPlot
# Converts vector plot.data into a set of factors for a stacked radial bar chart.
# plot.data: Height of each bar; NA indicates use of upper palette
# hole.size: Number of levels to be used for the hole (value == 1)
# bar.height: Number of levels in the data. Total bar height == hole.size + bar.height
#[todo] perhaps there is a standard data-reshaping method for doing this
#
      edf <- data.frame()
      for(i in 1:length(plot.data)) {
            if(!is.na(plot.data[[i]])) {
                  edf <- rbind(edf, data.frame(
                      factor=rep(i, plot.data[[i]] + hole.size),
                      value=c(1:hole.size, (hole.size+1):(plot.data[[i]]+hole.size)))
                  )
            } else {
                  edf <- rbind(edf, data.frame(
                      factor=rep(i, grey.ct + hole.size),
                      value=c(1:hole.size, (hole.size+bar.height+1):(bar.height+grey.ct+hole.size)))
                  )
            }
      }
      return(edf)
}
#
ExpandSubLabels <- function(labs) {
# Expand the encoded sublabels into a full string
    ret.val <- vector(mode="character", length=2*length(labs))
    for(i in 1:length(labs)) {
        if(is.na(labs[i])) {
            ret.val[2 * i - 1] <- ""
            ret.val[2 * i] <- ""
        } else {
            sub.lab <- strsplit(labs[i], ";")
            ret.val[2 * i - 1] <- sub.lab[[1]][1]
            ret.val[2 * i] <- sub.lab[[1]][2]
        }
    }
    return(ret.val)
}
