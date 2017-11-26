library(Calahanlab)
library(rtf)
source("EACCRPBT.R")
source("Settings.R")

nutN.df <- LoadNutrientData(nut.dir, "nitrogen")
nutP.df <- LoadNutrientData(nut.dir, "phosphorus")
Nsums.df <- read.table(Nsums.fn)
Psums.df <- read.table(Psums.fn)

# Useful values
P2N <- P.prp/N.prp
lim.df <- NutrientLimits(nutP.df, nutN.df, P.prp, N.prp) # Returns $val in tons
basin.ct <- nrow(Nsums.df)

# Trapezoid calculations
trap.df <- data.frame(long=lim.df$long, lat=lim.df$lat, val=lim.df$arearat)
ttrap.df <- TransformNutrientData(trap.df, fig.CRS)
ttrap.val <- ttrap.df[which(!is.na(ttrap.df$val)),]$val
s.ttrap.val <- sort(ttrap.val, decreasing=TRUE)
trap.10 <- max(which(s.ttrap.val > 0.1))
trap.01 <- max(which(s.ttrap.val > 0.01))
ntrap.90 <- WhichFewResponsible(s.ttrap.val, 0.9)
ntrap.50 <- WhichFewResponsible(s.ttrap.val, 0.5)

sum.rats <- Psums.df$val/Nsums.df$val
sum.rats[is.nan(sum.rats) | is.infinite(sum.rats)] <- NA
lims <- vector("character", length=basin.ct)
lims[1:basin.ct] <- NA
lims[sum.rats >= P2N] <- "N"
lims[sum.rats < P2N] <- "P"
plot.df <- merge(ftbasin.df, data.frame(id=1:basin.ct, lim=lims, val=sum.rats), by="id")

area.sums <- read.table(paste0(work.dir, "areas")) # [todo]
s.areas <- sort(unique(area.sums$val), decreasing=TRUE)
areas.90 <- WhichFewResponsible(s.areas, 0.9)
areas.50 <- WhichFewResponsible(s.areas, 0.5)

# Create data frame for plotting ranked area per basin
polys <- basin.df@polygons
data <- basin.df@data
id <- vector(mode="numeric", length=basin.ct)
basin.area <- vector(mode="numeric", length=basin.ct)
long <- vector(mode="numeric", length=basin.ct)
lat <- vector(mode="numeric", length=basin.ct)
for(i in 1:basin.ct) {
    id[i] <- as.numeric(polys[[i]]@ID) + 1
    basin.area[i] <- data[i,]$AREA_CALC
    long[i] <- polys[[i]]@labpt[1]
    lat[i] <- polys[[i]]@labpt[2]
}
ATSarea.df <- data.frame(id=id, basin.area=basin.area, ATS.area=area.sums$val, long=long, lat=lat, name=area.sums$name, nut=lims)
ftarea.sums <- merge(ftbasin.df, area.sums, by="id")

# Last minute code mods, not nicely integrated yet. Hard deadline.
#tbl2.ttl <- "Table 2"
tblA1.ttl <- "Table A1"
tblA1.fn <- paste0(tbl.dir, "Table A1.docx")
tblA1df.fn <- paste0(work.dir, "tbl2a")
tblA1.df <- read.table(tblA1df.fn)
t <- merge(tblA1.df, ATSarea.df, by="id")
tbl2.df <- data.frame(id=t$id, drainage=t$drainage, Nexcess=t$Nexcess, Pexcess=t$Pexcess, NPexcess=t$NPexcess, area=t$area, ATS.area=t$ATS.area)
tbl2.np.df <- tbl2.df[order(tbl2.df$NPexcess, decreasing=TRUE),]
tbl2.dr.df <- tbl2.df[order(tbl2.df$drainage),]
tbl2.dr.df$Algaefraction <- tbl2.dr.df$ATS.area/tbl2.dr.df$area

ftbl2.dr.df <- data.frame(BasinName=tbl2.dr.df$drainage,
                          CombinedNPexcess=format(tbl2.dr.df$NPexcess, scientific=TRUE, digits=2),
                          Nexcess=format(tbl2.dr.df$Nexcess, scientific=TRUE, digits=2),
                          Pexcess=format(tbl2.dr.df$Pexcess, scientific=TRUE, digits=2),
                          Basinarea=format(tbl2.dr.df$area, scientific=TRUE, digits=2),
                          Algaearea=format(tbl2.dr.df$ATS.area, scientific=TRUE, digits=2),
                          Algaefraction=format(tbl2.dr.df$Algaefraction, scientific=TRUE, digits=2)
)
tryCatch(WordTable(tblA1.fn, ftbl2.dr.df, 7, tblA1.ttl), finally=print("OK"))

# Create data frame for plotting squares of ATS surface area, with coordinates in degrees
sq.wid.deg <- sqrt(area.sums$val * 1000) # [todo]playing with multipliers to see what appears on map
ATSarea.df$sq.wid <- sq.wid.deg * DegreesPerMeter(ATSarea.df$lat)
ats.poly <- list()
for(i in 1:basin.ct) {
    if(ATSarea.df[i,]$ATS.area > 0) {
        long <- ATSarea.df[i,]$long
        lat <- ATSarea.df[i,]$lat
        wid <- ATSarea.df[i,]$sq.wid/2
        ats.poly[[i]] <- SpatialPolygons(list(Polygons(list(Polygon(matrix(c(long + wid, lat + wid, # upper left
                                long - wid, lat + wid, # upper right
                                long - wid, lat - wid, # lower right
                                long + wid, lat - wid), # lower left
                              ncol=2,
                              byrow=TRUE))), i)))
        proj4string(ats.poly[[i]]) <- CRS("+proj=longlat +datum=WGS84")
    } else {
        ats.poly[[i]] <- NA
    }
}
ats.poly <- ats.poly[which(!is.na(ats.poly))]
ats.poly.sp <- SpatialPolygons(lapply(ats.poly, function(x){x@polygons[[1]]}))
ats.poly.df <- SpatialPolygonsDataFrame(Sr=ats.poly.sp, data=data.frame(i=1:length(ats.poly.sp)),FALSE)
proj4string(ats.poly.df) <- CRS("+proj=longlat +datum=WGS84")
ftats.poly <- fortify(spTransform(ats.poly.df, CRS(fig.CRS)))

# Numbers for Results Table
tblA3_ttl <- "Table A3"

# tblA3.fn <- paste0(tbl.dir, tblA3.ttl, ".docx")
sig.figs <- 2
head <- list(NULL, c("name", "val", "unit"))
max.lat.nut <- max(max(abs(nutN.df[which(!is.na(nutN.df$val)),]$lat), na.rm=TRUE),
                   max(abs(nutP.df[which(!is.na(nutP.df$val)),]$lat), na.rm=TRUE))
max.lat.prod <- mod.m * abs(max.lat.nut) + mod.b
trap.N.ct <- sum(lim.df$nut=="N", na.rm=TRUE)
trap.P.ct <- sum(lim.df$nut=="P", na.rm=TRUE)
basin.P.ct <- sum(lims=="P", na.rm=TRUE)
basin.N.ct <- sum(lims=="N", na.rm=TRUE)
N.xs <- sum(nutN.df$val, na.rm=TRUE)
P.xs <- sum(nutP.df$val, na.rm=TRUE)
N.area <- sum(lim.df[which(lim.df$nut=="N"),]$ATSarea)
P.area <- sum(lim.df[which(lim.df$nut=="P"),]$ATSarea)
tot.area <- N.area + P.area # ha
N.bas.area <- sum(ATSarea.df[which(ATSarea.df$nut=="N"),]$ATS.area)
P.bas.area <- sum(ATSarea.df[which(ATSarea.df$nut=="P"),]$ATS.area)
tot.bas.area <- N.bas.area + P.bas.area
