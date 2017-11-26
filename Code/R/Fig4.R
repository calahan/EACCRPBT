library(Calahanlab)
library(ggplot2)
library(rgdal)
source("EACCRPBT.R")
source("Settings.R")

this.fig.dir <- paste0(fig.dir, "Figure 4/")
fig.fn <- paste0(this.fig.dir, "Figure 4.tiff")
A.fn <- paste0(this.fig.dir, "A.tiff")
B.fn <- paste0(this.fig.dir, "B.tiff")
C.fn <- paste0(this.fig.dir, "C.tiff")
D.fn <- paste0(this.fig.dir, "D.tiff")
panel.fns <- paste0(this.fig.dir, c("A.tiff", "B.tiff", "C.tiff", "D.tiff"))

# Load geospatial data sets.
bbox.df <- readOGR(bbox.fn, bbox)
cont.df <- readOGR(cont.fn, cont)
lake.df <- readOGR(lake.fn, lake)
river.df <- readOGR(river.fn, river)
basin.df <- readOGR(basin.fn, basin)
NP.lim.df <- read.table(NP.lim.fn)
nutN.df <- LoadNutrientData(nut.dir, "nitrogen")
nutP.df <- LoadNutrientData(nut.dir, "phosphorus")
Nsums.df <- read.table(Nsums.fn)
Psums.df <- read.table(Psums.fn)

# Transform and fortify the geospatial data.
ftbbox.df <- fortify(spTransform(bbox.df, CRS(fig.CRS)))
ftcont.df <- fortify(spTransform(cont.df, CRS(fig.CRS)))
ftlake.df <- fortify(spTransform(lake.df, CRS(fig.CRS)))
ftriver.df <- fortify(spTransform(river.df, CRS(fig.CRS)))
ftbasin.df <- fortify(spTransform(basin.df, CRS(fig.CRS)))
ftbasin.df$id <- as.numeric(ftbasin.df$id) + 1 # indexing from 1 rather than 0

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

# How much area for each nutrient [todo]there seems to be multiplication of data frames, perhaps a few unifying ones are in order (i.e. normalization)

# 4A - N or P Limited, trapezoids.
fig.fn <- A.fn
plot <- ggplot(data=ftbbox.df, aes(x=long, y=lat)) +
    geom_polygon(fill=fig.wcol) +
    geom_polygon(data=ftcont.df, aes(x=long, y=lat, group=group), fill=fig.ccol) +
    geom_point(data=NP.lim.df, aes(x=long, y=lat, color=nut), shape=fig.pt.sh, size=fig.pt.sz) +
    scale_color_manual(values=NP.pal) +
    geom_polygon(data=ftbasin.df, aes(x=long, y=lat, group=group), fill=NA, color=fig.bcol, size=fig.bline) +
    geom_path(data=ftriver.df, aes(x=long, y=lat, group=group), color=fig.wcol, size=fig.rline) +
    geom_polygon(data=ftlake.df, aes(x=long, y=lat, group=group), fill=fig.wcol) +
    theme.opts
ggsave(fig.fn, plot=plot, width=3*orig.map.wid, height=3*orig.map.hgt, dpi=fig.rdpi)
RemoveWhiteEdges(fig.fn, fig.fn, fig.rdpi)
ResaveTIFF(fig.fn, fig.fn, fig.rdpi, fig.rdpi, 8)

# 4B - N or P Limited, basins.
fig.fn <- B.fn
plot <- ggplot(data=ftbbox.df, aes(x=long, y=lat)) +
    geom_polygon(fill=fig.wcol) +
    geom_polygon(data=ftcont.df, aes(x=long, y=lat, group=group), fill=fig.ccol) +
    geom_polygon(data=plot.df[plot.df$hole==FALSE,], aes(x=long, y=lat, group=group, fill=lim), color=fig.bcol, size=fig.bline) +
    scale_fill_manual(values=NP.pal) +
    geom_path(data=ftriver.df, aes(x=long, y=lat, group=group), color=fig.wcol, size=fig.rline) +
    geom_polygon(data=ftlake.df, aes(x=long, y=lat, group=group), fill=fig.wcol) +
    theme.opts
ggsave(fig.fn, plot=plot, width=3*orig.map.wid, height=3*orig.map.hgt, dpi=fig.rdpi)
RemoveWhiteEdges(fig.fn, fig.fn, fig.rdpi)
ResaveTIFF(fig.fn, fig.fn, fig.rdpi, fig.rdpi, 8)

# 4C - ATS Area needed, trapezoids
fig.fn <- C.fn
plot <- ggplot(data=ftbbox.df, aes(x=long, y=lat)) +
    geom_polygon(fill=fig.wcol) +
    geom_polygon(data=ftcont.df, aes(x=long, y=lat, group=group), fill=fig.ccol) +
    geom_point(data=ttrap.df, aes(x=long, y=lat, color=val), shape=fig.pt.sh, size=fig.pt.sz) +
    geom_polygon(data=plot.df[plot.df$hole==FALSE,], aes(x=long, y=lat, group=group), color=fig.bcol, fill=NA, size=fig.bline) +
#    scale_color_gradientn(colors=c("red", "yellow", "green"), values=c(max(s.ttrap.val), s.ttrap.val[trap.10], s.ttrap.val[trap.01], min(s.ttrap.val))/max(s.ttrap.val), na.value=NA) +
    scale_color_gradientn(colors=c("red", "yellow", "green"), values=c(max(s.ttrap.val), s.ttrap.val[ntrap.50], s.ttrap.val[ntrap.90], min(s.ttrap.val))/max(s.ttrap.val), na.value=NA) +
    geom_path(data=ftriver.df, aes(x=long, y=lat, group=group), color=fig.wcol, size=fig.rline) +
    geom_polygon(data=ftlake.df, aes(x=long, y=lat, group=group), fill=fig.wcol) +
    theme.opts
ggsave(fig.fn, plot=plot, width=3*orig.map.wid, height=3*orig.map.hgt, dpi=fig.rdpi)
RemoveWhiteEdges(fig.fn, fig.fn, fig.rdpi)
ResaveTIFF(fig.fn, fig.fn, fig.rdpi, fig.rdpi, 8)

# 4D - ATS Area needed, basins.
fig.fn <- D.fn
plot <- ggplot(data=ftbbox.df, aes(x=long, y=lat)) +
    geom_polygon(fill=fig.wcol) +
    geom_polygon(data=ftcont.df, aes(x=long, y=lat, group=group), fill=fig.ccol) +
    geom_polygon(data=ftarea.sums[ftarea.sums$hole==FALSE,], aes(x=long, y=lat, group=group, fill=val), color=fig.bcol, size=fig.bline) +
    scale_fill_gradientn(colors=c("red", "yellow", "green"), values=c(max(s.areas), s.areas[areas.50], s.areas[areas.90], min(s.areas))/max(s.areas), na.value=NA) +
    geom_path(data=ftriver.df, aes(x=long, y=lat, group=group), color=fig.wcol, size=fig.rline) +
    geom_polygon(data=ftlake.df, aes(x=long, y=lat, group=group), fill=fig.wcol) +
#    geom_polygon(data=ftats.poly, aes(x=long, y=lat, group=group), fill="white") +
    theme.opts
ggsave(fig.fn, plot=plot, width=3*orig.map.wid, height=3*orig.map.hgt, dpi=fig.rdpi)
RemoveWhiteEdges(fig.fn, fig.fn, fig.rdpi)
ResaveTIFF(fig.fn, fig.fn, fig.rdpi, fig.rdpi, 8)

# Assemble panels into figure
fig.fn <- paste0(this.fig.dir, "Figure 4.tiff")
per.row <- c(2,2)
fig.gap <- 1/16
dpi <- 300
labels <- LETTERS[1:4]
label.cols <- rep("black", 4)
xoff <- 30
yoff <- -35
cex <- 1.0
AssemblePanels(fig.fn, per.row, panel.fns, fig.wid, fig.gap, fig.rdpi, labels, label.cols, xoff, yoff, cex)

# Numbers for Results Table
tblA3.ttl <- "Table A3"
tblA3.fn <- paste0(tbl.dir, tblA3.ttl, ".docx")
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
biomass <- sum(NP.lim.df$biomass, na.rm=TRUE) # t yr-1
biomass.rat.world <- biomass/world.npp
biomass.rat.ag <- biomass/ag.npp
mean.prod <- biomass/tot.area # t ha-1 yr-1

results <- c("N.xs",                           # excess N
             format(N.xs, digits=sig.figs, scientific=TRUE), # already in kg
             "kg yr-1",
             "P.xs",                           # excess P
             format(P.xs, digits=sig.figs, scientific=TRUE), # already in kg
             "kg yr-1",
             "trap.N.ct",                      # number of N-limited trapezoids
             format(trap.N.ct),
             "ct",
             "basin.N.ct",                     # number of N-limited basins
             format(basin.N.ct),
             "ct",
             "trap.P.ct",                      # number of P-limited trapezoids
             format(trap.P.ct),
             "ct",
             "basin.P.ct",                     # number of P-limited basins
             format(basin.P.ct),
             "ct",
             "prod.hi",                        # areal biomass productivity at equator
             format(prod.hi*1000, digits=sig.figs, scientific=TRUE), # [todo]converting to kg from t
             "kg ha-1 yr-1",
             "prod.lo",                        # areal biomass productivity at specified latitude
             format(prod.lo*1000, digits=sig.figs, scientific=TRUE), # [todo]converting to kg from t
             "kg ha-1 yr-1",
             "mod.lat.hi",                     # equator latitude
             format(mod.lat.eq, digits=sig.figs, scientific=TRUE),
             "deg",
             "mod.lat.lo",                     # specified latitude
             format(mod.lat.hi, digits=sig.figs, scientific=TRUE),
             "deg",
             "max.lat.nut",                    # max latitude with N or P
             format(max.lat.nut, digits=sig.figs, scientific=TRUE),
             "deg",
             "max.lat.prod",                   # prod at that latitude
             format(max.lat.prod*1000, digits=sig.figs, scientific=TRUE), # [todo]converting to kg from t
             "kg ha-1 yr-1",
             "N.area",                         # area needed for N
             format(N.area, digits=sig.figs, scientific=TRUE),
             "ha",
             "P.area",                         # area needed for P
             format(P.area, digits=sig.figs, scientific=TRUE),
             "ha",
             "tot.area",                       # total area needed
             format(tot.area, digits=sig.figs, scientific=TRUE),
             "ha",
             "N.bas.area",                     # basin area needed for N
             format(N.bas.area, digits=sig.figs, scientific=TRUE),
             "ha",
             "P.bas.area",                     # basin area needed for P
             format(P.area, digits=sig.figs, scientific=TRUE),
             "ha",
             "tot.bas.area",                   # total basin area needed
             format(tot.bas.area, digits=sig.figs, scientific=TRUE),
             "ha",
             "biomass",                        # total basin area needed
             format(biomass*1000, digits=sig.figs, scientific=TRUE), # [todo]converting to kg from t
             "kg yr-1",
             "world.npp",                      # algae to world biomass NPP
             format(world.npp, digits=sig.figs, scientific=TRUE),
             "rat",
             "ag.npp",                         # algae to world ag NPP
             format(ag.npp, digits=sig.figs, scientific=TRUE),
             "rat",
             "mean.prod",                      # global prod over area required
             format(mean.prod, digits=sig.figs, scientific=TRUE),
             "t ha-1 yr-1"
             )
results.df <- data.frame(matrix(results, ncol=3, dimnames=head, byrow=TRUE))
results.fn <- paste0(work.dir, "Fig4Res")
write.table(results.df, results.fn)
WordTable(tblA3.fn, results.df, 3, tblA3.ttl)