library(Calahanlab)
library(ggplot2)
library(rgdal)
source("EACCRPBT.R")
source("Settings.R")

# Folders and files
this.fig.dir <- paste0(fig.dir, "Figure 2/")

A.fn <- paste0(this.fig.dir, "A.tiff")
B.fn <- paste0(this.fig.dir, "B.tiff")
C.fn <- paste0(this.fig.dir, "C.tiff")
D.fn <- paste0(this.fig.dir, "D.tiff")
fig.fn <- paste0(this.fig.dir, "Figure 2.tiff")
panel.fns <- paste0(this.fig.dir, c("A.tiff", "B.tiff", "C.tiff", "D.tiff"))

tbl2.ttl <- "Table 2"
tblA1.ttl <- "Table A1"
tblA2.ttl <- "Table A2"
tbl2.fn <- paste0(tbl.dir, tbl2.ttl, ".docx")
tblA1.fn <- paste0(tbl.dir, tblA1.ttl, ".docx")
tblA2.fn <- paste0(tbl.dir, tblA2.ttl, ".docx")

# Load and transform geographical data
bbox.df <- readOGR(bbox.fn, bbox)
cont.df <- readOGR(cont.fn, cont)
lake.df <- readOGR(lake.fn, lake)
river.df <- readOGR(river.fn, river)
basin.df <- readOGR(basin.fn, basin)
ftbbox.df <- fortify(spTransform(bbox.df, CRS(fig.CRS)))
ftcont.df <- fortify(spTransform(cont.df, CRS(fig.CRS)))
ftlake.df <- fortify(spTransform(lake.df, CRS(fig.CRS)))
ftriver.df <- fortify(spTransform(river.df, CRS(fig.CRS)))
ftbasin.df <- fortify(spTransform(basin.df, CRS(fig.CRS)))
ftbasin.df$id <- as.numeric(ftbasin.df$id) + 1 # indexed from 0 here but 1 elsewhere

# Load and transform nutrient data
nutN.df <- LoadNutrientData(nut.dir, "nitrogen")
tnutN.df <- TransformNutrientData(nutN.df, fig.CRS)
nutP.df <- LoadNutrientData(nut.dir, "phosphorus")
tnutP.df <- TransformNutrientData(nutP.df, fig.CRS)
nutNP.df <- NutrientDataProduct(nutN.df, nutP.df)
tnutNP.df <- TransformNutrientData(nutNP.df, fig.CRS)

# Load basin sums computed by Precompute.R, combined N & P values
Nsums.df <- read.table(Nsums.fn)
Psums.df <- read.table(Psums.fn)
NPsums.df <- read.table(NPsums.fn)
NPcomb.df <- NutrientDataProduct(Nsums.df, Psums.df)
t <- merge(ftbasin.df, NPsums.df, by="id")

# Statistics about the data.
#cells.ct <- nrow(nutN.df)
Nval <- nutN.df[which(!is.na(nutN.df$val)),]$val
Pval <- nutP.df[which(!is.na(nutP.df$val)),]$val
NPval <- nutNP.df[which(!is.na(nutNP.df$val)),]$val
Nsumsval <- Nsums.df$val
Psumsval <- Psums.df$val
NPsumsval <- NPsums.df$val

s.Nval <- sort(Nval, decreasing=TRUE)
s.Pval <- sort(Pval, decreasing=TRUE)
s.Pval.pos <- s.Pval[which(s.Pval >= 0)] # Negative values indicate deficit, not excess
s.Pval.sh <- s.Pval - min(s.Pval) # Shifted so that all values are >= 0
s.NPval <- sort(NPval, decreasing=TRUE)
s.Nsumsval <- sort(Nsumsval, decreasing=TRUE)
s.Psumsval <- sort(Psumsval, decreasing=TRUE)
s.NPsumsval <-sort(NPsumsval, decreasing=TRUE)

N.wfr.50 <- WhichFewResponsible(s.Nval, 0.5)
P.wfr.50 <- WhichFewResponsible(s.Pval.pos, 0.5)
NP.wfr.50 <- WhichFewResponsible(s.NPval, 0.5)
N.wfr.90 <- WhichFewResponsible(s.Nval, 0.9)
P.wfr.90 <- WhichFewResponsible(s.Pval.pos, 0.9)
NP.wfr.90 <- WhichFewResponsible(s.NPval, 0.9)
#N.wfr.75 <- WhichFewResponsible(s.Nval, 0.75)
#P.wfr.75 <- WhichFewResponsible(s.Pval, 0.75)
#NP.wfr.75 <- WhichFewResponsible(s.NPval, 0.75)
Nsums.wfr.90 <- WhichFewResponsible(s.Nsumsval, 0.9)
Nsums.wfr.50 <- WhichFewResponsible(s.Nsumsval, 0.5)
Psums.wfr.90 <- WhichFewResponsible(s.Psumsval, 0.9)
Psums.wfr.50 <- WhichFewResponsible(s.Psumsval, 0.5)
NPsums.wfr.90 <- WhichFewResponsible(s.NPsumsval, 0.9)
NPsums.wfr.50 <- WhichFewResponsible(s.NPsumsval, 0.5)

N.wfr.50.pct <- 100 * N.wfr.50/length(s.Nval)
P.wfr.50.pct <- 100 * P.wfr.50/(length(s.Pval.pos))

# Figure 2A - Nitrogen excess
plot <- ggplot(data=ftbbox.df, aes(x=long, y=lat)) +
    geom_polygon(fill=fig.wcol) +
    geom_polygon(data=ftcont.df, aes(x=long, y=lat, group=group), fill=fig.ccol) +
    geom_point(data=tnutN.df, aes(x=long, y=lat, color=val), shape=fig.pt.sh, size=fig.pt.sz) +
    scale_color_gradientn(colors=c("red", "yellow", "green"), values=c(max(s.Nval), s.Nval[N.wfr.50], s.Nval[N.wfr.90], min(s.Nval))/max(s.Nval), na.value=NA) +
    geom_polygon(data=ftbasin.df[ftbasin.df$hole==FALSE,], aes(x=long, y=lat, group=group), fill=NA, color=fig.bcol, size=fig.bline) +
    geom_path(data=ftriver.df, aes(x=long, y=lat, group=group), color=fig.wcol, size=fig.rline) +
    geom_polygon(data=ftlake.df, aes(x=long, y=lat, group=group), fill=fig.wcol) +
    theme.opts
ggsave(A.fn, plot=plot, width=3*orig.map.wid, height=3*orig.map.hgt, dpi=fig.rdpi)
RemoveWhiteEdges(A.fn, A.fn, fig.rdpi)
ResaveTIFF(A.fn, A.fn, fig.rdpi, fig.rdpi, 8)

# Figure 2B - Phosphorus excess
plot <- ggplot(data=ftbbox.df, aes(x=long, y=lat)) +
    geom_polygon(fill=fig.wcol) +
    geom_polygon(data=ftcont.df, aes(x=long, y=lat, group=group), fill=fig.ccol) +
    geom_point(data=tnutP.df, aes(x=long, y=lat, color=val), shape=fig.pt.sh, size=fig.pt.sz) +
    scale_color_gradientn(colors=c("red", "yellow", "green", "midnightblue"), values=c(max(s.Pval.sh), s.Pval.sh[P.wfr.50], s.Pval.sh[P.wfr.90], s.Pval.sh[min(which(s.Pval <= 0))], min(s.Pval.sh))/max(s.Pval.sh), na.value=NA) +
    geom_polygon(data=ftbasin.df[ftbasin.df$hole==FALSE,], aes(x=long, y=lat, group=group), fill=NA, color=fig.bcol, size=fig.bline) +
    geom_path(data=ftriver.df, aes(x=long, y=lat, group=group), color=fig.wcol, size=fig.rline) +
    geom_polygon(data=ftlake.df, aes(x=long, y=lat, group=group), fill=fig.wcol) +
    theme.opts
ggsave(B.fn, plot=plot, width=3*orig.map.wid, height=3*orig.map.hgt, dpi=fig.rdpi)
RemoveWhiteEdges(B.fn, B.fn, fig.rdpi)
ResaveTIFF(B.fn, B.fn, fig.rdpi, fig.rdpi, 8)

# Figure 2C - Combined excess
plot <- ggplot(data=ftbbox.df, aes(x=long, y=lat)) +
    geom_polygon(fill=fig.wcol) +
    geom_polygon(data=ftcont.df, aes(x=long, y=lat, group=group), fill=fig.ccol) +
    geom_point(data=tnutNP.df, aes(x=long, y=lat, color=val), shape=fig.pt.sh, size=fig.pt.sz) +
    scale_color_gradientn(colors=c("red", "yellow", "green"), values=c(max(s.NPval), s.Nval[NP.wfr.50], s.Nval[NP.wfr.90], min(s.NPval))/max(s.NPval), na.value=NA) +
    geom_polygon(data=ftbasin.df[ftbasin.df$hole==FALSE,], aes(x=long, y=lat, group=group), fill=NA, color=fig.bcol, size=fig.bline) +
    geom_path(data=ftriver.df, aes(x=long, y=lat, group=group), color=fig.wcol, size=fig.rline) +
    geom_polygon(data=ftlake.df, aes(x=long, y=lat, group=group), fill=fig.wcol) +
    theme.opts
ggsave(C.fn, plot=plot, width=3*orig.map.wid, height=3*orig.map.hgt, dpi=fig.rdpi)
RemoveWhiteEdges(C.fn, C.fn, fig.rdpi)
ResaveTIFF(C.fn, C.fn, fig.rdpi, fig.rdpi, 8)

# Figure 2D - Basin sums
plot <- ggplot(data=ftbbox.df, aes(x=long, y=lat)) +
    geom_polygon(fill=fig.wcol) +
    geom_polygon(data=ftcont.df, aes(x=long, y=lat, group=group), fill=fig.ccol) +
    geom_polygon(data=t[t$hole==FALSE,], aes(x=long, y=lat, group=group, fill=val), color=fig.bcol, size=fig.bline) +
    scale_fill_gradientn(colors=c("red", "yellow", "green"), values=c(max(s.NPsumsval), s.NPsumsval[NPsums.wfr.90], s.NPsumsval[NPsums.wfr.50], min(s.NPsumsval))/max(s.NPsumsval), na.value=NA) +
    geom_path(data=ftriver.df, aes(x=long, y=lat, group=group), color=fig.wcol, size=fig.rline) +
    geom_polygon(data=ftlake.df, aes(x=long, y=lat, group=group), fill=fig.wcol) +
    theme.opts
ggsave(D.fn, plot=plot, width=3*orig.map.wid, height=3*orig.map.hgt, dpi=fig.rdpi)
RemoveWhiteEdges(D.fn, D.fn, fig.rdpi)
ResaveTIFF(D.fn, D.fn, fig.rdpi, fig.rdpi, 8)

# Assemble panels into figure
per.row <- c(2,2)
fig.gap <- 1/16
dpi <- 300
labels <- LETTERS[1:4]
label.cols <- rep("black", 4)
xoff <- 30
yoff <- -35
cex <- 1.0
AssemblePanels(fig.fn, per.row, panel.fns, fig.wid, fig.gap, fig.rdpi, labels, label.cols, xoff, yoff, cex)

# Tables 2, A1. Save some data for use later to create Table A1.
ord <- order(NPcomb.df$val, decreasing=TRUE)
oNPcomb.df <- NPcomb.df[ord,]
oNsums.df <- Nsums.df[ord,]
oPsums.df <- Psums.df[ord,]
tbl.df <- data.frame(id=oNPcomb.df$id, drainage=oNPcomb.df$name, NPexcess= oNPcomb.df$val, Nexcess=oNsums.df$val, Pexcess=oPsums.df$val)
tblA1.fn <- paste0(work.dir, "tblA1")
area.df <- data.frame(id=basin.df@data$BASIN_ID, area=basin.df@data$AREA_CALC * 100) # multiply by 100 to get ha, original in km2
tblA1.df <- merge(tbl.df, area.df, by="id")
write.table(tblA1.df, tblA1.fn)

# If the tbl2.fn is set read-only, the following line will cause a harmless error.
tbl2.df <- data.frame(Name=tbl.df$drainage, CombinedNPexcess=tbl.df$NPexcess, Nexcess=tbl.df$Nexcess, Pexcess=tbl.df$Pexcess)
tbl2.df$CombinedNPexcess <- format(tbl2.df$CombinedNPexcess, scientific=TRUE, digits=2)
tbl2.df$Nexcess <- format(tbl2.df$Nexcess, scientific=TRUE, digits=2)
tbl2.df$Pexcess <- format(tbl2.df$Pexcess, scientific=TRUE, digits=2)
tryCatch(WordTable(tbl2.fn, tbl2.df[1:20,], 4, tbl2.ttl), finally=print("OK"))

# Numbers for Results
sig.figs <- 2
head <- list(NULL, c("name", "val", "unit"))
results <- c("xsNpercell.min",                 # min excess N per grid cell
             format(min(nutN.df$val, na.rm=TRUE), digits=sig.figs, scientific=TRUE),
             "kg",
             "xsNpercell.max",                 # max excess N per grid cell
             format(max(nutN.df$val, na.rm=TRUE), digits=sig.figs, scientific=TRUE),
             "kg",
             "xsNpercell.mean",                # mean excess N per grid cell
             format(mean(nutN.df$val, na.rm=TRUE), digits=sig.figs, scientific=TRUE),
             "kg",
             "xsPpercell.min",                 # min excess P per grid cell
             format(min(nutP.df$val, na.rm=TRUE), digits=sig.figs, scientific=TRUE),
             "kg",
             "xsPpercell.max",                 # max excess P per grid cell
             format(max(nutP.df$val, na.rm=TRUE), digits=sig.figs, scientific=TRUE),
             "kg",
             "xsPpercell.mean",                # mean excess P per grid cell
             format(mean(nutP.df$val, na.rm=TRUE), digits=sig.figs, scientific=TRUE),
             "kg",
             "50%Ngridcells",                  # half of total N in this % of coords
             format(N.wfr.50.pct, digits=sig.figs, scientific=TRUE),
             "pct",
             "50%Ngridcells",                  # half of total P in this % of coords
             format(P.wfr.50.pct, digits=sig.figs, scientific=TRUE),
             "pct",
             "basin.ct",                       # number of basins in basins dataset
             format(nrow(basin.df), digits=sig.figs, scientific=TRUE),
             "ct",
             "basinN.pct",                     # N in basins / N total
             format(sum(Nsums.df$val)/sum(nutN.df$val, na.rm=TRUE), digits=sig.figs, scientific=TRUE),
             "pct",
             "basinP.pct",                     # P in basins / P total
             format(sum(Psums.df$val)/sum(nutP.df$val, na.rm=TRUE), digits=sig.figs, scientific=TRUE),
             "pct",
             "xsNperbas.min",                 # min excess N per basin
             format(min(Nsums.df$val, na.rm=TRUE), digits=sig.figs, scientific=TRUE),
             "kg",
             "xsNperbas.max",                 # max excess N per basin
             format(max(Nsums.df$val, na.rm=TRUE), digits=sig.figs, scientific=TRUE),
             "kg",
             "xsNperbas.mean",                # mean excess N per basin
             format(mean(Nsums.df$val, na.rm=TRUE), digits=sig.figs, scientific=TRUE),
             "kg",
             "xsPperbas.min",                 # min excess P per basin
             format(min(Psums.df$val, na.rm=TRUE), digits=sig.figs, scientific=TRUE),
             "kg",
             "xsPperbas.max",                 # max excess P per basin
             format(max(Psums.df$val, na.rm=TRUE), digits=sig.figs, scientific=TRUE),
             "kg",
             "xsPperbas.mean",                # mean excess P per basin
             format(mean(Psums.df$val, na.rm=TRUE), digits=sig.figs, scientific=TRUE),
             "kg",
             "50%Nbasin",                    # half the total N excess in this % of basins
             format(Nsums.wfr.50, digits=sig.figs, scientific=TRUE),
             "ct",
             "50%Pbasin",                    # half the total N excess in this % of basins
             format(Psums.wfr.50, digits=sig.figs, scientific=TRUE),
             "ct",
             "90%Nbasin",                    # half the total N excess in this % of basins
             format(Nsums.wfr.90, digits=sig.figs, scientific=TRUE),
             "ct",
             "90%Pbasin",                    # half the total N excess in this % of basins
             format(Psums.wfr.90, digits=sig.figs, scientific=TRUE),
             "ct",
             "topNbasin",                    # this basin has the most N
             as.character(Nsums.df[Nsums.df$val == max(Nsums.df$val),]$name),
             "id",
             "topPbasin",                    # this basin has the most P
             as.character(Psums.df[Psums.df$val == max(Psums.df$val),]$name),
             "id",
             "maxNbasin",                    # top N basin has this much
             format(max(Nsums.df$val), digits=sig.figs, scientific=TRUE),
             "kg",
             "maxPbasin",                    # top P basin has this much
             format(max(Psums.df$val), digits=sig.figs, scientific=TRUE),
             "kg"
             )
results.df <- data.frame(matrix(results, ncol=3, dimnames=head, byrow=TRUE))
WordTable(tblA2.fn, results.df, 3, tblA2.ttl)
