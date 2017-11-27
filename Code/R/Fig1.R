library(Calahanlab)
library(ggplot2)
library(rgdal)
library(tiff)
source("Settings.R")

this.fig.dir <- paste0(fig.dir, "Figure 1/")
ss.ix <- 7

# Load geospatial data sets.
bbox.df <- readOGR(bbox.fn, bbox)
cont.df <- readOGR(cont.fn, cont)
lake.df <- readOGR(lake.fn, lake)
river.df <- readOGR(river.fn, river)

# Prepare ATS locations data.
loc <- "EACCRPBT.xlsx"
loc.fn <- paste0(ss.dir, loc)
loc.df <- gdata::read.xls(loc.fn, ss.ix) # Warning or notification appears to be OK (Wide character in print at .../Library/R/3.3/library/gdata/perl/xls2csv.pl line 327.)
tbl.loc.df <- data.frame(long=loc.df$long, lat=loc.df$lat, citation=loc.df$citation)
map.loc.df <- loc.df[loc.df$status=="good",]
map.loc.df <- data.frame(long=map.loc.df$long, lat=map.loc.df$lat, type=map.loc.df$type)
map.loc.df$size <- 0.1
map.loc.df[map.loc.df$type== "prod",]$size <- 0.2
coordinates(map.loc.df) <- c("long", "lat") # map.loc.df is now a SpatialPointsDataFrame
proj4string(map.loc.df) <- CRS("+proj=longlat +datum=WGS84")

# Transform and fortify (if necessary) the data.
proj <- CRS(fig.CRS)
ftbbox.df <- fortify(spTransform(bbox.df, proj))
ftcont.df <- fortify(spTransform(cont.df, proj))
ftlake.df <- fortify(spTransform(lake.df, proj))
ftriver.df <- fortify(spTransform(river.df, proj))
fmap.loc.df <- as.data.frame(spTransform(map.loc.df, proj))

# Create map image, trim white border.
fig.fn <- paste0(this.fig.dir, "E.tiff")
fig.rline <- 0.01               # river line size
fig.bline <- 0.1                # basin line size
plot <- ggplot(data = ftbbox.df, aes(x = long, y = lat)) +
    geom_polygon(fill = fig.wcol) +
    geom_polygon(data = ftcont.df, aes(x = long, y = lat, group = group), fill = fig.ccol) +
    geom_polygon(data = ftlake.df, aes(x = long, y = lat, group = group), fill = fig.wcol) +
    geom_path(data = ftriver.df, aes(x = long, y = lat, group=group), color = fig.wcol, size = fig.rline) +
    geom_point(data = fmap.loc.df, aes(x = long, y = lat, size = size), color = fig.scol, alpha = 0.55) +
    scale_size(range = c(0.75, 2.25)) +
    theme.opts
ggsave(fig.fn, plot=plot, width=orig.map.wid, height=orig.map.hgt, dpi=fig.rdpi)
RemoveWhiteEdges(fig.fn, fig.fn, fig.rdpi)

# Assemble figure
#
fig.fn <- paste0(this.fig.dir, "Figure 1.tiff")
in.fn <- paste0(this.fig.dir, c("A.tiff", "B.tiff", "C.tiff", "D.tiff", "E.tiff"))
per.row <- c(1,2,2)
fig.gap <- 1/16
dpi <- 300
labels <- LETTERS[1:5]
label.cols <- c("white", rep("black", 4))
xoff <- 30
yoff <- -35
cex <- 1.0
AssemblePanels(fig.fn, per.row, in.fn, fig.wid, fig.gap, fig.rdpi, labels, label.cols, xoff, yoff, cex)
AddArrows2TIFF(fig.fn, fig.fn,
               c(219, 1231, 607, 1337, 1377, 1425, 1501, 1516),   # x's
               c(2062, 2152, 996, 1840, 1850, 1856, 1864, 1924),  # y's
               c(25, 25, 25, 15, 15, 15, 15, 15),                 # base widths
               c(40, 40, 40, 24, 24, 24, 24, 24),                 # base heights
               c(40, 40, 40, 24, 24, 24, 24, 24),                 # head widths
               c(40, 40, 40, 24, 24, 24, 24, 24),                 # head heights
               c(rep(1.57, 8)),                                   # angles
               c(rep("white", 7), "black"),                       # line colors
               c(rep(5, 8)))                                      # line widths

# Create table of peer-reviwed outside floways
tbl <- "Table 1.docx"
tbl.fn <- paste0(tbl.dir, tbl)
tbl.loc.df$long <- format(tbl.loc.df$long, digits=2)
tbl.loc.df$lat <- format(tbl.loc.df$lat, digits=3) #[todo] I don't understand why digits=3 works here but =2, above

# If the following fails, it's OK as long as tbl.fn has been intentionally set read-only
WordTable(tbl.fn, tbl.loc.df, 3, "Table 1")
