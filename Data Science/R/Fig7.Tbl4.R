library(Calahanlab)
source("EACCRPBT.R")
source("Settings.R")
library(plotrix)    #[todo]whatever depends on this should be moved to Calahanlab.R?

# Figure 7 - Effects of N, P, & C recycling on planetary boundary status

this.fig.dir <- paste0(fig.dir, '/Figure 7')
blank.fn <- paste0(this.fig.dir, '/blank.tiff')
fig.fn <- paste0(this.fig.dir, '/Figure 7.tiff')
A.fn <- paste0(this.fig.dir, '/A.tiff')
B.fn <- paste0(this.fig.dir, '/B.tiff')
C.fn <- paste0(this.fig.dir, '/C.tiff')
D.fn <- paste0(this.fig.dir, '/D.tiff')
E.fn <- paste0(this.fig.dir, '/E.tiff')
F.fn <- paste0(this.fig.dir, '/F.tiff')
G.fn <- paste0(this.fig.dir, '/G.tiff')
H.fn <- paste0(this.fig.dir, '/H.tiff')
I.fn <- paste0(this.fig.dir, '/I.tiff')
J.fn <- paste0(this.fig.dir, '/J.tiff')

tbl4.fn <- paste0(tbl.dir, "Table 4.docx")
NP100.fn <- paste0(work.dir, "LoCapLoOpHiLifeNP100")
C100.fn <- paste0(work.dir, "LoCapLoOpHiLifeC100")

# # # B E G I N   G E N E R A T E D   C O D E "Boundaries!AG3:AG16" # # #
tblS3.df <- data.frame(NE = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA), OD = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, 2, NA), AL = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA), OA = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, 2, NA), BFN = c(NA, NA, NA, 1, NA, NA, NA, NA, NA, 2, NA), BFP = c(NA, NA, NA, 1, NA, NA, NA, NA, NA, 2, 1), FU = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, 3, NA), LC = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, 3, NA), BIB = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA), BIE = c(NA, 1, NA, 2, 2, 2, 2, 2, NA, 2, NA), CC = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, 3, NA), row.names = c("NE", "OD", "AL", "OA", "BFN", "BFP", "FU", "LC", "BIB", "BIE", "CC"))
labels <- c("Novel entities", "Stratospheric ozone depletion", "Atmospheric aerosol loading", "Ocean acidification", "Biogeochemical flows", "Freshwater use", "Land system change", "Biosphere Integrity", "Climate change") # Major labels for bars
sub.labels <- c(NA, NA, NA, NA, "N;P", NA, NA, "BII;E/MSY", NA) # Sub-lables for split bars
labelsfull <- c("Novel entities", "Stratospheric ozone depletion", "Atmospheric aerosol loading", "Ocean acidification", "Biogeochemical flows (N)", "Biogeochemical flows (P)", "Freshwater use", "Land system change", "Biosphere Integrity (BII)", "Biosphere Integrity (E/MSY)", "Climate change") # Fully expanded labels for bars
fact.ct <- 100 # number of factors in bar
Vzl <- c(NA, 0.05, NA, 0.8, 62, 6.2, 4000, 0.75, 10, 10, 350) # ZOC low
Vzh <- c(NA, 0.1, NA, 0.7, 82, 11.2, 6000, 0.54, 100, 100, 450) # ZOX high
Vr <- c(NA, NA, NA, 2.07070938583208, NA, NA, NA, NA, NA, NA, 2.07070938583208) # Rates of change
Vo <- c(NA, 0.05, NA, 0.84, 150, 14, 2600, 0.62, NA, 200, 396.5) # Original boundary status
CO2.base <- 400 # ppm
CO2.rate <- 2.07070938583208 # ppm yr^-1
Vbf <- c(NA, 0.0111111111111111, NA, 0.0333333333333333, 0.0333333333333333, 0.0333333333333333, 0.0333333333333333, 0.0333333333333333, NA, 0.0333333333333333, NA) # Effect of biosphere integrity ON boundaries
Vbr <- c(NA, 0.0333333333333333, NA, 0.0333333333333333, 0.0333333333333333, 0.0333333333333333, 0.1, 0.1, NA, 0.0333333333333333, 0.1) # Effect of boundaries ON biosphere integrity
Vnpf <- c(NA, NA, NA, 0.0222222222222222, NA, NA, NA, NA, NA, 0.0333333333333333, 0.0111111111111111) # Effect of N&P boundaries on climate change, ocean acidification, and biosphere integrity    # Modify OA and LC, maintaining relative values but reversing "direction"
# # # E N D   G E N E R A T E D   C O D E "Boundaries!AG3:AG16" # # #

Vzl[4] <- 1 - Vzl[4]
Vzh[4] <- 1 - Vzh[4]
Vo[4] <- 1 - Vo[4]

Vzl[8] <- 1 - Vzl[8]
Vzh[8] <- 1 - Vzh[8]
Vo[8] <- 1 - Vo[8]

# Retrieve better case data frames for 100 yr NP & C cleanup created by Fig5Tbl3.R
NP100.df <- read.table(NP100.fn)
C100.df <- read.table(C100.fn)

# Calculate average removal rates [todo:units] for each 50-year interval for N and P
# Currently units appear to be tons
NP.Prem <- NP100.df$PRemoved/1e+6
NP.Nrem <- NP100.df$NRemoved/1e+6
NP.Crem <- NP100.df$CRemoved/1e+6
C.Prem <- C100.df$PRemoved/1e+6
C.Nrem <- C100.df$NRemoved/1e+6
C.Crem <- C100.df$CRemoved/1e+6

P.rem.100 <- NP.Prem[100]
P.rem.200 <- NP.Prem[200]
N.rem.100 <- NP.Nrem[100]
N.rem.200 <- NP.Nrem[200]

# Vzh - Vzl is central third for each range. Convert to fraction
interval <- 100
A.levels <- Vals2Levels(Vzl, Vzh, Vo, fact.ct) # Original status

B.vals <- SimpleBoundEvolution(Vo, Vzl, Vzh, interval, CO2.base, CO2.rate) # NP 100 years, no feedback
B.levels <- Vals2Levels(Vzl, Vzh, B.vals, fact.ct)

C.vals <- SimpleBoundEvolution(Vo, Vzl, Vzh, 2*interval, CO2.base, CO2.rate) # NP 200 years, no feedback
C.levels <- Vals2Levels(Vzl, Vzh, C.vals, fact.ct)

D.vals <- SimpleBoundFB(Vo, SimpleBoundEvolution(Vo, Vzl, Vzh, interval, CO2.base, CO2.rate), Vbf, Vbr) # NP 100 years, feedback
D.levels <- Vals2Levels(Vzl, Vzh, D.vals, fact.ct)

E.vals <- SimpleBoundFB(Vo, SimpleBoundEvolution(Vo, Vzl, Vzh, 2*interval, CO2.base, CO2.rate), Vbf, Vbr) # NP 200 years, feedback
E.levels <- Vals2Levels(Vzl, Vzh, E.vals, fact.ct)

F.vals <- SimpleBoundFB(Vo, SimpleBoundEvolution(Vo, Vzl, Vzh, interval, CO2.base, CO2.rate), Vbf, Vbr) # C 100 years, feedback
F.levels <- Vals2Levels(Vzl, Vzh, F.vals, fact.ct)

G.vals <- SimpleBoundFB(Vo, SimpleBoundEvolution(Vo, Vzl, Vzh, 2*interval, CO2.base, CO2.rate), Vbf, Vbr) # C 200 years, feedback
G.levels <- Vals2Levels(Vzl, Vzh, G.vals, fact.ct)

# A hole size half the size of the number of factors is nevertheless fairly small
hole.size <- round(fact.ct/2.75) # This is chosen manually to align the base of the green area

# These colors and ranges were chosen manually for similarity to Steffen et al
colors <- c(colorRampPalette(c('white', 'green4'))(round(0.17 * fact.ct)),
            colorRampPalette(c('green4', 'yellow'))(round(0.20 * fact.ct)),
            colorRampPalette(c('yellow', 'yellow'))(round(0.17 * fact.ct)),
            colorRampPalette(c('yellow', 'red'))(round(0.13 * fact.ct)),
            colorRampPalette(c('red', 'white'))(round(0.33 * fact.ct)))

# These colors are used to fill in NA wedges; to make its radial extent equal to
# the max of the colored bars, there have to be fewer factors the way we are doing
# it.
grey.rat <- 0.60 * (hole.size + fact.ct)/(hole.size + 2 * fact.ct)
greys <- c(colorRampPalette(c('white', '#E8E8E8'))(round(0.3 * grey.rat * fact.ct)),
           colorRampPalette(c('#E8E8E8', '#E8E8E8'))(round(0.4 * grey.rat * fact.ct)),
           colorRampPalette(c('#E8E8E8', 'white'))(round(0.3 * grey.rat * fact.ct)))
grey.ct <- length(greys)

# All the colors go into a single palette. The hole is simply repetitive white values
colors <- c(rep("#FFFFFF", hole.size), colors, greys)

A <- RadialBarPlot(A.levels, colors, hole.size, fact.ct, grey.ct, A.fn, fig.wid, fig.wid, fig.rdpi)
B <- RadialBarPlot(B.levels, colors, hole.size, fact.ct, grey.ct, B.fn, fig.wid, fig.wid, fig.rdpi)
C <- RadialBarPlot(C.levels, colors, hole.size, fact.ct, grey.ct, C.fn, fig.wid, fig.wid, fig.rdpi)
D <- RadialBarPlot(D.levels, colors, hole.size, fact.ct, grey.ct, D.fn, fig.wid, fig.wid, fig.rdpi)
E <- RadialBarPlot(E.levels, colors, hole.size, fact.ct, grey.ct, E.fn, fig.wid, fig.wid, fig.rdpi)
F <- RadialBarPlot(F.levels, colors, hole.size, fact.ct, grey.ct, F.fn, fig.wid, fig.wid, fig.rdpi)
G <- RadialBarPlot(G.levels, colors, hole.size, fact.ct, grey.ct, G.fn, fig.wid, fig.wid, fig.rdpi)

# Add text labels
#[todo] much or all of the rest of this code block might better belong in RadialBarPlot itself
# Add line breaks
labels <- mapply(WrapLines, labels, 5, USE.NAMES=FALSE)
xoff = 15 # there always seems to be a need for abitrary-seeming offsets.
yoff = 30 # there always seems to be a need for abitrary-seeming offsets.

plot.rad <- 0.7 * fig.rdpi * fig.wid/2 # radius of the radial plot
text.rad <- 0.78 * fig.rdpi * fig.wid/2 # radii for the text locations
sub.rad <- 0.54 * fig.rdpi * fig.wid/2 # radii for the sub text locations
fig.rad <- fig.rdpi * fig.wid/2 # offsets for placing the plot origin, assuming square plot

# Main labels
ml.angles <- seq((pi/2)-(2*pi/9), (-3*pi/2), -2*pi/9)
ml.x <- text.rad * cos(ml.angles) + fig.rad + xoff
ml.y <- text.rad * sin(ml.angles) + fig.rad + yoff

# Subsidiary labels
sub.angles <- seq((pi/2)-(6*pi/36), (-3*pi/2)-(2*pi/36), -2*pi/18)
sub.x <- sub.rad * cos(sub.angles) + fig.rad + xoff
sub.y <- sub.rad * sin(sub.angles) + fig.rad + yoff

blank <- BlankPlot(blank.fn, fig.wid, fig.wid, fig.rdpi)
imgs <- c(blank.fn, B.fn, C.fn, blank.fn, A.fn, D.fn, E.fn, blank.fn, F.fn, G.fn)
for(img in imgs) {
    if(img != blank.fn) {
        imgpng <- readTIFF(img)
        tiff(img, width=fig.wid, height=fig.wid, units='in', res=fig.rdpi)
        par(mar=c(0,0,0,0), xaxs='i', yaxs='i', fg='white')
        plot(c(0, fig.wid * fig.rdpi), c(0, fig.wid * fig.rdpi), type = 'n', xlab = '', ylab = '', xaxt='n', yaxt='n')
        rasterImage(imgpng,
                  0,
                  0,
                  fig.wid * fig.rdpi,
                  fig.wid * fig.rdpi)

        # Grid circles
        x.off <- 12 #[todo] Why is this arbitrary-seeming offset needed?
        y.off <- 12 #[todo] Why is this arbitrary-seeming offset needed?
        maj.lwd <- 1.5 # Major line width
        min.lwd <- 0.5 # Minor line width
        grey.lvl <- 211/256
        grey.alph <- 0.5
        line.col <- rgb(grey.lvl, grey.lvl, grey.lvl, grey.alph) # Grid line color
        orig.x <- x.off + fig.wid * fig.rdpi / 2
        orig.y <- y.off + fig.wid * fig.rdpi / 2
        incr <- plot.rad / 6
        draw.circle(orig.x, orig.y, incr, border=line.col, lwd=min.lwd)
        draw.circle(orig.x, orig.y, 2*incr, border=line.col, lwd=maj.lwd)
        draw.circle(orig.x, orig.y, 3*incr, border=line.col, lwd=min.lwd)
        draw.circle(orig.x, orig.y, 4*incr, border=line.col, lwd=maj.lwd)
        draw.circle(orig.x, orig.y, 5*incr, border=line.col, lwd=min.lwd)
        draw.circle(orig.x, orig.y, 6*incr, border=line.col, lwd=min.lwd)

        # Labels
        text(ml.x, y=ml.y, labels=labels, col='black', cex=0.74)
        text(sub.x, y=sub.y, labels=ExpandSubLabels(sub.labels), col='black', cex=0.65)
        dev.off()
    }
}
AssemblePanels(fig.fn, c(3, 3, 3), imgs, 8, 1/16, fig.rdpi, c("", LETTERS[2:4], LETTERS[1], LETTERS[5:7], "", LETTERS[8:10]), rep("black", 12), 600, 0.45)

# Table 3
tbl3.df <- data.frame(Boundary=labelsfull,
                      BonBI = round(Vbf, 3),
                      BIonB = round(Vbr, 3),
                      NPonB = round(Vnpf, 3))

tbl4.df <- data.frame(Boundary=labelsfull,
                      A = round(Vo, 3),
                      B = round(B.vals, 3),
                      C = round(C.vals, 3),
                      D = round(D.vals, 3),
                      E = round(E.vals, 3),
                      F = round(F.vals, 3),
                      G = round(G.vals, 3))

WordTable(tbl4.fn, tbl4.df, 8, "Table 4")
