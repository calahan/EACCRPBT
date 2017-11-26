library(Calahanlab)
source("EACCRPBT.R")
source("Settings.R")

# Get precomputed results that contain the ATS area needed
input.fn <- paste0(work.dir, "Fig4Res")
input.df <- read.table(input.fn)
ret <- mapply(assign, as.character(input.df$name), input.df$val, MoreArgs = list(envir = .GlobalEnv))

mean.prod <- input.df[input.df$name=="mean.prod",]$val
biomass <- input.df[input.df$name=="biomass",]$val
area.needed <- input.df[input.df$name=="tot.area",]$val

# Variables specific to this module
this.fig.dir <- paste0(fig.dir, "Figure 5/")
fig.fn <- paste0(this.fig.dir, "Figure 5.tiff")
A.fn <- paste0(this.fig.dir, "A.tiff")
B.fn <- paste0(this.fig.dir, "B.tiff")
C.fn <- paste0(this.fig.dir, "C.tiff")
D.fn <- paste0(this.fig.dir, "D.tiff")
E.fn <- paste0(this.fig.dir, "E.tiff")
F.fn <- paste0(this.fig.dir, "F.tiff")
imgs <- c(A.fn, B.fn, C.fn, D.fn, E.fn, F.fn)

tbl3.title <- "Table 3. Main Variables and Model Paramenters"
NP100.fn <- paste0(work.dir, "LoCapLoOpHiLifeNP100")
C100.fn <- paste0(work.dir, "LoCapLoOpHiLifeC100")

# Funding chosen so N/P is solved in 100 years
# To ask whether N and P are accounted for, use e.g. min(HiCapHiOpLoLifeNP100.df[125:200,]$TotalArea)-area.needed
LoCapLoOpHiLifeNP100.df <- EconDataFrame(1.83e+8, inc.prp, build.yr.hi, final.yr, mean.prod, cap.lo, op.lo, op.life.long, emp.ha, emp.sal)
HiCapHiOpLoLifeNP100.df <- EconDataFrame(4.86e+8, inc.prp, build.yr.hi, final.yr, mean.prod, cap.hi, op.hi, op.life.short, emp.ha, emp.sal)

# Funding chosen so C is solved in 100 years
# To ask what the minimum area is, use e.g. min(LoCapLoOpHiLifeC100.df[125:200,]$AlgalMass*C.prp)-CO2.gr
LoCapLoOpHiLifeC100.df <- EconDataFrame(1.28e+9, inc.prp, build.yr.hi, final.yr, mean.prod, cap.lo, op.lo, op.life.long, emp.ha, emp.sal)
HiCapHiOpLoLifeC100.df <- EconDataFrame(3.41e+9, inc.prp, build.yr.hi, final.yr, mean.prod, cap.hi, op.hi, op.life.short, emp.ha, emp.sal)

# Figure settings
line.size = 0.05
text.size = 7.5
legend.text.size = 6

# No legend
theme.overridesA <- list(theme(axis.line = element_line(color='black', size=line.size),
      plot.margin = unit(c(2, 2, 2, 2),'mm'),
      axis.title.x = element_text(family="Arial", color="black", face="bold", size=text.size, margin=margin(1,0,0,0,"mm")),
      axis.title.y = element_text(family="Arial", color="black", face="bold", size=text.size,  margin=margin(0,1,0,0,"mm"), angle=90),
      axis.ticks = element_line(size=0.3),
      axis.text.x = element_text(family="Arial", color="black", face="bold", size=text.size, margin=margin(0.5,0,0,0,"mm")),
      axis.text.y = element_text(family="Arial", color="black", face="bold", size=text.size, margin=margin(0,0.5,0,0,"mm")),
      axis.line.x = element_line(size=0.3, color="black"),
      axis.line.y = element_line(size=0.3, color="black")
      )
)

# With legend
theme.overridesB <- list(theme(axis.line = element_line(color='black', size=line.size),
      plot.margin = unit(c(2, 2, 2, 2),'mm'),
      axis.title.x = element_text(family="Arial", color="black", face="bold", size=text.size, margin=margin(1,0,0,0,"mm")),
      axis.title.y = element_text(family="Arial", color="black", face="bold", size=text.size,  margin=margin(0,1,0,0,"mm"), angle=90),
      axis.ticks = element_line(size=0.3),
      axis.text.x = element_text(family="Arial", color="black", face="bold", size=text.size, margin=margin(0.5,0,0,0,"mm")),
      axis.text.y = element_text(family="Arial", color="black", face="bold", size=text.size, margin=margin(0,0.5,0,0,"mm")),
      axis.line.x = element_line(size=0.3, color="black"),
      axis.line.y = element_line(size=0.3, color="black"),
      legend.key = element_rect(fill="white"),
      legend.position = "right",
      legend.title = element_text(family="Arial", color="black", size=legend.text.size),
      legend.text = element_text(family="Arial", color="black", size=legend.text.size),
      legend.key.height=unit(2, "mm")
      )
)

names2 <- c("lo lo hi", "hi hi lo")
lines2 <- c("11", "21")
names4 <- c("lo lo hi C", "hi hi lo C", "lo lo hi NP", "hi hi lo NP")
lines4 <- c("11", "21","41", "4111")
colors2 <- c("lightgreen", "lightcoral")
colors4 <- c("lightgreen", "lightcoral", "forestgreen", "firebrick4")

# Panel A
dfs <- list()
dfs[[1]] <- LoCapLoOpHiLifeNP100.df
dfs[[2]] <- HiCapHiOpLoLifeNP100.df
EconPanel(dfs, names2, lines2, "Spending", 1e+12, expression(Project~Year), expression("Spending (\u0024 \u00D7 10"^12*" yr"^-1*")"),
       "CapEx/OpEx/Life", 200, A.fn, theme.overridesA, colors2, .79)

# Panel B
EconPanel(dfs, names2, lines2, "AlgalMass", 1e+9, expression(Project~Year), expression("Algal Mass (t \u00D7 10"^9*" yr"^-1*")"),
       "CapEx/OpEx/Life", 200, B.fn, theme.overridesB, colors2)

# Panel C
dfs <- list()
dfs[[1]] <- LoCapLoOpHiLifeC100.df
dfs[[2]] <- HiCapHiOpLoLifeC100.df
dfs[[3]] <- LoCapLoOpHiLifeNP100.df
dfs[[4]] <- HiCapHiOpLoLifeNP100.df
EconPanel(dfs, names4, lines4, "Spending", 1e+12, expression(Project~Year), expression("Spending (\u0024 \u00D7 10"^12*" yr"^-1*")"),
       "CapEx/OpEx/Life", 200, C.fn, theme.overridesA, colors4, 0.79)

# Panel D
EconPanel(dfs, names4, lines4, "AlgalMass", 1e+9, expression(Project~Year), expression("Algal Mass (t \u00D7 10"^9*" yr"^-1*")"),
       "CapEx/OpEx/Life", 200, D.fn, theme.overridesB, colors4)

# Figure
AssemblePanels(fig.fn, c(2, 2), imgs, 8, 1/16, fig.rdpi, c(LETTERS[1:4]), rep("black", 4), 35, -35, 0.75)

# Table 3. Economic/Ecological data
tbl3.data <- c("cap.lo",
               format(cap.lo, scientific=TRUE, digits=2),
               "$ ha-1",

               "cap.hi",
               format(cap.hi, scientific=TRUE, digits=2),
               "$ ha-1",

               "op.lo",
               format(op.lo, scientific=TRUE, digits=2),
               "$ ha-1 yr-1",

               "op.hi",
               format(op.hi, scientific=TRUE, digits=2),
               "$ ha-1 yr-1",

               "op.life.long",
               format(op.life.long, scientific=TRUE, digits=2),
               "yr",

               "op.life.short",
               format(op.life.short, scientific=TRUE, digits=2),
               "yr",

               "betterNP.exp",
               format(LoCapLoOpHiLifeNP100.df[200,]$Spending, scientific=TRUE, digits=2),
               "$",

               "betterNP.bm",
               format(min(LoCapLoOpHiLifeNP100.df[125:200,]$AlgalMass), scientific=TRUE, digits=2),
               "t yr-1",

               "worseNP.exp",
               format(HiCapHiOpLoLifeNP100.df[200,]$Spending, scientific=TRUE, digits=2),
               "$",

               "worseNP.bm",
               format(min(HiCapHiOpLoLifeNP100.df[125:200,]$AlgalMass), scientific=TRUE, digits=2),
               "t yr-1",

               "betterC.exp",
               format(LoCapLoOpHiLifeC100.df[200,]$Spending, scientific=TRUE, digits=2),
               "$",

               "betterC.bm",
               format(min(LoCapLoOpHiLifeC100.df[125:200,]$AlgalMass), scientific=TRUE, digits=2),
               "t yr-1",

               "worseC.exp",
               format(HiCapHiOpLoLifeC100.df[200,]$Spending, scientific=TRUE, digits=2),
               "$",

               "worseC.bm",
               format(min(HiCapHiOpLoLifeC100.df[125:200,]$AlgalMass), scientific=TRUE, digits=2),
               "t yr-1"
)

tbl3.df <- data.frame(matrix(tbl3.data, ncol=3, dimnames=list(NULL, c("variable", "value", "unit")), byrow=TRUE))
tbl3.fn <- paste0(tbl.dir, "Table 3.docx")
WordTable(tbl3.fn, tbl3.df, 3, tbl3.title)
