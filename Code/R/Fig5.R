library(Calahanlab)
source("Code/R/EACCRPBT.R")
source("Code/R/Settings.R")

# Get precomputed results that contain the ATS area needed
input_fn <- paste0(work_dir, "tblA3")
input_df <- read.table(input_fn)
ret <- mapply(assign, as.character(input_df$var), input_df$val, MoreArgs = list(envir = .GlobalEnv))

# Variables specific to this module
fig_dir <- paste0(fig_dir, "Figure 5/")
fig_fn <- paste0(fig_dir, "Figure 5.tiff")
A_fn <- paste0(fig_dir, "A.tiff")
B_fn <- paste0(fig_dir, "B.tiff")
C_fn <- paste0(fig_dir, "C.tiff")
D_fn <- paste0(fig_dir, "D.tiff")
E_fn <- paste0(fig_dir, "E.tiff")
F_fn <- paste0(fig_dir, "F.tiff")
imgs <- c(A_fn, B_fn, C_fn, D_fn, E_fn, F_fn)

tbl3_title <- "Table 3. Main Variables and Model Paramenters"
NP100_fn <- paste0(work_dir, "LoCapLoOpHiLifeNP100")
C100_fn <- paste0(work_dir, "LoCapLoOpHiLifeC100")

# Funding chosen so N/P is solved in 100 years
# To ask whether N and P are accounted for, use e.g. min(HiCapHiOpLoLifeNP100_df[125:200,]$TotalArea)-area.needed
LoCapLoOpHiLifeNP100_df <- EconDataFrame(1.83e+8, inc_prp, build_yr_hi, final_yr, mean_prod, cap_lo, op_lo, op_life_long, emp_ha, emp_sal)
HiCapHiOpLoLifeNP100_df <- EconDataFrame(4.86e+8, inc_prp, build_yr_hi, final_yr, mean_prod, cap_hi, op_hi, op_life_short, emp_ha, emp_sal)

# Funding chosen so C is solved in 100 years
# To ask what the minimum area is, use e.g. min(LoCapLoOpHiLifeC100_df[125:200,]$AlgalMass*C_prp)-CO2.gr
LoCapLoOpHiLifeC100_df <- EconDataFrame(1.28e+9, inc_prp, build_yr_hi, final_yr, mean_prod, cap_lo, op_lo, op_life_long, emp_ha, emp_sal)
HiCapHiOpLoLifeC100_df <- EconDataFrame(3.41e+9, inc_prp, build_yr_hi, final_yr, mean_prod, cap_hi, op_hi, op_life_short, emp_ha, emp_sal)

# Figure settings
line_size = 0.05
text_size = 7.5
legend_text_size = 6

# Panels without legends
theme_overridesA <- list(theme(axis.line = element_line(color='black', size=line_size),
      plot.margin = unit(c(2, 2, 2, 2),'mm'),
      axis.title.x = element_text(family="Arial", color="black", face="bold", size=text_size, margin=margin(1,0,0,0,"mm")),
      axis.title.y = element_text(family="Arial", color="black", face="bold", size=text_size,  margin=margin(0,1,0,0,"mm"), angle=90),
      axis.ticks = element_line(size=0.3),
      axis.text.x = element_text(family="Arial", color="black", face="bold", size=text_size, margin=margin(0.5,0,0,0,"mm")),
      axis.text.y = element_text(family="Arial", color="black", face="bold", size=text_size, margin=margin(0,0.5,0,0,"mm")),
      axis.line.x = element_line(size=0.3, color="black"),
      axis.line.y = element_line(size=0.3, color="black")
      )
)

# Panels with legends
theme_overridesB <- list(theme(axis.line = element_line(color='black', size=line_size),
      plot.margin = unit(c(2, 2, 2, 2),'mm'),
      axis.title.x = element_text(family="Arial", color="black", face="bold", size=text_size, margin=margin(1,0,0,0,"mm")),
      axis.title.y = element_text(family="Arial", color="black", face="bold", size=text_size,  margin=margin(0,1,0,0,"mm"), angle=90),
      axis.ticks = element_line(size=0.3),
      axis.text.x = element_text(family="Arial", color="black", face="bold", size=text_size, margin=margin(0.5,0,0,0,"mm")),
      axis.text.y = element_text(family="Arial", color="black", face="bold", size=text_size, margin=margin(0,0.5,0,0,"mm")),
      axis.line.x = element_line(size=0.3, color="black"),
      axis.line.y = element_line(size=0.3, color="black"),
      legend.key = element_rect(fill="white"),
      legend.position = "right",
      legend.title = element_text(family="Arial", color="black", size=legend_text_size),
      legend.text = element_text(family="Arial", color="black", size=legend_text_size),
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
dfs[[1]] <- LoCapLoOpHiLifeNP100_df
dfs[[2]] <- HiCapHiOpLoLifeNP100_df
EconPanel(dfs, names2, lines2, "Spending", 1e+12, expression(Project~Year), expression("Spending (\u0024 \u00D7 10"^12*" yr"^-1*")"),
       "CapEx/OpEx/Life", 200, A_fn, theme_overridesA, colors2, .79)

# Panel B
EconPanel(dfs, names2, lines2, "AlgalMass", 1e+9, expression(Project~Year), expression("Algal Mass (t \u00D7 10"^9*" yr"^-1*")"),
       "CapEx/OpEx/Life", 200, B_fn, theme_overridesB, colors2)

# Panel C
dfs <- list()
dfs[[1]] <- LoCapLoOpHiLifeC100_df
dfs[[2]] <- HiCapHiOpLoLifeC100_df
dfs[[3]] <- LoCapLoOpHiLifeNP100_df
dfs[[4]] <- HiCapHiOpLoLifeNP100_df
EconPanel(dfs, names4, lines4, "Spending", 1e+12, expression(Project~Year), expression("Spending (\u0024 \u00D7 10"^12*" yr"^-1*")"),
       "CapEx/OpEx/Life", 200, C_fn, theme_overridesA, colors4, 0.79)

# Panel D
EconPanel(dfs, names4, lines4, "AlgalMass", 1e+9, expression(Project~Year), expression("Algal Mass (t \u00D7 10"^9*" yr"^-1*")"),
       "CapEx/OpEx/Life", 200, D_fn, theme_overridesB, colors4)

# Figure
AssemblePanels(fig_fn, c(2, 2), imgs, 8, 1/16, fig_rdpi, c(LETTERS[1:4]), rep("black", 4), 35, -35, 0.75)
