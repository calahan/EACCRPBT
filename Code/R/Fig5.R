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
library(Calahanlab)
source("Code/R/EACCRPBT.R")
source("Code/R/Settings.R")

# Load precomputed ATS area are biomass
NPlim_df <- read.table(paste0(work_dir, "NPlim"))
area_needed <- sum(NPlim_df$ATSarea, na.rm=TRUE)
total_biomass <- sum(NPlim_df$biomass, na.rm=TRUE)
mean_prod <- total_biomass/area_needed

# Variables specific to this module
fig_dir <- paste0(fig_dir, "Figure 5/")
fig_fn <- paste0(fig_dir, "Figure 5.tiff")
A_fn <- paste0(fig_dir, "A.tiff")
B_fn <- paste0(fig_dir, "B.tiff")
C_fn <- paste0(fig_dir, "C.tiff")
D_fn <- paste0(fig_dir, "D.tiff")
imgs <- c(A_fn, B_fn, C_fn, D_fn)

# Funding chosen so N/P is solved in 100 years
# To ask whether N and P are accounted for, use e.g. min(WorstNP_df[125:200,]$TotalArea) - area_needed
best_startNP <- 6.94e+7
worst_startNP <- 1.85e+8
best_startC <- 7.09e+8
worst_startC <- 1.89e+9

bestNP_df <- EconDataFrame(best_startNP, inc_prp, build_yr_hi, final_yr, mean_prod, cap_lo, op_lo, op_life_long, emp_ha, emp_sal)
worstNP_df <- EconDataFrame(worst_startNP, inc_prp, build_yr_hi, final_yr, mean_prod, cap_hi, op_hi, op_life_short, emp_ha, emp_sal)

# Funding chosen so C is solved in 100 years
# To ask what the minimum area is, use e.g. min(BestC_df[125:200,]$AlgalMass*C_prp)-CO2_gr
bestC_df <- EconDataFrame(best_startC, inc_prp, build_yr_hi, final_yr, mean_prod, cap_lo, op_lo, op_life_long, emp_ha, emp_sal)
worstC_df <- EconDataFrame(worst_startC, inc_prp, build_yr_hi, final_yr, mean_prod, cap_hi, op_hi, op_life_short, emp_ha, emp_sal)

# Save computed economic data for creating ¶Results/Economic Model
econ_df <- data.frame(var=c("best_startNP",
                            "worst_startNP",
                            "best_endNP",
                            "worst_endNP",
                            "best_startC",
                            "worst_startC",
                            "best_endC",
                            "worst_endC"),
                      val=c(best_startNP,
                            worst_startNP,
                            max(bestNP_df$Spending),
                            max(worstNP_df$Spending),
                            best_startC,
                            worst_startC,
                            max(bestC_df$Spending),
                            max(worstC_df$Spending))
                      )

econ_fn <- paste0(work_dir, "econ")
write.table(econ_df, econ_fn)

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
dfs[[1]] <- bestNP_df
dfs[[2]] <- worstNP_df
EconPanel(dfs, names2, lines2, "Spending", 1e+12, expression(Project~Year), expression("Spending (\u0024 \u00D7 10"^12*" yr"^-1*")"),
       "CapEx/OpEx/Life", 200, A_fn, theme_overridesA, colors2, .79)

# Panel B
EconPanel(dfs, names2, lines2, "AlgalMass", 1e+9, expression(Project~Year), expression("Algal Mass (t \u00D7 10"^9*" yr"^-1*")"),
       "CapEx/OpEx/Life", 200, B_fn, theme_overridesB, colors2)

# Panel C
dfs <- list()
dfs[[1]] <- bestC_df
dfs[[2]] <- worstC_df
dfs[[3]] <- bestNP_df
dfs[[4]] <- worstNP_df
EconPanel(dfs, names4, lines4, "Spending", 1e+12, expression(Project~Year), expression("Spending (\u0024 \u00D7 10"^12*" yr"^-1*")"),
       "CapEx/OpEx/Life", 200, C_fn, theme_overridesA, colors4, 0.79)

# Panel D
EconPanel(dfs, names4, lines4, "AlgalMass", 1e+9, expression(Project~Year), expression("Algal Mass (t \u00D7 10"^9*" yr"^-1*")"),
       "CapEx/OpEx/Life", 200, D_fn, theme_overridesB, colors4)

# Figure
AssemblePanels(fig_fn, c(2, 2), imgs, 8, 1/16, fig_rdpi, c(LETTERS[1:4]), rep("black", 4), 35, -35, 0.75)
