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
library(rtf)
source("Code/R/EACCRPBT.R")
source("Code/R/Settings.R")

# Determine the mean productivit
NPlim_df <- read.table(paste0(work_dir, "NPlim"))
area_needed <- sum(NPlim_df$ATSarea, na.rm=TRUE)
total_biomass <- sum(NPlim_df$biomass, na.rm=TRUE)
mean_prod <- total_biomass/area_needed

tbl3_title <- "Table 3"
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

# Table 3. Economic/Ecological data
tbl3_data <- c("cap_lo",
               Sci2RTF(format(cap_lo, scientific=TRUE, digits=2), 2),
               "$ ha-1",

               "cap_hi",
               Sci2RTF(format(cap_hi, scientific=TRUE, digits=2), 2),
               "$ ha-1",

               "op_lo",
               Sci2RTF(format(op_lo, scientific=TRUE, digits=2), 2),
               "$ ha-1 yr-1",

               "op_hi",
               Sci2RTF(format(op_hi, scientific=TRUE, digits=2), 2),
               "$ ha-1 yr-1",

               "op_life_long",
               Sci2RTF(format(op_life_long, scientific=TRUE, digits=2), 2),
               "yr",

               "op_life_short",
               Sci2RTF(format(op_life_short, scientific=TRUE, digits=2), 2),
               "yr",

               "betterNP.exp",
               Sci2RTF(format(LoCapLoOpHiLifeNP100_df[200,]$Spending, scientific=TRUE, digits=2), 2),
               "$",

               "betterNP.bm",
               Sci2RTF(format(min(LoCapLoOpHiLifeNP100_df[125:200,]$AlgalMass), scientific=TRUE, digits=2), 2),
               "t yr-1",

               "worseNP.exp",
               Sci2RTF(format(HiCapHiOpLoLifeNP100_df[200,]$Spending, scientific=TRUE, digits=2), 2),
               "$",

               "worseNP.bm",
               Sci2RTF(format(min(HiCapHiOpLoLifeNP100_df[125:200,]$AlgalMass), scientific=TRUE, digits=2), 2),
               "t yr-1",

               "betterC.exp",
               Sci2RTF(format(LoCapLoOpHiLifeC100_df[200,]$Spending, scientific=TRUE, digits=2), 2),
               "$",

               "betterC.bm",
               Sci2RTF(format(min(LoCapLoOpHiLifeC100_df[125:200,]$AlgalMass), scientific=TRUE, digits=2), 2),
               "t yr-1",

               "worseC.exp",
               Sci2RTF(format(HiCapHiOpLoLifeC100_df[200,]$Spending, scientific=TRUE, digits=2), 2),
               "$",

               "worseC.bm",
               Sci2RTF(format(min(HiCapHiOpLoLifeC100_df[125:200,]$AlgalMass), scientific=TRUE, digits=2), 2),
               "t yr-1"
)

tbl3_df <- data.frame(matrix(tbl3_data, ncol=3, dimnames=list(NULL, c("variable", "value", "unit")), byrow=TRUE))
tbl3_fn <- paste0(tbl_dir, "Table 3.docx")
tbl3_rtf <- RTF(tbl3_fn, width=8.5, height=11, font.size=12, omi=c(1,1,1,1))
addHeader(tbl3_rtf, "Table 3")
addTable(tbl3_rtf, tbl3_df)
done(tbl3_rtf)
