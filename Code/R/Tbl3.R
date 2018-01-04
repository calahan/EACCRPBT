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

# Get precomputed results that contain the ATS area needed
input_fn <- paste0(work_dir, "tblA3")
input_df <- read.table(input_fn)
ret <- mapply(assign, as.character(input_df$var), input_df$val, MoreArgs = list(envir = .GlobalEnv))

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

# Table 3. Economic/Ecological data
tbl3_data <- c("cap_lo",
               format(cap_lo, scientific=TRUE, digits=2),
               "$ ha-1",

               "cap_hi",
               format(cap_hi, scientific=TRUE, digits=2),
               "$ ha-1",

               "op_lo",
               format(op_lo, scientific=TRUE, digits=2),
               "$ ha-1 yr-1",

               "op_hi",
               format(op_hi, scientific=TRUE, digits=2),
               "$ ha-1 yr-1",

               "op_life_long",
               format(op_life_long, scientific=TRUE, digits=2),
               "yr",

               "op_life_short",
               format(op_life_short, scientific=TRUE, digits=2),
               "yr",

               "betterNP.exp",
               format(LoCapLoOpHiLifeNP100_df[200,]$Spending, scientific=TRUE, digits=2),
               "$",

               "betterNP.bm",
               format(min(LoCapLoOpHiLifeNP100_df[125:200,]$AlgalMass), scientific=TRUE, digits=2),
               "t yr-1",

               "worseNP.exp",
               format(HiCapHiOpLoLifeNP100_df[200,]$Spending, scientific=TRUE, digits=2),
               "$",

               "worseNP.bm",
               format(min(HiCapHiOpLoLifeNP100_df[125:200,]$AlgalMass), scientific=TRUE, digits=2),
               "t yr-1",

               "betterC.exp",
               format(LoCapLoOpHiLifeC100_df[200,]$Spending, scientific=TRUE, digits=2),
               "$",

               "betterC.bm",
               format(min(LoCapLoOpHiLifeC100_df[125:200,]$AlgalMass), scientific=TRUE, digits=2),
               "t yr-1",

               "worseC.exp",
               format(HiCapHiOpLoLifeC100_df[200,]$Spending, scientific=TRUE, digits=2),
               "$",

               "worseC.bm",
               format(min(HiCapHiOpLoLifeC100_df[125:200,]$AlgalMass), scientific=TRUE, digits=2),
               "t yr-1"
)

tbl3_df <- data.frame(matrix(tbl3_data, ncol=3, dimnames=list(NULL, c("variable", "value", "unit")), byrow=TRUE))
tbl3_fn <- paste0(tbl_dir, "Table 3.docx")
WordTable(tbl3_fn, tbl3_df, 3, tbl3_title)
