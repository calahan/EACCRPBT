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
#library(tiff)
# In general all graphics processing is done with TIFF files. PNGs are friendlier
# to Scrivener, so for the purposes of generating readable manuscripts, we use PNGs
# there.

source("Code/R/Settings.R")

in_fn <- paste0(fig_dir, "Figure ", 1:5, "/Figure ", 1:5, ".tiff")
out_fn <- paste0(fig_dir, "Figure ", 1:5, "/Figure ", 1:5, ".png")

for(i in 1:5) {
    TIFF2PNG(in_fn[i], out_fn[i], 8, 300)
}