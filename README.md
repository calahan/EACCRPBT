# Expanded Algae Cultivation Can Reverse Planetary Boundary Transgressions
## Dean Calahan, Ed Osenbaugh, Walter Adey
The code in this repository accesses geospatial data obtained from EarthStat, the
Global Runoff Data Center (GRDC), and NaturalEarth. The data file locations and/or
formats may have changed since our accession and may change in the future (see references in
the manuscript for accession details), so both the following information and the
code itself may need to be modified to correctly handle these potential changes.
## Directory Structure
Within the project directory, the relevant subdirectories are Code/R, which contains
the program code needed to generate the figures and tables. The program code depends
upon files residing in Code/Working. This directory is populated by running Precompute.R,
which depends upon the presence of the specified data files located in the
directories under Data.
.
+--Code
|   +--R
|   +--Working
+--Data
|   +--EarthStat
|   +--GRDC
|   +--NaturalEarth
|   +--Spreadsheets