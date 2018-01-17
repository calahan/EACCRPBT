# Expanded Algae Cultivation Can Reverse Key Planetary Boundary Transgressions
#### Dean Calahan, Ed Osenbaugh, Walter Adey  

### Overview
The code in this repository accesses geospatial data obtained from EarthStat, the
Global Runoff Data Center (GRDC), and NaturalEarth. The data file locations and/or
formats may have changed since our accession and may change in the future (see references in
the manuscript for accession details), so both the following information and the
code itself may need to be modified to correctly handle these potential changes.  
  
The program code for this project depends upon several R packages, most of which
are either installed with standard R distributions, or can be obtained from CRAN.
One library, Calahanlab, is available from the public repository https://github.com/calahan/Calahanlab,
and at present must be built from source. See the Methods section of manuscript
for complete package specification.  
  
### Directory Structure
Within the project directory (here named "ProjectDir", but the directory name is
irrelevant), the relevant subdirectories are Code/, Data/ and Visual Elements/.
Code/R contains the program code needed to generate intermediate data and to then
create the figures and tables. Code/Data contains the data files needed for running
program code. Code/Working is populated by running Precompute.R prior to running
any other code, which depends upon the presence of Data/Spreadsheets/EACCRPBT.xlsx.
The data files located in Data/EarthStat, Data/GRDC, and Data/NaturalEarth must
be downloaded manually and may require obtaining download permission. Visual Elements/
and its subdirectories are created, if necessary, by the program code.

```
<Project Dir>
|--Code
|  |--R
|  |--Working
|
|--Data
|  |--EarthStat
|  |  |--FertilizerBalance_Ascii
|  |     |--nitrogenbalanceonlandscape_140crops.txt
|  |     |--phosphorusbalanceonlandscape_140crops.txt
|  |
|  |--GRDC
|  |  |--GRDC_405_basins_from_mouth
|  |  |  |--GRDC_405_basins_from_mouth.dbf
|  |  |  |--GRDC_405_basins_from_mouth.prj
|  |  |  |--GRDC_405_basins_from_mouth.shp
|  |  |  |--GRDC_405_basins_from_mouth.shx
|  |  |
|  |  |--GRDC_lakes_join_rivers
|  |     |--GRDC_lakes_join_rivers.dbf
|  |     |--GRDC_lakes_join_rivers.prj
|  |     |--GRDC_lakes_join_rivers.shp
|  |     |--GRDC_lakes_join_rivers.shx
|  |
|  |--NaturalEarth
|  |  |--ne_110m_land
|  |  |  |--ne_110m_land.dbf
|  |  |  |--ne_110m_land.prj
|  |  |  |--ne_110m_land.shp
|  |  |  |--ne_110m_land.shx
|  |  |
|  |  |--ne_110m_wgs84_bounding_box
|  |     |--ne_110m_wgs84_bounding_box.dbf
|  |     |--ne_110m_wgs84_bounding_box.prj
|  |     |--ne_110m_wgs84_bounding_box.shp
|  |     |--ne_110m_wgs84_bounding_box.shx
|  |
|  |--Spreadsheets
|     |--EACCRPBT.xlsx
|  
|--Visual Elements
|  |--Figures  
|  |  |--1
|  |  |--2
|  |  |--3
|  |  |--4
|  |  |--5
|  |
|  |--Tables  
```

### Running the R Programs
To generate figures and tables, the R programs are run in this order:
1. Precompute.R
2. Fig1.R*
3. Fig2.R
4. Fig3.R
5. Fig4.R
6. Fig5.R
7. Tbl1.R
8. Tbl2.A1.R
9. Tbl3.R

These programs incoporate EACCRPBT.R and Settings.R. The remaining R programs, ConvertTiffs.R,
Paragraphs.R, and SyncRefs.R, and the RMarkdown files CheckFlags.R and CheckRefs.R,
are accessory programs for the convenience of the user; they depend on the configuration
of the software and directory configuration of the computer being used and may not
function correctly.

*Fig1.R depends on the existence of pre-existing image files that are not part of
this distribution, and need not be run.

### Changing initial assumptions  
The spreadsheet EACCRPBT.xlsx can be modified to change the initial conditions of
our model.