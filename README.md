# Expanded Algae Cultivation Can Reverse Planetary Boundary Transgressions
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
|  |--GRDC  
|  |--NaturalEarth  
|  |--Spreadsheets  
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

### Running Precompute.R  

### Generating Figures and Tables  

### Changing initial assumptions  