library(Calahanlab)
source("Code/R/Settings.R")

# Prepare ATS location data
ss_name <- "EACCRPBT.xlsx"
ss_ix <- 7 # Which worksheet contains the data?
ss_fn <- paste0(ss_dir, ss_name)
ss_df <- gdata::read.xls(ss_fn, ss_ix) # Warning or notification appears to be OK (Wide character in print at .../Library/R/3.3/library/gdata/perl/xls2csv.pl line 327.)
tbl_df <- data.frame(long=ss_df$long, lat=ss_df$lat, citation=ss_df$citation)

# Create table
tbl_name <- "Table 1"
tbl_fn <- paste0(tbl_dir, tbl_name, ".docx")
tbl_df$long <- format(tbl_df$long, digits=2)
tbl_df$lat <- format(tbl_df$lat, digits=3) #[todo] I don't understand why digits=3 works here but =2, above
WordTable(tbl_fn, tbl_df, 3, tbl_name)