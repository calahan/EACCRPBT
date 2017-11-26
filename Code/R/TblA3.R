# Create Table A3, a summary of settings and parameters that are precomputed or
# initialized for generating data for other figures and tables.

library(Calahanlab)
library(rtf)
source("EACCRPBT.R")
source("Settings.R")

# Filenames
tbl_df_fn <- paste0(work_dir, "tblA3")
tbl_fn <- paste0(tbl_dir, "A3.doc")

# Read data frame
tbl_df <- read.table(tbl_df_fn, stringsAsFactors = FALSE)

# number <- 2.0001e-4
# digits <- 2

# Create the RTF data frame
rtf_df <- data.frame(Variable=tbl_df$var,
                     Value=mapply(Sci2RTF, tbl_df$val, MoreArgs=list(2)),
                     Unit=mapply(Units2RTF, tbl_df$unit))


# rtf experiments
tbl_title <- "Table A3"

# test_df <- data.frame(varname=c("A", "B", "C"),
#                       val=c(1.25e+5, 2.0001e-4, 3.1111e+10),
#                       unit=c("g m-2 d-1", "t ha-1 yr-1", "furl ftnt-1"),
#                       stringsAsFactors = FALSE)
# rtf_df <- data.frame(varname=test_df$varname,
#                      val=mapply(Sci2RTF, test_df$val, MoreArgs=list(2)),
#                      unit=mapply(Units2RTF, test_df$unit),
#                      stringsAsFactors = FALSE)


#addText(tblA3_rtf,"g m2 d{\\super -1}")
tblA3_rtf <- RTF(tbl_fn, width=8.5, height=11, font.size=12, omi=c(1,1,1,1))
addTable(tblA3_rtf, rtf_df, font.size=12, row.names=FALSE, NA.string="-")
done(tblA3_rtf)

results_fn <- paste0(work_dir, "Fig4Res")
write.table(results_df, results_fn)
WordTable(tblA3_fn, results_df, 3, tblA3_ttl)
