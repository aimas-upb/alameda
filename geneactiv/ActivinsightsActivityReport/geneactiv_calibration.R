#!/usr/bin/env Rscript
library("optparse")
library(GENEAread)
source("Functions/03_naming_protocol.R")

split_path <- function(x) if (dirname(x)==x) x else c(basename(x),split_path(dirname(x)))

option_list = list(
  make_option(c("-f", "--binfile"), type="character", default=NULL, 
              help="GENEActiv bin file name", metavar="character")
)

opt_parser = OptionParser(option_list=option_list)
opt = parse_args(opt_parser)

# Get input file and determine the desired processing stage at which to start
if (is.null(opt$binfile)){
  print_help(opt_parser)
  stop("At least one argument must be supplied (GENEActiv bin input file).n", call.=FALSE)
}

binfile <- opt$binfile
binfile_name <- unlist(strsplit(split_path(binfile)[1], "\\."))[1]
calib_rds_file <- naming_protocol(binfile, suffix = "_calibration.rds")

if (!file.exists(file.path(getwd(), "/Outputs/", calib_rds_file))) {
  calib_results = GENEActiv.calibrate(
    binfile,
    use.temp = TRUE,
    spherecrit = 0.3,
    minloadcrit = 36,
    printsummary = TRUE,
    chunksize = c(0.5),
    windowsizes = c(5, 900, 3600)
  )
  
  saveRDS(calib_results, file = file.path(getwd(), "/Outputs/", calib_rds_file))
}
