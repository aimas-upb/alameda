library("GGIR")
library("argparser")
source("part4_save_sib.R")

# create the parser
parser <- arg_parser("GGIR sustained inactivity bout extractor")
parser <- add_argument(parser,
                       "metadatadir",
                       help = "GGIR analysis metadata directory",
                       type = "character",
                       flag = FALSE)

parser <- add_argument(parser,
                       "--startdate",
                       help = "String specifying the analysis start date in %Y%m%d format",
                       type = "character",
                       flag = FALSE)

parser <- add_argument(parser,
                       "--enddate",
                       help = "String specifying the analysis end date in %Y%m%d format",
                       type = "character",
                       flag = FALSE)

parser <- add_argument(parser,
                       "--timezone",
                       help = "String specifying the desired timezone for time localization",
                       default = "America/Chicago",
                       type = "character",
                       flag = FALSE)

parser <- add_argument(parser, "--sleeplog",
                       help = "Path to sleep log file",
                       type = "character",
                       default = NA,
                       flag = FALSE)

parser <- add_argument(parser, "--nr-nights",
                       help = "Minimum number of nights that are included in the sleeplog, corresponding to the analysis day interval",
                       type = "integer",
                       default = NA,
                       flag = FALSE)

# add optional arguments for default sleep and wake hours
parser <- add_argument(parser,
                       "--default-sleep-onset",
                       help = "Integer value specifying the default hour to consider as start of sleep, when no sleep log available",
                       default = 21,
                       type = "integer",
                       flag = FALSE)

parser <- add_argument(parser,
                       "--default-sleep-wake",
                       help = "Integer value specifying the default hour to consider as end of sleep, when no sleep log available",
                       default = 7,
                       type = "integer",
                       flag = FALSE)

# Step 0: parse script arguments
# args <- parse_args(parser, c("/home/alex/work/InvisALERT/dev/observsmart-activity-tracking/services/ggir_service/new_data/ggir/ggir_output/output_ggir_input",
#                              "--startdate", "2022-11-01",
#                              "--enddate", "2022-11-01",
#                              "--timezone", "America/Chicago",
#                              "--sleeplog", "/home/alex/work/InvisALERT/dev/observsmart-activity-tracking/services/ggir_service/new_data/raw/site_id=1/unit_id=5/sleep_log_20221101_20221101.csv",
#                              "--nr-nights", "1",
#                              "--default-sleep-onset", 21,
#                              "--default-sleep-wake", 7))
args <- parse_args(parser)
metadatadir <- args$metadatadir

start_date <- args$startdate
end_date <- args$enddate
timezone <- args$timezone

# set the sleep log location and the number of nights in the sleep log, 
# if information available
nr_nights = NA
if (!is.na(args$nr_nights)) {
  nr_nights = args$nr_nights
}

sleeplog_path = NA
if(!is.na(args$sleeplog)) {
  sleeplog_path = args$sleeplog
}

default_sleep_onset = args$default_sleep_onset
default_sleep_wake = args$default_sleep_wake

# define variables
outfile_base <- paste0("sib_overall_", start_date, "_", end_date)

ms3dir = file.path(metadatadir, "meta", "ms3.out")
rdata_files <- list.files(path = ms3dir, pattern = "^.*\\.(RData)$", 
                        full.names = TRUE)
num_files <- length(rdata_files)
file_batch_size <- num_files

start_indices = c(1)
end_indices = c(num_files)

for (i in 1:length(start_indices)) {
  print(paste0("Processing files: ", paste(rdata_files[start_indices[i]:end_indices[i]], collapse = ", ")) )
  
  part4_save_sib(
    metadatadir = metadatadir,
    
    f0 = start_indices[i],
    f1 = end_indices[i],
    overwrite = TRUE,
    
    #=====================
    # Part 1
    #=====================
    desiredtz = timezone,
    
    idloc=2,
    
    dayborder = 0,
    windowsizes = c(5, 900, 3600),
    acc.metric = "ENMO",
    
    # ignorenonwear = TRUE,
    timethreshold = 5,
    anglethreshold = 5,
    
    #=====================
    # Part 2
    #=====================
    strategy = 1,
    maxdur = 0, # Set as per your recommendation
    includedaycrit = 8, # Leaving default
    includenightcrit = 8, # Leaving default
    qwindow=c(0,24),
    excludefirstlast = FALSE,
    
    #=====================need at least two non-NA values to interpolate
    # Part 3 + 4
    #=====================
    def.noc.sleep = c(default_sleep_onset, default_sleep_wake),
    outliers.only = FALSE,
    criterror = 4,
    
    loglocation = sleeplog_path,
    colid = 1,
    coln1 = 2,
    sleeplogidnum = TRUE,
    nnights = nr_nights,
  )
}