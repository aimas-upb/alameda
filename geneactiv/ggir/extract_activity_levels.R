library("GGIR")
library("argparser")
source("part5_save_levels.R")

# create the parser
parser <- arg_parser("GGIR minute-wise activity level extractor")
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


start_date <- args$startdate
end_date <- args$enddate
timezone <- args$timezone

outfile_base <- paste0("activity_levels_overall_", start_date, "_", end_date)


# Step 1: run the extractor
ms3dir = file.path(metadatadir, "meta", "ms3.out")
ms4dir = file.path(metadatadir, "meta", "ms4.out")

# We are selecting the files in ms4dir because we are certain that
# any files who have a part4 output will necessarily 
# have a part3 with the same file name
rdata_files <- list.files(path = ms4dir, pattern = "^.*\\.(RData)$", 
                          full.names = TRUE)

if (length(rdata_files) == 0) {
  stop(paste0("No g.part4 files to process in folder ", ms4dir))
}

num_files <- length(rdata_files)
start_indices = c(1)
end_indices = c(num_files)

# if (file_batch_size < num_files) {
#   start_indices = seq(1, num_files, file_batch_size)
#   end_indices = seq(file_batch_size, num_files + file_batch_size - 1, file_batch_size)
#   end_indices[length(end_indices)] = num_files
# }

part5_levels_overall <- data.frame()

for (i in 1:length(start_indices)) {
  print(paste0("Processing files: ", paste(rdata_files[start_indices[i]:end_indices[i]], collapse = ", ")) )
  
  part5_save_levels(
    save_to_csv = TRUE,
    outfile_base = outfile_base,
    
    metadatadir = metadatadir,
    
    f0 = start_indices[i],
    f1 = end_indices[i],
    
    overwrite=TRUE,
    
    #=====================
    # Part 1
    #=====================
    # desiredtz = "America/Chicago",
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
    
    #=====================
    # Part 5
    #=====================
    threshold.lig = c(30),
    threshold.mod = c(100),
    threshold.vig = c(400),
    mvpathreshold =c(100), # Set as per your recommendation
    
    bout.metric = 2, # Set as per your recommendation
    boutcriter = 0.8,
    boutcriter.in = 0.9,
    boutcriter.lig = 0.5,
    boutcriter.mvpa = 0.8,
    boutdur.in = c(5,10,30),
    boutdur.lig = c(1,10),
    boutdur.mvpa = c(1, 5),
    
    minimum_MM_length.part5 = 6,
    includedaycrit.part5 = 1/3,
    
    part5_agg2_60seconds = TRUE
  )
}