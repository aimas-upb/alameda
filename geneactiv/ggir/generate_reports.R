library("GGIR")
library("argparser")

# create the parser
parser <- arg_parser("GGIR reports generator script")

# add the required arguments for input_dir and output_dir
parser <- add_argument(parser,
                       "input_dir",
                       help = "bin file input directory",
                       "../data/ggir/in",
                       flag = FALSE)

parser <- add_argument(parser,
                       "output_dir",
                       help = "GGIR results output directory",
                       default="../data/ggir/out",
                       flag = FALSE)

# add optional arguments for file_batch_size and chunk_proportion and timezone
parser <- add_argument(parser,
                       "--file-batch-size",
                       help = "Max number of files to process in parallel. Default 1",
                       default = 1,
                       type = "integer",
                       flag=FALSE)

parser <- add_argument(parser,
                       "--chunk-proportion",
                       help = "Float value specifying proportion of file recordings to load to memory at one time. Default is 1, which 12h at a time",
                       default = 1,
                       type = "double",
                       flag = FALSE)

parser <- add_argument(parser,
                       "--timezone",
                       help = "String specifying the desired timezone for time localization",
                       default = "Europe/Bucharest",
                       type = "character",
                       flag = FALSE)

# add optional arguments for default sleep and wake hours
parser <- add_argument(parser,
                       "--default-sleep-onset",
                       help = "Integer value specifying the default hour to consider as start of sleep, when no sleep log available",
                       default = 22,
                       type = "integer",
                       flag = FALSE)

parser <- add_argument(parser,
                       "--default-sleep-wake",
                       help = "Integer value specifying the default hour to consider as end of sleep, when no sleep log available",
                       default = 6,
                       type = "integer",
                       flag = FALSE)

# add optional arguments for studyname, sleep log path and number of nights in sleep log
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


args <- parse_args(parser, c("/home/alex/work/AI-MAS/projects/2020-ALAMEDA-H2020/dev/alameda/geneactiv/data/ggir/in", 
                             "/home/alex/work/AI-MAS/projects/2020-ALAMEDA-H2020/dev/alameda/geneactiv/data/ggir/out", 
                             "--file-batch-size", "1",
                             "--chunk-proportion", "1",
                             "--timezone", "Europe/Rome",
                             "--default-sleep-onset", "22",
                             "--default-sleep-wake", "6"
                             ))


# set sleeplog and number of nights if they exist
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

# Step 0
# ------
# Set input and output dir and the study name
input_dir <- args$input_dir
output_dir <- args$output_dir


# Step 1
# ------
# List all the CSV files in the input directory.
# Take the first file as representative and get its columns.
# Look for the indices of the accel_x, accel_y, accel_z and local_time columns
bin_files <- list.files(path = input_dir, pattern = "^.*\\.(bin|BIN)$",
                        full.names = TRUE)
num_files <- length(bin_files)
if (num_files == 0)
  stop(paste("NO bin files found in input_dir: ", input_dir))


# Step 2
# ------
# Generate batch ranges according to number of available files and provided file_batch_size
# `File batch size' is an optional argument representing the number of
# files to be processed in parallel at a time by the GGIR shell command. 
# The default (and upper limit) of this parameter is the number of CSV files 
# present in the input dir

# file_batch_size <- num_files
file_batch_size <- args$file_batch_size

# `Chunk' is an optional argument with a float value between 0.2 and 2
# It specifies how much of a given file to load for in-memory processing at a time
# A value of 1.0 is equal to chunks of 12 hours processed at a time. 
# A value of 0.5 is therefore equal to 6h of data, while a value of 2.0 is equal to 24h.

chunk_proportion <- args$chunk_proportion
# chunk_proportion = 1.0
if (chunk_proportion <= 0.2)
  chunk_proportion = 0.2


timezone <- args$timezone

# NOTE
# ----
# GGIR will process files in parallel as long as enough cores are available.
# Processing of one file (a day’s worth of recording):
#  - takes ~40s for the full report
#  - has a peak requirement of ~1.5GB of RAM: average resident memory is about 700 MB
#
# Therefore, in the case of a node with 16GB of memory and 8 cores it would be 
# advisable to run with a batch value of 8 and a chunk proportion of 1.0.
# Given the 8 cores, it means 8 files can be processed in parallel, leading to a 
# peak memory requirement of ~12GB of RAM.
# For 15 patient files, the script would run twice, leading to a processing
# time of about 90 seconds.

start_indices = c(1)
end_indices = c(num_files)

# if (file_batch_size < num_files) {
#   start_indices = seq(1, num_files, file_batch_size)
#   end_indices = seq(file_batch_size, num_files + file_batch_size - 1, file_batch_size)
#   end_indices[length(end_indices)] = num_files
# }


for (i in 1:length(start_indices)) {
  print(paste0("Processing files: ", paste(bin_files[start_indices[i]:end_indices[i]], collapse = ", ")) )
  
  print(paste("Proceeding with sleeplog at path: ", sleeplog_path))
  g.shell.GGIR(
    f0 = start_indices[i],
    f1 = end_indices[i],
    
    do.cal = TRUE,
    do.parallel = FALSE,
    overwrite=TRUE,
    
    #=====================
    # Part 1
    #=====================
    chunksize = chunk_proportion,
    
    # rmc.dec=".",
    # rmc.firstrow.acc = 2,
    # rmc.col.acc = acc_indices,
    # rmc.col.time=time_col_idx,
    # rmc.unit.acc = "g",
    # rmc.unit.time = "character",
    # rmc.format.time = "%Y-%m-%d %H:%M:%OS6",
    # rmc.desiredtz = timezone,
    # desiredtz = timezone,
    # 
    # rmc.sf = 24,
    # rmc.doresample= FALSE,
    # rmc.check4timegaps = TRUE,
    # rmc.noise = 0.13,
    
    mode=c(1,2,3,4,5),
    idloc=1,
    datadir = input_dir,
    outputdir = output_dir,
    do.report=c(2,4,5),
    
    dayborder = 0,
    
    windowsizes = c(5, 900, 3600),
    #windowsizes = c(5, 300, 300), # Use for small samples
    do.anglez = TRUE,
    do.anglex = TRUE,
    do.angley = TRUE,

    do.enmo = TRUE,
    do.enmoa = TRUE,
    acc.metric = "ENMO",
    
    spherecrit = 0.3,
    minloadcrit = 72,
    # ignorenonwear = TRUE,
    timethreshold = 10,
    anglethreshold = 5,

    #=====================
    # Part 2
    #=====================
    strategy = 4,
    maxdur = 0, # Set as per your recommendation
    includedaycrit = 8, # Leaving default
    includenightcrit = 8, # Leaving default
    qwindow=c(0,24),
    excludefirstlast = FALSE,
    #=====================need at least two non-NA values to interpolate
    
    # Part 3 + 4
    #=====================
    # def.noc.sleep = c(default_sleep_onset, default_sleep_wake),
    def.noc.sleep = 1,
    outliers.only = FALSE,
    criterror = 4,
    do.visual = TRUE,

    # loglocation = sleeplog_path,
    # colid = 1,
    # coln1 = 2,
    # sleeplogidnum = TRUE,
    # nnights = nr_nights,

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
    
    save_ms5rawlevels = TRUE,
    do.sibreport = TRUE,
    
    part5_agg2_60seconds = TRUE,
    frag.metrics = "all",
    
    #=====================
    # Visual report
    #=====================
    visualreport=TRUE
  )
  
}