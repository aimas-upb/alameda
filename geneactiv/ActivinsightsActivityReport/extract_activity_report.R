#!/usr/bin/env Rscript
library("optparse")

library(GENEAread)
library(GENEAclassify)
library(ggplot2)
library(scales)
library(reshape2)
library(RJSONIO)

# Functions to use.
source("Functions/01_library_installer.R")
source("Functions/02_combine_segment_data.R")
source("Functions/03_naming_protocol.R")
source("Functions/04_activity_create_df_pcp.R")
source("Functions/05_number_of_days.R")
source("Functions/06_bed_rise_detect.R")
source("Functions/07_activity_state_rearrange.R")
source("Functions/08_activity_daily_plot.R")
source("Functions/081_sleep_positionals.R")
source("Functions/082_light_temp.R")
source("Functions/083_activity_display.R")
source("Functions/09_activity_detect.R")

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

header_file <- naming_protocol(binfile, suffix = "_header.json")
df_pcp_file <- naming_protocol(binfile, suffix = "_df_pcp.rds")
bed_rise_df_file <- naming_protocol(binfile, suffix = "_bed_rise_df.rds") 
activity_summary_file <- naming_protocol(binfile, suffix = "_activity_summary.csv")
sleep_summary_file <- naming_protocol(binfile, suffix = "_sleep_summary.csv")

mmap.load <- FALSE

datacols <- c(
  "UpDown.mean", "UpDown.var", "UpDown.sd",
  "Degrees.mean", "Degrees.var", "Degrees.sd",
  "Magnitude.mean", "Magnitude.var", "Magnitude.meandiff", "Magnitude.mad",
  "Light.mean", "Light.max",
  "Temp.mean", "Temp.sumdiff", "Temp.meandiff", "Temp.abssumdiff",
  "Temp.sddiff", "Temp.var", "Temp.GENEAskew", "Temp.mad",
  "Step.GENEAcount", "Step.sd", "Step.mean", 
  "Principal.Frequency.mean", "Principal.Frequency.median"
)

# The Start Time is the time of day considered as a cutpoint between two days. That is, from the activity analysis perspective, a "day" starts at 3AM on day (t) and ends at 2:59AM on day (t+1).
start_time <- "03:00"

# 0. Write the header of the file as JSON
header <- header.info(binfile)
header[c("Start_Time_ts"), ] <- rbind(as.numeric(as.POSIXlt(as.character(header["Start_Time",]), format = "%Y-%m-%d %H:%M:%S", origin = "1970-01-01", tz="")))
header_json <- toJSON(setNames(header$Value, rownames(header)))
write(header_json, file.path(getwd(), "/Outputs/", header_file))

# 1. Segment the data using the Awake Sleep Model
segment_data <- combine_segment_data(binfile,
                                     start_time,
                                     datacols,
                                     mmap.load = mmap.load)

# 2. Create activity intensity level classification DF and bed-time / rise-time points  
if (!file.exists(file.path(getwd(), "/Outputs/", df_pcp_file))) {
  df_pcp <- activity_create_df_pcp(segment_data,
                                   summary_name,
                                   header = header,
                                   # r Cut_points
                                   Magsa_cut = 0.04, # 40mg
                                   
                                   Duration_low  = exp(5.5),
                                   Duration_high = exp(8),
                                   
                                   Mad.Score2_low  = 0.45,
                                   Mad.Score2_high = 5,
                                   
                                   Mad.Score4_low  = 0.45,
                                   Mad.Score4_high = 5,
                                   
                                   Mad.Score6_low  = 0.45,
                                   Mad.Score6_high = 5,
                                   
                                   Mad_low  = 0.45,
                                   Mad_high = 5
  )
  
  saveRDS(df_pcp, file.path(getwd(), "/Outputs/", df_pcp_file))
  
  csv_pcp_name <- naming_protocol(binfile, suffix = "_df_pcp.csv")
  write.csv(df_pcp,
            file = file.path(getwd(), "/Outputs/", csv_pcp_name))
} else {
  df_pcp <- readRDS(file.path(getwd(), "/Outputs/", df_pcp_file))
}

segment_data1 <- df_pcp
# Add the additional class onto the end.
segment_data1$Class.prior <- segment_data1$Class.current <- segment_data1$Class.post <- 2
# Create the correct classes for post and priors.
segment_data1$Class.prior[1:(length(segment_data1$Class.prior) - 2)] <- df_pcp$Class.current
segment_data1$Class.current[2:(length(segment_data1$Class.prior) - 1)] <- df_pcp$Class.current
segment_data1$Class.post[3:(length(segment_data1$Class.prior))] <- df_pcp$Class.current

# 3. Find the Bed and Rise times
no_days <- number_of_days(binfile, start_time) + 1 # For days rather than nights.
if (!file.exists(file.path(getwd(), "/Outputs/", bed_rise_df_file))) {
  bed_rise_df <- bed_rise_detect(binfile,
                                 df_pcp,
                                 no_days,
                                 verbose = FALSE
  )
  saveRDS(bed_rise_df, file.path(getwd(), "/Outputs/", bed_rise_df_file))
  
  csv_bedrise_name <- naming_protocol(binfile, suffix = "_bed_rise_df.csv")
  write.csv(bed_rise_df,
            file = file.path(getwd(), "/Outputs/", csv_bedrise_name))
} else {
  bed_rise_df <- readRDS(file.path(getwd(), "/Outputs/", bed_rise_df_file))
}

t <- as.POSIXct(as.numeric(as.character(segment_data1$Start.Time[1])), origin = "1970-01-01")
first_date <- as.Date(t)
first_time <- as.POSIXct(as.character(paste((first_date - 1), start_time)), format = "%Y-%m-%d %H:%M", origin = "1970-01-01")

min_boundary <- max_boundary <- c()

for (i in 1:(no_days)) {
  min_boundary[i] <- as.POSIXct(as.character(paste(first_date + i - 1, start_time)), format = "%Y-%m-%d %H:%M", origin = "1970-01-01")
  max_boundary[i] <- as.POSIXct(as.character(paste(first_date + i, start_time)), format = "%Y-%m-%d %H:%M", origin = "1970-01-01")
}

boundarys <- cbind(min_boundary, max_boundary)
segment_data1 <- activity_state_rearrange(
  segment_data1,
  boundarys,
  bed_rise_df$bed_time,
  bed_rise_df$rise_time,
  first_date
)

# 4. Compute Activity Summary
if (!file.exists(file.path(getwd(), "/Outputs/", activity_summary_file))) {
  activity_df <- activity_detect(
    segment_data1,
    boundarys
  )
  write.csv(activity_df, file.path(getwd(), "/Outputs/", activity_summary_file), row.names = FALSE)
}
