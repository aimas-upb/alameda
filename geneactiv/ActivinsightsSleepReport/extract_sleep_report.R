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
source("Functions/04_create_df_pcp.R")
source("Functions/05_number_of_days.R")
source("Functions/06_bed_rise_detect.R")
source("Functions/07_state_rearrange.R")
source("Functions/08_UpDown.mad_plot.R")
source("Functions/090_daily_plot.R")
source("Functions/091_sleep_positionals.R")
source("Functions/092_light_temp.R")
source("Functions/093_hypnogram.R")
source("Functions/10_sleep_summary.R")

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

# The Start Time is the time of day considered as a cutpoint between two days. That is, from the activity analysis perspective, a "day" starts at 3PM on day (t) and ends at 2:59PM on day (t+1).
start_time <- "15:00"

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
  df_pcp <- create_df_pcp(segment_data,
                          summary_name,
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

# Add the additional class onto the end.
segment_data$Class.prior <- segment_data$Class.current <- segment_data$Class.post <- 2
# Create the correct classes for post and priors.
segment_data$Class.prior[1:(length(segment_data$Class.prior) - 2)] <- df_pcp$Class.current
segment_data$Class.current[2:(length(segment_data$Class.prior) - 1)] <- df_pcp$Class.current
segment_data$Class.post[3:(length(segment_data$Class.prior))] <- df_pcp$Class.current


# 3. Find the Bed and Rise times
no_days <- number_of_days(binfile, start_time) + 1 # For days rather than nights.
if (!file.exists(file.path(getwd(), "/Outputs/", bed_rise_df_file))) {
  Sleep_Diary = NA
  
  tryCatch(
    {
      # Checking to find if there is a corresponding sleep diary 
      binfile_stripped = unlist(strsplit(binfile, "/"))
      binfile_stripped = binfile_stripped[length(binfile_stripped)]
      binfile_stripped = unlist(strsplit(binfile_stripped, ".bin"))
      
      Sleep_Diary = read.csv(paste0("Sleep_Diaries/", binfile_stripped, "_Sleep_Diary.csv"))
    },
    error=function(cond) {
      message(paste("Error Analysing Sleep Diaries. Reason:", cond))
    }
  )
  
  # Find the Bed and Rise times
  bed_rise_df <- bed_rise_detect(binfile,
                                 df_pcp,
                                 no_days,
                                 Sleep_Diary,
                                 verbose = FALSE,
                                 start_time = start_time,
                                 bed_threshold = start_time,
                                 rise_threshold = start_time,
                                 verbose_plot = TRUE
  )
  
  saveRDS(bed_rise_df, file.path(getwd(), "/Outputs/", bed_rise_df_file))
  
  csv_bedrise_name <- naming_protocol(binfile, suffix = "_bed_rise_df.csv")
  write.csv(bed_rise_df,
            file = file.path(getwd(), "/Outputs/", csv_bedrise_name))
} else {
  bed_rise_df <- readRDS(file.path(getwd(), "/Outputs/", bed_rise_df_file))
}

t <- as.POSIXct(as.numeric(as.character(segment_data$Start.Time[1])), origin = "1970-01-01")
first_date <- as.Date(t)
min_boundary <- max_boundary <- c()

for (i in 1:(no_days)) {
  min_boundary[i] <- as.POSIXct(as.character(paste(first_date + i - 1, start_time)), format = "%Y-%m-%d %H:%M", origin = "1970-01-01")
  max_boundary[i] <- as.POSIXct(as.character(paste(first_date + i, start_time)), format = "%Y-%m-%d %H:%M", origin = "1970-01-01")
}

boundarys <- cbind(min_boundary, max_boundary)

# Use the Bed Rise rules to determine what state this will
segment_data <- state_rearrange(
  segment_data,
  boundarys,
  bed_rise_df$bed_time,
  bed_rise_df$rise_time,
  first_date
)

# 4. Compute Sleep Summary
if (!file.exists(file.path(getwd(), "/Outputs/", sleep_summary_file))) {
  sleep_statistics <- sleep_summary(bed_rise_df,
                              first_date, 
                              boundarys)
  write.csv(sleep_statistics, file.path(getwd(), "/Outputs/", sleep_summary_file), row.names = FALSE)
  
  # text <- " "
  # write.table(text,
  #             file = file.path(paste0("Outputs/", sleep_summary_file)),
  #             append = T,
  #             sep = ",",
  #             row.names = F,
  #             col.names = F
  # )
  # 
  # text <- "Sleep Interruptions through each night"
  # write.table(text,
  #             file = file.path(paste0("Outputs/", sleep_summary_file)),
  #             append = T,
  #             sep = ",",
  #             row.names = F,
  #             col.names = F
  # )
  # 
  # for (j in 1:length(bed_rise_df$bed_time)) {
  #   # Skipping variable
  #   Skipping <- FALSE
  #   
  #   df <- segment_data[segment_data$Start.Time > bed_rise_df$bed_time[j] &
  #                        segment_data$Start.Time < bed_rise_df$rise_time[j], ]
  #   
  #   # Sleep Interruptions - From SIN, Inactive or Active class
  #   sleep_interruptions <- df[df$Class.current == 2 |
  #                               df$Class.current == 3 |
  #                               df$Class.current == 4, ]
  #   
  #   # If no interruptions
  #   if (length(df$Start.Time) == 0 | is.null(df) == TRUE) {
  #     Sleep_List <- c(as.character(j), "No interruptions")
  #     # Now writing these lines under the Date
  #     write.table(t(Sleep_List),
  #                 file = file.path(paste0("Outputs/", sleep_summary_file)),
  #                 append = T,
  #                 sep = ",",
  #                 row.names = F,
  #                 col.names = F
  #     )
  #     next
  #   } else {
  #     write.table(sleep_interruptions,
  #                 file = file.path(paste0("Outputs/", sleep_summary_file)),
  #                 append = T,
  #                 sep = ",",
  #                 row.names = F,
  #                 col.names = F)
  #   }
  # }
}
