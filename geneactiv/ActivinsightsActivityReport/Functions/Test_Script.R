#### Activity Markdown Test script ####
#' Description: 
#' 
#' This file can be used to run the funcitons in the markdown individually. 
#' This can be useful for debugging 
#' 
#### Code #####

#### 01 Load in Libraries ####
library(GENEAread)
library(signal)
library(mmap)
library(changepoint)
library(GENEAclassify)
library(dplyr)
library(knitr)
library(xtable)
library(ggplot2)
library(Scale)
library(scales)
library(pander)
library(gridExtra)
library(reshape)
library(lubridate)

#### Functions to use. ####
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
source("Functions/10_sleep_summary.R")

#### Variables ####
i <- 1
BinPattern <- "*\\.[bB][iI][nN]$"
files <- list.files(path = paste0(getwd(), "/Data"), pattern = BinPattern, full.names = TRUE)
binfile <- files[i]
header <- header.info(binfile)
summary_name <- paste0("Summary_Metrics_1_0_", strsplit(unlist(strsplit(binfile, "/"))[length(unlist(strsplit(binfile, "/")))], ".bin")[[1]])
timer <- TRUE
datacols <- c(
  "UpDown.mean", "UpDown.var", "UpDown.sd",
  "Degrees.mean", "Degrees.var", "Degrees.sd",
  "Magnitude.mean", "Magnitude.var", "Magnitude.meandiff", "Magnitude.mad",
  "Light.mean", "Light.max",
  "Temp.mean", "Temp.sumdiff", "Temp.meandiff", "Temp.abssumdiff",
  "Temp.sddiff", "Temp.var", "Temp.GENEAskew", "Temp.mad",
  "Step.GENEAcount", "Step.sd", "Step.mean"
)

start_time <- "03:00"

#### 02_combine_segment_data ####
if (timer) {
  # Starting the timer
  cat("
      #. Start of Segmenting data")
  print(Sys.time())
}

data_name <- naming_protocol(binfile, prefix = "")

# Segment the data using the Awake Sleep Model.
segment_data <- combine_segment_data(binfile,
                                     start_time,
                                     datacols,
                                     data_name,
                                     mmap.load = mmap.load
)

saveRDS(segment_data, file.path(getwd(), "/Outputs/", data_name))

#### 03_naming_protocol ####

data_name <- naming_protocol(binfile)

saveRDS(segment_data, file.path(getwd(), "/Outputs/", data_name))

#### 04_create_df_pcp ####
if (timer) {
  # Starting the timer
  cat("
      #. Deciding on classes")
  print(Sys.time())
}

header <- header.info(binfile)

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

segment_data1 <- df_pcp
# Add the additional class onto the end.
segment_data1$Class.prior <- segment_data1$Class.current <- segment_data1$Class.post <- 2
# Create the correct classes for post and priors.
segment_data1$Class.prior[1:(length(segment_data1$Class.prior) - 2)] <- df_pcp$Class.current
segment_data1$Class.current[2:(length(segment_data1$Class.prior) - 1)] <- df_pcp$Class.current
segment_data1$Class.post[3:(length(segment_data1$Class.prior))] <- df_pcp$Class.current

# # Once we've created a sleep score write out the data here. No longer need to save this
# saveRDS(df_pcp,
#         file = file.path(paste0("Outputs/", summary_name, "pcp.rds")))
#### 05_number_of_days ####

no_days <- number_of_days(binfile, start_time) + 1 # For days rather than nights.

#### 06_bed_rise_detect ####
if (timer) {
  # Starting the timer
  cat("
      #. Start of Bed Rise Algorithm")
  print(Sys.time())
}

# Find the Bed and Rise times
bed_rise_df <- bed_rise_detect(binfile,
                               df_pcp,
                               no_days,
                               verbose = FALSE
)

#### 06.1 Setting up the parameters ####
t <- as.POSIXct(as.numeric(as.character(segment_data1$Start.Time[1])), origin = "1970-01-01")
first_date <- as.Date(t)
first_time <- as.POSIXct(as.character(paste((first_date - 1), "15:00")), format = "%Y-%m-%d %H:%M", origin = "1970-01-01")

min_boundary <- max_boundary <- c()

for (i in 1:(no_days)) {
  min_boundary[i] <- as.POSIXct(as.character(paste(first_date + i - 1, start_time)), format = "%Y-%m-%d %H:%M", origin = "1970-01-01")
  max_boundary[i] <- as.POSIXct(as.character(paste(first_date + i, start_time)), format = "%Y-%m-%d %H:%M", origin = "1970-01-01")
}

boundarys <- cbind(min_boundary, max_boundary)

header <- header.info(binfile)

#### 07 state rearrange ####
# Use the Bed Rise rules to determine what state this will
segment_data1 <- activity_state_rearrange(
  segment_data1,
  boundarys,
  bed_rise_df$bed_time,
  bed_rise_df$rise_time,
  first_date
)

# Write out the version of data that we want people to see
csvname <- naming_protocol(binfile, suffix = "_All_Data.csv")

write.csv(segment_data1,
          file = file.path(paste0("Outputs/", csvname))
)
#### 08_activity_daily_plot ####

if (daily_plot_switch) {
  t <- as.POSIXct(as.numeric(as.character(segment_data1$Start.Time[1])), origin = "1970-01-01")
  first_date <- as.Date(t)
  first_start_time <- as.POSIXct(as.character(paste((first_date), start_time)), format = "%Y-%m-%d %H:%M", origin = "1970-01-01")
  
  if (t < first_start_time) {
    first_start_time <- as.POSIXct(as.character(paste((first_date - 1), start_time)), format = "%Y-%m-%d %H:%M", origin = "1970-01-01")
    first_date <- as.Date(first_date)
  }
  
  # Finding the first time
  first_time <- segment_data1$Start.Time[1]
  last_time <- segment_data1$Start.Time[length(segment_data1$Start.Time)] + segment_data1$Segment.Duration.current[length(segment_data1$Segment.Duration.current)]
  
  # Calculate boundaries of plots.
  min_boundary <- max_boundary <- c()
  
  for (i in 1:(no_days + 1)) {
    min_boundary[i] <- as.POSIXct(as.character(paste(first_date + i - 1, start_time)), format = "%Y-%m-%d %H:%M", origin = "1970-01-01")
    max_boundary[i] <- as.POSIXct(as.character(paste(first_date + i, start_time)), format = "%Y-%m-%d %H:%M", origin = "1970-01-01")
  }
  
  boundarys <- cbind(min_boundary, max_boundary)
  
  #### For Loop ####
  for (i in 1:(no_days + 1)) {
    if (timer) {
      # Starting the timer
      cat("
            #. Time of Day ", i, " plot creation is ", Sys.time())
    }
    
    # Read in the AccData
    if (i == 1) {
      AccData <- read.bin(binfile,
                          downsample = as.numeric(unlist(header$Value[2])),
                          start = as.numeric(first_time),
                          end = max_boundary[i],
                          Use.Timestamps = T,
                          mmap.load = mmap.load
      )
    } else if (i == no_days + 1) {
      AccData <- read.bin(binfile,
                          downsample = as.numeric(unlist(header$Value[2])),
                          start = min_boundary[i],
                          end = as.numeric(last_time),
                          Use.Timestamps = T,
                          mmap.load = mmap.load
      )
    } else {
      AccData <- read.bin(binfile,
                          downsample = as.numeric(unlist(header$Value[2])),
                          start = min_boundary[i],
                          end = max_boundary[i],
                          Use.Timestamps = T,
                          mmap.load = mmap.load
      )
    }
    
    # Now find the data between the threshold
    segment_data <- segment_data1[segment_data1$Start.Time > min_boundary[i] &
                                    segment_data1$Start.Time < max_boundary[i], ]
    
    # Ensures the markdown doesn't iterate over a day that doesn't exist.
    if (length(segment_data[, 1]) == 0) {
      next
    }
    
    if (i == 1) {
      activity_daily_plot(AccData,
                          segment_data,
                          boundarys = boundarys[i, ],
                          bed_time  = bed_rise_df$bed_time[i],
                          rise_time = bed_rise_df$rise_time[i],
                          first_date
      )
    } else if (i == no_days + 1) {
      activity_daily_plot(AccData,
                          segment_data,
                          boundarys = boundarys[i, ],
                          bed_time  = NA,
                          rise_time = NA,
                          first_date
      )
    } else {
      activity_daily_plot(AccData,
                          segment_data,
                          boundarys = boundarys[i, ],
                          bed_time  = bed_rise_df$bed_time[i],
                          rise_time = bed_rise_df$rise_time[i - 1],
                          first_date
      )
    }
  }
}

#### 

if (activity_daily_summary_switch) {
cat("##### Activity Daily Summary.")
if (timer) {
  # Starting the timer
  cat("
        #. Time at start of stats table ")
  print(Sys.time())
}
}

activity_df <- activity_detect(
  segment_data1,
  boundarys
)

if (activity_daily_summary_switch) {
  knitr::kable(activity_df)
}


write.csv(activity_df, file.path(paste0("Outputs/", summary_name, ".csv")), row.names = FALSE)



if (daily_summary_switch == TRUE) {
  
  statistics <- sleep_summary(bed_rise_df,
                              first_date)
  
  print(knitr::kable(statistics))
  
  write.csv(statistics, file.path(paste0("Outputs/", summary_name, ".csv")), row.names = FALSE)
}

#### Styling code ####
library(styler)
dir.path =  "C:/Users/Charlie/Documents/Activinsights/Activinsights_Activity_Report_Nov19_v1"
filename = paste0(dir.path, "/Functions/01_library_installer.R")
style_file(filename)
filename = paste0(dir.path, "/Functions/02_combine_segment_data.R")
style_file(filename)
filename = paste0(dir.path, "/Functions/03_naming_protocol.R")
style_file(filename)
filename = paste0(dir.path, "/Functions/04_activity_create_df_pcp.R")
style_file(filename)
filename = paste0(dir.path, "/Functions/05_number_of_days.R")
style_file(filename)
filename = paste0(dir.path, "/Functions/06_bed_rise_detect.R")
style_file(filename)
filename = paste0(dir.path, "/Functions/07_activity_state_rearrange.R")
style_file(filename)
filename = paste0(dir.path, "/Functions/08_activity_daily_plot.R")
style_file(filename)
filename = paste0(dir.path, "/Functions/081_sleep_positionals.R")
style_file(filename)
filename = paste0(dir.path, "/Functions/082_light_temp.R")
style_file(filename)
filename = paste0(dir.path, "/Functions/083_activity_display.R")
style_file(filename)
filename = paste0(dir.path, "/Functions/09_activity_detect.R")
style_file(filename)
filename = paste0(dir.path, "/Functions/10_sleep_summary.R")
style_file(filename)


