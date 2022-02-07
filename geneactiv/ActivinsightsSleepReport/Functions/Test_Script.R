#### Test Script ###
#' Description
#'    This file can be used to test the functions given with the sleep macro.
#' 
#' History of changes:
#'    CS | Initial Creation | 20/10/17
#' notes:
#' 
#########################
# Setup 
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

# Functions to use.
source("Functions/01_library_installer.R")
source("Functions/02_combine_segment_data.R")
source("Functions/03_naming_protocol.R")
source("Functions/04_create_df_pcp.R")
source("Functions/05_number_of_days.R")
source("Functions/06_bed_rise_detect.R")
source("Functions/07_state_rearrange.R")
source("Functions/08_UpDown.mad_plot.R")
source("Functions/091_sleep_positionals.R")
source("Functions/092_light_temp.R")
source("Functions/093_hypnogram.R")
source("Functions/09_daily_plot.R")

#### Variables ####
i <- 1
file_pattern <- "*\\.[bB][iI][nN]$"
files <- list.files(path = paste0(getwd(), "/Data"), pattern = file_pattern, full.names = TRUE)
binfile <- files[i]
summary_name <- paste0("Sleep_Summary_Metrics_2_1_", strsplit(unlist(strsplit(binfile, "/"))[length(unlist(strsplit(binfile, "/")))], ".bin")[[1]])
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

start_time <- "15:00"

#### 02_combine_segment_data ####

if (timer) {
  # Starting the timer
  cat("
      #. Start of Segmenting data")
  print(Sys.time())
}
# Segment the data using the Awake Sleep Model.
segment_data <- combine_segment_data(binfile,
                                     start_time,
                                     datacols,
                                     mmap.load = mmap.load
)

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

# Add the additional class onto the end.
segment_data$Class.prior <- segment_data$Class.current <- segment_data$Class.post <- 2
# Create the correct classes for post and priors.
segment_data$Class.prior[1:(length(segment_data$Class.prior) - 2)] <- df_pcp$Class.current
segment_data$Class.current[2:(length(segment_data$Class.prior) - 1)] <- df_pcp$Class.current
segment_data$Class.post[3:(length(segment_data$Class.prior))] <- df_pcp$Class.current

# # Once we've created a sleep score write out the data here. No longer need to save this
# saveRDS(df_pcp,
#         file = file.path(paste0("Outputs/", summary_name, "pcp.rds")))

#### 05_number_of_days ####

no_days <- number_of_days(binfile, start_time)

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

#### Setting up the parameters ####
t <- as.POSIXct(as.numeric(as.character(segment_data$Start.Time[1])), origin = "1970-01-01")
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
segment_data <- state_rearrange(
  segment_data,
  boundarys,
  bed_rise_df$bed_time,
  bed_rise_df$rise_time,
  first_date
)

# Write out the version of data that we want people to see
csvname <- naming_protocol(binfile, suffix = "_All_Data.csv")

write.csv(segment_data,
          file = file.path(paste0("Outputs/", csvname))
)

#### 08_UpDown.mad_plot ####

if (UpDown.mad_Plot) {
  if (timer) {
    # Starting the timer
    cat("
        #. UpDownMad Plot")
    print(Sys.time())
  }
  
  UpDown.mad_plot(
    binfile,
    segment_data,
    start_time,
    no_days,
    bed_rise_df$BedTime,
    bed_rise_df$RiseTime
  )
}

#### 09_daily_plot ####


# Seperate out the SegData that is needed
BedCut = as.POSIXct(as.numeric(as.character(MinBoundary[i])),  origin = "1970-01-01")
RiseCut= as.POSIXct(as.numeric(as.character(MaxBoundary[i])),  origin = "1970-01-01")

# Now find the data between the threshold 
SegData = SegData1[SegData1$Start.Time > MinBoundary[i] &
                     SegData1$Start.Time < MaxBoundary[i] ,]

Dailyplot(AccData, 
          SegData,
          Boundary = Boundarys[i,],
          BedTime  = vv$BedTime[i],
          RiseTime = vv$RiseTime[i],
          firstdate)

#### Testing the plots individually ####

# Light and Temperature function
source("Functions/LightTempPlot.R")
LightTempPlot(AccData,
              Boundary = Boundarys[i,],
              BedTime  = vv$BedTime[i],
              RiseTime = vv$RiseTime[i])


# Naming Porotocol 
source("Functions/NamingProtocol.R")

# Sleep positionals - Includes the support functions 
source("Functions/SleepPositionals.R")

# StepLinePlot function
source("Functions/SteplineAS.R")
SteplineAS(SegData,
           Boundary = Boundarys[i,],
           BedTime  = vv$BedTime[i],
           RiseTime = vv$RiseTime[i],
           firstdate)


