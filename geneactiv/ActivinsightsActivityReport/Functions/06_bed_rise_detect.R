#' @name bed_rise_detect
#' @title bed rise detection
#'
#' @description Bed Rise Detection is used to identify the sleep bed and rise times and produce sleep statistics
#' given these times.
#'
#' @param binfile file to process
#' @param segment_data segmented data to analyse
#' @param no_days number of days to iterate over
#' @param bed_threshold Where to look from for a bed time before midnight
#' @param rise_threshold Where to look until for a rise time after midnight
#' @param Sleep_Diary A sleep diary dataframe that allows the bed and rise times to be overwritten if provided by sleep diary
#' @param verbose give more details of calculations
#' @param win_len window length in seconds to find either side of intercepts
#' @param sleep_threshold the number of seconds
#'
#### Code ####
#
# binfile = binfile
# segment_data = df_pcp
# no_days = no_days
# start_time = "15:00"
# bed_threshold = "15:00"
# rise_threshold = "15:00"
# verbose = FALSE
# win_len = 4500 # Looks either side of the found intercepts
# sleep_threshold = 9000

bed_rise_detect <- function(binfile,
                            segment_data,
                            no_days,
                            Sleep_Diary,
                            start_time = "15:00",
                            bed_threshold = "15:00",
                            rise_threshold = "15:00",
                            verbose = FALSE,
                            verbose_plot = FALSE,
                            win_len = 4500, # Looks either side of the found intercepts
                            sleep_threshold = 9000) {

  #### 0. Exceptions ####
  # Suppressing output - Limit warning messages outputted to console when timer = TRUE
  options(warn = -1)
  
  #### 1. Useful_Time ####
  segment_data$useful_time <- as.POSIXct(as.numeric(as.character(segment_data$Start.Time)), origin = "1970-01-01")
  len <- length(segment_data$useful_time)
  
  if (missing(no_days)) {
    no_days <- number_of_days(binfile, start_time) # Speed up by passing this through the function
  }
  
  #### 2. First date ####
  t <- as.POSIXct(as.numeric(as.character(segment_data$Start.Time[1])), origin = "1970-01-01")
  first_date <- as.Date(t)
  
  segment_data$cumulative_duration <- cumsum(segment_data$Segment.Duration.current)
  
  # This is where the for loop will occur, for now lets stick with one test set of data,
  
  #### 3. Initialize the variables ####
  bed_time <- rise_time <- total_elapsed_time <-
    total_sleep <- total_wake <- sleep_efficiency <-
    no_active_periods <- median_active_period_length <-
    first_wake_after_sleep <- non_wear <- c()
  
  #### 4. Day Loop ####
  
  # Checking to see if the sleep_diary exists. 
  
  if (missing(Sleep_Diary)){
    Sleep_Diary = NA 
  } 
  
  for (i in 1:(no_days)) {
    # i = 2
    # Now combine with the desired time. From this I could convert back to Unix?
    bed_cut <- as.POSIXct(as.character(paste(first_date + i - 1, bed_threshold)), format = "%Y-%m-%d %H:%M", origin = "1970-01-01")
    rise_cut <- as.POSIXct(as.character(paste(first_date + i, rise_threshold)), format = "%Y-%m-%d %H:%M", origin = "1970-01-01")
    
    if (verbose) {
      print("Day No")
      print(i)
      print("bed_cut: ")
      print(bed_cut)
      print("rise_cut: ")
      print(rise_cut)
    }
    
    # Now find the data between the threshold
    tmp2 <- segment_data[segment_data$useful_time > bed_cut & segment_data$useful_time < rise_cut, ]
    
    ## If statement that says non-wear interference here. Setting variables so that they're never empty
    # Bed and Rise times
    bed_time[i] <- bed_cut + 3600 * 8 # 11pm
    bed_time_sleep_diary = NA
    rise_time[i] <- rise_cut - 3600 * 8 # 7am
    rise_time_sleep_diary = NA
    # Statistics to come from this.
    total_elapsed_time[i] <- NA
    total_sleep[i] <- NA
    total_wake[i] <- NA
    sleep_efficiency[i] <- NA
    no_active_periods[i] <- NA
    median_active_period_length[i] <- 0 # Have to be dummy variables for later
    first_wake_after_sleep[i] <- 0 # Have to be dummy variables for later
    non_wear[i] <- 0 # Have to be dummy variables for later
    
    # if there is more than 4 hours non-wear then move.
    non_wear_df <- tmp2[tmp2$Class.current == 0, ]
    
    if (length(non_wear_df$Segment.Duration.current) > 0) {
      non_wear[i] <- sum(non_wear_df$Segment.Duration.current)
      # if (non_wear[i] > 14400) {
      #   next
      # }
    }
    
    # If there is no data to analysis
    if (length(tmp2$useful_time) < 2) {
      next
    }
    
    # Do not find if there is less than 6 hours to analyse
    if (as.numeric(tmp2$useful_time[length(tmp2$useful_time)] - tmp2$useful_time[1]) < 6) {
      next
    }
    
    # Now find the Score Score associated with this and save to PP
    PP <- data.frame(
      Time = tmp2$Segment.Start.Time,
      NumericTime = tmp2$Start.Time,
      cumulative_duration = tmp2$cumulative_duration,
      SegmentDuration = tmp2$Segment.Duration.current,
      state = tmp2$Class.current,
      SleepScore = tmp2$Sleep.Score3
    )
    
    #### 4.1. Curve1 Approx ####
    # Verbose setting to plot out the Sleep Score
    # Need to smooth this curve by taking an epoch view potentially?
    curve1 <- approx(tmp2$Start.Time, tmp2$Sleep.Score3, n = 86400)
    curve2 <- approx(tmp2$Start.Time, tmp2$Active.current, n = 86400)
    
    if (verbose_plot == TRUE) {
      collen <- length(tmp2$Start.Time)
      Boundary <- c(bed_cut, rise_cut)
      ix1 <- tmp2$Start.Time[1:(collen - 1)]
      ix2 <- tmp2$Start.Time[2:(collen)]
      iy1 <- tmp2$Class.current[1:(collen - 1)]
      iy2 <- tmp2$Class.current[1:(collen - 1)]
      iz1 <- tmp2$Sleep.Score3[1:(collen - 1)]
      iz2 <- tmp2$Sleep.Score3[1:(collen - 1)]
      ia1 <- tmp2$Active.current[1:(collen - 1)]
      ia2 <- tmp2$Active.current[1:(collen - 1)]
      ix <- c(rbind(ix1, ix2))
      iy <- c(rbind(iy1, iy2))
      iz <- c(rbind(iz1, iz2))
      
      dd <- data.frame(ix, iy, iz)
      
      s <- subset(dd,
                  ix > Boundary[1] & ix < Boundary[2],
                  select = c(ix, iy)
      ) # Subs
      
      ss <- subset(dd,
                   ix > Boundary[1] & ix < Boundary[2],
                   select = c(ix, iz)
      ) # Subs
      
      # Creating my three plots here
      windows()
      par(mfrow = c(2, 1)) # Change to 3 if sleep positionals used again
      # Positonals
      start <- tmp2$Start.Time[1]
      end <- tmp2$Start.Time[length(tmp2$Start.Time)]
      AccData <- read.bin(
        binfile = binfile,
        start = start,
        end = end,
        Use.Timestamps = TRUE
      )
      
      # check_positionals2(AccData, Flip = TRUE, filter = 0, density = TRUE) # No longer used.
      
      # Sleep Score plot
      plot(as.POSIXlt(curve1$x, origin = "1970-01-01"), curve1$y)
      lines(as.POSIXlt(curve2$x, origin = "1970-01-01"), curve2$y, col = "darkgreen")
      lines(lowess(curve1, f = 1), col = "red")
      lines(lowess(curve1, f = 0.1), col = "blue")
      abline(h = 2400)
      
      # Hypnogram
      plot(as.POSIXlt(s$ix, origin = "1970-01-01"), s$iy,
           type = "l", col = "blue", xlab = "Time",
           ylab = "Classification", ylim = c(0, 4)
      )
      axis(4,
           at = c(0, 1, 2, 3, 4),
           labels = c(
             "Non-Wear",
             "Sleep",
             "SIN",
             "Inactive",
             "Active"
           ), 1
      )
    }
    
    #### 4.2 Sleep Diary loop ####
    
    if (!is.na(Sleep_Diary)){
      try({
        # Taking the Bed time from the sleep diary - No provision for sleep times past midnight right now!
        # Could we add in a bodge here? Another column TRUE/FALSE for past midnight or before? - Ask Joss
        # Making sure that this only works if the NightOfDate matches the first time of the boundaries that have been 
        # No provision for multiple sleep times on one night - this would be hard to do.
        # Taking the first index matched by date to sleep_diary - Called sdi (Sleep Diary Index)
        
        Sleep_Diary_Index = sdi = match(as.Date(tmp2$useful_time)[1], 
                                        as.Date(Sleep_Diary$NightOfDate, format = "%d/%m/%Y"))
        
        if (!is.na(sdi)){
          if(!is.na(as.POSIXct(as.character(paste(Sleep_Diary$NightOfDate[sdi], Sleep_Diary$Sleep_onset_time[sdi])), format = "%d/%m/%Y %H:%M", origin = "1970-01-01"))){
            try({
              NightOfDate = as.Date(Sleep_Diary$NightOfDate[sdi], format = "%d/%m/%Y")
              # We need take the Date of night and the sleep_onset time 
              bed_time_work = Sleep_Diary$Sleep_onset_time[sdi]
              
              if (Sleep_Diary$Sleep_Onset_Past_Midnight[sdi] == TRUE){
                NightOfDate = NightOfDate + 1 
              }
              
              bed_time_work = as.POSIXct(as.character(paste(NightOfDate, bed_time_work)), format = "%Y-%m-%d %H:%M", origin = "1970-01-01")
              bed_time[i] = as.numeric(bed_time_work)
              bed_time_sleep_diary = 1
            })
          } 
          
          if(!is.na(as.POSIXct(as.character(paste(Sleep_Diary$NightOfDate[sdi], Sleep_Diary$End_of_sleep_time[sdi])), format = "%d/%m/%Y %H:%M", origin = "1970-01-01"))){
            try({
              NightOfDate = Sleep_Diary$NightOfDate[sdi]
              # We need take the Date of night and the sleep_onset time 
              rise_time_work = Sleep_Diary$End_of_sleep_time[sdi]
              rise_time_work = as.POSIXct(as.character(paste(NightOfDate, rise_time_work)), format = "%d/%m/%Y %H:%M", origin = "1970-01-01")
              rise_time[i] = as.numeric(rise_time_work) + 86400 # Add in the additional day 
              rise_time_sleep_diary = 1
            })
          } 
          
        } else {
          if(verbose){
            cat("No Sleep Diary entry for ")
            cat(tmp2$useful_time[1])
          }
        }
        
      })
    }
    
    #### 4.3 Mid Sleep Time ####
    # If we have both bed and rise time have been found by sleep diary skip the auto detection. 
    
    if (is.na(bed_time_sleep_diary) | is.na(rise_time_sleep_diary)){
      
      if (verbose){cat("Autodetection enabled")}
      
      least_active <- which.min(curve2$y)
      mid_sleep <- which.max(curve1$y)
      # Take the window as 2400 Sleep Score or above. Another method could be use mid sleep time?
      list_val <- which(curve1$y > sleep_threshold) # This is the line that might need to be changed!
      
      # If list_val = 0
      if (length(list_val) == 0) {
        # Take it from the 3rd quantile of the sleep score instead.
        list_val <- which(curve1$y > as.numeric(unlist(summary(curve1$y)[5])[1]))
      }
      
      bed_time_work <- list_val[1]
      rise_time_work <- list_val[length(list_val)]
      
      #### 4.4 If list_val is still = 0 ####
      # I could find the windows another way
      # midsleep = FALSE # Make this an option at some point
      # if (midsleep == TRUE){
      if (length(list_val) == 0) {
        win_len <- 14400 # 4 hour window?
        BT_Win_min <- mid_sleep - win_len - 3600
        BT_Win_max <- mid_sleep - win_len + 3600
        RT_Win_min <- mid_sleep + win_len - 3600
        RT_Win_max <- mid_sleep + win_len + 3600
      } else {
        # Time differences
        if (verbose == TRUE) {
          print((rise_time_work - bed_time_work) / 3600) # Duration in Sleep
        }
        
        #### 4.5 Range Bed and Rise Times ####
        MS <- tmp2$Start.Time[1] + mid_sleep
        BT <- tmp2$Start.Time[1] + bed_time_work
        RT <- tmp2$Start.Time[1] + rise_time_work
        
        if (verbose == TRUE) {
          print(as.POSIXlt(BT, origin = "1970-01-01"))
          print(as.POSIXlt(MS, origin = "1970-01-01"))
          print(as.POSIXlt(RT, origin = "1970-01-01"))
        }
        
        BT_Win_min <- BT - win_len
        BT_Win_max <- BT + win_len
        RT_Win_min <- RT - win_len
        RT_Win_max <- RT + win_len
      }
      
      #### 4.6 bed_time ####
      # If this has been provided by the sleep diary already move on. 
      if (is.na(bed_time_sleep_diary)){
        BT_tmp <- tmp2[tmp2$Start.Time > BT_Win_min &
                         tmp2$Start.Time < BT_Win_max &
                         tmp2$Class.prior != 1 &
                         tmp2$Class.current == 1, ]
        
        # Make sure that BT has some records so that the bed_time can be calculated.
        if (length(BT_tmp[, 1]) != 0) {
          bed_time[i] <- as.numeric((BT_tmp$Start.Time[1]))
          # Expand the search area by 30 minutes?
        } else {
          BT_tmp <- tmp2[tmp2$Start.Time > (BT_Win_min - 1800) &
                           tmp2$Start.Time < (BT_Win_max + 1800) &
                           tmp2$Class.prior != 1 &
                           tmp2$Class.current == 1, ]
          
          if (length(BT_tmp[, 1]) != 0) {
            bed_time[i] <- as.numeric((BT_tmp$Start.Time[1]))
            # Expand the search area by 30 minutes?
          } else {
            bed_time[i] <- bed_cut
          }
        }
      }
      
      #### 4.7 rise_time ####
      # If this has been provided by the sleep diary already move on. 
      if (is.na(rise_time_sleep_diary)){
        RT_tmp <- tmp2[tmp2$Start.Time > RT_Win_min &
                         tmp2$Start.Time < RT_Win_max &
                         tmp2$Class.post != 1 &
                         tmp2$Class.current == 1, ]
        
        # Check that the length is correct
        if (length(RT_tmp[, 1]) != 0) {
          rise_time[i] <- as.numeric((RT_tmp$Start.Time[length(RT_tmp[, 1])] +
                                        RT_tmp$Segment.Duration.current[length(RT_tmp[, 1])]))
          # Expand the search area by 30 minutes?
        } else {
          RT_tmp <- tmp2[tmp2$Start.Time > (RT_Win_min - 1800) &
                           tmp2$Start.Time < (RT_Win_max + 1800) &
                           tmp2$Class.post != 1 &
                           tmp2$Class.current == 1, ]
          
          if (length(RT_tmp[, 1]) != 0) {
            rise_time[i] <- as.numeric((RT_tmp$Start.Time[length(RT_tmp[, 1])] +
                                          RT_tmp$Segment.Duration.current[length(RT_tmp[, 1])]))
            # Expand the search area by 30 minutes?
          } else {
            rise_time[i] <- rise_cut
          }
        }
      }
      
    }
    
    #### 4.8 Sleep summary statistics ####
    # Can I take the sleep variable closest to these now?
    df <- tmp2[tmp2$Start.Time > bed_time[i] &
               tmp2$Start.Time < rise_time[i], ]
    
    if (verbose){
      print(df)
    }
    
    # Statistics to come from this.
    total_elapsed_time[i] <- sum(df$Segment.Duration.current)
    total_sleep[i]        <- sum(df[df$Class.current == 1, ]$Segment.Duration.current)
    total_wake[i]         <- sum(df[df$Class.current != 1, ]$Segment.Duration.current)
    
    sleep_efficiency[i]   <- NA
    
    try({
      sleep_efficiency[i] <- (total_sleep[i] / total_elapsed_time[i]) * 100
    })
    
    #### Replacing this with routine at the end of the markdown - names are different! ####
    
    #### 4.9 Sleep Interruptions - From Awake class ####
    df <- segment_data[segment_data$Start.Time > bed_time[i] &
                       segment_data$Start.Time < rise_time[i], ]
    
    # Sleep Interruptions - Not including NW or Sleep
    sleep_interuptions <- df$Start.Time[df$Class.current != 0 &
                                        df$Class.current != 1]
    
    no_active_periods[i] = sum(length(sleep_interuptions))
    
    Duration <- df$Segment.Duration.current[df$Class.current != 0 &
                                            df$Class.current != 1]
    
    try({
      median_active_period_length[i] <- median(Duration)
    })
    
    # If no interruptions
    if (length(df$Start.Time) == 0 | is.null(df) == TRUE) {
      cat("no interuptions")
    }
    
    # Going to rewrite this section as it was too confusing to understand. 
    # This is a simpler algorithm
    
    # I need to find the first wake in the sleep 
    First_Sleep_index = which(df$Class.current == 1)[1]
    
    if (!is.na(First_Sleep_index)){
      # Now I need to find the next awake section 
      df_tmp = df[First_Sleep_index:length(df[,1]),]
      
      # Find the next wake section
      Awake_Index = which(df_tmp$Class.current != 0 &
                            df_tmp$Class.current != 1)
      
      if (length(Awake_Index) > 0){
        df_tmp2 = df_tmp[Awake_Index:length(df_tmp[,1]),]
        first_wake_after_sleep[i] = df_tmp2$Start.Time[1]
      }
    }
    
  }
  
  if (verbose) {
    tmp7 <- data.frame(
      bed_time,
      rise_time,
      total_elapsed_time,
      total_sleep,
      total_wake,
      sleep_efficiency,
      no_active_periods,
      median_active_period_length,
      first_wake_after_sleep,
      non_wear
    )
    print(tmp7)
  }
  # Turn warnings back on 
  options(warn = 0)
  
  # Return as a data frame later including statistics that is required.
  return(data.frame(
    bed_time,
    rise_time,
    total_elapsed_time,
    total_sleep,
    total_wake,
    sleep_efficiency,
    no_active_periods,
    median_active_period_length,
    first_wake_after_sleep,
    non_wear
  ))
}


#### Testing code ####
# library(GENEAread)
# library(GENEAclassify)
# 
# source("Functions/01_library_installer.R")
# source("Functions/02_combine_segment_data.R")
# source("Functions/03_naming_protocol.R")
# source("Functions/04_create_df_pcp.R")
# source("Functions/05_number_of_days.R")
# source("Functions/06_bed_rise_detect.R")
# source("Functions/07_state_rearrange.R")
# source("Functions/08_UpDown.mad_plot.R")
# source("Functions/090_daily_plot.R")
# source("Functions/091_sleep_positionals.R")
# source("Functions/092_light_temp.R")
# source("Functions/093_hypnogram.R")
# source("Functions/10_sleep_summary.R")
# 
# i <- 3
# file_pattern <- "*\\.[bB][iI][nN]$"
# files <- list.files(path = paste0(getwd(), "/Data"), pattern = file_pattern, full.names = TRUE)
# binfile <- files[i]
# summary_name = paste0("Sleep_Summary_Metrics_", strsplit(unlist(strsplit(binfile, "/"))[length(unlist(strsplit(binfile, "/")))], ".bin")[[1]])
# All_Data_name = paste0("Sleep_", strsplit(unlist(strsplit(binfile, "/"))[length(unlist(strsplit(binfile, "/")))], ".bin")[[1]])
# segment_data = readRDS(file.path(paste0("Outputs/", All_Data_name, "_All_Data.rds")))
# no_days = number_of_days(binfile, start_time = "15:00")
# binfile_stripped = unlist(strsplit(binfile, "/"))
# binfile_stripped = binfile_stripped[length(binfile_stripped)]
# binfile_stripped = unlist(strsplit(binfile_stripped, ".bin"))
# Sleep_Diary = read.csv(paste0("Sleep_Diaries/", binfile_stripped, "_Sleep_Diary.csv"))
# start_time = "15:00"
# bed_threshold = "15:00"
# rise_threshold = "15:00"
# verbose = FALSE
# win_len = 4500 # Looks either side of the found intercepts
# sleep_threshold = 9000
# 
# tmp5 = bed_rise_detect(binfile,
#                 segment_data,
#                 no_days,
#                 Sleep_Diary = read.csv(paste0("Sleep_Diaries/", binfile_stripped, "_Sleep_Diary.csv")),
#                 start_time = "15:00",
#                 bed_threshold = "15:00",
#                 rise_threshold = "15:00",
#                 verbose = FALSE,
#                 win_len = 4500, # Looks either side of the found intercepts
#                 sleep_threshold = 9000)
# tmp5


