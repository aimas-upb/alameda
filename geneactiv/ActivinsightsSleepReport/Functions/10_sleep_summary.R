
#' @name sleep_summary
#' @title sleep summary
#' 
#' @description Creates a table of statistics about the daily sleep patterns
#' 
#' @param bed_rise_df Output dataframe from the bed_rise_detect function
#' @param first_date first date analysis starts on.
#' @param boundarys min and max boundary for days to analyse
#' 

sleep_summary <- function(bed_rise_df,
                          first_date,
                          boundarys){
  
  # Creating a data table out of bed_rise_df
  day_no <- as.character(as.Date((as.Date(first_date)):(as.Date(first_date) - 1 + length(bed_rise_df$bed_time))))
  
  statistics <- data.frame(
    "Night Starting" = day_no,
    "Night Start Datetime" = strftime(as.POSIXlt(boundarys[,1], origin = "1970-01-01", tz = ""), format = "%Y-%m-%d %H:%M:%S.%z"),
    "Night End Datetime" = strftime(as.POSIXlt(boundarys[,2], origin = "1970-01-01", tz = ""), format = "%Y-%m-%d %H:%M:%S.%z"),
    "Sleep Onset Datetime" = as.character(strftime(as.POSIXlt(as.numeric(as.character(bed_rise_df$bed_time)), origin = "1970-01-01", tz = ""), format = "%Y-%m-%d %H:%M:%S.%z")),
    "Sleep Onset Time" = as.character(strftime(as.POSIXct(as.numeric(as.character(bed_rise_df$bed_time)), origin = "1970-01-01"), format = "%H:%M")),
    "Rise Datetime" = as.character(strftime(as.POSIXlt(as.numeric(as.character(bed_rise_df$rise_time)), origin = "1970-01-01", tz = ""), format = "%Y-%m-%d %H:%M:%S.%z")),
    "Rise Time" = as.character(strftime(as.POSIXct(as.numeric(as.character(bed_rise_df$rise_time)), origin = "1970-01-01"), format = "%H:%M")),
    "Total Elapsed Bed Time" = as.GRtime(bed_rise_df$total_elapsed_time, format = "%H:%M"),
    "Total Sleep Time" = as.GRtime(bed_rise_df$total_sleep, format = "%H:%M"),
    "Total Wake Time" = as.GRtime(bed_rise_df$total_wake, format = "%H:%M"),
    "First WASO" = bed_rise_df$first_wake_after_sleep - bed_rise_df$bed_time,
    "Sleep Efficiency" = round(bed_rise_df$sleep_efficiency, digits = 1),
    "Num Active Periods" = bed_rise_df$no_active_periods,
    "Median Activity Length" = floor(bed_rise_df$median_active_period_length)
  )
  
  # Add extra row by repeating row 1
  statistics <- rbind(statistics, statistics[1, ])
  
  # Total Elapsed Bed Time
  statistics$Total.Elapsed.Bed.Time[length(bed_rise_df$bed_time) + 1] <- as.GRtime(mean(bed_rise_df$total_elapsed_time), format = "%H:%M")
  
  # Total Sleep
  statistics$Total.Sleep.Time[length(bed_rise_df$bed_time) + 1] <- as.GRtime(mean(bed_rise_df$total_sleep), format = "%H:%M")
  
  # Total Wake
  statistics$Total.Wake.Time[length(bed_rise_df$bed_time) + 1] <- as.GRtime(mean(bed_rise_df$total_wake), format = "%H:%M")
  
  # WASO
  statistics$First.WASO[length(bed_rise_df$bed_time) + 1] <- mean(bed_rise_df$first_wake_after_sleep - bed_rise_df$bed_time)
  statistics$First.WASO[statistics$First.WASO < 0] <- 0
  
  
  statistics$Night.Starting <- as.character(statistics$Night.Starting)
  statistics$Night.Starting[(length(bed_rise_df$bed_time) + 1)] <- "Mean"
  
  ## Overall statistics
  BTimes <- RTimes <- c()
  
  for (i in 1:length(bed_rise_df$bed_time)) {
    BTimes[i] <- as.POSIXct(as.numeric(as.character((bed_rise_df$bed_time[i]))), origin = "1970-01-01") + (length(bed_rise_df$bed_time) - i) * 86400
    RTimes[i] <- as.POSIXct(as.numeric(as.character((bed_rise_df$rise_time[i]))), origin = "1970-01-01") + (length(bed_rise_df$rise_time) - i) * 86400
  }
  
  statistics$Sleep.Onset.Time <- as.character(statistics$Sleep.Onset.Time)
  statistics$Sleep.Onset.Time[(length(bed_rise_df$bed_time) + 1)] <- as.character(strftime(mean(round(as.POSIXct(as.numeric(as.character((BTimes))), origin = "1970-01-01")), na.rm = T), format = "%H:%M"))
  
  statistics$Rise.Time <- as.character(statistics$Rise.Time)
  statistics$Rise.Time[(length(bed_rise_df$bed_time) + 1)] <- as.character(strftime(mean(round(as.POSIXct(as.numeric(as.character((RTimes))), origin = "1970-01-01")), na.rm = T), format = "%H:%M"))
  
  statistics$Sleep.Efficiency[length(bed_rise_df$bed_time) + 1] <- round(mean(bed_rise_df$sleep_efficiency, na.rm = T), digits = 1)
  statistics$Num.Active.Periods[length(bed_rise_df$bed_time) + 1] <- mean(bed_rise_df$no_active_periods, na.rm = T)
  statistics$Median.Activity.Length[length(bed_rise_df$bed_time) + 1] <- mean(bed_rise_df$median_active_period_length, na.rm = T)
  
  # Perform some rounding
  statistics$Num.Active.Periods <- round(statistics$Num.Active.Periods)
  statistics$Median.Activity.Length <- round(statistics$Median.Activity.Length)
  
  # Changing all NAs in statistics
  statistics[is.na(statistics)] <- 0
  
  return(statistics)
}