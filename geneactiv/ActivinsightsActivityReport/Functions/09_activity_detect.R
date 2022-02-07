
#' @name Activity_Detect
#' @title Activity_Detect
#'
#' @description Detecting Activity Statistics
#'
#' @param segment_data The segmented GENEActiv.bin data.
#' @param boundarys A vector of UNIX timestamps
#'
#' @details Detecting the activity statistics to print in the daily activity table frm the segmented GENEActiv.bin data.
#'

activity_detect <- function(segment_data,
                            boundarys) {

  # Called tmp1. The data between the boundaries
  len <- length(boundarys[, 1])

  # Initialise variables
  DayNum <- DayStart <- DayEnd <- Steps <- Non_Wear <- Sleep <- Sedentary <- Light <- Moderate <- Vigorous <- c()

  # For loop to run over the days
  for (i in 1:len) {
    day_data <- segment_data[segment_data$Start.Time > boundarys[i, 1] &
      segment_data$Start.Time <= boundarys[i, 2], ]

    # Steps[i] <- sum(day_data$Step.GENEAcount.current[day_data$Step.GENEAcount.current > 4 &
    #                                                  day_data$Step.sd.current < 1.33  &
    #                                                  day_data$Step.mean.current > 20])

    Steps[i] <- sum(day_data$Step.GENEAcount.current[day_data$Step.mean.current > 20], na.rm = T)
    
    DayNum[i] <- i
    DayStart[i] <- strftime(as.POSIXlt(boundarys[i, 1], origin = "1970-01-01", tz = ""), format = "%Y-%m-%d %H:%M:%S.%z")
    DayEnd[i] <- strftime(as.POSIXlt(boundarys[i,2], origin = "1970-01-01", tz = ""), format = "%Y-%m-%d %H:%M:%S.%z")
    #### The numbers are: ####
    #'  0. Non-Wear
    #'  1. Sleep
    #'  1.5. Day Sleep? - Leaving this out here will be included on the sleep markdown
    #'  2. Sedentary
    #'  3. Light
    #'  4. Moderate
    #'  5. Vigorous

    Non_Wear[i]  <- sum(day_data[day_data$Class.current == 0, ]$Segment.Duration.current)
    Sleep[i]     <- sum(day_data[day_data$Class.current == 1, ]$Segment.Duration.current)
    Sedentary[i] <- sum(day_data[day_data$Class.current == 2, ]$Segment.Duration.current)
    Light[i]     <- sum(day_data[day_data$Class.current == 3, ]$Segment.Duration.current)
    Moderate[i]  <- sum(day_data[day_data$Class.current == 4, ]$Segment.Duration.current)
    Vigorous[i]  <- sum(day_data[day_data$Class.current == 5, ]$Segment.Duration.current)
  }

  Activity_df <- data.frame(
    "Day_Number" = DayNum,
    "Day_Start" = DayStart,
    "Day_End" = DayEnd,
    "Steps" = Steps,
    "Non_Wear" = as.GRtime(Non_Wear, format = "%H:%M"),
    "Sleep" = as.GRtime(Sleep, format = "%H:%M"),
    "Sedentary" = as.GRtime(Sedentary, format = "%H:%M"),
    "Light" = as.GRtime(Light, format = "%H:%M"),
    "Moderate" = as.GRtime(Moderate, format = "%H:%M"),
    "Vigorous" = as.GRtime(Vigorous, format = "%H:%M")
  )
  return(Activity_df)
}

# activity_df = activity_detect(segment_data,
#                                boundarys)
