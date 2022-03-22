
#' @name Hypnogram
#' @title Hypnogram 
#' 
#' @description plots a step line of activity
#' 
#' @param segment_data segmented data to analyse
#' @param boundarys min and max boundary for days to analyse
#' @param bed_time as calculated by bed_rise_detect
#' @param rise_time as calculated by bed_rise_detect
#' @param first_date first date analysis starts on.


hypnogram <- function(segment_data, 
                      boundarys, 
                      bed_time, 
                      rise_time, 
                      first_date) {
  # Remove any previous variables
  rm(list = c(
    "collen",
    "ix1", "iy1",
    "ix2", "iy2",
    "ix", "iy",
    "s", "ss", "sss", "ssss"
  ))

  # I need to seperate out the data first otherwise I overwrite loads of data.
  collen <- length(segment_data$Start.Time)

  # If there is no data to report return an error plot
  if (collen < 2) {
    iy <- c(1, 1)
    ix <- boundarys
    s <- data.frame(ix, iy)
    p <- ggplot()
    p <- p + geom_line(aes(
      y = iy,
      x = (as.POSIXct(as.numeric(as.character(ix)), origin = "1970-01-01"))
    ),
    data = s, stat = "identity", colour = "Blue"
    )
    p <- p + labs(x = "Time", y = "")
    p <- p + scale_x_datetime(
      breaks = seq(
        as.POSIXct(as.numeric(as.character(boundarys[1])), origin = "1970-01-01"),
        as.POSIXct(as.numeric(as.character(boundarys[2])), origin = "1970-01-01"),
        "6 hours"
      ),
      labels = date_format("%a-%d\n%H:%M"),
      expand = c(0, 0),
      limits = c(
        as.POSIXct(as.numeric(as.character(boundarys[1])), origin = "1970-01-01"),
        as.POSIXct(as.numeric(as.character(boundarys[2])), origin = "1970-01-01")
      )
    )
    p <- p + scale_y_discrete(
      breaks = c("error"),
      limits = c("error")
    )

    p <- p + theme(plot.margin = unit(c(0, 1.49, 0, 0), "cm"))

    return(p)
  } else {
    ix1 <- c(segment_data$Start.Time[1:(collen - 1)])
    ix2 <- c(segment_data$Start.Time[2:(collen)])

    iy1 <- c(segment_data$Class.current[1:(collen - 1)])
    iy2 <- c(segment_data$Class.current[1:(collen - 1)])

    ix <- c(rbind(ix1, ix2))
    iy <- c(rbind(iy1, iy2))

    dd <- data.frame(ix, iy)

    # Find the segments between 3pm and 3pm.
    s <- subset(dd,
      ix >= as.numeric(boundarys[1]) &
        ix <= as.numeric(boundarys[2]),
      select = c(ix, iy)
    ) # Subset to get the data needed

    if (length(s$ix) > 2) {
      # Adding in the end points to the data frame
      start_point <- c(as.numeric(boundarys[1]), s$iy[1])
      end_point <- c(as.numeric(boundarys[2]), s$iy[length(s$iy)])

      s <- rbind(
        start_point,
        start_point,
        s,
        end_point,
        end_point
      )
    }

    # If a bed time has not been found then add 8 hours (3pm to 11pm) to bed_time
    if (is.na(bed_time)) {
      bed_time <- as.numeric(as.character(unlist(boundarys[1])[1])) + 3600 * 8
    }

    # Rise time then is 3pm minus 8 hours to get 7am.
    if (is.na(rise_time)) {
      rise_time <- as.numeric(as.character(unlist(boundarys[2])[1])) - 3600 * 8
    }

    #' # Taking the data from min boundarys to the known bed time
    #' ss = s[s$ix < as.numeric(as.character(bed_time)) &
    #'        s$ix >= as.numeric(as.character(boundarys[1])),]
    #'
    #' # Changing any SIN events between the min boundarys and bed time to inactive
    #' if (length(ss[ss$iy == 2, 1]) != 0){
    #'   ss[ss$iy == 2, ]$iy = 3
    #' }
    #'
    #' # Changing any Sleep events between the min boundarys and bed time to Day Time nap
    #' if (length(ss[ss$iy == 1, 1]) != 0){
    #'   ss[ss$iy == 1, ]$iy = 1.5
    #' }
    #'
    #' # Taking the data between the Bed and Rise Time.
    #' sss = s[s$ix >= as.numeric(as.character(bed_time)) &
    #'         s$ix <= as.numeric(as.character(rise_time)), ]
    #'
    #' if (length(sss$iy) != 0){sss$iy[1] = 1}
    #' if (length(sss$iy) != 0){sss$iy[length(sss$iy)] = 1}
    #'
    #' # Referring all SIN events to sleep if undecided.
    #' if (length(sss[sss$iy == 2, 1]) != 0){
    #'   sss[sss$iy == 2, ]$iy = 1
    #' }
    #'
    #' ssss = s[s$ix > as.numeric(as.character(rise_time)) &
    #'          s$ix <= as.numeric(as.character(boundarys[2])),]
    #'
    #' # Changing any SIN events between the Rise time and the max boundarys to inactive
    #' if (length(ssss[ssss$iy == 2, 1]) != 0){
    #'   ssss[ssss$iy == 2, ]$iy = 3
    #' }
    #'
    #' # Changing any Sleep events between the Rise time and the max boundarys to Day Time nap
    #' if (length(ssss[ssss$iy == 1, 1]) != 0){
    #'   ssss[ssss$iy == 1, ]$iy = 1.5
    #' }
    #'
    #' #### Checking that the Bed Time does not straddle two different states ####
    #' #' This needs to be a back propagation
    #' #' Checking Bed Time first
    #'
    #' # Change made here.
    #' # Needs to be a line confirming that these  - Using 2 as there needs to be a class that is split!
    #' if (length(ss[,2]) >= 2 & length(sss[,2]) >= 2){
    #'   if (ss[length(ss[,2]),2] != sss[1,2]){
    #'     sss[1,2] = ss[length(ss[,2]), 2]
    #'   }
    #' }
    #'
    #' ## Now looking at the Rise time
    #' if (length(sss[,2]) >= 2 & length(ssss[,2]) >= 2){
    #'   if (sss[length(sss[,2]),2] != ssss[1,2]){
    #'     ssss[1,2] = sss[length(sss[,2]), 2]
    #'   }
    #' }
    #'
    #' s = rbind(ss,sss,ssss)
    #'
    # Day time sleep -> 1.5 as naps?

    ## Changing the active and inactive down 1 on the y axis
    #
    # if (length(s[s$iy == 3,1]) != 0){
    #   s[s$iy == 3,]$iy = 2 # Moving the inactive state down 1.
    # }
    #
    # if (length(s[s$iy == 4,1]) != 0){
    #   s[s$iy == 4,]$iy = 3 # Moving the active state down 1.
    # }
    #
    p <- ggplot()
    p <- p + geom_line(aes(
      y = iy,
      x = (as.POSIXct(as.numeric(as.character(ix)), origin = "1970-01-01"))
    ),
    data = s, stat = "identity", colour = "Blue"
    )
    p <- p + labs(x = "Time", y = "")
    p <- p + scale_x_datetime(
      breaks = seq(
        as.POSIXct(as.numeric(as.character(boundarys[1])), origin = "1970-01-01"),
        as.POSIXct(as.numeric(as.character(boundarys[2])), origin = "1970-01-01"),
        "6 hours"
      ),
      labels = date_format("%a-%d\n%H:%M"),
      expand = c(0, 0),
      limits = c(
        as.POSIXct(as.numeric(as.character(boundarys[1])), origin = "1970-01-01"),
        as.POSIXct(as.numeric(as.character(boundarys[2])), origin = "1970-01-01")
      )
    )
    p <- p + scale_y_continuous(
      breaks = c(0, 1, 1.5, 2, 3),
      limits = c(0, 3),
      labels = c(
        "Non-Wear", "Sleep", "Day-Sleep",
        "Inactive", "Active"
      )
    )

    # Add Red lines to show the Bed and Rise times.
    p <- p + geom_vline(aes(xintercept = bed_time), colour = "#BB0000", size = 1)
    p <- p + geom_vline(aes(xintercept = rise_time), colour = "#BB0000", size = 1)

    # Margins work, "Top", "Right", "Bottom", "Left"
    # Test1  p = p + theme(plot.margin = unit(c(0, 1.32, 0 ,0), "cm"))
    p <- p + theme(plot.margin = unit(c(0, 1.6, 0, 0), "cm"))

    return(p)
  }
}

# I need to run this as an exa,[ple to make sure this works correctly.

# i = 1
# AccData = AccData
# segment_data = segment_data
# boundarys = boundaryss[i,]
# bed_time  = vv$bed_time[i]
# rise_time = vv$rise_time[i]
# first_date = first_date
#
# windows()
# SteplineAS(segment_data,
#            boundarys = boundaryss[i,],
#            bed_time  = vv$bed_time[i],
#            rise_time = vv$rise_time[i],
#            first_date = first_date)
