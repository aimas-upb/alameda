
#' @name activity display
#' @title activity display
#'
#' @description displays the classification of a segment as a line plot.
#'
#' @param segment_data GENEActiv.bin Segmented data
#' @param boundarys The Min and Max UNIX timestamp to plot over.
#' @param bed_time A vector from BedRiseDetect that gives the Bed Times of a subject as a UNIX timstamp
#' @param rise_time A vector from BedRiseDetect that gives the Rise Times of a subject as a UNIX timstamp
#' @param first_date The first_date found in the AccData object.
#'

activity_display <- function(segment_data,
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
    ix1 <- segment_data$Start.Time[1:(collen - 1)]
    ix2 <- segment_data$Start.Time[2:(collen)]
    iy1 <- segment_data$Class.current[1:(collen - 1)]
    iy2 <- segment_data$Class.current[1:(collen - 1)]
    ix <- c(rbind(ix1, ix2))
    iy <- c(rbind(iy1, iy2))
    dd <- data.frame(ix, iy)

    #### This should be in a function before here... ####
    # Find the segments between 3pm and 3pm.
    s <- subset(dd,
      ix > as.numeric(boundarys[1]) &
        ix < as.numeric(boundarys[2]),
      select = c(ix, iy)
    ) # Subset to get the data needed

    # # If a bed time has not been found then add 8 hours (3pm to 11pm) to bed_time
    # if (is.na(bed_time)){
    #   bed_time = as.numeric(as.character(unlist(boundarys[1])[1])) + 3600*8
    # }
    #
    # # Rise time then is 3pm minus 8 hours to get 7am.
    # if (is.na(rise_time)){
    #   rise_time = as.numeric(as.character(unlist(boundarys[2])[1])) - 3600*8
    # }

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
      breaks = c(0, 1, 2, 3, 4, 5),
      limits = c(0, 5),
      labels = c(
        "Non-Wear", "Sleep",
        "Sedentary", "Light",
        "Moderate", "Vigorous"
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
