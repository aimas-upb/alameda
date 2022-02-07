
#' @name light_temp
#' @title Light and Temperature function plot
#'
#' @description plots the light and temperature from the raw data.
#'
#' @param AccData object to plot, can be matrix or Accdata object
#' @param boundarys The Min and Max UNIX timestamp to plot over.
#' @param bed_time A vector from BedRiseDetect that gives the Bed Times of a subject as a UNIX timstamp
#' @param rise_time A vector from BedRiseDetect that gives the Rise Times of a subject as a UNIX timstamp
#'

light_temp <- function(AccData,
                       boundarys,
                       bed_time,
                       rise_time) {
  res <- 100
  epoch <- floor((nrow(AccData$data.out) / res + 1) / AccData$freq)
  lightobj <- apply.epoch(AccData$data.out, epoch.size = epoch, incl.date = T, function(t) max(t[, 5]))
  tempobj <- apply.epoch(AccData$data.out, epoch.size = epoch, incl.date = T, function(t) mean(t[, 7]))
  timeobj <- apply.epoch(AccData$data.out, epoch.size = epoch, incl.date = T, function(t) mean(t[, 1]))
  time <- timeobj[, 2]
  light <- lightobj[, 2]
  temp <- tempobj[, 2]

  bt <- as.POSIXct(as.numeric(as.character(bed_time)), origin = "1970-01-01")
  rt <- as.POSIXct(as.numeric(as.character(rise_time)), origin = "1970-01-01")
  # Bring together as data frame
  s <- data.frame(
    time = as.POSIXct(as.numeric(as.character(round(time))), origin = "1970-01-01"),
    light,
    temp
  )

  # Melt the dataframe

  dd <- c()
  dd <- melt(s, id.vars = "time", measure.vars = c("light", "temp"))

  # New data frame plot
  # Create the plot to go with this. (This is the hours )

  p <- ggplot(dd, aes(time, value))

  p <- p + geom_line(
    data = dd[dd$variable != "light", ],
    aes(
      y = temp,
      x = time
    ),
    stat = "summary", colour = "red"
  )

  # scalefactor = abs((min(light) - max(light)) / mean(temp)) # Variable axis
  scalefactor <- 600

  p <- p + geom_line(
    data = dd[dd$variable == "light", ],
    aes(
      y = ((value) / scalefactor),
      x = time
    ), colour = "yellow"
  )

  p <- p + scale_y_continuous("Temp Values",
    limits = c(0, 40),
    sec.axis = sec_axis(~ . * scalefactor,
      name = "light values"
    )
  )

  p <- p + labs(x = " ", y = "Temperature")
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
  # Add bed and rise times
  p <- p + geom_vline(aes(xintercept = bt), colour = "#BB0000", size = 1)
  p <- p + geom_vline(aes(xintercept = rt), colour = "#BB0000", size = 1)

  # Margins work, "Top", "Right", "Bottom", "Left"
  # margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")
  p <- p + theme(plot.margin = unit(c(0, 0, 0, 1.10), "cm"))

  return(p)
}
