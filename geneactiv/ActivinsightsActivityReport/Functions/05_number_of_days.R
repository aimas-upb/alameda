
#' @name number_of_days
#' @title Number of days function
#'
#' @description  File to find the number of days to iterate over.
#'
#' @param binfile file to process
#' @param start_time The time of day to start from.
#'
#' @details start_time details for 3pm for the sleep markdown.
#'
#' @example
#'
#' # number_of_days(binfile, start_time)

number_of_days <- function(binfile,
                           start_time = "15:00") {
  header <- header.info(binfile)
  mt <- as.numeric(unlist(strsplit(unlist(header$Value[3]), split = " "))[1])
  mt_start <- unlist(strsplit(unlist(header$Value[4]), split = " "))[2]

  # Deciding on the number of days to for loop through dependning on the amount of data avaiable.
  l <- readLines(binfile)
  times <- grep("\\bPage Time:\\b", l, value = T)
  rm(list = c("l"))
  # Remove the lines as I no longer need them
  first_time <- as.POSIXct(unlist(strsplit(times[1], split = "Page Time:"))[2])
  last_time <- as.POSIXct(unlist(strsplit(times[length(times)], split = "Page Time:"))[2])
  first_time
  last_time
  no_days <- ceiling(difftime(
    last_time,
    first_time
  ))

  time_diff <- as.numeric(as.GRtime(start_time) - as.GRtime(mt_start))

  if (time_diff > 0) {
    time_diff <- as.numeric(as.GRtime(start_time) - as.GRtime(paste("1 ", mt_start)))
  }

  if (time_diff < -as.numeric(as.GRtime(start_time))) {
    no_days <- no_days - 1
  }

  return(no_days)
}
