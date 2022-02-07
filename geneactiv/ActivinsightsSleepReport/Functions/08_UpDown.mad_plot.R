
#' @name UpDown.mad_plot
#' @title  UpDown.mad plot
#' 
#' @description Creates a UpDown.mad plot with 5 rows showing a 48 hour period. 
#' 
#' @param binfile file to process
#' @param segment_data segmented data to analyse
#' @param start_time Start_time is split the days up by
#' @param no_days number of days to iterate over
#' @param bed_time as calculated by bed_rise_detect
#' @param rise_time as calculated by bed_rise_detect
#' 

UpDown.mad_plot <- function(binfile,
                          segment_data,
                          start_time,
                          no_days,
                          bed_time,
                          rise_time) {

  # Creating the variables required for boundary
  t <- as.POSIXct(as.numeric(as.character(segment_data$Start.Time[1])), origin = "1970-01-01")
  first_date <- as.Date(t)

  first_time <- as.POSIXct(as.character(paste(first_date, "00:00")), format = "%Y-%m-%d %H:%M", origin = "1970-01-01")

  # Deciding on the number of days to for loop through dependning on the amount of data avaiable.

  # Set the number of plots to create
  no_plots <- ceiling((no_days) / 5) * 5

  # Find where to subset from.
  # Need to replace the ix[1] with the nearest 3pm value hmm
  min_boundary <- max_boundary <- c()

  for (i in 0:(no_plots)) {
    min_boundary[i + 1] <- first_time + i * 86400 # Gives the first boundaries
    max_boundary[i + 1] <- (first_time + 172800) + i * 86400
  }

  boundarys <- cbind(min_boundary, max_boundary)
  plots <- list()

  # Get the data that is required.
  collen <- length(segment_data$Segment.Start.Time)

  ix1 <- segment_data$Start.Time[1:(collen - 1)]
  ix2 <- segment_data$Start.Time[2:(collen)]
  iy1 <- segment_data$UpDown.mad[1:(collen - 1)]
  iy2 <- segment_data$UpDown.mad[1:(collen - 1)]

  ix <- c(rbind(ix1, ix2))
  iy <- c(rbind(iy1, iy2))
  dd <- data.frame(ix, iy)

  for (j in 1:no_plots) {
    s <- subset(dd,
      ix > min_boundary[j] &
      ix < max_boundary[j],
      select = c(ix, iy)
    ) # Subset to get the data needed

    if (length(s$ix) > 2) {
      # Adding in the end points to the data frame
      start_point <- c(min_boundary[j], s$iy[1])
      end_point <- c(max_boundary[j], s$iy[length(s$iy)])

      s <- rbind(
        start_point,
        s,
        end_point
      )
    }

    bt <- as.POSIXct(as.numeric(as.character(bed_time)), origin = "1970-01-01")
    rt <- as.POSIXct(as.numeric(as.character(rise_time)), origin = "1970-01-01")

    assign(
      paste0("plot", j),
      ggplot() +
        geom_line(aes(
          y = iy,
          x = (as.POSIXct(as.numeric(as.character(ix)), origin = "1970-01-01"))
        ),
        data = s, stat = "identity", colour = "blue"
        ) +
        geom_vline(aes(xintercept = bt), colour = "#BB0000", size = 1) +
        geom_vline(aes(xintercept = rt), colour = "#BB0000", size = 1) +
        labs(x = "", y = "") + scale_x_datetime(
          breaks = seq(
            as.POSIXct(as.numeric(as.character(min_boundary[j])), origin = "1970-01-01"),
            as.POSIXct(as.numeric(as.character(max_boundary[j])), origin = "1970-01-01"),
            "6 hours"
          ),
          labels = date_format("%a-%d-%m-%y\n%H:%M"),
          expand = c(0, 0),
          limits = c(
            as.POSIXct(as.numeric(as.character(min_boundary[j])), origin = "1970-01-01"),
            as.POSIXct(as.numeric(as.character(max_boundary[j])), origin = "1970-01-01")
          )
        ) + coord_cartesian(ylim = c(0, 100)) +
        theme(plot.margin = unit(c(0, 1, 0, 0.0), "cm"))
    )
  }

  # Now arrange them
  multiplot <- function(plots, cols) {
    require(grid)

    # Make a list from the ... arguments and plotlist
    num_plots <- length(plots)

    # Make the panel
    plot_cols <- cols # Number of columns of plots
    plot_rows <- ceiling(num_plots / plot_cols) # Number of rows needed, calculated from # of cols

    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(plot_rows, plot_cols)))
    vplayout <- function(x, y) {
      viewport(layout.pos.row = x, layout.pos.col = y)
    }

    # Make each plot, in the correct location
    for (i in 1:num_plots) {
      cur_row <- ceiling(i / plot_cols)
      cur_col <- (i - 1) %% plot_cols + 1
      print((eval(as.name(plots[[i]]))), vp = vplayout(cur_row, cur_col))
    }
  }

  # Do this inside a single for loop instead
  plots <- list()
  k <- 1
  for (j in 1:(no_plots)) {
    plots[k] <- (paste0("plot", (j)))
    if (k %% 5 == 0) {
      multiplot(plots, cols = 1)
      plots <- list()
      k <- 0
      if (j != no_plots) {
        cat("\n\n")
        cat("#####.")
      }
    }
    k <- k + 1
  }
}

#### Testing the plot ####
# UpDownMadPlot(binfile, segment_data1, start_time, no_days, vv$bed_time, vv$rise_time)
