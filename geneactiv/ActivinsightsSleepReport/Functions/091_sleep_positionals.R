#' @name sleep_positionals
#' @title sleep positionals
#' 
#' 
#' @details Adaptation of positionals to show Bed and Rise Times ####
#' @description This function has been created for the sleep reports.
#'    Currently, this will take far too long to produce and another method might
#'    need to be introduced later where all of the daily plots exist in one function.
#'    This is so data is not read in more than once and might have to come by taking
#'    apart some of the functions within R. However, this will come later.
#'
#' @param AccData Raw acceleration data to plot
#' @param boundarys min and max boundary for days to analyse
#' @param bed_time as calculated by bed_rise_detect
#' @param rise_time as calculated by bed_rise_detect
#' @param first_date first date analysis starts on.
#' 

sleep_positionals <- function(AccData, 
                              boundarys, 
                              bed_time, 
                              rise_time, 
                              first_date) {
  
  # Reading the data in correctly.
  tmp2 <- get.intervals(AccData, incl.date = T)
  
  # Make sure that tmp2 is of a correct length
  if (length(tmp2[1,]) > 3 & length(tmp2[,1]) >= 100){
    ind <- expand((bapply.basic(
      1:nrow(tmp2), 100,
      function(t) {
        sum(cov(1:100, tmp2[t, 2:4])^2) / sum(apply(tmp2[t, 2:4], 2, sd)^2)
      }
    ) / var(1:100)) %bq% "[0, 0.5]", nrow(tmp2))
  
    bt <- as.POSIXct(as.numeric(as.character(bed_time)), origin = "1970-01-01")
    rt <- as.POSIXct(as.numeric(as.character(rise_time)), origin = "1970-01-01")
    
    # Taking the timestamp as our x value
    x <- tmp2[ind, 1]
    # Finding the first date from this
  
    # Y axis as the up arm elevation
    y <- -acos(tmp2[ind, 3] / sqrt(rowSums(tmp2[ind, -1]^2))) * 180 / pi + 90
  
    # Colours on the plot - set as a and b then ab is the difference. this can be changed
    a <- 90
    b <- 360
    ab <- b - a
    cols <- hcl(a:b)
    col <- cols[floor(length(cols) * (sign(-tmp2[ind, 2]) * 180 * acos(-tmp2[ind, 4] / sqrt(rowSums(tmp2[ind, -c(1, 3)]^2))) / pi + 180) / 360) + 1]
    # cols =
    # Dataframe
    xx <- as.POSIXct(tmp2[ind, 1], origin = "1970-01-01")
    s <- data.frame(
      x = as.POSIXct(tmp2[ind, 1], origin = "1970-01-01"),
      y = -acos(tmp2[ind, 3] / sqrt(rowSums(tmp2[ind, -1]^2))) * 180 / pi + 90,
      col
    )
  
    # Legend variables
    lx <- as.numeric(as.character(seq(x[1], quantile(x, 0.1), len = (ab + 1))))
    ly <- rep(95, ab)
  
    # lcol = cols[ floor(length(col)* seq(0.999, 0 , len = 359)) +1 ]
    x1 <- x2 <- y1 <- y2 <- c()
  
    # Set up legend data frame here.
    for (i in 1:(ab)) {
      x1[i] <- as.POSIXct(lx[i], origin = "1970-01-01")
      x2[i] <- as.POSIXct(lx[i + 1], origin = "1970-01-01")
      y1[i] <- 90
      y2[i] <- 110
    }
  
    tempindex <- data.frame(
      xmin = as.POSIXct(x1, origin = "1970-01-01"),
      xmax = as.POSIXct(x2, origin = "1970-01-01"),
      ymin = y1,
      ymax = y2,
      lcol = cols[-(ab + 1)]
    )
  
    tempindex1 <- transform(tempindex,
      id = 1:ab,
      tier = lcol
    )
    # Legend lines - Black lines to break down the colours.
    # The lines Should show 90, 180, 270 and 360 Degrres
  
    lineindex <- rbind(
      tempindex1[1, ],
      tempindex1[90, ],
      tempindex1[180, ],
      tempindex1[ab, ]
    )
  
    #### Conversion to ggplot2 . ####
  
    p <- ggplot(
      data = s,
      aes(
        y = y,
        x = x
      )
    )
  
    p <- p + geom_line(colour = col)
  
    # Adding Legned box
    p <- p + geom_rect(
      data = tempindex1, inherit.aes = FALSE,
      aes(
        xmin = xmin,
        xmax = xmax,
        ymin = ymin,
        ymax = ymax,
        fill = factor(tier)
      ), alpha = 1
    )
    p <- p + scale_fill_manual(values = cols[-(ab + 1)]) + guides(fill = "none")
  
    # Adding the legend lines - Vertical
    p <- p + geom_rect(
      data = lineindex, inherit.aes = FALSE,
      aes(
        xmin = xmin,
        xmax = xmax,
        ymin = ymin,
        ymax = ymax
      ), alpha = 1
    )
  
    # Bottom horizontal
    p <- p + geom_rect(
      data = lineindex, inherit.aes = FALSE,
      aes(
        xmin = xmin,
        xmax = xmax,
        ymin = ymin,
        ymax = ymin
      ), alpha = 1
    )
  
    # Top Horizontal
    p <- p + geom_rect(
      data = lineindex, inherit.aes = FALSE,
      aes(
        xmin = xmin,
        xmax = xmax,
        ymin = ymax,
        ymax = ymax
      ), alpha = 1
    )
  
    # Now adding in the labels for the legend
    p <- p + annotate("text", x = tempindex1$xmin[30], y = 105, label = "CCW")
    p <- p + annotate("text", x = tempindex1$xmin[240], y = 105, label = "CW")
  
    # Degrees points - Can be added later
    # p = p + annotate("text", x = tempindex1$xmin[1], y = 95, label = "90")
    # p = p + annotate("text", x = tempindex1$xmin[89], y = 95, label = "180")
    # p = p + annotate("text", x = tempindex1$xmin[179], y = 95, label = "270")
    # p = p + annotate("text", x = tempindex1$xmin[ab], y = 95, label = "360")
  
    # Adding in the Bed and Rise time lines as red lines
  
    bedcut <- as.POSIXct(as.character(paste(first_date, bed_time)), format = "%Y-%m-%d %H:%M", origin = "1970-01-01")
    risecut <- as.POSIXct(as.character(paste(first_date + 1, rise_time)), format = "%Y-%m-%d %H:%M", origin = "1970-01-01")
  
    BedRiseIndex <- data.frame(
      xmin = c(bedcut, risecut),
      ymin = c(120, -120)
    )
  
    p <- p + geom_rect(
      data = BedRiseIndex, inherit.aes = FALSE,
      aes(
        xmin = xmin,
        xmax = xmin,
        ymin = ymin,
        ymax = ymin
      ), alpha = 1, colour = "red"
    )
  
    # Add Axis labels
    p <- p + labs(x = "", y = "Arm Elevation")
  
    ### library(scale)
  
    # X axis scale - Change to boundaries later for the functions sake
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
  
    # Y axis
    p <- p + scale_y_continuous(limits = c(-120, 120))
  
    # Adding the vertical lines here
    p <- p + geom_vline(aes(xintercept = bt), colour = "#BB0000", size = 1)
    p <- p + geom_vline(aes(xintercept = rt), colour = "#BB0000", size = 1)
  
    # Margins work, "Top", "Right", "Bottom", "Left"
    # margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")
    # Test 1 p = p + theme(plot.margin = unit(c(0, 1.25, 0, 0.85), "cm"))
    p <- p + theme(plot.margin = unit(c(0, 1.6, 0, 0.85), "cm"))
  
    return(p)
  } else {
    p = ggplot()
    return(p)
  }
}

######### Supporting functions #####

expand <- function(X, length = (length(X) * 100)) {
  c(rep(X, each = floor(length / length(X))), rep(tail(X, 1), length - length(X) * floor(length / length(X))))
}

"%bq%" <- function(X, y) {
  if (is.character(y)) {
    if (length(y) == 4) {
      y <- paste(y[1], y[2], ",", y[3], y[4], sep = "")
    }
    y <- strsplit(y, ",")[[1]]
    nc <- nchar(y[2])
    yl <- quantile(X, c(as.numeric(substring(y[1], 2)), as.numeric(substring(y[2], 1, nc - 1))))
    if (substr(y[1], 1, 1) == "[") {
      res <- (X >= yl[1])
    } else {
      res <- (X > yl[1])
    }
    if (substr(y[2], nc, nc) == "]") {
      res <- res & (X <= yl[2])
    } else {
      res <- res & (X < yl[2])
    }
  } else {
    y <- quantile(X, y)
    res <- (X >= y[1]) & (X <= y[2])
  }
  res
}

bapply.basic <- function(X, k, FUN) {
  res <- rep(0, floor(length(X) / k))
  for (i in 1:floor(length(X) / k)) {
    res[i] <- FUN(X[ (i - 1) * k + 1:k])
  }
  return(res)
}

bapply <- function(X, k, FUN) {
  dimout <- length(FUN(X[1:k]))
  res <- matrix(0, dimout, floor(length(X) / k))
  for (i in 1:floor(length(X) / k)) {
    res[(i - 1) * dimout + 1:dimout] <- FUN(X[ (i - 1) * k + 1:k])
    return(res)
  }
}
