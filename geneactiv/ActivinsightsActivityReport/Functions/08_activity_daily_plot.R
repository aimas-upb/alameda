
#' @name activity_daily_plot
#' @title activity_daily_plot
#' 
#' @description Create a daily view of the segmented data.
#' 
#' @param AccData object to plot, can be matrix or Accdata object
#' @param segment_data GENEActiv.bin Segmented data
#' @param boundarys The Min and Max UNIX timestamp to plot over.
#' @param bed_time A vector from bed_rise_detect that gives the Bed Times of a subject as a UNIX timstamp
#' @param rise_time A vector from bed_rise_detect that gives the Rise Times of a subject as a UNIX timstamp
#' @param first_date The first_date found in the AccData object. 
#'
#' @details Arranges sleep positionals, light temp and activity display as three plots given on a single A4 landscape page. 
#'

activity_daily_plot <- function(AccData, 
                                segment_data, 
                                boundarys, 
                                bed_time, 
                                rise_time, 
                                first_date){
  
  a = sleep_positionals(AccData, boundarys, bed_time, rise_time, first_date) 
  
  b = light_temp(AccData, boundarys, bed_time, rise_time)
  
  c = activity_display(segment_data, boundarys, bed_time, rise_time, first_date)
  
  plots = c(a,b,c)
  return(multiplot2(a,b,c, cols = 1))
}


multiplot2 <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
