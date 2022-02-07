
#' @name combine_segment_data
#' @title combining segmented data
#'
#' @description Classify 24 hours of data and combining them to the previous section
#'
#' @param binfile A filename of a GENEActiv.bin to process.
#' @param start_time Start_time is split the days up by
#' @param datacols see segmentation in GENEAclassify
#' @param mmap.load see read.bin in GENEAread
#'

combine_segment_data <- function(binfile,
                                 start_time,
                                 datacols,
                                 mmap.load = T) {

  # Naming of the csv file protocol.
  dataname <- naming_protocol(binfile)

  # Take the header file
  header <- header.info(binfile)

  # Check to see if the CSV file for this exists already
  if (!file.exists(file.path(getwd(), "/Outputs/", dataname))) {

    # Finding the first time
    AccData1 <- read.bin(binfile, start = 0, end = 0.01)
    First_Time <- AccData1$data.out[1, 1]

    # Now I need to check that there is at least 24 hours of data
    # Last Time
    AccData2 <- read.bin(binfile, start = 0.99, end = 1)
    Last_Time <- AccData2$data.out[length(AccData2$data.out[, 1]), 1]

    DayNo <- as.numeric(ceiling((Last_Time - First_Time) / 86400))

    # Initialise the segmented data
    segment_data <- c()
    # Break this into 7 steps.
    for (i in 1:DayNo) {
      if (First_Time + i * 86400 < Last_Time + 900) {
        segment_data1 <- getGENEAsegments(binfile,
          start = First_Time + 86400 * (i - 1),
          end = First_Time + 86400 * i,
          mmap.load = mmap.load,
          Use.Timestamps = TRUE,
          changepoint = "UpDownMeanVarDegreesMeanVar",
          penalty = "Manual",
          pen.value1 = 40,
          pen.value2 = 400,
          datacols = datacols,
          intervalseconds = 30,
          mininterval = 1,
          downsample = as.numeric(unlist(header$Value[2])),
          samplefreq = as.numeric(unlist(header$Value[2])), 
          filterorder = 2, 
          boundaries = c(0.5, 5),
          Rp = 3,
          hysteresis = 0.1
        )
        segment_data <- rbind(segment_data, segment_data1)
      } else {
        segment_data1 <- getGENEAsegments(binfile,
          start = First_Time + 86400 * (i - 1),
          end = Last_Time,
          mmap.load = mmap.load,
          Use.Timestamps = TRUE,
          changepoint = "UpDownMeanVarDegreesMeanVar",
          penalty = "Manual",
          pen.value1 = 40,
          pen.value2 = 400,
          datacols = datacols,
          intervalseconds = 30,
          mininterval = 1,
          downsample = as.numeric(unlist(header$Value[2])),
          samplefreq = as.numeric(unlist(header$Value[2])), 
          filterorder = 2, 
          boundaries = c(0.5, 5),
          Rp = 3,
          hysteresis = 0.1
        )
        segment_data <- rbind(segment_data, segment_data1)
      }
    }

    # Add a date into this.
    segment_data$Date <- as.Date(as.POSIXct(as.numeric(segment_data$Start.Time), origin = "1970-01-01"))
    
    # Routine to remove overlap segments.
    k = 1 # Counter
    collen = length(segment_data$Start.Time)
    overlap_list = which(segment_data$Start.Time[1:(collen - k)] > segment_data$Start.Time[(k+1):collen])
    
    while (length(overlap_list) > 0){
      segment_data = segment_data[-overlap_list,]
      k = k + 1 
      overlap_list = which(segment_data$Start.Time[1:(collen - (k))] > segment_data$Start.Time[(k+1):collen])
    }
    
    # Write the data out to the folder
    saveRDS(segment_data, file.path(getwd(), "/Outputs/", dataname))
    
    csvname <- naming_protocol(binfile, suffix = "_All_Data.csv")
    write.csv(segment_data,
              file = file.path(paste0("Outputs/", csvname))
    )
  } else {
    segment_data <- readRDS(file.path(getwd(), "/Outputs/", dataname))
  }
  return(segment_data)
}
