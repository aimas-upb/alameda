
#' @name combine_segment_data
#' @title combining segmented data
#'
#' @description Classify 24 hours of data and combining them to the previous section
#'
#' @param binfile A filename of a GENEActiv.bin to process.
#' @param start_time Start_time is split the days up by
#' @param datacols see segmentation in GENEAclassify
#' @param mmap.load see read.bin in GENEAread

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
    First_Time <- as.numeric(AccData1$data.out[1, 1])

    # Now I need to check that there is at least 24 hours of data
    # Last Time
    AccData2 <- read.bin(binfile, start = 0.99, end = 1)
    Last_Time <- as.numeric(AccData2$data.out[length(AccData2$data.out[, 1]), 1])

    DayNo <- as.numeric(ceiling((Last_Time - First_Time) / 86400))

    First_Start_Time  = as.POSIXct(First_Time, origin = "1970-01-01", tz = "GMT")
    First_Start_Time = as.Date(First_Start_Time)
    First_Start_time = as.POSIXct(paste(First_Start_Time, start_time), origin = "1970-01-01", tz = "GMT")
    First_Start_time = as.numeric(First_Start_time)
    
    # Is the start time before or after the first record time? 
    
    if (First_Time > First_Start_time){
      First_Start_time = First_Start_time + 86400
    } 
    
    # Initialise the segmented data
    segment_data <- c()
    # Break this into partial days -  needs to be based on start_time 
    
    for (i in 0:DayNo) {
      if (i == 0){
        segment_data1 <- getGENEAsegments(binfile,
                                          start = First_Time,
                                          end = First_Start_time,
                                          mmap.load = mmap.load,
                                          Use.Timestamps = TRUE,
                                          changepoint = "UpDownDegrees",
                                          penalty = "Manual",
                                          pen.value1 = 100,
                                          pen.value2 = 400,
                                          datacols = datacols,
                                          intervalseconds = 30,
                                          mininterval = 5,
                                          downsample = as.numeric(unlist(header$Value[2]))
                                          )
      } else if (i == DayNo){
        segment_data1 <- getGENEAsegments(binfile,
                                          start = First_Start_time + 86400 * (i - 1) ,
                                          end = Last_Time,
                                          mmap.load = mmap.load,
                                          Use.Timestamps = TRUE,
                                          changepoint = "UpDownDegrees",
                                          penalty = "Manual",
                                          pen.value1 = 40,
                                          pen.value2 = 400,
                                          datacols = datacols,
                                          intervalseconds = 30,
                                          mininterval = 5,
                                          downsample = as.numeric(unlist(header$Value[2]))
                                          )
      } else {
        segment_data1 <- getGENEAsegments(binfile,
                                          start = First_Start_time + 86400 * (i - 1),
                                          end = First_Start_time + 86400 * i,
                                          mmap.load = mmap.load,
                                          Use.Timestamps = TRUE,
                                          changepoint = "UpDownDegrees",
                                          penalty = "Manual",
                                          pen.value1 = 40,
                                          pen.value2 = 400,
                                          datacols = datacols,
                                          intervalseconds = 30,
                                          mininterval = 5,
                                          downsample = as.numeric(unlist(header$Value[2]))
                                          )
      }
      
      segment_data <- rbind(segment_data, segment_data1)
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
    
    # Write the data out to the folder - This doesnt seem to work? - Need to test 
    saveRDS(segment_data, file.path(getwd(), "/Outputs/", dataname))
    
    # Write out the version of data that we want people to see
    csvname <- naming_protocol(binfile, suffix = "_All_Data.csv")
    
    write.csv(segment_data,
              file = file.path(getwd(), "/Outputs/", csvname))
    
  } else {
    segment_data <- readRDS(file.path(getwd(), "/Outputs/", dataname))
    # dataname = naming_protocol(binfile, suffix = "_All_Data.csv")
    # segment_data <- read.csv(file.path(getwd(), "/Outputs/", dataname))
  }
  return(segment_data)
}

#### Testing file ####
# 
# binfile ="someGENEActiv.bin"
# 
# test_data = combine_segment_data(binfile,
#                                  start_time = "15:00",
#                                  datacols = c("UpDown.mean", "UpDown.var", "UpDown.sd",
#                                               "Degrees.mean", "Degrees.var", "Degrees.sd",
#                                               "Magnitude.mean", "Magnitude.var", 
#                                               "Magnitude.meandiff", "Magnitude.mad",
#                                               "Light.mean", "Light.max",
#                                               "Temp.mean", "Temp.sumdiff", 
#                                               "Temp.meandiff", "Temp.abssumdiff",
#                                               "Temp.sddiff", "Temp.var", 
#                                               "Temp.GENEAskew", "Temp.mad",
#                                               "Step.GENEAcount", "Step.sd", "Step.mean",
#                                               "Principal.Frequency.mean", "Principal.Frequency.median"),
#                                  mmap.load = T)


