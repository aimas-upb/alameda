
#' @name create_df_pcp
#' @title Creating a data frame of piror, current and post data.
#' @Description This function uses the rules created from modelling the various cut points open to us
#'
#' @param segment_data The segmented GENEActiv.bin data.
#' @param Summary_name Summary name of the csv file to write out.
#' @param header Header object passed from header.info
#' @param Magsa_cut The magnitude cut off between Active and SIN
#' @param Duration_Low The Segment Duration cut off point at the low end.
#' @param Duration_high The Segment Duration cut off point at the high end.
#' @param Mad.Score2_Low The low Mad.Score cut off for instances defined as SIN-SIN-SIN
#' @param Mad.Score2_high The high Mad.Score cut off for instances defined as SIN-SIN-SIN
#' @param Mad.Score4_Low The low Mad.Score cut off for instances defined as A-SIN-SIN
#' @param Mad.Score4_high The high Mad.Score cut off for instances defined as A-SIN-SIN
#' @param Mad.Score6_Low The low Mad.Score cut off for instances defined as SIN-SIN-A
#' @param Mad.Score6_high The high Mad.Score cut off for instances defined as SIN-SIN-A
#' @param Mad_low The low Mad.Score cut off for instances defined as A-SIN-A
#' @param Mad_high The high Mad.Score cut off for instances defined as A-SIN-A
#' @param verbose
#'
#' @details The following function creates a data frame from segmented GENEActiv.bin data that has the prior,
#' current and post parts in a single row.
#'

create_df_pcp <- function(segment_data,
                          Summary_name,
                          # Cut_points
                          Magsa_cut = 0.04, # 40mg

                          Duration_low = exp(5.5),
                          Duration_high = exp(8),

                          Mad.Score2_low = 0.5,
                          Mad.Score2_high = 5,

                          Mad.Score4_low = 0.5,
                          Mad.Score4_high = 5,

                          Mad.Score6_low = 0.5,
                          Mad.Score6_high = 5,

                          Mad_low = 0.5,
                          Mad_high = 5,

                          verbose = FALSE) {

  #### Adding our new rules here! ####
  #' As R is terrible we're using numbers instead of factors.
  #' The numbers are:
  #'  0. Non-Wear
  #'  1. Sleep
  #'  2. SIN
  #'  3. Inactive
  #'  4. Active

  segment_data$Class.rules <- 2 # SIN to start with.

  #### Additional info ####
  #' From the binning methods we can see the classifications taking place
  #' I want to keep a log of when each file has been classified. (Which bin)
  #'
  #' 0. Undecided.
  #' 1. SIN-SIN-SIN
  #' 2. A-SIN-SIN
  #' 3. SIN-SIN-A
  #' 4. A-SIN-A

  segment_data$Binning_Method <- 0

  df <- segment_data

  df$Temp.ratio <- df$Temp.sumdiff / df$Temp.abssumdiff
  df[is.na(df$Temp.ratio), ]$Temp.ratio <- 0

  # Setting active rules
  if (length(df[df$Magnitude.mean > 0.04, ][, 1]) != 0) {
    df[df$Magnitude.mean > 0.04, ]$Class.rules <- 4 # "Active"
  }


  #### Non-Wear Rules ####
  # Non-Wear Rules
  if (length(df[df$UpDown.mad < 0.1 & abs(df$Temp.ratio) == 1, ][, 1]) != 0) {
    df[df$UpDown.mad < 0.1 &
      abs(df$Temp.ratio) == 1, ]$Class.rules <- 0 # "Non-Wear"
  }

  if (length(df[df$Segment.Duration > 3600, ][, 1]) != 0) {
    df[df$Segment.Duration > 3600, ]$Class.rules <- 0 # "Non-Wear"
  }

  # Non-wear rule if between Duration low and high
  # And delta T = -1 for medium length non-wear parts.
  if (length(df[df$Segment.Duration.current > Duration_low &
    df$Segment.Duration.current < Duration_high &
    df$UpDown.mad.current < Mad_low &
    df$Temp.ratio.current == -1, 1]) != 0) {
    df[df$Segment.Duration.current > Duration_low &
      df$Segment.Duration.current < Duration_high &
      df$UpDown.mad.current < Mad_low &
      df$Temp.ratio.current == -1, ]$Class.rules <- 0 # "Non-Wear"
  }

  #### SIN/Active Score ####
  # Creating Post and Prior variables #
  for (j in 1:length(df[, 1])) {
    df$SIN.Score1[j] <- sum(df[df$Class.rules == 2 & # SIN
      df$Start.Time < (df$Start.Time[j] + 9000) &
      df$Start.Time > (df$Start.Time[j] - 9000), ]$Segment.Duration) # 6683)

    df$Active.Score1[j] <- sum(df[df$Class.rules == 4 & # Active
      df$Start.Time < (df$Start.Time[j] + 18000) &
      df$Start.Time > (df$Start.Time[j] - 18000), ]$Segment.Duration) # 6683)
  }

  #### df_pcp dataframe ####
  df_prior <- df[(1:(length(df[, 1]) - 2)), ]
  df_current <- df[(2:(length(df[, 1]) - 1)), ]
  df_post <- df[(3:length(df[, 1])), ]

  df_pcp <- data.frame(
    # Need some features on what file and time
    "Filename" = df_current$Source,
    "Segment.Start.Time" = df_current$Segment.Start.Time,
    "Start.Time" = df_current$Start.Time,

    "Segment.Duration.prior" = df_prior$Segment.Duration,
    "Segment.Duration.current" = df_current$Segment.Duration,
    "Segment.Duration.post" = df_post$Segment.Duration,

    "UpDown.mad.prior" = df_prior$UpDown.mad,
    "UpDown.mad.current" = df_current$UpDown.mad,
    "UpDown.mad.post" = df_post$UpDown.mad,

    "Temp.ratio.prior" = df_prior$Temp.sumdiff / df_prior$Temp.abssumdiff,
    "Temp.ratio.current" = df_current$Temp.sumdiff / df_current$Temp.abssumdiff,
    "Temp.ratio.post" = df_post$Temp.sumdiff / df_post$Temp.abssumdiff,

    "Magnitude.mean.prior" = df_prior$Magnitude.mean,
    "Magnitude.mean.current" = df_current$Magnitude.mean,
    "Magnitude.mean.post" = df_post$Magnitude.mean,

    "Class.prior" = df_prior$Class.rules,
    "Class.current" = df_current$Class.rules,
    "Class.post" = df_post$Class.rules,

    "SIN.prior" = df_prior$SIN.Score1,
    "SIN.current" = df_current$SIN.Score1,
    "SIN.post" = df_post$SIN.Score1,

    "Active.prior" = df_prior$Active.Score1,
    "Active.current" = df_current$Active.Score1,
    "Active.post" = df_post$Active.Score1,

    "Binning_Method" = df_current$Binning_Method
  )

  # Create a mad score1
  df_pcp$Mad.Score1 <- df_pcp$UpDown.mad.prior *
    df_pcp$UpDown.mad.current *
    df_pcp$UpDown.mad.post

  df_pcp$Mad.Score2 <- (df_pcp$UpDown.mad.prior +
    df_pcp$UpDown.mad.current +
    df_pcp$UpDown.mad.post) / 3

  df_pcp$Mad.Score3 <- df_pcp$UpDown.mad.current *
    df_pcp$UpDown.mad.post

  df_pcp$Mad.Score4 <- (df_pcp$UpDown.mad.current +
    df_pcp$UpDown.mad.post) / 2

  df_pcp$Mad.Score5 <- df_pcp$UpDown.mad.prior *
    df_pcp$UpDown.mad.current

  df_pcp$Mad.Score6 <- (df_pcp$UpDown.mad.prior +
    df_pcp$UpDown.mad.current) / 2

  # Reset Tempnas
  if (length(df_pcp[is.na(df_pcp$Temp.ratio.prior), ]$Temp.ratio.prior) > 0) {
    df_pcp[is.na(df_pcp$Temp.ratio.prior), ]$Temp.ratio.prior <- 0
  }
  if (length(df_pcp[is.na(df_pcp$Temp.ratio.current), ]$Temp.ratio.current) > 0) {
    df_pcp[is.na(df_pcp$Temp.ratio.current), ]$Temp.ratio.current <- 0
  }
  if (length(df_pcp[is.na(df_pcp$Temp.ratio.post), ]$Temp.ratio.post) > 0) {
    df_pcp[is.na(df_pcp$Temp.ratio.post), ]$Temp.ratio.post <- 0
  }

  df_pcp <- na.omit(df_pcp)

  # inactive from non-wear
  if (length(df_pcp[df_pcp$Class.prior == 0 &
    df_pcp$Class.current == 2, ][, 1]) != 0) {
    df_pcp[df_pcp$Class.prior == 0 &
      df_pcp$Class.current == 2, ]$Class.current <- 3
  }

  #### Sin_sin_sin ####
  ## Cut 1.12 Sleep
  if (length(df_pcp[df_pcp$Magnitude.mean.post < Magsa_cut &
    df_pcp$Magnitude.mean.prior < Magsa_cut &
    df_pcp$Class.current == 2 & # Don't overwrite Non-Wear
    df_pcp$Segment.Duration.current > Duration_high, 1]) != 0) {
    df_pcp[df_pcp$Magnitude.mean.post < Magsa_cut &
      df_pcp$Magnitude.mean.prior < Magsa_cut &
      df_pcp$Class.current == 2 &
      df_pcp$Segment.Duration.current > Duration_high, ]$Binning_Method <- 1.12

    df_pcp[df_pcp$Magnitude.mean.post < Magsa_cut &
      df_pcp$Magnitude.mean.prior < Magsa_cut &
      df_pcp$Class.current == 2 &
      df_pcp$Segment.Duration.current > Duration_high, ]$Class.current <- 1 # Sleep
  }

  ## Cut 1.200 - Sleep
  if (length(df_pcp[df_pcp$Magnitude.mean.post < Magsa_cut &
    df_pcp$Magnitude.mean.prior < Magsa_cut &
    df_pcp$Class.current == 2 & # Don't overwrite Non-Wear
    df_pcp$Segment.Duration.current < Duration_low &
    df_pcp$Mad.Score2 < Mad.Score2_low, 1]) != 0) {
    df_pcp[df_pcp$Magnitude.mean.post < Magsa_cut &
      df_pcp$Magnitude.mean.prior < Magsa_cut &
      df_pcp$Class.current == 2 & # Don't overwrite Non-Wear
      df_pcp$Segment.Duration.current < Duration_low &
      df_pcp$Mad.Score2 < Mad.Score2_low, ]$Binning_Method <- 1.200

    df_pcp[df_pcp$Magnitude.mean.post < Magsa_cut &
      df_pcp$Magnitude.mean.prior < Magsa_cut &
      df_pcp$Class.current == 2 & # Don't overwrite Non-Wear
      df_pcp$Segment.Duration.current < Duration_low &
      df_pcp$Mad.Score2 < Mad.Score2_low, ]$Class.current <- 1 # Sleep
  }

  ## Cut 1.202 - Inactive
  if (length(df_pcp[df_pcp$Magnitude.mean.post < Magsa_cut &
    df_pcp$Magnitude.mean.prior < Magsa_cut &
    df_pcp$Class.current == 2 & # Don't overwrite Non-Wear
    df_pcp$Segment.Duration.current < Duration_low &
    df_pcp$Mad.Score2 > Mad.Score2_high, 1]) != 0) {
    df_pcp[df_pcp$Magnitude.mean.post < Magsa_cut &
      df_pcp$Magnitude.mean.prior < Magsa_cut &
      df_pcp$Class.current == 2 & # Don't overwrite Non-Wear
      df_pcp$Segment.Duration.current < Duration_low &
      df_pcp$Mad.Score2 > Mad.Score2_high, ]$Binning_Method <- 1.202

    df_pcp[df_pcp$Magnitude.mean.post < Magsa_cut &
      df_pcp$Magnitude.mean.prior < Magsa_cut &
      df_pcp$Class.current == 2 & # Don't overwrite Non-Wear
      df_pcp$Segment.Duration.current < Duration_low &
      df_pcp$Mad.Score2 > Mad.Score2_high, ]$Class.current <- 3 # Inactive
  }

  ## Need to remove the segments that have large segments either side
  # Cut 1.1010 - Inactive
  if (length(df_pcp[df_pcp$Magnitude.mean.post < Magsa_cut &
    df_pcp$Magnitude.mean.prior < Magsa_cut &
    df_pcp$Class.current == 2 & # Don't overwrite Non-Wear
    df_pcp$Segment.Duration.current < Duration_low &
    df_pcp$Mad.Score2 > Mad.Score2_low &
    df_pcp$Mad.Score2 < Mad.Score2_high &
    df_pcp$Segment.Duration.prior < Duration_low, 1]) != 0) {
    df_pcp[df_pcp$Magnitude.mean.post < Magsa_cut &
      df_pcp$Magnitude.mean.prior < Magsa_cut &
      df_pcp$Class.current == 2 & # Don't overwrite Non-Wear
      df_pcp$Segment.Duration.current < Duration_low &
      df_pcp$Mad.Score2 > Mad.Score2_low &
      df_pcp$Mad.Score2 < Mad.Score2_high &
      df_pcp$Segment.Duration.prior < Duration_low, ]$Binning_Method <- 1.1010 # Inactive

    df_pcp[df_pcp$Magnitude.mean.post < Magsa_cut &
      df_pcp$Magnitude.mean.prior < Magsa_cut &
      df_pcp$Class.current == 2 & # Don't overwrite Non-Wear
      df_pcp$Segment.Duration.current < Duration_low &
      df_pcp$Mad.Score2 > Mad.Score2_low &
      df_pcp$Mad.Score2 < Mad.Score2_high &
      df_pcp$Segment.Duration.prior < Duration_low, ]$Class.current <- 3 # Inactive
  }

  ## Cut 1.1012 - Sleep
  if (length(df_pcp[df_pcp$Magnitude.mean.post < Magsa_cut &
    df_pcp$Magnitude.mean.prior < Magsa_cut &
    df_pcp$Class.current == 2 & # Don't overwrite Non-Wear
    df_pcp$Segment.Duration.current < Duration_low &
    df_pcp$Mad.Score2 > Mad.Score2_low &
    df_pcp$Mad.Score2 < Mad.Score2_high &
    df_pcp$Segment.Duration.prior > Duration_low, 1]) != 0) {
    df_pcp[df_pcp$Magnitude.mean.post < Magsa_cut &
      df_pcp$Magnitude.mean.prior < Magsa_cut &
      df_pcp$Class.current == 2 & # Don't overwrite Non-Wear
      df_pcp$Segment.Duration.current < Duration_low &
      df_pcp$Mad.Score2 > Mad.Score2_low &
      df_pcp$Mad.Score2 < Mad.Score2_high &
      df_pcp$Segment.Duration.prior > Duration_low, ]$Binning_Method <- 1.1012

    df_pcp[df_pcp$Magnitude.mean.post < Magsa_cut &
      df_pcp$Magnitude.mean.prior < Magsa_cut &
      df_pcp$Class.current == 2 & # Don't overwrite Non-Wear
      df_pcp$Segment.Duration.current < Duration_low &
      df_pcp$Mad.Score2 > Mad.Score2_low &
      df_pcp$Mad.Score2 < Mad.Score2_high &
      df_pcp$Segment.Duration.prior > Duration_low, ]$Class.current <- 1 # Sleep
  }


  ## Cut 1.1010 - Inactive
  if (length(df_pcp[df_pcp$Magnitude.mean.post < Magsa_cut &
    df_pcp$Magnitude.mean.prior < Magsa_cut &
    df_pcp$Class.current == 2 & # Don't overwrite Non-Wear
    df_pcp$Segment.Duration.current < Duration_low &
    df_pcp$Mad.Score2 > Mad.Score2_low &
    df_pcp$Mad.Score2 < Mad.Score2_high &
    df_pcp$Segment.Duration.post < Duration_low, 1]) != 0) {
    df_pcp[df_pcp$Magnitude.mean.post < Magsa_cut &
      df_pcp$Magnitude.mean.prior < Magsa_cut &
      df_pcp$Class.current == 2 & # Don't overwrite Non-Wear
      df_pcp$Segment.Duration.current < Duration_low &
      df_pcp$Mad.Score2 > Mad.Score2_low &
      df_pcp$Mad.Score2 < Mad.Score2_high &
      df_pcp$Segment.Duration.post < Duration_low, ]$Binning_Method <- 1.1010 # Inactive

    df_pcp[df_pcp$Magnitude.mean.post < Magsa_cut &
      df_pcp$Magnitude.mean.prior < Magsa_cut &
      df_pcp$Class.current == 2 & # Don't overwrite Non-Wear
      df_pcp$Segment.Duration.current < Duration_low &
      df_pcp$Mad.Score2 > Mad.Score2_low &
      df_pcp$Mad.Score2 < Mad.Score2_high &
      df_pcp$Segment.Duration.post < Duration_low, ]$Class.current <- 3 # Inactive
  }

  ## Cut 1.1012 - Sleep
  if (length(df_pcp[df_pcp$Magnitude.mean.post < Magsa_cut &
    df_pcp$Magnitude.mean.prior < Magsa_cut &
    df_pcp$Class.current == 2 & # Don't overwrite Non-Wear
    df_pcp$Segment.Duration.current < Duration_low &
    df_pcp$Mad.Score2 > Mad.Score2_low &
    df_pcp$Mad.Score2 < Mad.Score2_high &
    df_pcp$Segment.Duration.post > Duration_low, 1]) != 0) {
    df_pcp[df_pcp$Magnitude.mean.post < Magsa_cut &
      df_pcp$Magnitude.mean.prior < Magsa_cut &
      df_pcp$Class.current == 2 & # Don't overwrite Non-Wear
      df_pcp$Segment.Duration.current < Duration_low &
      df_pcp$Mad.Score2 > Mad.Score2_low &
      df_pcp$Mad.Score2 < Mad.Score2_high &
      df_pcp$Segment.Duration.post > Duration_low, ]$Binning_Method <- 1.1012

    df_pcp[df_pcp$Magnitude.mean.post < Magsa_cut &
      df_pcp$Magnitude.mean.prior < Magsa_cut &
      df_pcp$Class.current == 2 & # Don't overwrite Non-Wear
      df_pcp$Segment.Duration.current < Duration_low &
      df_pcp$Mad.Score2 > Mad.Score2_low &
      df_pcp$Mad.Score2 < Mad.Score2_high &
      df_pcp$Segment.Duration.post > Duration_low, ]$Class.current <- 1 # Sleep
  }


  ## Cut 1.110 - sleep
  if (length(df_pcp[df_pcp$Magnitude.mean.post < Magsa_cut &
    df_pcp$Magnitude.mean.prior < Magsa_cut &
    df_pcp$Class.current == 2 & # Don't overwrite Non-Wear
    df_pcp$Segment.Duration.current > Duration_low &
    df_pcp$Segment.Duration.current < Duration_high &
    df_pcp$Mad.Score2 < Mad.Score2_low, 1]) != 0) {
    df_pcp[df_pcp$Magnitude.mean.post < Magsa_cut &
      df_pcp$Magnitude.mean.prior < Magsa_cut &
      df_pcp$Class.current == 2 & # Don't overwrite Non-Wear
      df_pcp$Segment.Duration.current > Duration_low &
      df_pcp$Segment.Duration.current < Duration_high &
      df_pcp$Mad.Score2 < Mad.Score2_low, ]$Binning_Method <- 1.110

    df_pcp[df_pcp$Magnitude.mean.post < Magsa_cut &
      df_pcp$Magnitude.mean.prior < Magsa_cut &
      df_pcp$Class.current == 2 & # Don't overwrite Non-Wear
      df_pcp$Segment.Duration.current > Duration_low &
      df_pcp$Segment.Duration.current < Duration_high &
      df_pcp$Mad.Score2 < Mad.Score2_low, ]$Class.current <- 1 # Sleep
  }

  ## Cut 1.111
  if (length(df_pcp[df_pcp$Magnitude.mean.post < Magsa_cut &
    df_pcp$Magnitude.mean.prior < Magsa_cut &
    df_pcp$Class.current == 2 & # Don't overwrite Non-Wear
    df_pcp$Segment.Duration.current > Duration_low &
    df_pcp$Segment.Duration.current < Duration_high &
    df_pcp$Mad.Score2 > Mad.Score2_high, 1]) != 0) {
    df_pcp[df_pcp$Magnitude.mean.post < Magsa_cut &
      df_pcp$Magnitude.mean.prior < Magsa_cut &
      df_pcp$Class.current == 2 & # Don't overwrite Non-Wear
      df_pcp$Segment.Duration.current > Duration_low &
      df_pcp$Segment.Duration.current < Duration_high &
      df_pcp$Mad.Score2 > Mad.Score2_high, ]$Binning_Method <- 1.111

    df_pcp[df_pcp$Magnitude.mean.post < Magsa_cut &
      df_pcp$Magnitude.mean.prior < Magsa_cut &
      df_pcp$Class.current == 2 & # Don't overwrite Non-Wear
      df_pcp$Segment.Duration.current > Duration_low &
      df_pcp$Segment.Duration.current < Duration_high &
      df_pcp$Mad.Score2 > Mad.Score2_high, ]$Class.current <- 3 # Inactive
  }

  ### Looking in front and behind for long segments ###
  # Grid like this
  #'             | Post_short | Post_long
  #' Prior_short |  1. Inactive  | 3. Sleep
  #' Prior_long  |  2. Sleep     | 0. Sleep

  # in front is 3 and behind is 4. If both are larger then definitely is sleep
  ## Cut 1.1110 - Sleep
  if (length(df_pcp[df_pcp$Magnitude.mean.post < Magsa_cut &
    df_pcp$Magnitude.mean.prior < Magsa_cut &
    df_pcp$Class.current == 2 & # Don't overwrite Non-Wear
    df_pcp$Segment.Duration.current > Duration_low &
    df_pcp$Segment.Duration.current < Duration_high &
    df_pcp$Mad.Score2 > Mad.Score2_low &
    df_pcp$Mad.Score2 < Mad.Score2_high &
    df_pcp$Segment.Duration.prior > Duration_low &
    df_pcp$Segment.Duration.post > Duration_low, 1]) != 0) {
    df_pcp[df_pcp$Magnitude.mean.post < Magsa_cut &
      df_pcp$Magnitude.mean.prior < Magsa_cut &
      df_pcp$Class.current == 2 & # Don't overwrite Non-Wear
      df_pcp$Segment.Duration.current > Duration_low &
      df_pcp$Segment.Duration.current < Duration_high &
      df_pcp$Mad.Score2 > Mad.Score2_low &
      df_pcp$Mad.Score2 < Mad.Score2_high &
      df_pcp$Segment.Duration.prior > Duration_low &
      df_pcp$Segment.Duration.post > Duration_low, ]$Binning_Method <- 1.1110

    df_pcp[df_pcp$Magnitude.mean.post < Magsa_cut &
      df_pcp$Magnitude.mean.prior < Magsa_cut &
      df_pcp$Class.current == 2 & # Don't overwrite Non-Wear
      df_pcp$Segment.Duration.current > Duration_low &
      df_pcp$Segment.Duration.current < Duration_high &
      df_pcp$Mad.Score2 > Mad.Score2_low &
      df_pcp$Mad.Score2 < Mad.Score2_high &
      df_pcp$Segment.Duration.prior > Duration_low &
      df_pcp$Segment.Duration.post > Duration_low, ]$Class.current <- 1 # Sleep
  }

  # cut 1.1111
  if (length(df_pcp[df_pcp$Magnitude.mean.post < Magsa_cut &
    df_pcp$Magnitude.mean.prior < Magsa_cut &
    df_pcp$Class.current == 2 & # Don't overwrite Non-Wear
    df_pcp$Segment.Duration.current > Duration_low &
    df_pcp$Segment.Duration.current < Duration_high &
    df_pcp$Mad.Score2 > Mad.Score2_low &
    df_pcp$Mad.Score2 < Mad.Score2_high &
    df_pcp$Segment.Duration.prior < Duration_low &
    df_pcp$Segment.Duration.post < Duration_low, 1]) != 0) {
    df_pcp[df_pcp$Magnitude.mean.post < Magsa_cut &
      df_pcp$Magnitude.mean.prior < Magsa_cut &
      df_pcp$Class.current == 2 & # Don't overwrite Non-Wear
      df_pcp$Segment.Duration.current > Duration_low &
      df_pcp$Segment.Duration.current < Duration_high &
      df_pcp$Mad.Score2 > Mad.Score2_low &
      df_pcp$Mad.Score2 < Mad.Score2_high &
      df_pcp$Segment.Duration.prior < Duration_low &
      df_pcp$Segment.Duration.post < Duration_low, ]$Binning_Method <- 1.1111

    df_pcp[df_pcp$Magnitude.mean.post < Magsa_cut &
      df_pcp$Magnitude.mean.prior < Magsa_cut &
      df_pcp$Class.current == 2 & # Don't overwrite Non-Wear
      df_pcp$Segment.Duration.current > Duration_low &
      df_pcp$Segment.Duration.current < Duration_high &
      df_pcp$Mad.Score2 > Mad.Score2_low &
      df_pcp$Mad.Score2 < Mad.Score2_high &
      df_pcp$Segment.Duration.prior < Duration_low &
      df_pcp$Segment.Duration.post < Duration_low, ]$Class.current <- 3 # Inactive
  }

  # Cut 1.1112
  if (length(df_pcp[df_pcp$Magnitude.mean.post < Magsa_cut &
    df_pcp$Magnitude.mean.prior < Magsa_cut &
    df_pcp$Class.current == 2 & # Don't overwrite Non-Wear
    df_pcp$Segment.Duration.current > Duration_low &
    df_pcp$Segment.Duration.current < Duration_high &
    df_pcp$Mad.Score2 > Mad.Score2_low &
    df_pcp$Mad.Score2 < Mad.Score2_high &
    df_pcp$Segment.Duration.prior < Duration_low &
    df_pcp$Segment.Duration.post > Duration_low, 1]) != 0) {
    df_pcp[df_pcp$Magnitude.mean.post < Magsa_cut &
      df_pcp$Magnitude.mean.prior < Magsa_cut &
      df_pcp$Class.current == 2 & # Don't overwrite Non-Wear
      df_pcp$Segment.Duration.current > Duration_low &
      df_pcp$Segment.Duration.current < Duration_high &
      df_pcp$Mad.Score2 > Mad.Score2_low &
      df_pcp$Mad.Score2 < Mad.Score2_high &
      df_pcp$Segment.Duration.prior < Duration_low &
      df_pcp$Segment.Duration.post > Duration_low, ]$Binning_Method <- 1.1112

    df_pcp[df_pcp$Magnitude.mean.post < Magsa_cut &
      df_pcp$Magnitude.mean.prior < Magsa_cut &
      df_pcp$Class.current == 2 & # Don't overwrite Non-Wear
      df_pcp$Segment.Duration.current > Duration_low &
      df_pcp$Segment.Duration.current < Duration_high &
      df_pcp$Mad.Score2 > Mad.Score2_low &
      df_pcp$Mad.Score2 < Mad.Score2_high &
      df_pcp$Segment.Duration.prior < Duration_low &
      df_pcp$Segment.Duration.post > Duration_low, ]$Class.current <- 1 # Sleep
  }

  # Cut 1.1113
  if (length(df_pcp[df_pcp$Magnitude.mean.post < Magsa_cut &
    df_pcp$Magnitude.mean.prior < Magsa_cut &
    df_pcp$Class.current == 2 & # Don't overwrite Non-Wear
    df_pcp$Segment.Duration.current > Duration_low &
    df_pcp$Segment.Duration.current < Duration_high &
    df_pcp$Mad.Score2 > Mad.Score2_low &
    df_pcp$Mad.Score2 < Mad.Score2_high &
    df_pcp$Segment.Duration.prior > Duration_low &
    df_pcp$Segment.Duration.post < Duration_low, 1]) != 0) {
    df_pcp[df_pcp$Magnitude.mean.post < Magsa_cut &
      df_pcp$Magnitude.mean.prior < Magsa_cut &
      df_pcp$Class.current == 2 & # Don't overwrite Non-Wear
      df_pcp$Segment.Duration.current > Duration_low &
      df_pcp$Segment.Duration.current < Duration_high &
      df_pcp$Mad.Score2 > Mad.Score2_low &
      df_pcp$Mad.Score2 < Mad.Score2_high &
      df_pcp$Segment.Duration.prior > Duration_low &
      df_pcp$Segment.Duration.post < Duration_low, ]$Binning_Method <- 1.1113

    df_pcp[df_pcp$Magnitude.mean.post < Magsa_cut &
      df_pcp$Magnitude.mean.prior < Magsa_cut &
      df_pcp$Class.current == 2 & # Don't overwrite Non-Wear
      df_pcp$Segment.Duration.current > Duration_low &
      df_pcp$Segment.Duration.current < Duration_high &
      df_pcp$Mad.Score2 > Mad.Score2_low &
      df_pcp$Mad.Score2 < Mad.Score2_high &
      df_pcp$Segment.Duration.prior > Duration_low &
      df_pcp$Segment.Duration.post < Duration_low, ]$Class.current <- 1 # Sleep
  }

  #### A_Sin_Sin ####
  ## From A_SIN_SIN ##
  # Cut 2.12
  if (length(df_pcp[df_pcp$Magnitude.mean.post < Magsa_cut &
    df_pcp$Magnitude.mean.prior > Magsa_cut &
    df_pcp$Class.current == 2 & # Don't overwrite Non-Wear
    df_pcp$Segment.Duration.current > Duration_high, 1]) != 0) {
    df_pcp[df_pcp$Magnitude.mean.post < Magsa_cut &
      df_pcp$Magnitude.mean.prior > Magsa_cut &
      df_pcp$Class.current == 2 &
      df_pcp$Segment.Duration.current > Duration_high, ]$Binning_Method <- 2.12

    df_pcp[df_pcp$Magnitude.mean.post < Magsa_cut &
      df_pcp$Magnitude.mean.prior > Magsa_cut &
      df_pcp$Class.current == 2 &
      df_pcp$Segment.Duration.current > Duration_high, ]$Class.current <- 1 # Sleep
  }

  ## Cut 2.202
  # Inactive 1.0.2
  if (length(df_pcp[df_pcp$Magnitude.mean.prior > Magsa_cut &
    df_pcp$Magnitude.mean.post < Magsa_cut &
    df_pcp$Class.current == 2 & # Don't overwrite Non-Wear
    df_pcp$Segment.Duration.current < Duration_low &
    df_pcp$Mad.Score4 > Mad.Score4_high, 1]) != 0) {
    df_pcp[df_pcp$Magnitude.mean.prior > Magsa_cut &
      df_pcp$Magnitude.mean.post < Magsa_cut &
      df_pcp$Class.current == 2 & # Don't overwrite Non-Wear
      df_pcp$Segment.Duration.current < Duration_low &
      df_pcp$Mad.Score4 > Mad.Score4_high, ]$Binning_Method <- 2.202 # Inactive

    df_pcp[df_pcp$Magnitude.mean.prior > Magsa_cut &
      df_pcp$Magnitude.mean.post < Magsa_cut &
      df_pcp$Class.current == 2 & # Don't overwrite Non-Wear
      df_pcp$Segment.Duration.current < Duration_low &
      df_pcp$Mad.Score4 > Mad.Score4_high, ]$Class.current <- 3 # Inactive
  }

  ## Cut 2.200
  # Sleep 1.0.0
  if (length(df_pcp[df_pcp$Magnitude.mean.prior > Magsa_cut &
    df_pcp$Magnitude.mean.post < Magsa_cut &
    df_pcp$Class.current == 2 & # Don't overwrite Non-Wear
    df_pcp$Segment.Duration.current < Duration_low &
    df_pcp$Mad.Score4 < Mad.Score4_low, 1]) != 0) {
    df_pcp[df_pcp$Magnitude.mean.prior > Magsa_cut &
      df_pcp$Magnitude.mean.post < Magsa_cut &
      df_pcp$Class.current == 2 & # Don't overwrite Non-Wear
      df_pcp$Segment.Duration.current < Duration_low &
      df_pcp$Segment.Duration.current < Duration_high &
      df_pcp$Mad.Score4 < Mad.Score4_low, ]$Binning_Method <- 2.200

    df_pcp[df_pcp$Magnitude.mean.prior > Magsa_cut &
      df_pcp$Magnitude.mean.post < Magsa_cut &
      df_pcp$Class.current == 2 & # Don't overwrite Non-Wear
      df_pcp$Segment.Duration.current < Duration_low &
      df_pcp$Mad.Score4 < Mad.Score4_low, ]$Class.current <- 1 # Sleep
  }

  ## Cut 2.1010
  # Inactive 1.0.1.0
  if (length(df_pcp[df_pcp$Magnitude.mean.prior > Magsa_cut &
    df_pcp$Magnitude.mean.post < Magsa_cut &
    df_pcp$Class.current == 2 & # Don't overwrite Non-Wear
    df_pcp$Segment.Duration.current < Duration_low &
    df_pcp$Mad.Score4 > Mad.Score4_low &
    df_pcp$Mad.Score4 < Mad.Score4_high &
    df_pcp$Segment.Duration.post < Duration_low, 1]) != 0) {
    df_pcp[df_pcp$Magnitude.mean.prior > Magsa_cut &
      df_pcp$Magnitude.mean.post < Magsa_cut &
      df_pcp$Class.current == 2 & # Don't overwrite Non-Wear
      df_pcp$Segment.Duration.current < Duration_low &
      df_pcp$Mad.Score4 > Mad.Score4_low &
      df_pcp$Mad.Score4 < Mad.Score4_high &
      df_pcp$Segment.Duration.post < Duration_low, ]$Binning_Method <- 2.1010

    df_pcp[df_pcp$Magnitude.mean.prior > Magsa_cut &
      df_pcp$Magnitude.mean.post < Magsa_cut &
      df_pcp$Class.current == 2 & # Don't overwrite Non-Wear
      df_pcp$Segment.Duration.current < Duration_low &
      df_pcp$Mad.Score4 > Mad.Score4_low &
      df_pcp$Mad.Score4 < Mad.Score4_high &
      df_pcp$Segment.Duration.post < Duration_low, ]$Class.current <- 3 # Inactive
  }

  ## Cut 2.1011
  # Sleep 1.0.1.1
  if (length(df_pcp[df_pcp$Magnitude.mean.prior > Magsa_cut &
    df_pcp$Magnitude.mean.post < Magsa_cut &
    df_pcp$Class.current == 2 & # Don't overwrite Non-Wear
    df_pcp$Segment.Duration.current < Duration_low &
    df_pcp$Mad.Score4 > Mad.Score4_low &
    df_pcp$Mad.Score4 < Mad.Score4_high &
    df_pcp$Segment.Duration.post > Duration_low, 1]) != 0) {
    df_pcp[df_pcp$Magnitude.mean.prior > Magsa_cut &
      df_pcp$Magnitude.mean.post < Magsa_cut &
      df_pcp$Class.current == 2 & # Don't overwrite Non-Wear
      df_pcp$Segment.Duration.current < Duration_low &
      df_pcp$Mad.Score4 > Mad.Score4_low &
      df_pcp$Mad.Score4 < Mad.Score4_high &
      df_pcp$Segment.Duration.post > Duration_low, ]$Binning_Method <- 2.1011

    df_pcp[df_pcp$Magnitude.mean.prior > Magsa_cut &
      df_pcp$Magnitude.mean.post < Magsa_cut &
      df_pcp$Class.current == 2 & # Don't overwrite Non-Wear
      df_pcp$Segment.Duration.current < Duration_low &
      df_pcp$Mad.Score4 > Mad.Score4_low &
      df_pcp$Mad.Score4 < Mad.Score4_high &
      df_pcp$Segment.Duration.post > Duration_low, ]$Class.current <- 1 # Sleep
  }

  ## Cut 2.111 ##
  # Inactive 1.1.2 + 1.1.1 from individual markdowns
  if (length(df_pcp[df_pcp$Magnitude.mean.prior > Magsa_cut &
    df_pcp$Magnitude.mean.post < Magsa_cut &
    df_pcp$Class.current == 2 & # Don't overwrite Non-Wear
    df_pcp$Segment.Duration.current > Duration_low &
    df_pcp$Segment.Duration.current < Duration_high &
    df_pcp$Mad.Score4 > Mad.Score4_high, 1]) != 0) {
    df_pcp[df_pcp$Magnitude.mean.prior > Magsa_cut &
      df_pcp$Magnitude.mean.post < Magsa_cut &
      df_pcp$Class.current == 2 & # Don't overwrite Non-Wear
      df_pcp$Segment.Duration.current > Duration_low &
      df_pcp$Segment.Duration.current < Duration_high &
      df_pcp$Mad.Score4 > Mad.Score4_high, ]$Binning_Method <- 2.111 # Inactive

    df_pcp[df_pcp$Magnitude.mean.prior > Magsa_cut &
      df_pcp$Magnitude.mean.post < Magsa_cut &
      df_pcp$Class.current == 2 & # Don't overwrite Non-Wear
      df_pcp$Segment.Duration.current > Duration_low &
      df_pcp$Segment.Duration.current < Duration_high &
      df_pcp$Mad.Score4 > Mad.Score4_high, ]$Class.current <- 3 # Inactive
  }

  ## Cut 2.110
  # Sleep 1.1.0
  if (length(df_pcp[df_pcp$Magnitude.mean.prior > Magsa_cut &
    df_pcp$Magnitude.mean.post < Magsa_cut &
    df_pcp$Class.current == 2 & # Don't overwrite Non-Wear
    df_pcp$Segment.Duration.current > Duration_low &
    df_pcp$Segment.Duration.current < Duration_high &
    df_pcp$Mad.Score4 < Mad.Score4_high, 1]) != 0) {
    df_pcp[df_pcp$Magnitude.mean.prior > Magsa_cut &
      df_pcp$Magnitude.mean.post < Magsa_cut &
      df_pcp$Class.current == 2 & # Don't overwrite Non-Wear
      df_pcp$Segment.Duration.current > Duration_low &
      df_pcp$Segment.Duration.current < Duration_high &
      df_pcp$Mad.Score4 < Mad.Score4_high, ]$Binning_Method <- 2.110 # Sleep

    df_pcp[df_pcp$Magnitude.mean.prior > Magsa_cut &
      df_pcp$Magnitude.mean.post < Magsa_cut &
      df_pcp$Class.current == 2 & # Don't overwrite Non-Wear
      df_pcp$Segment.Duration.current > Duration_low &
      df_pcp$Segment.Duration.current < Duration_high &
      df_pcp$Mad.Score4 < Mad.Score4_high, ]$Class.current <- 1 # Sleep
  }

  #### Sin_Sin_A ####
  ## From SIN_SIN_A ##
  # Sleep 1.2
  if (length(df_pcp[df_pcp$Magnitude.mean.prior < Magsa_cut &
    df_pcp$Magnitude.mean.post > Magsa_cut &
    df_pcp$Class.current == 2 & # Don't overwrite
    df_pcp$Segment.Duration.current > Duration_high, 1]) != 0) {
    df_pcp[df_pcp$Magnitude.mean.prior < Magsa_cut &
      df_pcp$Magnitude.mean.post > Magsa_cut &
      df_pcp$Class.current == 2 & # Don't overwrite Non-Wear
      df_pcp$Segment.Duration.current > Duration_high, ]$Binning_Method <- 3.12 # Sleep

    df_pcp[df_pcp$Magnitude.mean.prior < Magsa_cut &
      df_pcp$Magnitude.mean.post > Magsa_cut &
      df_pcp$Class.current == 2 & # Don't overwrite Non-Wear
      df_pcp$Segment.Duration.current > Duration_high, ]$Class.current <- 1 # Sleep
  }

  # Inactive 1.0.2
  if (length(df_pcp[df_pcp$Magnitude.mean.prior < Magsa_cut &
    df_pcp$Magnitude.mean.post > Magsa_cut &
    df_pcp$Class.current == 2 & # Don't overwrite
    df_pcp$Segment.Duration.current < Duration_low &
    df_pcp$Mad.Score6 > Mad.Score6_high, 1]) != 0) {
    df_pcp[df_pcp$Magnitude.mean.prior < Magsa_cut &
      df_pcp$Magnitude.mean.post > Magsa_cut &
      df_pcp$Class.current == 2 & # Don't overwrite Non-Wear
      df_pcp$Segment.Duration.current < Duration_low &
      df_pcp$Mad.Score6 > Mad.Score6_high, ]$Binning_Method <- 3.102 # Inactive

    df_pcp[df_pcp$Magnitude.mean.prior < Magsa_cut &
      df_pcp$Magnitude.mean.post > Magsa_cut &
      df_pcp$Class.current == 2 & # Don't overwrite Non-Wear
      df_pcp$Segment.Duration.current < Duration_low &
      df_pcp$Mad.Score6 > Mad.Score6_high, ]$Class.current <- 3 # Inactive
  }

  # Sleep 1.0.0
  if (length(df_pcp[df_pcp$Magnitude.mean.prior < Magsa_cut &
    df_pcp$Magnitude.mean.post > Magsa_cut &
    df_pcp$Class.current == 2 & # Don't overwrite
    df_pcp$Segment.Duration.current < Duration_low &
    df_pcp$Mad.Score6 < Mad.Score6_low, 1]) != 0) {
    df_pcp[df_pcp$Magnitude.mean.prior < Magsa_cut &
      df_pcp$Magnitude.mean.post > Magsa_cut &
      df_pcp$Class.current == 2 & # Don't overwrite Non-Wear
      df_pcp$Segment.Duration.current < Duration_low &
      df_pcp$Mad.Score6 < Mad.Score6_low, ]$Binning_Method <- 3.100 # Sleep

    df_pcp[df_pcp$Magnitude.mean.prior < Magsa_cut &
      df_pcp$Magnitude.mean.post > Magsa_cut &
      df_pcp$Class.current == 2 & # Don't overwrite Non-Wear
      df_pcp$Segment.Duration.current < Duration_low &
      df_pcp$Mad.Score6 < Mad.Score6_low, ]$Class.current <- 1 # Sleep
  }

  # Inactive 1.0.1.0 + 1.0.1.1
  if (length(df_pcp[df_pcp$Magnitude.mean.prior < Magsa_cut &
    df_pcp$Magnitude.mean.post > Magsa_cut &
    df_pcp$Class.current == 2 & # Don't overwrite
    df_pcp$Segment.Duration.current < Duration_low &
    df_pcp$Mad.Score6 > Mad.Score6_low &
    df_pcp$Mad.Score6 < Mad.Score6_high &
    df_pcp$Segment.Duration.post < Duration_low, 1]) != 0) {
    df_pcp[df_pcp$Magnitude.mean.prior < Magsa_cut &
      df_pcp$Magnitude.mean.post > Magsa_cut &
      df_pcp$Class.current == 2 & # Don't overwrite Non-Wear
      df_pcp$Segment.Duration.current < Duration_low &
      df_pcp$Mad.Score6 > Mad.Score6_low &
      df_pcp$Mad.Score6 < Mad.Score6_high &
      df_pcp$Segment.Duration.post < Duration_low, ]$Binning_Method <- 3.1010 # Inactive

    df_pcp[df_pcp$Magnitude.mean.prior < Magsa_cut &
      df_pcp$Magnitude.mean.post > Magsa_cut &
      df_pcp$Class.current == 2 & # Don't overwrite Non-Wear
      df_pcp$Segment.Duration.current < Duration_low &
      df_pcp$Mad.Score6 > Mad.Score6_low &
      df_pcp$Mad.Score6 < Mad.Score6_high &
      df_pcp$Segment.Duration.post < Duration_low, ]$Class.current <- 3 # Inactive
  }

  # Sleep 1.0.1.2
  if (length(df_pcp[df_pcp$Magnitude.mean.prior < Magsa_cut &
    df_pcp$Magnitude.mean.post > Magsa_cut &
    df_pcp$Class.current == 2 & # Don't overwrite
    df_pcp$Segment.Duration.current < Duration_low &
    df_pcp$Mad.Score6 > Mad.Score6_low &
    df_pcp$Mad.Score6 < Mad.Score6_high &
    df_pcp$Segment.Duration.post > Duration_low, 1]) != 0) {
    df_pcp[df_pcp$Magnitude.mean.prior < Magsa_cut &
      df_pcp$Magnitude.mean.post > Magsa_cut &
      df_pcp$Class.current == 2 & # Don't overwrite Non-Wear
      df_pcp$Segment.Duration.current < Duration_low &
      df_pcp$Mad.Score6 > Mad.Score6_low &
      df_pcp$Mad.Score6 < Mad.Score6_high &
      df_pcp$Segment.Duration.post > Duration_low, ]$Binning_Method <- 3.1012 # Sleep

    df_pcp[df_pcp$Magnitude.mean.prior < Magsa_cut &
      df_pcp$Magnitude.mean.post > Magsa_cut &
      df_pcp$Class.current == 2 & # Don't overwrite Non-Wear
      df_pcp$Segment.Duration.current < Duration_low &
      df_pcp$Mad.Score6 > Mad.Score6_low &
      df_pcp$Mad.Score6 < Mad.Score6_high &
      df_pcp$Segment.Duration.post > Duration_low, ]$Class.current <- 1 # Sleep
  }

  # Mad Score 5
  # Sleep 1.1.0
  if (length(df_pcp[df_pcp$Magnitude.mean.prior < Magsa_cut &
    df_pcp$Magnitude.mean.post > Magsa_cut &
    df_pcp$Class.current == 2 & # Don't overwrite
    df_pcp$Segment.Duration.current > Duration_low &
    df_pcp$Segment.Duration.current < Duration_high &
    df_pcp$Mad.Score6 < Mad.Score6_high, 1]) != 0) {
    df_pcp[df_pcp$Magnitude.mean.prior < Magsa_cut &
      df_pcp$Magnitude.mean.post > Magsa_cut &
      df_pcp$Class.current == 2 & # Don't overwrite Non-Wear
      df_pcp$Segment.Duration.current > Duration_low &
      df_pcp$Segment.Duration.current < Duration_high &
      df_pcp$Mad.Score6 < Mad.Score6_high, ]$Binning_Method <- 3.110 # Sleep

    df_pcp[df_pcp$Magnitude.mean.prior < Magsa_cut &
      df_pcp$Magnitude.mean.post > Magsa_cut &
      df_pcp$Class.current == 2 & # Don't overwrite Non-Wear
      df_pcp$Segment.Duration.current > Duration_low &
      df_pcp$Segment.Duration.current < Duration_high &
      df_pcp$Mad.Score6 < Mad.Score6_high, ]$Class.current <- 1 # Sleep
  }

  ## 3.112
  if (length(df_pcp[df_pcp$Magnitude.mean.prior < Magsa_cut &
    df_pcp$Magnitude.mean.post > Magsa_cut &
    df_pcp$Class.current == 2 & # Don't overwrite
    df_pcp$Segment.Duration.current > Duration_low &
    df_pcp$Segment.Duration.current < Duration_high &
    df_pcp$Mad.Score6 > Mad.Score6_high, 1]) != 0) {
    df_pcp[df_pcp$Magnitude.mean.prior < Magsa_cut &
      df_pcp$Magnitude.mean.post > Magsa_cut &
      df_pcp$Class.current == 2 & # Don't overwrite Non-Wear
      df_pcp$Segment.Duration.current > Duration_low &
      df_pcp$Segment.Duration.current < Duration_high &
      df_pcp$Mad.Score6 > Mad.Score6_high, ]$Binning_Method <- 3.112 # Inactive

    df_pcp[df_pcp$Magnitude.mean.prior < Magsa_cut &
      df_pcp$Magnitude.mean.post > Magsa_cut &
      df_pcp$Class.current == 2 & # Don't overwrite Non-Wear
      df_pcp$Segment.Duration.current > Duration_low &
      df_pcp$Segment.Duration.current < Duration_high &
      df_pcp$Mad.Score6 > Mad.Score6_high, ]$Class.current <- 3 # Inactive
  }

  ####  A-Sin-A ####
  ## FROM A_SIN_A ##
  # Sleep (Anything Below Mad Score of 1) 1.1
  if (length(df_pcp[df_pcp$Magnitude.mean.prior > Magsa_cut &
    df_pcp$Magnitude.mean.post > Magsa_cut &
    df_pcp$Class.current == 2 & # Don't overwrite
    df_pcp$Segment.Duration.current > Duration_low &
    df_pcp$UpDown.mad.current < Mad_low, 1]) != 0) {
    df_pcp[df_pcp$Magnitude.mean.prior > Magsa_cut &
      df_pcp$Magnitude.mean.post > Magsa_cut &
      df_pcp$Class.current == 2 & # Don't overwrite Non-Wear
      df_pcp$Segment.Duration.current > Duration_low &
      df_pcp$UpDown.mad.current < Mad_low, ]$Binning_Method <- 4.11 # Sleep

    df_pcp[df_pcp$Magnitude.mean.prior > Magsa_cut &
      df_pcp$Magnitude.mean.post > Magsa_cut &
      df_pcp$Class.current == 2 & # Don't overwrite Non-Wear
      df_pcp$Segment.Duration.current > Duration_low &
      df_pcp$UpDown.mad.current < Mad_low, ]$Class.current <- 1 # Sleep
  }

  ## 4.102
  # Inactive 1.0.2
  if (length(df_pcp[df_pcp$Magnitude.mean.prior > Magsa_cut &
    df_pcp$Magnitude.mean.post > Magsa_cut &
    df_pcp$Class.current == 2 & # Don't overwrite
    df_pcp$Segment.Duration.current < Duration_low &
    df_pcp$UpDown.mad.current > Mad_high, 1]) != 0) {
    df_pcp[df_pcp$Magnitude.mean.prior > Magsa_cut &
      df_pcp$Magnitude.mean.post > Magsa_cut &
      df_pcp$Class.current == 2 & # Don't overwrite Non-Wear
      df_pcp$Segment.Duration.current < Duration_low &
      df_pcp$UpDown.mad.current > Mad_high, ]$Binning_Method <- 4.102 # Inactive

    df_pcp[df_pcp$Magnitude.mean.prior > Magsa_cut &
      df_pcp$Magnitude.mean.post > Magsa_cut &
      df_pcp$Class.current == 2 & # Don't overwrite Non-Wear
      df_pcp$Segment.Duration.current < Duration_low &
      df_pcp$UpDown.mad.current > Mad_high, ]$Class.current <- 3 # Inactive
  }

  ## 4.100
  # Sleep 1.0.0
  if (length(df_pcp[df_pcp$Magnitude.mean.prior > Magsa_cut &
    df_pcp$Magnitude.mean.post > Magsa_cut &
    df_pcp$Class.current == 2 & # Don't overwrite
    df_pcp$Segment.Duration.current < Duration_low &
    df_pcp$UpDown.mad.current < Mad_low, 1]) != 0) {
    df_pcp[df_pcp$Magnitude.mean.prior > Magsa_cut &
      df_pcp$Magnitude.mean.post > Magsa_cut &
      df_pcp$Class.current == 2 & # Don't overwrite Non-Wear
      df_pcp$Segment.Duration.current < Duration_low &
      df_pcp$UpDown.mad.current < Mad_low, ]$Binning_Method <- 4.100 # Sleep

    df_pcp[df_pcp$Magnitude.mean.prior > Magsa_cut &
      df_pcp$Magnitude.mean.post > Magsa_cut &
      df_pcp$Class.current == 2 & # Don't overwrite Non-Wear
      df_pcp$Segment.Duration.current < Duration_low &
      df_pcp$UpDown.mad.current < Mad_low, ]$Class.current <- 1 # Sleep
  }

  ## 4.1011
  # Inactive 1.0.1.1 + 1.0.1.2
  if (length(df_pcp[df_pcp$Magnitude.mean.prior > Magsa_cut &
    df_pcp$Magnitude.mean.post > Magsa_cut &
    df_pcp$Class.current == 2 & # Don't overwrite
    df_pcp$Segment.Duration.current < Duration_low &
    df_pcp$UpDown.mad.current > Mad_low &
    df_pcp$UpDown.mad.current < Mad_high &
    df_pcp$Mad.Score2 > Mad.Score2_low, 1]) != 0) {
    df_pcp[df_pcp$Magnitude.mean.prior > Magsa_cut &
      df_pcp$Magnitude.mean.post > Magsa_cut &
      df_pcp$Class.current == 2 & # Don't overwrite Non-Wear
      df_pcp$Segment.Duration.current < Duration_low &
      df_pcp$UpDown.mad.current > Mad_low &
      df_pcp$UpDown.mad.current < Mad_high &
      df_pcp$Mad.Score2 > Mad.Score2_low, ]$Binning_Method <- 4.1011 # Inactive

    df_pcp[df_pcp$Magnitude.mean.prior > Magsa_cut &
      df_pcp$Magnitude.mean.post > Magsa_cut &
      df_pcp$Class.current == 2 & # Don't overwrite Non-Wear
      df_pcp$Segment.Duration.current < Duration_low &
      df_pcp$UpDown.mad.current > Mad_low &
      df_pcp$UpDown.mad.current < Mad_high &
      df_pcp$Mad.Score2 > Mad.Score2_low, ]$Class.current <- 3 # Inactive
  }

  # 4.1010
  if (length(df_pcp[df_pcp$Magnitude.mean.prior > Magsa_cut &
    df_pcp$Magnitude.mean.post > Magsa_cut &
    df_pcp$Class.current == 2 & # Don't overwrite
    df_pcp$Segment.Duration.current < Duration_low &
    df_pcp$UpDown.mad.current > Mad_low &
    df_pcp$UpDown.mad.current < Mad_high &
    df_pcp$Mad.Score2 < Mad.Score2_low, 1]) != 0) {
    df_pcp[df_pcp$Magnitude.mean.prior > Magsa_cut &
      df_pcp$Magnitude.mean.post > Magsa_cut &
      df_pcp$Class.current == 2 & # Don't overwrite Non-Wear
      df_pcp$Segment.Duration.current < Duration_low &
      df_pcp$UpDown.mad.current > Mad_low &
      df_pcp$UpDown.mad.current < Mad_high &
      df_pcp$Mad.Score2 < Mad.Score2_low, ]$Binning_Method <- 4.1010 # Sleep

    df_pcp[df_pcp$Magnitude.mean.prior > Magsa_cut &
      df_pcp$Magnitude.mean.post > Magsa_cut &
      df_pcp$Class.current == 2 & # Don't overwrite Non-Wear
      df_pcp$Segment.Duration.current < Duration_low &
      df_pcp$UpDown.mad.current > Mad_low &
      df_pcp$UpDown.mad.current < Mad_high &
      df_pcp$Mad.Score2 < Mad.Score2_low, ]$Class.current <- 1 # Sleep
  }

  #### Write out pcp ####

  # Creating a Bed Rise score here now? Perhaps refactor this at a later date
  for (j in 1:length(df_pcp[, 1])) {
    df_pcp$Sleep.Score1[j] <-
      sum(df_pcp[df_pcp$Class.current == 2 &
        df_pcp$Start.Time < (df_pcp$Start.Time[j] + 1800) &
        df_pcp$Start.Time > (df_pcp$Start.Time[j] - 1800), ]$Segment.Duration.current) +
      sum(df_pcp[df_pcp$Class.current == 1 &
        df_pcp$Start.Time < (df_pcp$Start.Time[j] + 1800) &
        df_pcp$Start.Time > (df_pcp$Start.Time[j] - 1800), ]$Segment.Duration.current) # 6683)

    df_pcp$Sleep.Score2[j] <-
      sum(df_pcp[df_pcp$Class.current == 2 &
        df_pcp$Start.Time < (df_pcp$Start.Time[j] + 3600) &
        df_pcp$Start.Time > (df_pcp$Start.Time[j] - 3600), ]$Segment.Duration.current) +
      sum(df_pcp[df_pcp$Class.current == 1 &
        df_pcp$Start.Time < (df_pcp$Start.Time[j] + 3600) &
        df_pcp$Start.Time > (df_pcp$Start.Time[j] - 3600), ]$Segment.Duration.current) # 6683)

    df_pcp$Sleep.Score3[j] <-
      sum(df_pcp[df_pcp$Class.current == 2 &
        df_pcp$Start.Time < (df_pcp$Start.Time[j] + 9000) &
        df_pcp$Start.Time > (df_pcp$Start.Time[j] - 9000), ]$Segment.Duration.current) +
      sum(df_pcp[df_pcp$Class.current == 1 &
        df_pcp$Start.Time < (df_pcp$Start.Time[j] + 9000) &
        df_pcp$Start.Time > (df_pcp$Start.Time[j] - 9000), ]$Segment.Duration.current) # 6683)
  }

  #### Create a plot like the step line to show what I've found so far... ####
  if (verbose) {
    plot(as.POSIXlt(df_pcp$Start.Time, origin = "1970-01-01"), df_pcp$Sleep.Score1, type = "l")
  }

  return(df_pcp)
}
