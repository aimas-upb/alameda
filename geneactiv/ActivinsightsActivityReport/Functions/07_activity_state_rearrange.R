
#' @name activity_state_rearrange
#' @title activity state rearrange
#' @description Rearrange the state value in df_pcp
#'
#' @param segment_data The segmented GENEActiv.bin data.
#' @param boundarys A vector of UNIX timestamps
#' @param bed_time A vector from BedRiseDetect that gives the Bed Times of a subject as a UNIX timstamp
#' @param rise_time A vector from BedRiseDetect that gives the Rise Times of a subject as a UNIX timstamp
#' @param first_date The first_date found in the AccData object.
#'
#' @details Takes the prior, current and post data frame and rearranges the states to create a plot.
# Testing parameters  
# segment_data = segment_data1
# boundarys
# bed_time = bed_rise_df$bed_time
# rise_time = bed_rise_df$rise_time
# first_date


activity_state_rearrange <- function(segment_data,
                                     boundarys,
                                     bed_time,
                                     rise_time,
                                     first_date) {
  
  collen <- length(segment_data$Start.Time)
  tmp1 <- c() # empty dataframe?
  # For loop running through the Boundaries - each day
  ix <- c(segment_data$Start.Time[1:(collen)])
  iy <- c(segment_data$Class.current[1:(collen)])
  dd <- data.frame(ix, iy)

  for (i in 0:(length(boundarys[, 1]) + 1)) {
    # for (i in 0:1){
    if (i == 0) {
      s <- subset(dd,
        ix < as.numeric(boundarys[(i + 1), 1]),
        select = c(ix, iy)
      ) # Subset to get the data needed

      # Changing any SIN events between the min boundarys and bed time to inactive
      if (length(s[s$iy == 2, 1]) != 0) {
        s[s$iy == 2, ]$iy <- 3
      }

      # Changing any Sleep events between the min boundarys and bed time to Day Time nap
      if (length(s[s$iy == 1, 1]) != 0) {
        s[s$iy == 1, ]$iy <- 1
      }

      ## Changing active states down one!

      if (length(s[s$iy == 3, 1]) != 0) {
        s[s$iy == 3, ]$iy <- 2 # Moving the Sedentary state down 1.
      }

      if (length(s[s$iy == 4, 1]) != 0) {
        s[s$iy == 4, ]$iy <- 3 # Moving the Light state down 1.
      }

      if (length(s[s$iy == 5, 1]) != 0) {
        s[s$iy == 5, ]$iy <- 4 # Moving the Moderate state down 1.
      }

      if (length(s[s$iy == 6, 1]) != 0) {
        s[s$iy == 6, ]$iy <- 5 # Moving the Vigorous state down 1.
      }


      tmp1 <- rbind(tmp1, s)
    } else if (i == (length(boundarys[, 1]) + 1)) {
      s <- subset(dd,
        ix > as.numeric(boundarys[(i - 1), 2]),
        select = c(ix, iy)
      ) # Subset to get the data needed

      # Changing any SIN events between the min boundarys and bed time to inactive
      if (length(s[s$iy == 2, 1]) != 0) {
        s[s$iy == 2, ]$iy <- 3
      }

      # Changing any Sleep events between the min boundarys and bed time to Day Time nap
      if (length(s[s$iy == 1, 1]) != 0) {
        s[s$iy == 1, ]$iy <- 1
      }

      ## Changing active states down one!

      if (length(s[s$iy == 3, 1]) != 0) {
        s[s$iy == 3, ]$iy <- 2 # Moving the Sedentary state down 1.
      }

      if (length(s[s$iy == 4, 1]) != 0) {
        s[s$iy == 4, ]$iy <- 3 # Moving the Light state down 1.
      }

      if (length(s[s$iy == 5, 1]) != 0) {
        s[s$iy == 5, ]$iy <- 4 # Moving the Moderate state down 1.
      }

      if (length(s[s$iy == 6, 1]) != 0) {
        s[s$iy == 6, ]$iy <- 5 # Moving the Vigorous state down 1.
      }



      tmp1 <- rbind(tmp1, s)
    } else {
      s <- subset(dd,
        ix >= as.numeric(boundarys[i, 1]) &
          ix <= as.numeric(boundarys[i, 2]),
        select = c(ix, iy)
      ) # Subset to get the data needed

      # If a bed time has not been found then add 8 hours (3pm to 11pm) to bed_time
      if (is.na(bed_time[i])) {
        bed_time <- as.numeric(as.character(unlist(boundarys[i, 1])[1])) + 3600 * 8
      }

      # Rise time then is 3pm minus 8 hours to get 7am.
      if (is.na(rise_time[i])) {
        rise_time <- as.numeric(as.character(unlist(boundarys[i, 2])[1])) - 3600 * 8
      }

      # Taking the data from min boundarys to the known bed time
      ss <- s[s$ix < as.numeric(as.character(bed_time[i])) &
        s$ix >= as.numeric(as.character(boundarys[i, 1])), ]

      # Changing any SIN events between the min boundarys and bed time to inactive
      if (length(ss[ss$iy == 2, 1]) != 0) {
        ss[ss$iy == 2, ]$iy <- 3
      }

      # Changing any Sleep events between the min boundarys and bed time to Day Time nap
      if (length(ss[ss$iy == 1, 1]) != 0) {
        ss[ss$iy == 1, ]$iy <- 1
      }

      # Taking the data between the Bed and Rise Time.
      sss <- s[s$ix >= as.numeric(as.character(bed_time[i])) &
        s$ix <= as.numeric(as.character(rise_time[i])), ]

      if (length(sss$iy) != 0) {
        sss$iy[1] <- 1
      }
      if (length(sss$iy) != 0) {
        sss$iy[length(sss$iy)] <- 1
      }

      # Referring all SIN events to sleep if undecided.
      if (length(sss[sss$iy == 2, 1]) != 0) {
        sss[sss$iy == 2, ]$iy <- 1
      }

      ssss <- s[s$ix > as.numeric(as.character(rise_time[i])) &
        s$ix <= as.numeric(as.character(boundarys[i, 2])), ]

      # Changing any SIN events between the Rise time and the max boundarys to inactive
      if (length(ssss[ssss$iy == 2, 1]) != 0) {
        ssss[ssss$iy == 2, ]$iy <- 3
      }

      # Changing any Sleep events between the Rise time and the max boundarys to Day Time nap
      if (length(ssss[ssss$iy == 1, 1]) != 0) {
        ssss[ssss$iy == 1, ]$iy <- 1
      }

      #### Checking that the Bed Time does not straddle two different states ####
      #' This needs to be a back propagation
      #' Checking Bed Time first

      # Change made here.
      # Needs to be a line confirming that these  - Using 2 as there needs to be a class that is split!
      if (length(ss[, 2]) >= 2 & length(sss[, 2]) >= 2) {
        if (ss[length(ss[, 2]), 2] != sss[1, 2]) {
          sss[1, 2] <- ss[length(ss[, 2]), 2]
        }
      }

      ## Now looking at the Rise time
      if (length(sss[, 2]) >= 2 & length(ssss[, 2]) >= 2) {
        if (sss[length(sss[, 2]), 2] != ssss[1, 2]) {
          ssss[1, 2] <- sss[length(sss[, 2]), 2]
        }
      }

      s <- rbind(ss, sss, ssss)

      # Day time sleep -> 1.5 as naps?

      ## Changing the active and inactive down 1 on the y axis

      ## Changing active states down one!

      if (length(s[s$iy == 3, 1]) != 0) {
        s[s$iy == 3, ]$iy <- 2 # Moving the Sedentary state down 1.
      }

      if (length(s[s$iy == 4, 1]) != 0) {
        s[s$iy == 4, ]$iy <- 3 # Moving the Light state down 1.
      }

      if (length(s[s$iy == 5, 1]) != 0) {
        s[s$iy == 5, ]$iy <- 4 # Moving the Moderate state down 1.
      }

      if (length(s[s$iy == 6, 1]) != 0) {
        s[s$iy == 6, ]$iy <- 5 # Moving the Vigorous state down 1.
      }


      tmp1 <- rbind(tmp1, s)
    }
  }

  # Creating the state names from
  #'  0. Non-Wear
  #'  1. Sleep
  #'  2. SIN
  #'  3. Inactive
  #'  4. Active
  #'
  #'  to:
  #'
  #'  0. Non-Wear
  #'  1. Sleep
  #'  1.5. Day Sleep
  #'  2. Inactive
  #'  3. Active
  #'
  #'
  #'      #'  0. Non-Wear
  #'  1. Sleep
  #'  1.5. Day Sleep? - Leaving this out here will be included on the sleep markdown
  #'  2. Sedentary
  #'  3. Light
  #'  4. Moderate
  #'  5. Vigorous

  #### Exception here to remove any duplicated timestamps ####
  # Removing any duplications 
  if (length(tmp1[, 1]) != length(segment_data[, 1])) {
    tmp1 <- tmp1[-which(duplicated(tmp1$ix)), ] 
  }
  
  #### Drop the 3 columns names from df_pcp ####

  tmp2 <- subset(segment_data,
    select = -c(
      Class.post,
      Class.current,
      Class.prior
    )
  )
  
  tmp2$State <- "Unassigned"
  tmp2$Class.current <- tmp1$iy

  # Changing the name of states now.

  if (length(tmp2[tmp2$Class.current == 0, ]$State) > 0) {
    tmp2[tmp2$Class.current == 0, ]$State <- "Non-Wear"
  }

  if (length(tmp2[tmp2$Class.current == 1, ]$State) > 0) {
    tmp2[tmp2$Class.current == 1, ]$State <- "Sleep"
  }

  # if (length(tmp2[tmp2$Class.current == 1.5, ]$State) > 0 ){
  #   tmp2[tmp2$Class.current == 1.5, ]$State = "Day-Sleep"
  # }

  if (length(tmp2[tmp2$Class.current == 2, ]$State) > 0) {
    tmp2[tmp2$Class.current == 2, ]$State <- "Sedentary"
  }

  if (length(tmp2[tmp2$Class.current == 3, ]$State) > 0) {
    tmp2[tmp2$Class.current == 3, ]$State <- "Light"
  }

  if (length(tmp2[tmp2$Class.current == 4, ]$State) > 0) {
    tmp2[tmp2$Class.current == 4, ]$State <- "Moderate"
  }

  if (length(tmp2[tmp2$Class.current == 5, ]$State) > 0) {
    tmp2[tmp2$Class.current == 5, ]$State <- "Vigorous"
  }

  return(tmp2)
}
