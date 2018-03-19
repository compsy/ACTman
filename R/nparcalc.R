#############################################################################
### nparcalc{ACTman}                                                      ###
### Script authors: Yoram Kunkels, Stefan Knapen, & Ando Emerencia        ###
### Most recent Update: 27-02-2018                                        ###
###~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~###

#' nparcalc
#'
#' Calculate non-parametrical circadian rhythm variables:
#' IS, IV, RA, L5, L5_starttime, M10, and M10_starttime.
#'
#' @param myACTdevice Name of the input device used. Should be either Actiwatch2 or MW8.
#' @param movingwindow A boolean indicating whether moving window is used.
#' @param CRV.data CRV data
#' @param ACTdata.1.sub Managed data set
#' @param out Optional. When movingwindow is TRUE, this is the current window of data.
#'
#' @return A list with the result values IS, IV, RA, L5, L5_starttime, M10, and M10_starttime.
#'

## Specify nparcalc function and arguments:
nparcalc <- function(myACTdevice, movingwindow, CRV.data, ACTdata.1.sub, out = NULL) {


  ## Step 1: Basic Operations-------------------------------------------------------------------

  ## Define constants
  secshour <- 60 * 60 # Seconds per hour
  secsday <- 24 * secshour # Seconds per day

  ## Initialise results list:
  result <- list()

  ## Read Data, and combine seperate date and time variables when needed:
  ## If there are more than 2 columns, combine date and time variables, and remove extra column.
  if (ncol(CRV.data) > 2) {
    CRV.data$Date <- paste(CRV.data$Date, " ", CRV.data$Time)
    CRV.data <- CRV.data[, -2]
  ## Else, just assign correct column names.
  } else {
    colnames(CRV.data) <- c("Date", "Activity")
  }

  ## Prune data untill only full 24h days are obtained
  CRV.data.wholehours <- CRV.data[grep("00:00", CRV.data[, "Date"]), ]
  CRV.data.start <- which(CRV.data$Date == CRV.data.wholehours[1, "Date"])

  ## Device- and functionality specific identification of dataset end:
  #! Note: There is repetition in this loop > can be written more efficiently!
  ## If device is MW8, then take dataset end at last "00:00:00" of dataset.
  if (myACTdevice == "MW8") {
    CRV.data.end <- tail(grep("00:00:00", ACTdata.1.sub$Date), 2)[1]
    ## Else, if moving window is required take dataset end at last "00:00:00" of "out" dataset.
  } else {
    if (movingwindow) {
      CRV.data.end <- which(out == "00:00:00")[length(which(out == "00:00:00"))]
      ## Else, assume device to be Actiwatch2 and take dataset end at last "00:00:00" of dataset.
    } else {CRV.data.end <- tail(grep("00:00:00", ACTdata.1.sub$Date), 2)[1]}

  }

  ## Assign dataset for circadian rhythm analysis to be dataset "CRV.data" with a period of the
  ## aforementioned start and end date/times:
  CRV.data <- CRV.data[CRV.data.start:CRV.data.end, ]


  ## Step 2: Calculate Circadian Rhythm Variables-------------------------------------------------

  ## IS: Interdaily stability
  ## Source: Van Someren, E., Swaab, D., Colenda, C., Cohen, W., McCall, W. and Rosenquist, P. (1999).
  ## Bright Light Therapy: Improved Sensitivity to Its Effects on Rest-Activity Rhythms in Alzheimer Patients
  ## by Application of Nonparametric Methods. Chronobiology International, 16(4), pp.505-518.

  ## Calculate Xi (which represent the individual datapoints / consecutive hourly means)
  ## by aggregation with hourly breaks:
  xi <- aggregate(CRV.data[, "Activity"],
                  list(hour = cut(as.POSIXct(CRV.data[, "Date"]), breaks = "hour")),
                  mean, na.action = na.pass, na.rm = TRUE)

  ## Exception for moving window to retain last observation for correct number of observtions:
  if (!movingwindow) {
    xi <- xi[1:(nrow(xi) - 1), ]
  } else {
    xi <- xi[1:(nrow(xi)), ]
  }

  ## Assign only activity data to xi:
  xi <- xi$x

  ## Calculate X (or X_bar; the mean of all the data):
  X <- mean(xi, na.rm = T)

  ## Calculate the Denominator part of the IS formula (van Someren et al., 1999):
  xi_X <- xi - X # Differences between consecutive hourly means and overall mean.
  sq.xi_X <- xi_X ^ 2 # Square of differences.
  sum.sq.xi_X <- sum(sq.xi_X, na.rm = T) # Sum of squares.
  n <- sum(!is.na(xi)) # Get number of hours (should be 168 for 7 day intervals (7*24)).
  sum.sq.xi_X.perhour <- sum.sq.xi_X / n # Sum of squares per hour


  ## Calculate the Nominator part of the IS formula (van Someren et al., 1999):
  ## Workaround to assure xi length is multiple of the number of rows [24]
  xi_sub <- xi[1:(24 * floor(length(xi) / 24))]
  Xh <- rowMeans(matrix(xi_sub, nrow = 24), na.rm = T) # Calculate hourly means.
  Xh_X <- Xh - X # difference 24 hour means and overall mean.
  sum.sq.Xh_X <- sum(Xh_X ^ 2, na.rm = T) # sum of squares.
  sum.sq.Xh_X.perhour <- sum.sq.Xh_X / 24 # sum of squares per hour.

  IS <- sum.sq.Xh_X.perhour / sum.sq.xi_X.perhour # hourly variance divided by overall variance.
  IS <- round(x = IS, digits = 2) # rounding down to 2 decimals.
  result$IS <- IS # Write IS to results

  ##---------------------------------------------------------------------------------------------------------

  ## IV: Interdaily Variability
  ## Source: Van Someren, E., Swaab, D., Colenda, C., Cohen, W., McCall, W. and Rosenquist, P. (1999).
  ## Bright Light Therapy: Improved Sensitivity to Its Effects on Rest-Activity Rhythms in Alzheimer Patients
  ## by Application of Nonparametric Methods. Chronobiology International, 16(4), pp.505-518.

  Xi_diffXi <- diff(xi) # difference Xi and previous Xi (the difference between all successive hours) (!!! Warning because no previous hour for 1st obs.)
  sum.sq.Xi_diffXi <- sum(Xi_diffXi ^ 2, na.rm = T) # sum of squares
  sum.sq.Xi_diffXi.perhour <- sum.sq.Xi_diffXi / n # sum of squares per hour

  Xi_X <- xi - X # difference Xi and overall mean
  sum.sq.Xi_X <- sum(Xi_X ^ 2, na.rm = T) # sum of squares
  sum.sq.Xi_X.perhour <- sum.sq.Xi_X / (n - 1) # sum of squares per hour minus 1

  IV <- sum.sq.Xi_diffXi.perhour / sum.sq.Xi_X.perhour
  IV <- round(x = IV, digits = 2) # rounding down to 2 decimals.
  result$IV <- IV # Write IS to results

  ##---------------------------------------------------------------------------------------------------------

  ## L5: Average of the 5 Lowest Hourly Means (within each day(!))
  ## Source: Witting, W., Kwa, I., Eikelenboom, P., Mirmiran, M., & Swaab, D. (1990).
  ## Alterations in the circadian rest-activity rhythm in aging and Alzheimer's disease.
  ## Biological Psychiatry, 27(6), 563-572. http://dx.doi.org/10.1016/0006-3223(90)90523-5

  ## Obtain locations of "midnight" (12:00:00 for L5) in dataset
  CRV.data.locmidnight <- grep(pattern = "12:00:00", x = CRV.data[, "Date"]) # Locations of midnight (!!!Is "12:00:00" midnight or "00:00:00"???)
  # CRV.data.locmidnight <- grep(pattern = "00:00:00", x = CRV.data[, "Date"]) # Locations of midnight (!!!Is "12:00:00" midnight or "00:00:00"???)
  # CRV.data.locmidnight <- grep(pattern = strftime(ACTdata.1.sub$Date[1], format = "%H:%M:%S"), x = CRV.data[, "Date"])

  ## Initialise empty data.frame for L5 and L5_starttime results
  TEST.df.L5 <- data.frame("L5" <- NA, "L5_starttime" <- NA)

  ## Initialise empty data.frame for L5_starttimes
  TEST_L5_starttimes <- as.data.frame(matrix(NA, ncol = 1, nrow = length(CRV.data.locmidnight)))


  ## Loop for calculating L5 and L5_starttime
  for (f in 1:(length(CRV.data.locmidnight) - 1)) {

    ## Select data period as midnight to midnight of current iteration:
    CRV.data.full24minavg <- CRV.data[(CRV.data.locmidnight[f]:CRV.data.locmidnight[f + 1]), ]
    colnames(CRV.data.full24minavg) <- c("hour", "x")

    ## Assure period is exactly 1 day (1440 minutes):
    CRV.data.full24minavg <- CRV.data.full24minavg[1:1440, ]

    ## Initialise empty vector for L5 location:
    CRV.data.loc.L5 <- NA

    ## Extract only activity counts from CRV.data.full24minavg data
    CRV.data.full24minavg.act <- CRV.data.full24minavg$x

    ## Loop for calculating mean activity in 300 minute intervals ~ 5 hours
    for (g in 1:length(CRV.data.full24minavg.act)) {
      CRV.data.loc.L5[g] <- mean(c(CRV.data.full24minavg.act, CRV.data.full24minavg.act[c(1:300)])[c(g:(299 + g))], na.rm = T)
    }

    ## Workaround NaN's in CRV.data.loc.L5 (set to zero)
    CRV.data.loc.L5[is.na(CRV.data.loc.L5)] <- 0

    ## Take minimum value for L5 and assign to dataset
    L5 <- min(CRV.data.loc.L5)
    TEST.df.L5[f, "L5"] <- L5

    ## Initialise data.frame with CRV.data.loc.L5 to facilitate L5onset identification
    L5M10frame <- data.frame(CRV.data.loc.L5 = CRV.data.loc.L5)

    ## Locate the first value that equals L5 and get the number of hours from start
    L5onset <- which(L5M10frame$CRV.data.loc.L5 == L5)[1] / 60

    ## Calculate L5_starttime as L5onset time from "midnight", and assign to data.frame
    L5_starttime <- as.POSIXct(CRV.data[CRV.data.locmidnight[f], "Date"]) + (L5onset * secshour)
    TEST.df.L5[f, "L5_starttime"] <- L5_starttime

    ##debug
    TEST_L5_starttimes[f, ] <- (as.POSIXct(CRV.data[CRV.data.locmidnight[f], "Date"]) + (L5onset * secshour)) - (as.POSIXct(CRV.data[CRV.data.locmidnight[f], "Date"]))

  }

  ## After - Loop for calculating L5 and L5_starttime - processing:
  ## Take L5 as mean from L5 values from data.frame, and assign to result
  L5 <- mean(TEST.df.L5[, "L5"], na.action = na.pass, na.rm = T)
  result$L5 <- L5

  ## NA starttime workaround
  TEST.df.L5[(which(is.na(TEST.df.L5[, "L5_starttime"]))), "L5_starttime"] <- TEST.df.L5[(which(!is.na(TEST.df.L5[, "L5_starttime"]))), "L5_starttime"][1] # Replace NA with first non-NA value

  ## Paste "1970-01-01" in front of L5_starttimes to cancel out day-differences
  L5.temp <- as.POSIXct(paste("1970-01-01", format(as.POSIXct(TEST.df.L5[, "L5_starttime"], origin = "1970-01-01"), "%H:%M:%S")))

  ## Assign corrected L5_starttime, and assign to result
  L5_starttime <- (as.POSIXct(CRV.data[CRV.data.locmidnight[f], "Date"]) + (mean(unlist(TEST_L5_starttimes), na.action = na.pass, na.rm = T) * secshour))
  result$L5_starttime <- L5_starttime


  ##---------------------------------------------------------------------------------------------------------

  ## M10: Average of the 10 Highest Hourly Means (within each day(!))
  ## Source: Witting, W., Kwa, I., Eikelenboom, P., Mirmiran, M., & Swaab, D. (1990).
  ## Alterations in the circadian rest-activity rhythm in aging and Alzheimer's disease.
  ## Biological Psychiatry, 27(6), 563-572. http://dx.doi.org/10.1016/0006-3223(90)90523-5

  ## Obtain locations of "midnight" (00:00:00 for M10) in dataset
  CRV.data.locmidnight <- grep(pattern = "00:00:00", x = CRV.data[, "Date"])

  ## Initialise empty data.frame for M10_starttimes
  TEST_M10_starttimes <- as.data.frame(matrix(NA, ncol = 1, nrow = length(CRV.data.locmidnight)))

  ## Initialise empty data.frame for M10 and M10_starttime results
  TEST.df <- data.frame("M10" <- NA, "M10_starttime" <- NA)


  ## Loop for calculating M10 and M10_starttimes
  for (f in 1:(length(CRV.data.locmidnight) - 1)) {

    ## Aggregate minute averages of whole 24 hour components in dataset:
    CRV.data.full24minavg <-  aggregate(CRV.data[(CRV.data.locmidnight[f]:CRV.data.locmidnight[f + 1]), "Activity"],
                                        list(hour = cut(as.POSIXct(CRV.data[(CRV.data.locmidnight[f]:CRV.data.locmidnight[f + 1]), "Date"]), breaks = "min")),
                                        mean)

    ## Extract only activity counts from CRV.data.full24minavg data
    CRV.data.full24minavg.act <- CRV.data.full24minavg$x

    ## Initialise empty vector for M10 location:
    CRV.data.loc.M10 <- NA

    ## Calculate mean activity in 600 minute intervals ~ 10 hours
    for (h in 1:length(CRV.data.full24minavg.act)) {
      CRV.data.loc.M10[h] <- mean(c(CRV.data.full24minavg.act, CRV.data.full24minavg.act[c(1:600)])[c(h:(599 + h))], na.rm = T)
    }

    ## Take minimum value for L5 and assign to dataset
    M10 <- max(CRV.data.loc.M10)
    TEST.df[f, "M10"] <- M10

    ## Initialise data.frame with CRV.data.loc.L5 to facilitate L5onset identification
    L5M10frame <- data.frame(CRV.data.loc.M10 = CRV.data.loc.M10)

    ## Locate the first value that equals M10 and get the number of hours from start
    M10onset <- which(L5M10frame$CRV.data.loc.M10 == M10)[1] / 60

    ## Calculate M10_starttime as M10onset time from "midnight", and assign to data.frame
    M10_starttime <- as.POSIXct(CRV.data[CRV.data.locmidnight[f], "Date"]) + (M10onset * secshour)
    TEST.df[f, "M10_starttime"] <- M10_starttime

    ##debug
    TEST_M10_starttimes[f, ] <- (as.POSIXct(CRV.data[CRV.data.locmidnight[f], "Date"]) + (M10onset * secshour)) - (as.POSIXct(CRV.data[CRV.data.locmidnight[f], "Date"]))

  }


  ## After - Loop for calculating M10 and M10_starttime - processing:
  ## Take M10 as mean from M10 values from data.frame, and assign to result
  M10 <- mean(TEST.df[, "M10"], na.action = na.pass, na.rm = T)
  result$M10 <- M10

  ## Assign corrected L5_starttime, and assign to result
  M10_starttime <- (as.POSIXct(CRV.data[CRV.data.locmidnight[f], "Date"]) + (mean(unlist(TEST_M10_starttimes), na.action = na.pass, na.rm = T) * secshour))
  result$M10_starttime <- M10_starttime


  ##---------------------------------------------------------------------------------------------------------

  ## RA: Relative Amplitude
  ## Source: Van Someren, E., Swaab, D., Colenda, C., Cohen, W., McCall, W. and Rosenquist, P. (1999).
  ## Bright Light Therapy: Improved Sensitivity to Its Effects on Rest-Activity Rhythms in Alzheimer Patients
  ## by Application of Nonparametric Methods. Chronobiology International, 16(4), pp.505-518.

  ## Calculate Amplitude by subtracting M10 from L5
  Amp <- M10 - L5

  ## Calculate Relative Amplitude by dividing Amplitude by the sum of L5 and M10, and assign to result
  RA <- Amp / (L5 + M10)
  result$RA <- RA

  ## Assign data to results
  result$CRV_data <- CRV.data

  ## Return the list with all result values.
  result
}
