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
#' @importFrom stats na.pass
#' @importFrom utils tail
#' @importFrom stats aggregate
#' @importFrom moments skewness
#' @importFrom moments kurtosis
nparcalc <- function(myACTdevice, movingwindow, CRV.data, ACTdata.1.sub, out = NULL) {

  ## Step 1: Basic Operations-------------------------------------------------------------------

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
  #! Wat mij niet helemaal duidelijk was is waarom je het laatste meetpunt in xi weggooit als er geen moving window is?
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
  sum.sq.Xi_diffXi.perhour <- sum.sq.Xi_diffXi / (n - 1) # sum of squares per hour

  Xi_X <- xi - X # difference Xi and overall mean
  sum.sq.Xi_X <- sum(Xi_X ^ 2, na.rm = T) # sum of squares
  # sum.sq.Xi_X.perhour <- sum.sq.Xi_X / (n - 1) #! Delen door n niet (n - 1)!
  sum.sq.Xi_X.perhour <- sum.sq.Xi_X / n # sum of squares per hour minus 1

  IV <- sum.sq.Xi_diffXi.perhour / sum.sq.Xi_X.perhour
  IV <- round(x = IV, digits = 2) # rounding down to 2 decimals.
  result$IV <- IV # Write IS to results

  ##---------------------------------------------------------------------------------------------------------

  ## L5: Average of the 5 Lowest Hourly Means (within each day(!))
  ## Source: Witting, W., Kwa, I., Eikelenboom, P., Mirmiran, M., & Swaab, D. (1990).
  ## Alterations in the circadian rest-activity rhythm in aging and Alzheimer's disease.
  ## Biological Psychiatry, 27(6), 563-572. http://dx.doi.org/10.1016/0006-3223(90)90523-5

  ## Calculate mean activity per minute over the whole period.
  ## The idea is to calculate an average day to calculate L5 and M10 over.
  averageday <- matrix(c(substr(CRV.data[1:1440, "Date"], 14, 22), rep(NA, 1440)), nrow = 1440, ncol = 2)

  selection_mat <- matrix(FALSE, 1440)


  for (aa in 1:1440) {

    selection_mat[aa, ] <- TRUE
    mean(CRV.data[selection_mat, "Activity"])
    averageday[aa, 2] <- mean(CRV.data[selection_mat, "Activity"], na.rm = TRUE) # added "na.rm = TRUE" for moving window
    selection_mat <- matrix(FALSE, 1440)
  }

  averageday[, 2] <- as.numeric(averageday[, 2])

  #! DEBUG to allow for window to cross midnight point
  averageday <- rbind(averageday, averageday)

  averageday_loc_L5 <- matrix(NA, nrow(averageday))

  ## Loop for calculating mean activity in 300 minute intervals ~ 5 hours
  for (gg in 1:(nrow(averageday) - 300)) {
    averageday_loc_L5[gg] <- mean(as.numeric(averageday[c(gg:(299 + gg)), 2]))
  }

  L5_starttime <- averageday[which.min(averageday_loc_L5), 1]
  L5 <- round(averageday_loc_L5[which.min(averageday_loc_L5)], 2)

  result$L5 <- L5
  result$L5_starttime <- L5_starttime

  ##---------------------------------------------------------------------------------------------------------

  ## M10: Average of the 10 Highest Hourly Means (within each day(!))
  ## Source: Witting, W., Kwa, I., Eikelenboom, P., Mirmiran, M., & Swaab, D. (1990).
  ## Alterations in the circadian rest-activity rhythm in aging and Alzheimer's disease.
  ## Biological Psychiatry, 27(6), 563-572. http://dx.doi.org/10.1016/0006-3223(90)90523-5

  ## Calculate mean activity per minute over the whole period.
  ## The idea is to calculate an average day to calculate L5 and M10 over.

  averageday_loc_M10 <- matrix(NA, nrow(averageday))

  ## Loop for calculating mean activity in 600 minute intervals ~ 10 hours
  for (hh in 1:(nrow(averageday) - 600)) {
    averageday_loc_M10[hh] <- mean(as.numeric(averageday[c(hh:(599 + hh)), 2]), na.rm = TRUE)
  }

  M10_starttime <- averageday[which.max(averageday_loc_M10), 1]
  M10 <- round(averageday_loc_M10[which.max(averageday_loc_M10)], 2)

  result$M10 <- M10
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

  ##---------------------------------------------------------------------------------------------------------

  ## EWS Part

  ## Mean
  Mean <- round(mean(CRV.data[, "Activity"], na.rm = TRUE), 2)
  result$Mean <- Mean

  ## Variance
  Variance <- round(var(CRV.data[, "Activity"], na.rm = TRUE), 2)
  result$Variance <- Variance

  ## Standard Deviation
  SD <- round(sd(CRV.data[, "Activity"], na.rm = TRUE), 2)
  result$SD <- SD

  ## Coefficient of Variation
  CoV <- round(((sd(CRV.data[, "Activity"], na.rm = TRUE) / mean(CRV.data[, "Activity"], na.rm = TRUE)) * 100), 2)
  result$CoV <- CoV

  ## Skewness
  Skewness <- moments::skewness(CRV.data[, "Activity"], na.rm = TRUE)
  result$Skewness <- round(Skewness, 2)

  ## Kurtosis
  Kurtosis <- moments::kurtosis(CRV.data[, "Activity"], na.rm = TRUE)
  result$Kurtosis <- round(Kurtosis, 2)

  ## Autocorrelation at lag-1, lag-2, lag-3, etc.
  Autocorr <- acf(x = CRV.data[, "Activity"], lag.max = 120, na.action = na.pass, plot = FALSE)
  result$Autocorr <- round(Autocorr$acf[2], 2)
  result$Autocorr_lag2 <- round(Autocorr$acf[3], 2)
  result$Autocorr_lag3 <- round(Autocorr$acf[4], 2)
  result$Autocorr_lag60 <- round(Autocorr$acf[61], 2)
  result$Autocorr_lag120 <- round(Autocorr$acf[121], 2)

  ## Time to recovery (if not recovered within timeframe assign max_lag))
  if(length(which(Autocorr$acf < 0.2)) != 0){

      result$Time_to_Recovery <- which(Autocorr$acf < 0.2)[1]

  } else {

      result$Time_to_Recovery <- 120

  }

  ## Assign data to results
  result$CRV_data <- CRV.data

  ## Return the list with all result values.
  result
}
