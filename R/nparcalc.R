#' Nparcalc
#'
#' Calculate non-parametrical circadian rhythm variables, such as: IS, IV, L5, and M10.
#'
#' @param myACTdevice Name of the input device used. Should be either Actiwatch2 or MW8.
#' @param movingwindow A boolean indicating whether moving window is used.
#' @param CRV.data CRV data
#'
#' @return A list with the result values IS, IV, RA, L5, L5_starttime, M10, and M10_starttime.
nparcalc <- function(myACTdevice, movingwindow, CRV.data) {
  ## Defined constants
  secshour <- 60*60 # Seconds per hour
  secsday <- 24*secshour # Seconds per day

  result = list()

  ## Read Data
  # CRV.data <- read.table(file = file.path(newdir, paste(gsub(pattern = ".csv", replacement = "", x = ACTdata_file), "MANAGED.txt")))

  if (ncol(CRV.data) > 2) {
    CRV.data$Date <- paste(CRV.data$V1, " ", CRV.data$V2)
    CRV.data$Date <- as.POSIXct(CRV.data$Date)
    CRV.data$Activity <- CRV.data$V3
    CRV.data <- CRV.data[, c("Date", "Activity")]
  } else {
    colnames(CRV.data) <- c("Date", "Activity")
  }


  ## Prune data untill only full 24h days are obtained
  CRV.data.wholehours <- CRV.data[grep("00:00", CRV.data[, "Date"]), ]
  CRV.data.start <- which(CRV.data$Date == CRV.data.wholehours[1, "Date"])

  if (myACTdevice == "MW8") {
    # CRV.data.end <- lastwhole24h.pos
    CRV.data.end <- tail(grep("00:00:00", ACTdata.1.sub$Date), 2)[1]
  } else {# Actiwatch2 assumed
    CRV.data.end <- which(CRV.data[, "Date"] == CRV.data.wholehours[1, "Date"] + (secsday*13))
  }

  CRV.data <- CRV.data[CRV.data.start:CRV.data.end, ]
  #test.data <- CRV.data # Checking if nparACT and IS Calculation Module use the same data (Seems OK)
  # write.table(test.data, row.names = FALSE, col.names = FALSE,
  #             file = paste(gsub(pattern = ".csv", replacement = "", x = ACTdata_file), "MANAGED.txt"))


  ##---------------------------------------------------------------------------------------------------------

  ## IS: Interdaily stability
  ## Source: Van Someren, E., Swaab, D., Colenda, C., Cohen, W., McCall, W. and Rosenquist, P. (1999).
  ## Bright Light Therapy: Improved Sensitivity to Its Effects on Rest-Activity Rhythms in Alzheimer Patients
  ## by Application of Nonparametric Methods. Chronobiology International, 16(4), pp.505-518.

  ## Xi
  xi <- aggregate(CRV.data[, "Activity"],
                  list(hour = cut(as.POSIXct(CRV.data[, "Date"]), breaks = "hour")),
                  mean)


  if (movingwindow == FALSE){
      xi <- xi[1:(nrow(xi) - 1), ]
  } else {
      xi <- xi[1:(nrow(xi)), ]
  }


  xi <- xi$x

  # X
  X <- mean(xi, na.rm = T)

  xi_X <- xi - X # difference consecutive hourly means and overall mean
  sq.xi_X <- xi_X^2 # square of differences
  sum.sq.xi_X <- sum(sq.xi_X, na.rm = T) # sum of squares
  n <- sum(!is.na(xi)) # get number og hours (should be 168 for 7 day intervals (7*24))
  sum.sq.xi_X.perhour <- sum.sq.xi_X/n # sum of squares per hour


  Xh <- rowMeans(matrix(xi, nrow = 24), na.rm = T)
  Xh_X <- Xh - X # difference 24 hour means and overall mean
  #! #! Deze variabele wordt verder niet gebruikt? (Ik heb m gecomment)
  #sq.Xh_X <- Xh_X^2 # square of difference
  sum.sq.Xh_X <- sum(Xh_X^2, na.rm = T) # sum of squares
  sum.sq.Xh_X.perhour <- sum.sq.Xh_X/24 # sum of squares per hour

  IS <- sum.sq.Xh_X.perhour/sum.sq.xi_X.perhour # hourly variance divided by overall variance
  IS <- round(x = IS, digits = 2)
  result$IS <- IS

  ##---------------------------------------------------------------------------------------------------------

  ## IV: Interdaily Variability
  Xi_diffXi <- diff(xi) # difference Xi and previous Xi (the difference between all successive hours) (!!! Warning because no previous hour for 1st obs.)
  sum.sq.Xi_diffXi <- sum(Xi_diffXi^2, na.rm = T) # sum of squares
  sum.sq.Xi_diffXi.perhour <- sum.sq.Xi_diffXi/n # sum of squares per hour

  Xi_X <- xi - X # difference Xi and overall mean
  #! #! Deze variabele wordt verder niet gebruikt? (Ik heb m gecomment)
  #sq.Xi_X <- Xi_X^2 # square of difference
  sum.sq.Xi_X <- sum(Xi_X^2, na.rm = T) # sum of squares
  sum.sq.Xi_X.perhour <- sum.sq.Xi_X/(n - 1) # sum of squares per hour minus 1

  IV <- sum.sq.Xi_diffXi.perhour / sum.sq.Xi_X.perhour
  IV <- round(x = IV, digits = 2)
  result$IV <- IV

  ##---------------------------------------------------------------------------------------------------------

  ## L5: Average of the 5 Lowest Hourly Means (within each day(!))
  ## daily minute means

  # CRV.data.locmidnight <- grep(pattern = "12:00:00", x = CRV.data[, "Date"]) # Locations of midnight (!!!Is "12:00:00" midnight or "00:00:00"???)
  CRV.data.locmidnight <- grep(pattern = "00:00:00", x = CRV.data[, "Date"]) # Locations of midnight (!!!Is "12:00:00" midnight or "00:00:00"???)
  # CRV.data.locmidnight <- grep(pattern = strftime(ACTdata.1.sub$Date[1], format = "%H:%M:%S"), x = CRV.data[, "Date"])

  TEST.df.L5 <- data.frame("L5" <- NA, "L5_starttime" <- NA)

  for (f in 1:(length(CRV.data.locmidnight) - 1)) {

    CRV.data.full24minavg <- CRV.data[(CRV.data.locmidnight[f]:CRV.data.locmidnight[f + 1]), ]
    colnames(CRV.data.full24minavg) <- c("hour", "x")

    CRV.data.full24minavg <- CRV.data.full24minavg[1:1440, ]

    CRV.data.loc.L5 <- NA
    CRV.data.full24minavg.act <- CRV.data.full24minavg$x
    for (g in 1:length(CRV.data.full24minavg.act)) {
      CRV.data.loc.L5[g] <- mean(c(CRV.data.full24minavg.act, CRV.data.full24minavg.act[c(1:300)])[c(g:(299 + g))], na.rm = T)
    } # calculating mean activity in 300 minute intervals ~ 5 hours

    L5 <- min(CRV.data.loc.L5) #round(min(CRV.data.loc.L5), 2)
    # M10 <- max(CRV.data.loc.M10) #round(max(CRV.data.loc.M10), 2)
    TEST.df.L5[f, "L5"] <- L5
    # TEST.df[f, "M10"] <- M10

    # print(M10)

    L5M10frame <- data.frame(CRV.data.loc.L5 = CRV.data.loc.L5) #, CRV.data.loc.M10 = CRV.data.loc.M10)

    L5onset <- which(L5M10frame$CRV.data.loc.L5 == L5)[1]/60 # locating the first value that equals L5 and get the number of hours from start



    # M10onset <- which(L5M10frame$CRV.data.loc.M10 == M10)[1]/60 # locating the first value that equals M10 and get the number of hours from start

    L5_starttime <- as.POSIXct(CRV.data[CRV.data.locmidnight[f], "Date"]) + (L5onset * secshour)
    # print(CRV.data[CRV.data.locmidnight[f], "Date"] + (L5onset*secshour))
    # TEST.df[f, 1] <- as.POSIXct(L5_starttime, origin = "1970-01-01")
    TEST.df.L5[f, "L5_starttime"] <- L5_starttime

  }

  L5 <- mean(TEST.df.L5[, "L5"])
  result$L5 <- L5

  L5.temp <- as.POSIXct(paste("1970-01-01", format(as.POSIXct(TEST.df.L5[, "L5_starttime"], origin = "1970-01-01"), "%H:%M:%S")))

  for (a in 1:length(L5.temp)) {

    # print(as.POSIXct("1970-01-01 00:00:00 CET") - L5.temp[a])

    if (L5.temp[a] > "1970-01-01 21:00:00 CET") {
      L5.temp[a] <- (L5.temp[a] + (secshour * 6))
    }
    # print(L5.temp[a])
  }


  L5.temp <- as.POSIXct(paste("1970-01-01", format(L5.temp, "%H:%M:%S")))

  L5_starttime <- mean(L5.temp)
  result$L5_starttime <- L5_starttime

  ##---------------------------------------------------------------------------------------------------------

  ## M10: Average of the 10 Highest Hourly Means (within each day(!))
  ## daily minute means

  TEST.df <- data.frame("M10" <- NA, "M10_starttime" <- NA)

  for (f in 1:(length(CRV.data.locmidnight) - 1)) {

    # CRV.data.locmidnight <- grep(pattern = "00:00:00", x = CRV.data[, "Date"]) # Locations of midnight (!!!Is "12:00:00" midnight or "00:00:00"???)

    CRV.data.full24minavg <-  aggregate(CRV.data[(CRV.data.locmidnight[f]:CRV.data.locmidnight[f + 1]), "Activity"],
                                        list(hour = cut(as.POSIXct(CRV.data[(CRV.data.locmidnight[f]:CRV.data.locmidnight[f + 1]), "Date"]), breaks = "min")),
                                        mean)

    # CRV.data.full24minavg <- CRV.data.full24minavg


    CRV.data.full24minavg.act <- CRV.data.full24minavg$x
    CRV.data.loc.M10 <- NA
    for (h in 1:length(CRV.data.full24minavg.act)) {
      CRV.data.loc.M10[h] <- mean(c(CRV.data.full24minavg.act, CRV.data.full24minavg.act[c(1:600)])[c(h:(599 + h))], na.rm = T)
    } # calculating mean activity in 600 minute intervals ~ 10 hours
    # L5 <- min(CRV.data.loc.L5) #round(min(CRV.data.loc.L5), 2)
    M10 <- max(CRV.data.loc.M10) #round(max(CRV.data.loc.M10), 2)
    # TEST.df[f, "L5"] <- L5
    TEST.df[f, "M10"] <- M10

    # print(M10)

    L5M10frame <- data.frame(CRV.data.loc.M10 = CRV.data.loc.M10)

    # L5onset <- which(L5M10frame$CRV.data.loc.L5 == L5)[1]/60 # locating the first value that equals L5 and get the number of hours from start
    M10onset <- which(L5M10frame$CRV.data.loc.M10 == M10)[1]/60 # locating the first value that equals M10 and get the number of hours from start

    # L5_starttime <- CRV.data[CRV.data.locmidnight[f], "Date"] + (L5onset*secshour)
    # print(CRV.data[CRV.data.locmidnight[f], "Date"] + (L5onset*secshour))
    # # TEST.df[f, 1] <- as.POSIXct(L5_starttime, origin = "1970-01-01")
    # TEST.df[f, "L5_starttime"] <- L5_starttime

    M10_starttime <- as.POSIXct(CRV.data[CRV.data.locmidnight[f], "Date"]) + (M10onset * secshour)
    # print(CRV.data[CRV.data.locmidnight[f], "Date"] + (M10onset * secshour))
    # TEST.df[f, 2] <- as.POSIXct(M10_starttime, origin = "1970-01-01")
    TEST.df[f, "M10_starttime"] <- M10_starttime

  }

  M10 <- mean(TEST.df[, "M10"])
  result$M10 <- M10

  ### !!!! HERE is the whole problem (!!!) a mean is calculated over differing days and times. However, with differing days
  ### strong discrepancies appear as WHOLE days are averaged. The SOLUTION is simple, attach the same day to all times.
  ### There is, however, a smaller problem with multiple M10 starttimes being found within one day (!!)

  M10_starttime <- mean(as.POSIXct(paste("1970-01-01", format(as.POSIXct(TEST.df[, "M10_starttime"], origin = "1970-01-01"), "%H:%M:%S"))))
  result$M10_starttime <- M10_starttime

  Amp <- M10 - L5 # self explanatory
  RA <- Amp/(L5 + M10)
  result$RA <- RA

  # return actogram data
  result$CRV_data <- CRV.data

  # Return the list with all result values.
  result
}
