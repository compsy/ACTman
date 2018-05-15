#' sleepdata_overview
#'
#' Calculate sleep data
#'
#' @param workdir The directory where the sleep files are located.
#' @param actdata The activity data.
#' @param i The index of the current file in ACTdata.files
#' @param lengthcheck Boolean value. If TRUE, the dataset is shortened to the start date plus 14 days, and observations more than 14 days after the start date are removed.
#'
#' @return Returns a sleepdata overview.
#'
#' @importFrom stats na.omit
#' @importFrom utils read.csv
#' @importFrom utils write.csv
sleepdata_overview <- function(workdir, actdata, i, lengthcheck) {
  # Voeg fragmentation index toe!

  ## Step 1: Basic Operations.----------------------------------------------------------------------------

  # # Set Working directory
  # oldworkdir <- getwd()
  # setwd(workdir)

  # Load data
  data <- actdata
  # TEMPdat <<- data

  # data$Activity..MW.counts. <- as.numeric(as.character(data$Activity..MW.counts.)) #Use when data <- ACTdata.1 !!!
  data$Activity..MW.counts. <- as.numeric(as.character(data$Activity))

  ## Read sleeplog
  data.sleeplog <- read.csv(file = list.files(pattern = "sleeplog.csv")[i])
  data.sleeplog.sub <- data.sleeplog[, c(1, 2, 3)]

  # Recreate data$Time var for this module
  data$Time <- strftime(data$Date, format = "%H:%M:%S")

  # Select different nights
  startdates.new.days <- which(strftime(data$Date, format = "%H:%M:%S") == "12:00:00")
  # startdates.new.days <- which(strftime(data$Date, format = "%H:%M:%S") == "00:00:00")
  # startdates.new.days <- which(data$Time == "12:00:00") #Use when data <- ACTdata.1 !!!
  end.night.1 <- startdates.new.days[1]

  ## Initialise Output Overview
  sleepdata.overview <- data.frame("date" = NA, "sleep.start" = NA, "sleep.end" = NA,
                                   "sleep.efficiency" = NA, "sleep.latency" = NA)

  ## END OF Step 1: Basic Operations.

  ## Exception for lengtcheck in number ook LOOP iterations
  if (lengthcheck) {
    loop_steps <- 14
    if (nrow(data.sleeplog) < 14) {
      print("ERROR: we need at least 14 days of sleeplog. Terminating program.")
      stop('ERROR: we need at least 14 days of sleeplog. Terminating program.')
    }
  } else {
    loop_steps <- nrow(data.sleeplog)
  }

  ## LOOP for Calculatins Sleep Variables
  for (a in 1:loop_steps) {

    if (a == 1) {
      # aaa <- data[1:(end.night.1 + 1440), ] #! 17-4-18: was "aaa <- data[1:(end.night.1), ]" (!!)
      aaa <- data[1:(end.night.1), ]
    } else {
      aaa <- data[(end.night.1 + (1 + (1440 * (a - 2)))):(end.night.1 + (1440 * (a - 1))), ]
    }

    aaa$Date <- as.POSIXct(aaa$Date)

    ## Formula to create a score value per epoch
    aaa <- dplyr::mutate(aaa, score = (
      (dplyr::lag(Activity..MW.counts., n = 1L, default = 0) / 5) +
        (dplyr::lag(Activity..MW.counts., n = 2L, default = 0) / 25) +
        (dplyr::lead(Activity..MW.counts., n = 1L, default = 0) / 5) +
        (dplyr::lead(Activity..MW.counts., n = 2L, default = 0) / 25) +
        Activity..MW.counts.))

    ## Calculate whether score indicates awake or not
    ## NOTE: 40 is 'Medium Sensitivity', 20 is 'High Sensitivity'
    aaa$WakeSleep <- ifelse(aaa$score > 20, 1, 0) # 1 is awake, 0 is sleep
    aaa$MobileImmobile <- ifelse(aaa$Activity..MW.counts. > 3, 1, 0) # 1 is mobile, 0 is immobile

    ## Now calculate sleep start from a certain point in the data (based on sleep log)
    bedtime <- paste((as.character(data.sleeplog.sub$BedTime[a])), ":00", sep = "")

    if (nchar(bedtime) > 8) {
      bedtime <- substr(bedtime, 1, nchar(bedtime) - 3)
    }

    #debug
    print(paste("rownr.bedtime is:", which(aaa$Time == bedtime)[1]))

    rownr.bedtime <- which(aaa$Time == bedtime)[1]


    gotup <- paste((as.character(data.sleeplog.sub$GotUp[a])), ":00", sep = "")

    if (nchar(gotup) > 8) {
      gotup <- substr(gotup, 1, nchar(gotup) - 3)
    }

    rownr.gotup <- which(aaa$Time == gotup)[1]

    ## Calculation should indicate the moment of sleep start and 10 consecutive non-active epochs. 1 epoch can have activity.

    ## Dit gedeelte aanpassen a.d.h.v. MW8 Info Bullitin
    ## !!Open Question: Where is Number of Counts Allowed above Threshold specified?? (6 seems arbitrary?)
    aaa$epoch.sleep.chance <- ifelse(aaa$Activity..MW.counts. > 6, 1, 0) # 1 is above treshold, 0 is below treshold
    aaa$sleep.chance <- (dplyr::lead(aaa$epoch.sleep.chance, n = 1L) +
                           dplyr::lead(aaa$epoch.sleep.chance, n = 2L) +
                           dplyr::lead(aaa$epoch.sleep.chance, n = 3L) +
                           dplyr::lead(aaa$epoch.sleep.chance, n = 4L) +
                           dplyr::lead(aaa$epoch.sleep.chance, n = 5L) +
                           dplyr::lead(aaa$epoch.sleep.chance, n = 6L) +
                           dplyr::lead(aaa$epoch.sleep.chance, n = 7L) +
                           dplyr::lead(aaa$epoch.sleep.chance, n = 8L) +
                           dplyr::lead(aaa$epoch.sleep.chance, n = 9L) +
                           aaa$epoch.sleep.chance
    )


    ##--------------------------------------------------
    ##debug
    # print("##--------------------------------------------------")
    # print(paste("Iteration No.:", a))
    # print(paste("rownr.gotup:", rownr.gotup))
    # print(paste("rownr.bedtime:", rownr.bedtime))

    ## Exception for when rownr.gotup is NA
    if (is.na(rownr.gotup)) {

      message("rownr.gotup is NA!")
      message("Skipping current day!")
      next()

    }

    # rownr.bedtime <- which(aaa$Time == bedtime)[1]
    # print(rownr.bedtime)

    ## Exception for when rownr.bedtime is NA
    if (is.na(rownr.bedtime)) {

      message("rownr.bedtime is NA!")
      message("Skipping current day!")
      next()

    }


    #! rownr.sleep.start is NA when obs with sleep.chance < 2 is NOT in aaa.bedtime (!!)
    aaa.bedtime <- aaa[rownr.bedtime:rownr.gotup, ] # This includes only the time in which the subject is in bed handpicked in this sample based on lights out (00:17) / got up (7:59) data

    ## Now create a function which returns first $Time after certain time (lights out in sleep log)
    sleep.start. <- aaa.bedtime[which(aaa.bedtime$sleep.chance < 2), ]
    ## First row now contains the start of sleep.
    sleep.start <- as.character(sleep.start.$Time[1])
    rownr.sleep.start <- which(aaa$Time == sleep.start)[1]

    ## Calculate wake up time
    aaa$wakeup.chance <- (dplyr::lag(aaa$epoch.sleep.chance, n = 1L) +
                            dplyr::lag(aaa$epoch.sleep.chance, n = 2L) +
                            dplyr::lag(aaa$epoch.sleep.chance, n = 3L) +
                            dplyr::lag(aaa$epoch.sleep.chance, n = 4L) +
                            aaa$epoch.sleep.chance
    )


    ##--------------------------------------------------
    #! debug
    print(paste("rownr.sleep.start:", rownr.sleep.start))

    ## Exception for when rownr.sleep.start is NA
    if (is.na(rownr.sleep.start)) {

      message("Warning: rownr.sleep.start is NA!")
      message("Cause: obs with sleep.chance < 2 is NOT in aaa.bedtime")
      message("Action: Take bedtime from sleeplog instead")
      # message("Skipping current day!")
      # next()
      # stop()

      #! rownr.sleep.start is NA when obs with sleep.chance < 2 is NOT in aaa.bedtime (!!)
      #! Take bedtime from sleeplog instead (!!)
      rownr.sleep.start <- rownr.bedtime

    }

    # Sanity check to make sure that we wake up after going to bed
    if (rownr.bedtime >= rownr.gotup || rownr.sleep.start >= rownr.gotup || rownr.bedtime > rownr.gotup) {
      message("Warning: sanity checks for bedtime, gotup, and/or sleep start failed")
      message("Skipping current day!")
      next()
    }

    ## Get sleeptime
    aaa.sleeptime <- aaa[rownr.sleep.start:(rownr.gotup + (4 * 60)), ] # A (4 * 60) minute extra window is included, for when a subject filled the diary incorrectly (with a too early time). This makes sure that if sleep actually ended after the GotUp time the sleep end is somewhere near the gotup, instead of in the middle of the night.

    ## Now create a function which returns first $Time after certain time (lights out in sleep log)
    #  Changed "aaa.sleeptime$wakeup.chance == 2" to "aaa.sleeptime$wakeup.chance >= 2" to circumvent error
    sleep.end. <- aaa.sleeptime[which(aaa.sleeptime$wakeup.chance >= 2 & dplyr::lead(aaa.sleeptime$wakeup.chance > 2)), ]

    # Calculate the difference, so the beginning of the last wake period is selected.
    sleep.end.$diff[2:nrow(sleep.end.)] <- diff(as.numeric(row.names(sleep.end.)))

    ## First row now contains the start of sleep.
    sleep.end.new <- sleep.end.[which(sleep.end.$diff > 4), ] # Bigger than 4, as the difference should be at least 5 minutes, so a small difference with possible sleep is left out.

    ## err ex
    if (sum(na.omit(sleep.end.$diff > 4)) == 0) {

      # message("sleep.end.$diff is NOT > 4!")
      sleep.end.new <- sleep.end.[1, ]

    }

    sleep.end.row <- as.numeric(rownames(sleep.end.new[nrow(sleep.end.new), ]))
    sleep.end <- as.character(aaa$Time[ifelse(sleep.end.row > rownr.gotup, rownr.gotup, sleep.end.row)])
    rownr.sleep.end <- ifelse(sleep.end.row > rownr.gotup, rownr.gotup, sleep.end.row)


    #! debug
    # print(paste("rownr.sleep.end:", rownr.sleep.end))

    ## Exception for when rownr.sleep.end is NA
    if (is.na(rownr.sleep.end)) {

      message("rownr.sleep.end is NA!")
      message("Skipping current day!")
      next()

      #! Take sleep.end from sleeplog instead (!!)
      # rownr.sleep.end <- rownr.gotup

    }


    ## END OF Step 2: Calculate sleep for night1.------------------------------------------------------------------------

    ## Step 3: Calculate sleep analysis data.----------------------------------------------------------------------------
    aaa.assumedsleeptime <- aaa[rownr.sleep.start:(rownr.sleep.end - 1), ] # Subframe the assumed sleep time, -1 is done otherwise it includes the first wake bout.
    TimeInBed <- (rownr.gotup - rownr.bedtime) / 60 # The total elapsed time between the "Lights Out" and "Got Up" times
    AssumedSleep <- (rownr.sleep.end - rownr.sleep.start) / 60 # The total elapsed time between the "Fell Asleep" and "Woke Up" times.
    WakeEpochs <- sum(aaa.assumedsleeptime$WakeSleep == 1) # Number of epochs scored as "awake"
    ActualSleep <- ((AssumedSleep * 60) - WakeEpochs) / 60 # The total time spent in sleep according to the epoch-by-epoch wake/sleep scores.

    # Commented out because not used:
    ActualSleepPerc <- (ActualSleep / AssumedSleep) * 100 # Actual sleep time expressed as a percentage of the assumed sleep time
    ActualWakeTime <- WakeEpochs / 60 # Total time spent in wake according to the epoch-by-epoch wake/sleep scores.
    ActualWakePerc <- 100 - ActualSleepPerc # Actual sleep time expressed as a percentage of the assumed sleep time.
    SleepEfficiency <- (ActualSleep/TimeInBed) * 100 # Actual sleep time expressed as a percentage of time in bed.
    SleepLatency <- (rownr.sleep.start - rownr.bedtime) / 60 # The time between "Lights Out" and "Fell Asleep"


    # To add:
    #SleepBouts # The number of contiguous sections categorised as sleep in the epoch-by-epoch wake/sleep categorisation
    #WakeBouts # The number of contiguous sections categorised as wake in the epoch-by-epoch wake/sleep categorisation
    #MeanSleepBout # The average length of each of the sleep bouts
    #MeanWakeBout # The average length of each of the wake bouts
    #ImmobileMins <- (AssumedSleep*60) - sum(aaa.assumedsleeptime$MobileImmobile == 1) # The total time categorised as Immobile in the epoch-by-epoch mobile/immobile categorisation
    #ImmobileTime <- (ImmobileMins/(AssumedSleep*60))*100 # The immobile time expressed as a percentage of the assumed sleep time.
    #MobileMins <- (AssumedSleep*60) - sum(aaa.assumedsleeptime$MobileImmobile == 0) # The total time categorised as mobile in the epoch-by-epoch mobile/immobile categorisation
    #MobileTime <- (MobileMins/(AssumedSleep*60))*100 # The mobile time expressed as a percentage of the assumed sleep time.
    # To add:
    #Immobile bouts # The number of contiguous sections categorised as immobile in the epoch-by-epoch mobile/immobile categorisation
    #MeanImmobileBout # The average length of each of the immobile bouts
    #ImmobileBouts.1 # The number of immobile bouts which were less than or equal to one moinute in length.
    #ImmobileBouts.1Perc # The humber of immobile bouts less than or equal to one minute expressed as a percentage of the total number of immobile bouts.
    #TotalActivityScore # The total of all the activity counts during the assumed sleep period.
    #MeanNonZero # The total activity score divided by the number of epochs with greater than zero activity in the assumed sleep period. Note that this result will be expected to scale depending on the length of the epoch.
    #FragmentationIndex # The sum of the MobileTime and the ImmobileBouts.1Perc. This is an indication of the degree of fragmentation of the sleep period, and can be used as an indication of sleep quality (or the lack of it).

    ## END OF Step 3: Calculate sleep analysis data.------------------------------------------------------------------

    # print(aaa[1, ])

    ## Step 4: Fill in the Sleep Overview

    # Attach Sleep Analysis output to overview
    sleepdata.overview[a, "date"] <- as.character(data.sleeplog[a, "Date"])
    sleepdata.overview[a, "bedtime_sleeplog"] <- bedtime
    sleepdata.overview[a, "gotup_sleeplog"] <- gotup
    sleepdata.overview[a, "sleep.start"] <- sleep.start
    sleepdata.overview[a, "sleep.end"] <- sleep.end
    sleepdata.overview[a, "timeinbed"] <- round(TimeInBed, 2)
    sleepdata.overview[a, "assumed_sleep"] <- round(AssumedSleep, 2)
    sleepdata.overview[a, "wakepochs_duration"] <- WakeEpochs
    sleepdata.overview[a, "actual_sleep_duration"] <- round(ActualSleep, 2)
    sleepdata.overview[a, "actual_sleep_perc"] <- round(ActualSleepPerc, 2)
    sleepdata.overview[a, "actual_wake_duration"] <- round(ActualWakeTime, 2)
    sleepdata.overview[a, "actual_wake_perc"] <- round(ActualWakePerc, 2)
    sleepdata.overview[a, "sleep.efficiency"] <- round(SleepEfficiency, 2)
    sleepdata.overview[a, "sleep.latency"] <- round(SleepLatency, 2)


  }

  # ## Create "Results" directory:
  workdir_temp <- getwd()
  dir.create(file.path(workdir_temp, "Results"), showWarnings = FALSE)
  setwd(file.path(workdir_temp, "Results"))

  ## Write sleepdata output as .CSV into "Results" directory:
  write.csv(sleepdata.overview, file = paste("sleepdata", i, ".csv", sep = ""))

  ## Set working directory back to main working directory:
  setwd(workdir_temp)
  rm(workdir_temp)

  # Return a result
  sleepdata.overview
}
