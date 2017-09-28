###########################################################
### ACTman Sleep Analysis Module                        ###
### Script authors: Yoram Kunkels & Stefan Knapen       ###
### Edited on: 23/06/2017                               ###
### > Now uses Managed data (ACTdata.1.sub)             ###
###~@~@~@~@~@~@~@~@~@~@~@~@~@~@~@~@~@~@~@~@~@~@~@~@~@~@~###
#' sleepdata_overview
#'
#' Calculate sleep data
#'
#' @param workdir The directory where the sleep files are located.
#' @param actdata The activity data.
#'
#' @return Returns a sleepdata overview.
#' @export
sleepdata_overview <- function(workdir, actdata) {
  ## Step 1: Basic Operations.----------------------------------------------------------------------------

  # Set Working directory
  oldworkdir <- getwd()
  setwd(workdir)

  # Load data (example data: actigraphy file from MotionWatch8 worn by Nicolien Knapen)
  data <- actdata # Previously read.csv("NK_data.csv")
  # data$Activity..MW.counts. <- as.numeric(as.character(data$Activity..MW.counts.)) #Use when data <- ACTdata.1 !!!
  data$Activity..MW.counts. <- as.numeric(as.character(data$Activity))

  #! Ik snap niet waarom je hier keihard die nkdata sleeplog inleest. Is dat niet steeds dezelfde data voor alle personen?
  data.sleeplog <- read.csv("NKdata_sleeplog.csv")
  data.sleeplog.sub <- data.sleeplog[, c(1,2,3)]

  # Recreate data$Time var for this module
  data$Time <- strftime(data$Date, format = "%H:%M:%S")

  # Select different nights
  startdates.new.days <- which(strftime(data$Date, format = "%H:%M:%S") == "12:00:00")
  # startdates.new.days <- which(data$Time == "12:00:00") #Use when data <- ACTdata.1 !!!
  end.night.1 <- startdates.new.days[1]

  ## Initialise Output Overview
  sleepdata.overview <- data.frame("date" = NA, "sleep.start" = NA, "sleep.end" = NA,
                                   "sleep.efficiency" = NA, "sleep.latency" = NA)

  ## END OF Step 1: Basic Operations.

  ## LOOP for Calculatins Sleep Variables
  for (a in 1:nrow(data.sleeplog)) {
    print(paste("data.sub.night", a, sep = ""))

    if (a == 1) {
      aaa <- data[1:end.night.1,]
    } else {
      aaa <- data[(end.night.1 + (1 + (1440 * (a - 2)))):(end.night.1 + (1440 * (a - 1))), ]
    }

    aaa$Date <- as.POSIXct(aaa$Date)

    ## Formula to create a score value per epoch
    aaa <- dplyr::mutate(aaa, score = (
      (dplyr::lag(Activity..MW.counts., n = 1L, default = 0)/5) +
        (dplyr::lag(Activity..MW.counts., n = 2L, default = 0)/25) +
        (dplyr::lead(Activity..MW.counts., n = 1L, default = 0)/5) +
        (dplyr::lead(Activity..MW.counts., n = 2L, default = 0)/25) +
        Activity..MW.counts.))

    ## Calculate whether score indicates awake or not
    ## NOTE: 40 is 'Medium Sensitivity', 20 is 'High Sensitivity'
    aaa$WakeSleep <- ifelse(aaa$score > 20, 1, 0) # 1 is awake, 0 is sleep
    aaa$MobileImmobile <- ifelse(aaa$Activity..MW.counts. > 3, 1, 0) # 1 is mobile, 0 is immobile

    ## Now calculate sleep start from a certain point in the data (based on sleep log)
    bedtime <- paste((as.character(data.sleeplog.sub$BedTime[a])), ":00", sep = "")
    rownr.bedtime <- which(aaa$Time == bedtime)

    gotup <- paste((as.character(data.sleeplog.sub$GotUp[a])), ":00", sep = "")
    rownr.gotup <- which(aaa$Time == gotup)

    ## Calculation should indicate the moment of sleep start and 10 consecutive non-active epochs. 1 epoch can have activity.

    ## Dit gedeelte aanpassen a.d.h.v. MW8 Info Bullitin
    ## !!Open Question: Where is Number of Counts Allowed above Threshold specified?? (6 seems arbitrary?)
    ## !! Day 4 sleep.end zou 08:53:00 moeten zijn i.p.v. 05:44:00??
    aaa$epoch.sleep.chance <- ifelse(aaa$Activity..MW.counts. > 6, 1, 0) # 1 is under treshold, 0 is above treshold
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

    aaa.bedtime <- aaa[rownr.bedtime:rownr.gotup,] # This includes only the time in which the subject is in bed handpicked in this sample based on lights out (00:17) / got up (7:59) data

    ## Now create a function which returns first $Time after certain time (lights out in sleep log)
    sleep.start. <- aaa.bedtime[which(aaa.bedtime$sleep.chance < 2),]
    ## First row now contains the start of sleep.
    sleep.start <- as.character(sleep.start.$Time[1])
    rownr.sleep.start <- which(aaa$Time == sleep.start)

    ## Calculate wake up time
    aaa$wakeup.chance <- (lag(aaa$epoch.sleep.chance, n = 1L) +
                                        lag(aaa$epoch.sleep.chance, n = 2L) +
                                        lag(aaa$epoch.sleep.chance, n = 3L) +
                                        lag(aaa$epoch.sleep.chance, n = 4L) +
                                        aaa$epoch.sleep.chance
    )

    ## !!Problem with day 4 sleep.end might originate here, as sleep.end (08:53:00) ligt buiten range rownr.gotup (<08:00:00)??
    ## !! Adding "+24" to rownr.gotup is a work-around. (24 seems to be minimum to switch 05:54:00 to 08:53:00)??
    # aaa.sleeptime <- aaa[rownr.sleep.start:(rownr.gotup+24),] # This includes only the time in which the subject is in bed handpicked in this sample based on sleep.start (00:29) / got up (7:59) data
    aaa.sleeptime <- aaa[rownr.sleep.start:(rownr.gotup),]

    ## Now create a function which returns first $Time after certain time (lights out in sleep log)
    sleep.end. <- aaa.sleeptime[which(aaa.sleeptime$wakeup.chance == 2 & dplyr::lead(aaa.sleeptime$wakeup.chance > 2)),]
    ## First row now contains the start of sleep.
    sleep.end <- as.character(sleep.end.$Time[nrow(sleep.end.)])
    rownr.sleep.end <- which(aaa$Time == sleep.end)

    ## END OF Step 2: Calculate sleep for night1.------------------------------------------------------------------------

    ## Step 3: Calculate sleep analysis data.----------------------------------------------------------------------------
    aaa.assumedsleeptime <- aaa[rownr.sleep.start:(rownr.sleep.end - 1),] # Subframe the assumed sleep time, -1 is done otherwise it includes the first wake bout.
    TimeInBed <- (rownr.gotup - rownr.bedtime)/60 # The total elapsed time between the "Lights Out" and "Got Up" times
    AssumedSleep <- (rownr.sleep.end - rownr.sleep.start)/60 # The total elapsed time between the "Fell Asleep" and "Woke Up" times.
    WakeEpochs <- sum(aaa.assumedsleeptime$WakeSleep == 1) # Number of epochs scored as "awake"
    ActualSleep <- ((AssumedSleep * 60) - WakeEpochs)/60 # The total time spent in sleep according to the epoch-by-epoch wake/sleep scores.
    #ActualSleepPerc <- (ActualSleep / AssumedSleep)*100 # Actual sleep time expressed as a percentage of the assumed sleep time
    #
    #ActualWakeTime <- WakeEpochs/60 # Total time spent in wake according to the epoch-by-epoch wake/sleep scores.
    #ActualWakePerc <- 100 - ActualSleepPerc # Actual sleep time expressed as a percentage of the assumed sleep time.
    SleepEfficiency <- (ActualSleep/TimeInBed)*100 # Actual sleep time expressed as a percentage of time in bed.
    SleepLatency <- (rownr.sleep.start - rownr.bedtime)/60 # The time between "Lights Out" and "Fell Asleep"
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

    # print(aaa[1,])

    ## Step 4: Fill in the Sleep Overview

    # Attach Sleep Analysis output to overview
    sleepdata.overview[a,"date"] <- as.character(data.sleeplog[a, "Date"])
    sleepdata.overview[a,"sleep.start"] <- sleep.start
    sleepdata.overview[a,"sleep.end"] <- sleep.end
    sleepdata.overview[a,"sleep.efficiency"] <- SleepEfficiency
    sleepdata.overview[a,"sleep.latency"] <- SleepLatency

  }

  # Restore workdir
  setwd(oldworkdir)

  # Return the sleepdata overview
  sleepdata.overview
}

