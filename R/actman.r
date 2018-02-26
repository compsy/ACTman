#############################################################################
### ACTman package                                                        ###
### Script authors: Yoram Kunkels, Stefan Knapen, & Ando Emerencia        ###
### Most recent Update: 26-02-2018                                        ###
### Supported devices: Actiwatch 2 Respironics & MW8                      ###
###~@~@~@~@~@~@~@~@~@~@~@~@~@~@~@~@~@~@~@~@~@~@~@~@~@~@~@~@~@~@~@~@~@~@~@~###

#' ACTman - Actigraphy Manager
#'
#' ACTman manages actigraphy data whilst offering pre-processing and analyses options.
#' This initial version supports the 'Actiwatch 2' and 'MotionWatch 8' actigraphy devices,
#' whilst allowing for both sleep and circadian rhythm analyses.
#'
#' @param workdir The working directory of the script.
#' @param sleepdatadir An optional vector specifying the directory for actogram and sleep analysis data.
#' @param myACTdevice Name of the input device used. Should be either 'Actiwatch2' or 'MW8'.
#' @param iwantsleepanalysis  Boolean value indicating whether sleep analysis should be performed.
#' @param plotactogram Boolean value indicating whether an actogram has to be plotted.
#' @param selectperiod Boolean value indicating whether a specific period has to be selected.
#' @param startperiod An optional vector specifying single or multiple period starts. Should be in the format "2016-10-03 00:00:00".
#' @param daysperiod An optional vector specifying the length in days of the period.
#' @param movingwindow Boolean value indicating whether a moving window should be utilised.
#' @param movingwindow.size An optional vector specifying the length in days of the moving window. Default is 14 days.
#' @param circadian_analysis Boolean value indicating whether non-parametric circadian rhythm analysis should be performed.
#' @param na_omit Boolean value indicating whether NA's should be omitted.
#' @param missings_report Boolean value indicating whether missings promt should appear.
#'
#' @return if iwantsleepanalysis, this returns the sleepdata overview, else if movingwindow, it returns the moving window results, and otherwise it returns the actdata overview.
#' @examples
#' \dontrun{
#' View(ACTman::ACTman(workdir = "C:/Bibliotheek/Studie/PhD/Publishing/ACTman/R-part/mydata2",
#'                     myACTdevice = "Actiwatch2",
#'                     iwantsleepanalysis = FALSE,
#'                     plotactogram = FALSE,
#'                     selectperiod = FALSE,
#'                     startperiod = "2016-10-03 00:00:00",
#'                     daysperiod = 14, movingwindow = FALSE, circadian_analysis = TRUE))
#' }
ACTman <- function(workdir = "C:/Bibliotheek/Studie/PhD/Publishing/ACTman/R-part/mydata2",
                   sleepdatadir = "C:/Bibliotheek/Studie/PhD/Publishing/ACTman/R-part/Actogram & Sleep analysis",
                   myACTdevice = "Actiwatch2", iwantsleepanalysis = FALSE, plotactogram = FALSE,
                   selectperiod = FALSE, startperiod = NULL, daysperiod = FALSE, endperiod = NULL, movingwindow = FALSE, movingwindow.size = 14,
                   circadian_analysis = TRUE, nparACT_compare = FALSE, na_omit = TRUE,
                   missings_report = TRUE) {

  ## Step 1: Basic Operations:
  # Set current working directory and set back to old working directory on exit
  setwd(workdir)

  # List files and initiate overview file
  pattern_file <- ""
  if (iwantsleepanalysis) { # iwantsleepanalysis determines input .csv's because of added sleeplog .csv
    pattern_file <- ".csv"
    ACTdata.files <- sort(list.files(getwd(), pattern = pattern_file))
    if (any((grep(pattern = "sleeplog", x = ACTdata.files)))){
      ACTdata.files <- ACTdata.files[-(grep(pattern = "sleeplog", x = ACTdata.files))] # Remove any SLEEPLOG's from list if not needed
    }
  } else {
    pattern_file <- ".csv"
    ACTdata.files <- sort(list.files(getwd(), pattern = pattern_file))
    if (any((grep(pattern = "sleeplog", x = ACTdata.files)))){
      ACTdata.files <- ACTdata.files[-(grep(pattern = "sleeplog", x = ACTdata.files))] # Remove any SLEEPLOG's from list if not needed
    }
  }


  ACTdata.overview <- data.frame("filename" = ACTdata.files, "start" = NA, "end" = NA, "end2" = NA,
                                 "numberofobs" = NA, "numberofobs2" = NA, "recordingtime" = NA,
                                 "recordingtime2" = NA, "summertime.start" = NA, "summertime.end" = NA, "missings" = NA,
                                 "missings_perc" = NA, "IS" = NA, "IV" = NA, "RA" = NA, "L5" = NA, "L5_starttime" = NA, "M10" = NA,
                                 "M10_starttime" = NA, "r2.IS" = NA, "r2.IV" = NA, "r2.RA" = NA, "r2.L5" = NA,
                                 "r2.L5_starttime" = NA, "r2.M10" = NA, "r2.M10_starttime" = NA, "last5act.active" = NA)

  # Initiate loop parameters
  i <- 1 # set i
  secshour <- 60 * 60 # Seconds per hour
  secsday <- 24 * secshour # Seconds per day
  secs14day <- secsday * 14 # Seconds in 14 days
  minsaday <- (secsday / 60) # Minutes per day

  # Semantic checks
  if (selectperiod && length(startperiod) != length(ACTdata.files)) {
    stop(paste("The number of start periods does not match the number of data files found:", startperiod, length(ACTdata.files)))
  }

  ## END OF Step 1: Basic Operations.----------------------------------------------------------------------------

  ## Step 2: Loop:
  ## Loop Description: for each of the files listed in working directory, ...
  for (i in 1:length(ACTdata.files)) {

    # Test for user-defined arguments
    if (myACTdevice != "MW8" && myACTdevice != "Actiwatch2" ) {
      stop(paste("Unknown value for myACTdevice (should be Actiwatch2 or MW8):", myACTdevice))
    }

    print(paste("*** Start of Dataset", i, "***"))
    print("")
    print(paste("Dataset Name:", ACTdata.overview[i, "filename"]))
    print("")

    # Read and manage data:
    if (myACTdevice == "Actiwatch2") { # Device-specific Data Management

      ACTdata.1 <- read.csv(paste(ACTdata.files[i]), header = FALSE)
      ACTdata.1.sub <- ACTdata.1[, c(4, 5, 6)]
      colnames(ACTdata.1.sub) <- c("Date", "Time", "Activity")

    } else if (myACTdevice == "MW8") { # Device-specific Data Management

      ACTdata.1 <- read.csv(paste(ACTdata.files[i]), header = FALSE, fill = TRUE, stringsAsFactors = FALSE, col.names = c("A", "B", "C"))

      if (any(ACTdata.1[, 1] == "Raw data:")) {
        ACTdata.1 <- as.data.frame(ACTdata.1[((which(ACTdata.1[, 1] == "Raw data:")) + 2):nrow(ACTdata.1), ])
      } else {
        ACTdata.1 <- read.csv(paste(ACTdata.files[i]), header = TRUE, fill = TRUE, stringsAsFactors = FALSE, col.names = c("A", "B", "C"))
      }

      ACTdata.1.sub <- ACTdata.1
      colnames(ACTdata.1.sub) <- c("Date", "Time", "Activity")
      ACTdata.1.sub$Activity <- as.numeric(ACTdata.1.sub$Activity)

      # Test for 30 sec. bins!
      if (any(grepl(pattern = ":30", x = ACTdata.1$B[1:2]))) {
        print("Detecting Epoch Length.......")
        print("Warning: 30 sec. Epoch's Detected!")
        print("Action: Binning 30 sec. Epochs in 60 sec. Epochs")
        print("")



        ACTdata.TEMP <- ACTdata.1[(grepl(pattern = ":00", x = ACTdata.1$B)), ]
        #! Deze regel geeft een warning:
        #! In as.numeric(ACTdata.TEMP$C) + as.numeric(ACTdata.1[(grepl(pattern = ":30",  :
        #!   longer object length is not a multiple of shorter object length
        #! Dit komt omdat er 1 tijd meer is die eindigt op :00 dan op :30
        #! Maar volgens mij gaat het voor alle gevallen wel zoals je zou verwachten, en om alles
        #! om te schrijven is een boel werk en veel meer code. Ik zou het zo laten.
        #! >> Error doordat selecteren op ":00" niet alle 30 sec epochs verwijderd;
        #! >> e.g. "16:00:30" bevat zowel ":00" als ":30"!
        ACTdata.TEMP$C <- as.numeric(ACTdata.TEMP$C) + as.numeric(ACTdata.1[(grepl(pattern = ":30", x = ACTdata.1$B)), ]$C)

        #! Workaround for aforementioned issue
        ACTdata.TEMP <- ACTdata.TEMP[ - which(grepl("00:30", ACTdata.TEMP$B)), ]


        ACTdata.1.sub <- ACTdata.TEMP
        colnames(ACTdata.1.sub) <- c("Date", "Time", "Activity")
        ACTdata.1.sub$Activity <- as.numeric(ACTdata.1.sub$Activity)

        rm(ACTdata.TEMP)

      } else {
        print("Detecting Epoch Length.......")
        print("Normal 60 sec. Epochs detected")
        print("No changes made")
        print("")
      }

    }


    ## Step 2.1: Managing the Data
    # Reformat dates and times into required format for further processing:
    ACTdata.1.sub$Date <- gsub(pattern = "/20", replacement = "/", x = ACTdata.1.sub$Date) # Take only last two year digits
    ACTdata.1.sub$Date <- paste(ACTdata.1.sub$Date, ACTdata.1.sub$Time) # Merge Date Time

    if (grepl("-", ACTdata.1.sub$Date[1])) { # Reformat Date
      ACTdata.1.sub$Date <- strptime(ACTdata.1.sub$Date, "%Y-%m-%d %H:%M:%S")
    } else {
      ACTdata.1.sub$Date <- strptime(ACTdata.1.sub$Date, "%d/%m/%y %H:%M:%S")
    }


    ACTdata.1.sub$Time <- NULL # Remove empty Time variable

    # Check for empty first row and if so, remove it:
    if (all(is.na(ACTdata.1.sub[1, ]))) { ACTdata.1.sub <- ACTdata.1.sub[-1, ] } # If first row is empty, remove it.

    # Add Period Selection
    # Custom enddates should be added
    if (selectperiod) {
      startperiod.loc <- which(ACTdata.1.sub$Date == startperiod[i])

      if (daysperiod) {
        ACTdata.1.sub <- ACTdata.1.sub[(startperiod.loc:(startperiod.loc + (daysperiod*minsaday))), ]
      } else if (endperiod %in% ACTdata.1.sub$Date) {
        endperiod.loc <- which(ACTdata.1.sub$Date == endperiod)
        ACTdata.1.sub <- ACTdata.1.sub[(startperiod.loc:endperiod.loc), ]
      } else {
        ACTdata.1.sub <- ACTdata.1.sub[(startperiod.loc:(nrow(ACTdata.1.sub))), ]
      }
    }

    start_date <- ACTdata.1.sub$Date[1]
    end_date <- ACTdata.1.sub$Date[nrow(ACTdata.1.sub)]
    nr_obs <- nrow(ACTdata.1.sub)

    # Write start- and end dates and times of actigraph data to overview file:
    ACTdata.overview[i, "start"] <- as.character(start_date) # Write start date to overview
    ACTdata.overview[i, "end"] <- as.character(end_date) # Write end date to overview

    # Assign recordingtime as time difference between start and end date to overview file:
    ACTdata.overview[i, "recordingtime"] <- round((as.POSIXct(start_date) - as.POSIXct(end_date)), 2) # write recordingtime to overview

    # Write starting number of obs. to overview file:
    ACTdata.overview[i, "numberofobs"] <- nr_obs

    # Identify Last Whole 24 Hour component and its Position: (EDITED for NK_data)
    ACTdata.1.sub.lastwhole24h <- ACTdata.1.sub[tail(grep("00:00:00", ACTdata.1.sub$Date), 2), "Date"]
    ACTdata.1.sub.lastwhole24h <- ACTdata.1.sub.lastwhole24h[1]

    # Add 14 days in a way that respects daylight savings time changes:
    ACTdata.1.sub.14day <- increase_by_days(ACTdata.1.sub$Date[1], 14)


    ## Remove NA's
    print("Task 6: Reporting NA's")
    ACTdata.overview[i, "missings"] <- table(is.na(ACTdata.1.sub))["TRUE"]  # write missings to overview
    ACTdata.overview[i, "missings_perc"] <- round(ACTdata.overview[i, "missings"] / ACTdata.overview[i, "numberofobs"], 3) # write missings perc to overview
    if (na_omit) {
      ACTdata.1.sub <- na.omit(ACTdata.1.sub)
    }
    print(paste("Number of NA's in this Dataset:", ACTdata.overview[i, "missings"]))
    print(paste("This is:", round(ACTdata.overview[i, "missings"] / ACTdata.overview[i, "numberofobs"], 3), "% of the total number of observations!"))
    print("")

    # User-control over Analysis if too much Missings!

    #### Function below gave an error when there are NO missings (ACTdata.overview[i, "missings"] == NA), so made a function first to test this. If it is NA, than return 0, so the next test is FALSE.
    number_of_missings <- ifelse(is.na(ACTdata.overview[i, "missings"]), 0, ACTdata.overview[i, "missings"])

    if(missings_report){
      if ((number_of_missings / ACTdata.overview[i, "numberofobs"]) > 0.01){ # Gives error when there are NO missings.
        # if (winDialog(type = "yesno", message = "More than 0.01% of data is missing!\nAnalysis results might deviate from true values!\nDo you want to continue?") == "NO"){
        #   stop("Stopped by user!")
        # }

        message("\nMore than 0.01% of data is missing!\nAnalysis results might deviate from true values!")
        message("Do you want to continue?")
        missings_prompt_answer <- readline(prompt="Enter 'y' for Yes or 'n' for No:")

        if(missings_prompt_answer == "n"){
          stop("Stopped by user!")
        }
    }


    }

    # ## Set NA to 0
    # ACTdata.1.sub[is.na(ACTdata.1.sub)] <- 0


    # ## Impute Missings
    # library(mice); library(pscl); library(accelmissing)
    # tempData <- mice(matrix(data = c(ACTdata.1.sub$Activity, rep.int(x = 0, times = (ACTdata.overview[i, "numberofobs"]))), ncol = 2), m=5, maxit=50, meth='pmm', seed=500)
    # tempData2 <- complete(tempData, 1)
    # ACTdata.1.sub$Activity <- tempData2$V1


    ## If Activity in Last 5 observations is on average zero, Skip to Last Activity:
    ACTdata.1.sub.last5act <- ACTdata.1.sub$Activity[(nrow(ACTdata.1.sub) - 4):nrow(ACTdata.1.sub)] # Last 5 activity counts in dataset
    ACTdata.1.sub.last5act.active <- sum(ACTdata.1.sub.last5act, na.rm = T) >= (5 * length(ACTdata.1.sub.last5act)) # Is there on average more than 5 counts per obs?
    print("Task 7: Checking for Activity in Last 5 observations")
    if (ACTdata.1.sub.last5act.active == FALSE) {
      print("Warning: No Activity in Last 5 observations!")
      print("Last 5 Activity Counts, before Correction:")
      print(ACTdata.1.sub.last5act)
      ACTdata.1.sub <- ACTdata.1.sub[1:max(which(ACTdata.1.sub$Activity >= (5 * length(ACTdata.1.sub.last5act)))), ] # Shortens data untill reached last activity
      ACTdata.1.sub.last5act <- ACTdata.1.sub$Activity[(nrow(ACTdata.1.sub) - 4):nrow(ACTdata.1.sub)] # Last 5 activity counts in dataset
      ACTdata.overview$last5act.active[i] <- FALSE
      print("Last 5 Activity Counts, after Correction:")
      print(ACTdata.1.sub.last5act)
      print("Task 7 DONE: Dataset Skipped to last Activity.")
      print("")
    } else {
      print("Task 7 OK: Dataset contained Activity in Last 5 observations.")
      print("")
    }


    ## END OF Step 2.1: Managing the Data

    # Update overview file after NA- and non-activity removal
    ACTdata.overview[i, "numberofobs2"] <- nrow(ACTdata.1.sub)
    ACTdata.overview[i, "recordingtime2"] <- round(as.POSIXct(ACTdata.1.sub$Date[1]) - as.POSIXct(ACTdata.1.sub$Date[nrow(ACTdata.1.sub)]), 2)
    ACTdata.overview[i, "end2"] <- as.character(ACTdata.1.sub$Date[nrow(ACTdata.1.sub)]) # write end date to overview

    ## Use nparACT Package to calculate Experimental Variables
    ## Pre-process
    dir.create(file.path(workdir, "Managed Datasets"), showWarnings = FALSE)
    setwd(file.path(workdir, "Managed Datasets"))

    wd <- getwd()
    name <- paste(gsub(pattern = ".csv", replacement = "", x = ACTdata.files[i]))
    newdir <- paste(wd, name, sep = "/")
    dir.create(newdir, showWarnings = FALSE)
    setwd(newdir)

    write.table(ACTdata.1.sub, row.names = FALSE, col.names = FALSE,
                file = paste(gsub(pattern = ".csv", replacement = "", x = ACTdata.files[i]), "MANAGED.txt"))


    ## Calculate IS, etc. with Circadian Rhythm Variables (CRV) Calculation Module (nparcalc)

    # Read managed dataset for CRV analysis
    CRV.data <- read.table(file = file.path(newdir, paste(gsub(pattern = ".csv", replacement = "", x = ACTdata.files[i]), "MANAGED.txt")),
                           stringsAsFactors = FALSE)
    colnames(CRV.data) <- c("Date", "Time", "Activity")


    # Plot actogram
    if (plotactogram) {
      plot_actogram(workdir = workdir, ACTdata.1.sub = ACTdata.1.sub, i = i)
    }

    ## Moving Window
    if (movingwindow) {

      rollingwindow <- function(x, window){

        out <- data.frame()
        n <- nrow(x)
        rollingwindow.results <- as.data.frame(matrix(nrow = (floor(((n - window) / 1440))), ncol = 9))

        for (i in 1:(floor(((n - window) / 1440)))) {

          if (i == 1) {
            out <- x[i:window, ]
          } else {
            out <- x[((i - 1) * 1440):(((i - 1) * 1440) + window), ]
          }

          CRV.data <- out
          if (ncol(CRV.data) > 2) {
            colnames(CRV.data) <- c("Date", "Time", "Activity")
          } else if (ncol(CRV.data) == 2) {
            colnames(CRV.data) <- c("Date", "Activity")
          }

          r2 <- nparcalc(myACTdevice = myACTdevice, movingwindow = movingwindow, CRV.data = CRV.data, ACTdata.1.sub = ACTdata.1.sub, out = out)
          rollingwindow.results[i, 1] <- as.character(strftime(CRV.data[1, "Date"], format = "%Y-%m-%d %H:%M:%S"))
          rollingwindow.results[i, 2] <- as.character(strftime(CRV.data[nrow(CRV.data), "Date"], format = "%Y-%m-%d %H:%M:%S"))
          rollingwindow.results[i, 3] <- r2$IS
          rollingwindow.results[i, 4] <- r2$IV
          rollingwindow.results[i, 5] <- round(r2$RA, 2)
          rollingwindow.results[i, 6] <- round(r2$L5, 2)
          rollingwindow.results[i, 7] <- as.character(strftime(r2$L5_starttime, format = "%H:%M:%S"))
          rollingwindow.results[i, 8] <- round(r2$M10, 2)
          rollingwindow.results[i, 9] <- as.character(strftime(r2$M10_starttime, format = "%H:%M:%S"))
          colnames(rollingwindow.results) <- c("starttime", "endtime", "IS", "IV", "RA", "L5", "L5_starttime", "M10", "M10_starttime")

          print("---------------------------------------------------------------------------------")
          print(paste("Roling window CRV analysis output - Window step:", (i - 1)))

          print(paste("Begin time:", CRV.data[1, "Date"]))
          print(paste("End time:", CRV.data[nrow(CRV.data), "Date"]))

          print(paste("nOBS:", nrow(CRV.data)))
          print("")
          print(paste("IS: ", r2$IS))
          print(paste("IV: ", r2$IV))
          print(paste("RA: ", round(r2$RA, 2)))
          print(paste("L5: ", round(r2$L5, 2)))
          print(paste("L5_starttime: ", as.character(strftime(r2$L5_starttime, format = "%H:%M:%S"))))
          print(paste("M10: ", round(r2$M10, 2)))
          print(paste("M10_starttime: ", as.character(strftime(r2$M10_starttime, format = "%H:%M:%S"))))
          print("---------------------------------------------------------------------------------")

        }
        rollingwindow.results
      }

      rollingwindow.results <- rollingwindow(x = CRV.data, window = (1440 * (movingwindow.size)))

    } else {

      if (circadian_analysis){
        r2 <- nparcalc(myACTdevice = myACTdevice, movingwindow = movingwindow, CRV.data = CRV.data, ACTdata.1.sub = ACTdata.1.sub)

        # Attach r2 output to overview
        ACTdata.overview[i, "r2.IS"] <- r2$IS
        ACTdata.overview[i, "r2.IV"] <- r2$IV
        ACTdata.overview[i, "r2.RA"] <- round(r2$RA, 2)
        ACTdata.overview[i, "r2.L5"] <- round(r2$L5, 2)
        ACTdata.overview[i, "r2.L5_starttime"] <- as.character(strftime(r2$L5_starttime, format = "%H:%M:%S"))
        ACTdata.overview[i, "r2.M10"] <- round(r2$M10, 2)
        ACTdata.overview[i, "r2.M10_starttime"] <- as.character(strftime(r2$M10_starttime, format = "%H:%M:%S"))
      }
    }

    if (nparACT_compare) {
      # Use nparACT Package to calculate Experimental Variables
      # Calculate IS, etc. with nparACT
      r <- nparACT::nparACT_base_loop(path = newdir, SR = 1/60, fulldays = T, plot = F)

      # Attach nparACT output to overview
      ACTdata.overview[i, "IS"] <- r$IS
      ACTdata.overview[i, "IV"] <- r$IV
      ACTdata.overview[i, "RA"] <- r$RA
      ACTdata.overview[i, "L5"] <- r$L5
      ACTdata.overview[i, "L5_starttime"] <- r$L5_starttime
      ACTdata.overview[i, "M10"] <- r$M10
      ACTdata.overview[i, "M10_starttime"] <- r$M10_starttime
    }

    # Set wd back to main workdir
    setwd(workdir)


    ## Sleep Analysis Source
    ## Loop for sleep_calc
    ## (!!) "Sleep_calculation_functional v1" still has to be made dynamic, now only woks for NK_data (!!)
    if (iwantsleepanalysis) {
      sleepdata.overview <- sleepdata_overview(workdir = sleepdatadir, actdata = ACTdata.1.sub, i = i)
    }

    print(paste("--------------------------------------", "END OF DATASET", i, "---", "@", round(i * (100 / length(ACTdata.files))), "% DONE",  "--------------------------------------"))
  }

  ## END OF Step 2: Loop.----------------------------------------------------------------------------------------

  ## Step 3: After loop processing:
  # Transform negative recordingtime to positive
  ACTdata.overview$recordingtime <- ((ACTdata.overview$recordingtime) ^ 2) ^ (1 / 2)
  # Assign zero to missings without missings
  ACTdata.overview[is.na(ACTdata.overview[, "missings"]), "missings"] <- 0
  # Subset experimental variables
  ACTdata.1.sub.expvars <- ACTdata.overview[c("IS", "IV", "RA", "L5", "L5_starttime", "M10", "M10_starttime")]
  colnames(ACTdata.1.sub.expvars) <- c("IS", "IV", "RA", "L5", "L5 Start time", "M10", "M10 Start time")
  # Update overview, depending on nparACT_compare
  if (!nparACT_compare) {
    ACTdata.overview["IS"] <- NULL
    ACTdata.overview["IV"] <- NULL
    ACTdata.overview["RA"] <- NULL
    ACTdata.overview["L5"] <- NULL
    ACTdata.overview["L5_starttime"] <- NULL
    ACTdata.overview["M10"] <- NULL
    ACTdata.overview["M10_starttime"] <- NULL

    # Rename Circdin variables if not nparACT_compare
    colnames(ACTdata.overview) <- gsub(pattern = "r2.", x = colnames(ACTdata.overview), replacement = "")
  }

  # Export Experimental variables to .pdf
  pdf("Table - Experimental Variables.pdf")
  gridExtra::grid.table(ACTdata.1.sub.expvars)
  dev.off()

  # Export ACTdata.overview to .pdf
  pdf("Table - ACTdata.overview.pdf")
  gridExtra::grid.table(ACTdata.overview)
  dev.off()

  # Returned result.
  if (iwantsleepanalysis) {
    View(sleepdata.overview)
  } else if (movingwindow) {
    rollingwindow.results
  } else {
    View(ACTdata.overview)
  }
}

