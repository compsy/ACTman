#' Loop script - Actiwatch 2 Respironics
#'
#' This script loops and does some stuff.
#'
#' @param workdir The working directory of the script.
#' @return nothing
#' @examples
#' \dontrun{
#' actman("C:/Users/Y. K. Kunkels/Desktop/loopscript/datatest2") #eee
#' }
#' @export
actman <- function(workdir) {
  setwd(workdir)
  # List files and initiate overview file
  ACTdata.files <- list.files(setwd(workdir), pattern = ".csv")
  ACTdata.overview <- data.frame("filename" = ACTdata.files, "start" = NA, "end" = NA, "end2" = NA , "end3" = NA,
                                 "numberofobs" = NA, "numberofobs2" = NA, "numberofobs3" = NA, "recordingtime" = NA,
                                 "recordingtime2" = NA, "recordingtime3" = NA, "summertime.start" = NA,
                                 "summertime.end" = NA, "missings" = NA, "IS" = NA, "IV" = NA, "RA" = NA, "L5" = NA,
                                 "L5_starttime" = NA, "M10" = NA, "M10_starttime" = NA, "lengthcheck" = NA,
                                 "last5act.active" = NA)
  # Initiate loop parameters
  i <- 1 # set i
  secshour <- 60*60 # Seconds per hour #ECHTWAAR
  secsday <- 24*secshour # Seconds per day
  secs14day <- secsday*14 # Seconds in 14 days

  ## END OF Step 1: Basic Operations.----------------------------------------------------------------------------

  ## Step 2: Loop:
  ## Loop Description: for each of the files listed in working directory, ...
  pdf("Actigraphy Data - Plot.pdf") # Initialise .pdf plotting
  for (i in 1:length(ACTdata.files)) {

    print(paste("*** Start of Dataset", i, "***"))
    print("")
    print(paste("Dataset Name:", ACTdata.overview[i, "filename"]))
    print("")

    # Read and manage data:
    ACTdata.1 <- read.csv(paste(ACTdata.files[i]), header = FALSE)
    ACTdata.1.sub <- ACTdata.1[, c(4, 5, 6)]
    colnames(ACTdata.1.sub) <- c("Date", "Time", "Activity")

    # Reformat dates and times into required format for further processing:
    ACTdata.1.sub$Date <- gsub(pattern = "/20", replacement = "/", x = ACTdata.1.sub$Date) # Take only last two year digits
    ACTdata.1.sub$Date <- paste(ACTdata.1.sub$Date, ACTdata.1.sub$Time) # Merge Date Time, format: "2016-01-04 20:00:00"
    ACTdata.1.sub$Date <- strptime(ACTdata.1.sub$Date, "%d/%m/%y %H:%M:%S") # Reformat Date
    ACTdata.1.sub$Time <- NULL # Remove empty Time variable

    # Check for empty first row and if so, remove it:
    if (all(is.na(ACTdata.1.sub[1, ]))) { ACTdata.1.sub <- ACTdata.1.sub[-1, ] } # If first row is empty, remove it.

    # Write start- and end dates and times of actigraph data to overview file:
    ACTdata.overview[i, "start"] <- as.character(ACTdata.1.sub$Date[1]) # Write start date to overview
    ACTdata.overview[i, "end"] <- as.character(ACTdata.1.sub$Date[nrow(ACTdata.1.sub)]) # Write end date to overview

    # Assign recordingtime as time difference between start and end date to overview file:
    ACTdata.overview[i, "recordingtime"] <- round(ACTdata.1.sub$Date[1] - ACTdata.1.sub$Date[nrow(ACTdata.1.sub)], 2) # write recordingtime to overview

    # Write starting number of obs. to overview file:
    ACTdata.overview[i, "numberofobs"] <- nrow(ACTdata.1.sub)

    # Check for Activity in Last 5 observations:
    ACTdata.1.sub.last <- ACTdata.1.sub$Activity[(nrow(ACTdata.1.sub) - 4):nrow(ACTdata.1.sub)]

    # Check if Activity in Last 5 observations contain substantial activity (on average, 5 activity counts per obs.)
    ACTdata.1.sub.done <- sum(ACTdata.1.sub.last) > 5*length(ACTdata.1.sub.last)

    # Identify Last Whole 24 Hour component and its Position:
    ACTdata.1.sub.lastwhole24h <- as.POSIXct(ACTdata.overview[i, "start"]) + secs14day - secsday # Last whole 24 hour component
    ACTdata.1.sub.lastwhole24h.pos <- which(ACTdata.1.sub$Date == ACTdata.1.sub.lastwhole24h) # Position of Last whole 24 hour component

    # Summertime start and end Detection Parameters
    summertime.start.2015 <- "2015-03-29 02:00:00" # Assign summertime start date for 2015
    summertime.start.2014 <- "2014-03-30 02:00:00" # Assign summertime start date for 2014
    summertime.start.indata <- summertime.start.2014 %in% ACTdata.1.sub$Date | summertime.start.2015 %in% ACTdata.1.sub$Date # Detect if summertime start is in dataset
    # summertime.start.pos <- which(ACTdata.1.sub$Date == summertime.start) # Position of summertime start
    summertime.end <- "2015-10-25 02:00:00" # Assign summertime end date for 2015
    summertime.end.indata <- summertime.end %in% ACTdata.1.sub$Date # Detect if summertime end is in dataset
    # summertime.end.pos <- which(ACTdata.1.sub$Date == summertime.end) # Position of summertime end

    ## Summertime start Detection and 14day length correction
    print("Task 1: Summertime Start Detection and Correction")
    if (summertime.start.indata == TRUE) {
      print("Warning: Start of Summertime Detected!")
      ACTdata.1.sub.14day <- ACTdata.1.sub$Date[1] + secs14day # Start Date plus 14 days
      ACTdata.1.sub.14day <- ACTdata.1.sub.14day - secshour
      ACTdata.overview$summertime.start[i] <- TRUE
      print("Action: Subtracted the one hour excess from 14day length.")
      print("Task 1 DONE: Dataset corrected for Summertime.")
      print("")
    } else {
      ACTdata.1.sub.14day <- ACTdata.1.sub$Date[1] + secs14day # Start Date plus 14 days
      print("Task 1 OK: No Start of Summertime in Dataset.")
      print("")
    }

    ## If Dataset is longer than Start Date plus 14 days, Remove data recorded thereafter:
    print("Task 2: Detecting if Dataset is longer than Start Date plus 14 full days")
    if (ACTdata.1.sub[nrow(ACTdata.1.sub), "Date"] > ACTdata.1.sub.14day) {
      print("Warning: Dataset is longer than Start Date plus 14 full days!")
      ACTdata.1.sub <- ACTdata.1.sub[1:which(ACTdata.1.sub$Date == ACTdata.1.sub.14day), ]
      print("Action: Observations recorded after Start Date plus 14 full days were removed.")
      print("Task 2 DONE: Dataset shortened to Start Date plus 14 days. Tasks 3, 4, and 5 also OK")
      print("")
      ACTdata.overview$lengthcheck[i] <- TRUE
    } else {
      print("Task 2 OK: Dataset not longer than Start Date plus 14 days.")
      print("")
    }


    ## If Dataset is longer than Start Date plus 14 days minus one hour, Remove data recorded thereafter:
    print("Task 3: Detecting if Dataset is longer than Start Date plus 14 full days minus 1 hour")
    if (ACTdata.1.sub[nrow(ACTdata.1.sub), "Date"] > (ACTdata.1.sub.14day - secshour) &
      ((ACTdata.1.sub[nrow(ACTdata.1.sub), "Date"] - ACTdata.1.sub.14day) != 0)) {
      print("Warning: Data longer than Start Date plus 14 full days minus 1 hour!")
      ACTdata.1.sub <- ACTdata.1.sub[1:which(ACTdata.1.sub$Date == (ACTdata.1.sub.14day - secshour)), ]
      print("Action: Observations recorded after Start Date plus 14 full days minus 1 hour were removed.")
      print("Task 3 DONE: Dataset shortened to Start Date plus 14 days minus 1 hour. Tasks 4 and 5 also OK")
      print("")
      ACTdata.overview$lengthcheck[i] <- TRUE
    } else {
      print("Task 3 OK: Dataset not longer than Start Date plus 14 days minus 1 hour.")
      print("")
    }

    ## If data longer than Start Date plus 14 days minus Two hours, Remove data recorded thereafter:
    print("Task 4: Detecting if Dataset is longer than Start Date plus 14 full days minus 2 hours")
    if ((is.na(ACTdata.overview$lengthcheck[i])) &
       ACTdata.1.sub[nrow(ACTdata.1.sub), "Date"] > (ACTdata.1.sub.14day - (secshour*2))) {
      print("Warning: Data longer than Start Date plus 14 full days minus 2 hours!")
      ACTdata.1.sub <- ACTdata.1.sub[1:which(ACTdata.1.sub$Date == (ACTdata.1.sub.14day - (secshour*2))), ]
      print("Action: Observations recorded after Start Date plus 14 full days were removed.")
      print("Task 4 DONE: Dataset shortened to Start Date plus 14 days minus 2 hours. Task 5 also OK")
      print("")
      ACTdata.overview$lengthcheck[i] <- TRUE
    } else {
      print("Task 4 OK: Dataset not longer than Start Date plus 14 days minus 2 hours.")
      print("")
    }

    ## If data shorter than- or equal to Start Date plus 14 days minus Two hours, Skip to last Midnight:
    ## !! Must be skip to previous whole 24h !!
    print("Task 5: Detecting if Dataset is shorter than- or equal to Start Date plus 14 days minus Two hours")
    if ((is.na(ACTdata.overview$lengthcheck[i]))) {
      print("Warning: Data shorter than- or equal to Start Date plus 14 days minus Two hours!")
      ACTdata.1.sub <- ACTdata.1.sub[1:ACTdata.1.sub.lastwhole24h.pos, ]
      print("Action: Skipping to last whole 24h component")
      print("Task 5 DONE: Dataset Skipped to last whole 24h component")
      print("")
    } else {
      print("Task 5 OK: Dataset not shorter than- or equal to Start Date plus 14 days minus Two hours.")
      print("")
    }


      # Update overview after removal
    ACTdata.overview[i, "numberofobs2"] <- nrow(ACTdata.1.sub)
    ACTdata.overview[i, "recordingtime2"] <- round(ACTdata.1.sub$Date[1] - ACTdata.1.sub$Date[nrow(ACTdata.1.sub)], 2)
    ACTdata.overview[i, "end2"] <- as.character(ACTdata.1.sub$Date[nrow(ACTdata.1.sub)]) # write end date to overview

    ## Remove NA's
    print("Task 6: Reporting NA's")
    ACTdata.overview[i, "missings"] <- table(is.na(ACTdata.1.sub))["TRUE"]  # write missings to overview
    ACTdata.1.sub <- na.omit(ACTdata.1.sub)
    print(paste("Number of NA's in this Dataset:", ACTdata.overview[i, "missings"]))
    print("")

    ## If Activity in Last 5 observations is on average zero, Skip to Last Activity:
    ACTdata.1.sub.last5act <- ACTdata.1.sub$Activity[(nrow(ACTdata.1.sub) - 4):nrow(ACTdata.1.sub)] # Last 5 activity counts in dataset
    ACTdata.1.sub.last5act.active <- sum(ACTdata.1.sub.last5act) >= (5*length(ACTdata.1.sub.last5act)) # Is there on average more than 5 counts per obs?
    print("Task 7: Checking for Activity in Last 5 observations")
    if (ACTdata.1.sub.last5act.active == FALSE) {
      print("Warning: No Activity in Last 5 observations!")
      print("Last 5 Activity Counts, before Correction:")
      print(ACTdata.1.sub.last5act)
      ACTdata.1.sub <- ACTdata.1.sub[1:max(which(ACTdata.1.sub$Activity >= (5*length(ACTdata.1.sub.last5act)))), ] # Shortens data untill reached last activity
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

    # Update overview file after NA- and non-activity removal
    ACTdata.overview[i, "numberofobs3"] <- nrow(ACTdata.1.sub)
    ACTdata.overview[i, "recordingtime3"] <- round(ACTdata.1.sub$Date[1] - ACTdata.1.sub$Date[nrow(ACTdata.1.sub)], 2)
    ACTdata.overview[i, "end3"] <- as.character(ACTdata.1.sub$Date[nrow(ACTdata.1.sub)]) # write end date to overview

    ## Use nparACT Package to calculate Experimental Variables
    wd <- getwd()
    name <- paste(gsub(pattern = ".csv", replacement = "", x = ACTdata.files[i]), "- Cleaned Data")
    newdir <- paste(wd, name, sep = "/")
    dir.create(newdir)
    setwd(newdir)
    write.table(ACTdata.1.sub, row.names = FALSE, col.names = FALSE,
                file = paste(gsub(pattern = ".csv", replacement = "", x = ACTdata.files[i]), "NEW!!!!.txt"))
    r <- nparACT_base_loop(newdir, SR = 1/60, fulldays = T, plot = F)
    # Attach nparACT output to overview
    ACTdata.overview[i, "IS"] <- r$IS
    ACTdata.overview[i, "IV"] <- r$IV
    ACTdata.overview[i, "RA"] <- r$RA
    ACTdata.overview[i, "L5"] <- r$L5
    ACTdata.overview[i, "L5_starttime"] <- r$L5_starttime
    ACTdata.overview[i, "M10"] <- r$M10
    ACTdata.overview[i, "M10_starttime"] <- r$M10_starttime
    setwd(workdir) # Reset working directory to main directory

    # Plot data
    plot(x = ACTdata.1.sub$Date, y = ACTdata.1.sub$Activity, type = "l", col = "blue",
         xlab = "Date", ylab = "Activity", main = paste("Activity Data - Participant", i))

    print(paste("--------------------------------------", "END OF DATASET", i, "---", "@", round(i*(100/length(ACTdata.files))), "% DONE",  "--------------------------------------"))
  }
  dev.off()

  ## END OF Step 2: Loop.----------------------------------------------------------------------------------------

  ## Step 3: After loop processing:
  # Transform negative recordingtime to positive
  ACTdata.overview$recordingtime <- ((ACTdata.overview$recordingtime)^2)^(1/2)
  ACTdata.overview$recordingtime2 <- ((ACTdata.overview$recordingtime2)^2)^(1/2)
  # Assign zero to missings without missings
  ACTdata.overview[is.na(ACTdata.overview[, "missings"]), "missings"] <- 0
  # Subset experimental variables
  ACTdata.1.sub.expvars <- ACTdata.overview[c("IS", "IV", "RA", "L5", "L5_starttime", "M10", "M10_starttime", "recordingtime2")]
  colnames(ACTdata.1.sub.expvars) <- c("IS", "IV", "RA", "L5", "L5 Start time", "M10", "M10 Start time", "No. of Days")

  # Export Experimental variables to .pdf
  pdf("Experimental Variables - Table.pdf")
  grid.table(ACTdata.1.sub.expvars)
  dev.off()

  View(ACTdata.overview)

  ## END OF Step 3: After loop processing.------------------------------------------------------------------------
}
