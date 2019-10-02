#############################################################################
### ACTman package                                                        ###
### Script authors: Yoram Kunkels, Stefan Knapen, & Ando Emerencia        ###
### Most recent Update: 16-04-2018                                        ###
### Supported devices: Actiwatch 2 Respironics & MW8                      ###
###=======================================================================###
### Revision History:                                                     ###
### 16-04-2018: Added Actogram Functionality.                             ###
###~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~###

#' ACTman - Actigraphy Manager
#'
#' ACTman manages actigraphy data whilst offering pre-processing and analyses options.
#' This initial version supports the 'Actiwatch 2' and 'MotionWatch 8' actigraphy devices,
#' whilst allowing for both sleep and circadian rhythm analyses.
#'
#' @param workdir The working directory of the script.
#' @param sleepdatadir An optional vector specifying the directory for actogram and sleep analysis data.
#' @param myACTdevice Name of the input device used. Should be either 'Actiwatch2' or 'MW8'.
#' @param iwantsleepanalysis Boolean value indicating whether sleep analysis should be performed.
#' @param plotactogram Value indicating if and what kind of actogram has to be plotted. Can be either '48h', '24h', or FALSE.
#' @param selectperiod Boolean value indicating whether a specific period has to be selected.
#' @param startperiod An optional vector specifying single or multiple period starts. Should be in the format "2016-10-03 00:00:00".
#' @param daysperiod An optional vector specifying the length in days of the period.
#' @param endperiod An optional argument that is a date string (format: "2016-10-03 00:00:00"), denoting the end of the data subset to be analyzed. Only used if daysperiod is not specified.
#' @param movingwindow Boolean value indicating whether a moving window should be utilised.
#' @param movingwindow.size An optional vector specifying the length in days of the moving window. Default is 14 days.
#' @param movingwindow.jump An optional vector specifying the length of the jumps with which the moving window is shifted each iteration. Default is 1 day.
#' @param circadian_analysis Boolean value indicating whether non-parametric circadian rhythm analysis should be performed.
#' @param nparACT_compare Boolean value indicating that comparison with another actigraphy R package should be performed. If TRUE, the values for IS, IV, RA, L5, L5_starttime, M10, and M10_starttime of the nparACT_base_loop function are recorded in the returned overview variable.
#' @param na_omit Boolean value indicating whether NA's should be omitted.
#' @param na_impute Boolean value indicating whether NA's should be imputed.
#' @param missings_report Boolean value indicating whether missings promt should appear.
#' @param lengthcheck Boolean value. If TRUE, the dataset is shortened to the start date plus 14 days, and observations more than 14 days after the start date are removed.
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
#' @importFrom stats na.omit
#' @importFrom utils View
#' @importFrom utils read.csv
#' @importFrom utils read.table
#' @importFrom utils tail
#' @importFrom utils write.table
#'
#' @export
ACTman <- function(workdir = "C:/mydata",
                   sleepdatadir = paste("C:/Bibliotheek/Studie/PhD/Publishing/",
                                        "ACTman/R-part/Actogram & Sleep analysis", sep = ""),
                   myACTdevice = "Actiwatch2", iwantsleepanalysis = FALSE, plotactogram = FALSE,
                   selectperiod = FALSE, startperiod = NULL, daysperiod = FALSE, endperiod = NULL,
                   movingwindow = FALSE, movingwindow.size = 14, movingwindow.jump = 1,
                   circadian_analysis = TRUE, nparACT_compare = FALSE, na_omit = FALSE, na_impute = FALSE,
                   missings_report = TRUE, lengthcheck = TRUE, i_want_EWS = FALSE) {

  ## Step 1: Basic Operations-----------------------------------------------------------------

  ## Set working directory:
  setwd(workdir)

  ## List actigraphy files and seperate out sleeplogs:
  pattern_file <- ".csv"
  ACTdata.files <- sort(list.files(getwd(), pattern = pattern_file))
  if (any((grep(pattern = "sleeplog", x = ACTdata.files)))) {
    ACTdata.files <- ACTdata.files[-(grep(pattern = "sleeplog", x = ACTdata.files))] # Remove any sleeplogs from data listing
  }
  if (any((grep(pattern = "markers", x = ACTdata.files)))) {
    ACTdata.files <- ACTdata.files[-(grep(pattern = "markers", x = ACTdata.files))] # Remove any markers files from data listing
  }

  ## Initialise overview:
  ACTdata.overview <- data.frame("filename" = ACTdata.files, "start" = NA, "end" = NA, "end2" = NA,
                                 "numberofobs" = NA, "numberofobs2" = NA, "recordingtime" = NA,
                                 "recordingtime2" = NA, "summertime.start" = NA, "summertime.end" = NA, "missings" = NA,
                                 "missings_perc" = NA, "IS" = NA, "IV" = NA, "RA" = NA, "L5" = NA, "L5_starttime" = NA, "M10" = NA,
                                 "M10_starttime" = NA, "r2.IS" = NA, "r2.IV" = NA, "r2.RA" = NA, "r2.L5" = NA,
                                 "r2.L5_starttime" = NA, "r2.M10" = NA, "r2.M10_starttime" = NA, "last5act.active" = NA,
                                 "lengthcheck" = NA)

  ## Initiate parameters:
  i <- 1 # set i
  secshour <- 60 * 60 # Seconds per hour.
  secsday <- 24 * secshour # Seconds per day.
  secs14day <- secsday * 14 # Seconds in 14 days.
  minsaday <- (secsday / 60) # Minutes per day.

  ## Semantic checks:
  if (selectperiod && length(startperiod) != length(ACTdata.files)) {
    stop(paste("The number of start periods does not match the number of data files found:", startperiod, length(ACTdata.files)))
  }

  if (".mtn" %in% substr(list.files(getwd()), nchar(list.files(getwd())) - 4 + 1, nchar(list.files(getwd())))) {
    message(paste("There is at least 1 unsupported Actigraphy file format present in the working directory!
                  Please convert, stash, or remove these unsupported files before rerunning:"))
    print(list.files(getwd())[grep(".mtn", list.files(getwd()))])
    stop()
  }


  ## Step 2: Main ACTman Loop------------------------------------------------------------------------
  ## Description: ...

  for (i in 1:length(ACTdata.files)) {

    ## Test for user-defined arguments:
    if (myACTdevice != "MW8" && myACTdevice != "Actiwatch2" && myACTdevice != "Actical") {
      stop(paste("Unknown value for myACTdevice (should be MW8, Actiwatch2, or Actical):", myACTdevice))
    }

    print(paste("*** Start of Dataset", i, "***"))
    print("")
    print(paste("Dataset Name:", ACTdata.overview[i, "filename"]))
    print("")


    ## Step 2.1.: Reading Data-----------------------------------------------------------------------

    ## Reading in .CSV data, plus some device-specific Data Management:
    ## Device-specific Data Management is required as raw data format differs between
    ## devices, in e.g., headers and raw data locations.
    if (myACTdevice == "Actiwatch2") {

      ACTdata.1 <- read.csv(paste(ACTdata.files[i]), header = FALSE)
      ACTdata.1.sub <- ACTdata.1[, c(4, 5, 6)]
      colnames(ACTdata.1.sub) <- c("Date", "Time", "Activity")

    } else if (myACTdevice == "MW8") {

      ACTdata.1 <- read.csv(paste(ACTdata.files[i]), header = FALSE, fill = TRUE, stringsAsFactors = FALSE, col.names = c("A", "B", "C"))

      if (all(is.na(ACTdata.1$B)) && all(is.na(ACTdata.1$C))) { ## Note! If TRUE ACTdata.1 is (suddenly) tab-seperated!

        ACTdata.1 <- read.csv(paste(ACTdata.files[i]), header = FALSE, fill = TRUE, stringsAsFactors = FALSE, col.names = c("A", "B", "C"), sep = "\t")

      }

      if (any(ACTdata.1[, 1] == "Raw data:")) {
        ACTdata.1 <- as.data.frame(ACTdata.1[((which(ACTdata.1[, 1] == "Raw data:")) + 2):nrow(ACTdata.1), ])
      } else {
        ACTdata.1 <- read.csv(paste(ACTdata.files[i]), header = TRUE, fill = TRUE, stringsAsFactors = FALSE, col.names = c("A", "B", "C"))
      }

    } else if (myACTdevice == "Actical"){


      #! Check if Actical software saved csv files with EITHER ","-seperators OR ";"-seperator (???)
      substrRight <- function(x, n){
        substr(x, nchar(x)-n+1, nchar(x))
      }

      Actical_seperator <- substrRight(x = (as.character(
        # read.csv(ACTdata.files[i])[1,1]
        read.csv(ACTdata.files[i], sep = ",", col.names = paste0(rep("V", 53), 1:53), header = F)[1,1]
        )), n = 1)


      if (Actical_seperator == ";"){

        ## Read in data
        ACTdata.1 <- read.csv(ACTdata.files[i], sep = ";")

      } else if (Actical_seperator == ")"){

        ## Read in data
        ACTdata.1 <- read.csv(ACTdata.files[i], sep = ",", col.names = paste0(rep("V", 53), 1:53), header = F)

      } else {

        warning("Actical data seperator not recognized, should be either ';' or ','\nStopping process...")
        stop()

        }




      ## Start of colnames before raw data (this row contains 'Epoch#')
      colnames_startrow <- (which(ACTdata.1 == "Epoch#", arr.ind=TRUE)[1])

      ## which row precedes raw data (row before this contains '(4=vigorous)')?
      data_startrow <- (which(ACTdata.1 == "(4=vigorous)", arr.ind=TRUE)[1] + 1)

      ## Get last column of colnames
      data_colnames_lastcol <- (which(ACTdata.1 == "Event Marker", arr.ind=TRUE)[2])


      # ## Make exception for when Actical csv file suddenly decides to use other seperator (!)
      # if (is.na(colnames_startrow)){
      #
      #   ## Start of colnames before raw data
      #   colnames_startrow <- ((which((ACTdata.1 == "------------------- Epoch-by-Epoch Data --------------------"), arr.ind=TRUE)[1]) + 1)
      #
      #   ## which row precedes raw data
      #   data_startrow <- (which(ACTdata.1 == ",,,,,,,,(4=vigorous)", arr.ind=TRUE)[1] + 1)
      #
      #   # ## Get last column of colnames
      #   # data_colnames_lastcol <- (which(ACTdata.1 == "Event Marker", arr.ind=TRUE)[2])
      #
      # }



      # Extract colnames
      data_colnames <- as.matrix(ACTdata.1[(colnames_startrow:(data_startrow - 1)), 1:data_colnames_lastcol])


      ## Loop for collapsing and assigning columnnames
      data_colnames_TEMP <- matrix(NA, nrow = 1, ncol = ncol(data_colnames))

      for(col_count in 1:ncol(data_colnames)){

        data_colnames_TEMP[, col_count] <- paste(data_colnames[, col_count], collapse = "")

      }
      data_colnames <- data_colnames_TEMP
      rm(data_colnames_TEMP)


      ## Remove header
      ACTdata.1 <- ACTdata.1[(data_startrow:nrow(ACTdata.1)), ]


      ## Select only required columns
      ACTdata.1 <- ACTdata.1[, (1:ncol(data_colnames))]


      ## Add column names
      colnames(ACTdata.1) <- data_colnames


      ## Select only columns for Date, Time, and Activity counts
      ACTdata.1 <- ACTdata.1[, c("Date", "Time", "ActivityCounts")]


      ## Add missing ":00" to time variable
      ACTdata.1$Time <- paste0(ACTdata.1$Time, ":00")


      # ## Column bind Date and Time into DateTime
      # TEST_TEMP <- as.data.frame(ACTdata.1)
      # TEST_TEMP$DateTime <- paste(TEST_TEMP$Date, TEST_TEMP$Time)
      # TEST_TEMP <- TEST_TEMP[, c("DateTime", "ActivityCounts")]

      ## Make matrix
      # ACTdata.1 <- as.matrix(TEST_TEMP)
      # ACTdata.1 <- as.matrix(ACTdata.1)

    }


      ## Make a copy of the original data to work on:
      ACTdata.1.sub <- ACTdata.1
      colnames(ACTdata.1.sub) <- c("Date", "Time", "Activity")
      ACTdata.1.sub$Activity <- as.numeric(levels(ACTdata.1.sub$Activity))[ACTdata.1.sub$Activity]

      #! as.numeric on factors is meaningless! Us on levels as done above.
      # ACTdata.1.sub$Activity <- as.numeric(ACTdata.1.sub$Activity)

      ## Test for 30 sec. bins:
      if (any(grepl(pattern = ":30", x = ACTdata.1$B[1:2]))) {
        print("Detecting Epoch Length.......")
        print("Warning: 30 sec. Epoch's Detected!")
        print("Action: Binning 30 sec. Epochs in 60 sec. Epochs")
        print("")

        ## If epochs are 30 sec. instead of 60 sec., bin them together to form 60 sec. epochs.
        ACTdata.TEMP <- ACTdata.1[(grepl(pattern = ".*(?<!:30)$", x = ACTdata.1$B, perl = TRUE)), ]

        halfminute_data <- as.numeric(ACTdata.1[(grepl(pattern = ".*(?<=:30)$", x = ACTdata.1$B, perl = TRUE)), ]$C)
        if (length(grep(pattern = ":30$", x = ACTdata.1$B[1]))) {
          # if it starts wtih :30, throw first val away
          halfminute_data <- tail(halfminute_data, n = -1)
        }
        if (length(grep(pattern = ":00$", x = ACTdata.1$B[length(ACTdata.1$B)]))) {
          # if it ends on a full minute, add a zero value
          halfminute_data <- c(halfminute_data, 0)
        }
        ACTdata.TEMP$C <- as.numeric(ACTdata.TEMP$C) + halfminute_data
        # #! Workaround for aforementioned issue
        # ACTdata.TEMP <- ACTdata.TEMP[ - which(grepl("00:30", ACTdata.TEMP$B)), ]

        ## Write binned data in ACTdata.TEMP to workable data object ACTdata.1.sub:
        ACTdata.1.sub <- ACTdata.TEMP
        colnames(ACTdata.1.sub) <- c("Date", "Time", "Activity")
        ACTdata.1.sub$Activity <- as.numeric(ACTdata.1.sub$Activity)
        rm(ACTdata.TEMP) # Remove temporary data object
        rm(halfminute_data)

      } else {
        ## Make no changes if 60 sec. bins
        print("Detecting Epoch Length.......")
        print("Normal 60 sec. Epochs detected")
        print("No changes made")
        print("")
      }


    ## Step 2.2: Managing the Data---------------------------------------------------------------

    ## Reformat dates and times into required format for further processing:
    ACTdata.1.sub$Date <- gsub(pattern = "/20", replacement = "/", x = ACTdata.1.sub$Date) # Take only last two year digits
    ACTdata.1.sub$Date <- paste(ACTdata.1.sub$Date, ACTdata.1.sub$Time) # Merge Date Time

    if (myACTdevice == "Actical"){

      month_char1 <- substr(ACTdata.1.sub$Date, 4, 6)
      month_char2 <- paste(toupper(substr(month_char1, 1, 1)), substr(month_char1, 2, nchar(month_char1)), sep="") # month jan, feb, etc to numeric


      #! Exceptions for when month is specified with non-English abbreviations
      # if (month_char2[1] == "Mrt"){month_char2 <- rep("Mar", length(month_char2))}
      # if (month_char2[1] == "Okt"){month_char2 <- rep("Oct", length(month_char2))}

      if ("Mrt" %in% month_char2){month_char2[which(month_char2 == "Mrt")] <- "Mar"}
      if ("Mei" %in% month_char2){month_char2[which(month_char2 == "Mei")] <- "May"}
      if ("Okt" %in% month_char2){month_char2[which(month_char2 == "Okt")] <- "Oct"}


      if (match(month_char2, month.abb)[1] < 10){

        month_char3 <- paste0(0, match(month_char2, month.abb))

      } else {month_char3 <- match(month_char2, month.abb)}




      ACTdata.1.sub$Date <- paste0(substr(ACTdata.1.sub$Date, 1, 3), month_char3, substr(ACTdata.1.sub$Date, 7, nrow(ACTdata.1.sub)))

      time_ind <- which(nchar(substr(ACTdata.1.sub$Date, 10, nrow(ACTdata.1.sub))) < 5)

      ACTdata.1.sub$Date[time_ind] <- paste0(substr(ACTdata.1.sub$Date[time_ind], 1, 9), 0, substr(ACTdata.1.sub$Date[time_ind], 10, 14), ":", 0, 0)



    } else if (grepl("-", ACTdata.1.sub$Date[1])) { # Reformat Date
      ACTdata.1.sub$Date <- strptime(ACTdata.1.sub$Date, "%Y-%m-%d %H:%M:%S")
    } else {
      ACTdata.1.sub$Date <- strptime(ACTdata.1.sub$Date, "%d/%m/%y %H:%M:%S")
    }

    ACTdata.1.sub$Time <- NULL # Remove empty Time variable


    ## Check for empty first row and if so, remove it:
    if (all(is.na(ACTdata.1.sub[1, ]))) { ACTdata.1.sub <- ACTdata.1.sub[-1, ] }


    ## Period Selection (user-defined option):
    ## Start by obtaining the row location of the user-defined start of period (startperiod)
    ## (format: "2016-10-03 00:00:00").
    if (selectperiod) {
      startperiod.loc <- which(ACTdata.1.sub$Date == startperiod[i])
      ## Then, if number of days from startperiod (daysperiod) is given, take only data from
      ## start (startperiod.loc) untill start + specified number of days (startperiod.loc + (daysperiod*minsaday)).
      if (daysperiod) {
        ACTdata.1.sub <- ACTdata.1.sub[(startperiod.loc:(startperiod.loc + (daysperiod*minsaday))), ]
        ## Else, if no daysperiod if given, see if user-defined end of period (endperiod) is given.
        ## If so, take only data from user-defined start of period to end of period.
      } else if (endperiod %in% ACTdata.1.sub$Date) {
        endperiod.loc <- which(ACTdata.1.sub$Date == endperiod)
        ACTdata.1.sub <- ACTdata.1.sub[(startperiod.loc:endperiod.loc), ]
        ## Else, take only data from start of period to end of dataset.
      } else {
        ACTdata.1.sub <- ACTdata.1.sub[(startperiod.loc:(nrow(ACTdata.1.sub))), ]
      }
    }


    ## Write values to overview file:
    ## Start- and end dates and times of actigraph data:
    start_date <- ACTdata.1.sub$Date[1] # Get start date
    end_date <- ACTdata.1.sub$Date[nrow(ACTdata.1.sub)] # Get end date.
    nr_obs <- nrow(ACTdata.1.sub) # Get number of observations.
    ACTdata.overview[i, "start"] <- as.character(start_date) # Write start date to overview.
    ACTdata.overview[i, "end"] <- as.character(end_date) # Write end date to overview.
    ## Recordingtime (time difference between start and end date):
    ACTdata.overview[i, "recordingtime"] <- round((as.POSIXct(start_date) - as.POSIXct(end_date)), 2) # write recordingtime to overview
    ## Number of observations:
    ACTdata.overview[i, "numberofobs"] <- nr_obs


    ## Identify Last Whole 24 Hour component and its Position:
    ACTdata.1.sub.lastwhole24h <- ACTdata.1.sub[tail(grep("00:00:00", ACTdata.1.sub$Date), 2), "Date"]
    ACTdata.1.sub.lastwhole24h <- ACTdata.1.sub.lastwhole24h[1]


    ## Add 14 days in a way that respects daylight savings time changes:
    #! Number of days needs to be made dynamic!! Needs to correspond to number of days in dataset!!
    ACTdata.1.sub.14day <- increase_by_days(ACTdata.1.sub$Date[1], 14)


    ## Lengthcheck:

    if (lengthcheck) {
      ## If Dataset is longer than Start Date plus 14 days, Remove data recorded thereafter:
      print("Task 2: Detecting if Dataset is longer than Start Date plus 14 full days")
      if (ACTdata.1.sub[nrow(ACTdata.1.sub), "Date"] > ACTdata.1.sub.14day) {
        print("Warning: Dataset is longer than Start Date plus 14 full days!")

        ACTdata.1.sub <- ACTdata.1.sub[1:((secs14day/60) + 1), ]

        print("Action: Observations recorded after Start Date plus 14 full days were removed.")
        print("Task 2 DONE: Dataset shortened to Start Date plus 14 days. Tasks 3, 4, and 5 also OK")
        print("")
        ACTdata.overview$lengthcheck[i] <- TRUE
      } else {
        print("Task 2 OK: Dataset not longer than Start Date plus 14 days.")
        print("")
      }

      # Update overview after removal
      ACTdata.overview[i, "numberofobs2"] <- nrow(ACTdata.1.sub)
      ACTdata.overview[i, "recordingtime2"] <- round(as.POSIXct(ACTdata.1.sub$Date[1]) - as.POSIXct(ACTdata.1.sub$Date[nrow(ACTdata.1.sub)]), 2)
      ACTdata.overview[i, "end2"] <- as.character(ACTdata.1.sub$Date[nrow(ACTdata.1.sub)]) # write end date to overview
    }



    ## Handling of Missing Data (NA's):
    ## Write Missings values (total number of missings and percentage of total data missing)
    ## to overview file:
    ACTdata.overview[i, "missings"] <- sum(is.na(ACTdata.1.sub$Activity))  # write missings to overview
    ACTdata.overview[i, "missings_perc"] <- round(ACTdata.overview[i, "missings"] / ACTdata.overview[i, "numberofobs"], 3) # write missings percentage to overview
    ## Report missings in console:
    print("Task: Reporting NA's")
    print(paste("Number of NA's in this Dataset:", ACTdata.overview[i, "missings"]))
    print(paste("This is:", round(ACTdata.overview[i, "missings"] / ACTdata.overview[i, "numberofobs"], 3), "% of the total number of observations!"))
    print("")
    ## If user-defined argument "na_omit" is TRUE, then use na.omit{stats} to row-wise delete NA's:
    if (na_omit) {
      print("Row-wise removal of NA's as user defined na.omit = TRUE")
      ACTdata.1.sub <- na.omit(ACTdata.1.sub) #! Creates error because many NA's!!!!
      print("All NA's removed!")
    }
    ## If user-defined argument "na_impute" is TRUE, then use mice{mice} to impute missings through
    ## Multivariate Imputation by Chained Equations (MICE). This installs the 'mice' package and dependencies.
    if (na_impute) {
      ## Impute Missings
      # if (!require('mice')) { install.packages('mice', dep = TRUE)}; library('mice')
      # if (!require('pscl')) { install.packages('pscl', dep = TRUE)}; library('pscl')
      # if (!require('accelmissing')) { install.packages('accelmissing', dep = TRUE)}; library('accelmissing')
      tempData <- mice::mice(matrix(data = c(ACTdata.1.sub$Activity, rep.int(x = 0, times = (ACTdata.overview[i, "numberofobs"]))), ncol = 2), m = 5, maxit = 50, meth = "pmm", seed = 500)
      tempData2 <- mice::complete(tempData, 1)
      ACTdata.1.sub$Activity <- tempData2$V1
    }

    ## User-control over Analysis if too much Missings:
    ## Too much is specified in this case as > 0.01% missing of total dataset.
    #! This 0.01% is arbitrarily chosen, as of now no suitable validated criterion is yet found.
    ## Initialise exception handling for when there are no missings:
    number_of_missings <- ifelse(is.na(ACTdata.overview[i, "missings"]), 0, ACTdata.overview[i, "missings"])
    ## If a missings-prompt is required (default is missings_report = TRUE), ..
    if (missings_report && !na_impute) {
      ## .. see if number of missings exceeds 0.01% of total number of data.
      if ((number_of_missings / ACTdata.overview[i, "numberofobs"]) > 0.01) {
        ## If so, explain situation to user via text prompt, and give them the choice to
        ## either continue or stop with the analyses.
        message("\nMore than 0.01% of data is missing!\nAnalysis results might deviate from true values!")
        message("Do you want to continue?")
        missings_prompt_answer <- readline(prompt = "Enter 'y' for Yes or 'n' for No:")
        ## If user decides to stop the analyses, stop the analyses and report stop message.
        if (missings_prompt_answer == "n") {
          message("Stopped by user!")
          ({break()})
        }
        ## If user decides to continue the analyses, continue and report continue message.
        if (missings_prompt_answer == "y") {
          print("Continue analysis with > 0.01% missings")
          print("")
        } else {
          message("Unknown input!")
          ({break()})
        }
      }
    }


    # ## Set NA to 0
    # #! Debug and/or possible future functionality if required
    # ACTdata.1.sub[is.na(ACTdata.1.sub)] <- 0


    ## Check if there is activity in the tail of the dataset. As sometimes at the end of the study
    ## the actigraph is handed over by the participant, but not immediately stopped.
    ## If Activity in Last 5 observations is on average zero, Skip to Last Activity:
    ACTdata.1.sub.last5act <- ACTdata.1.sub$Activity[(nrow(ACTdata.1.sub) - 4):nrow(ACTdata.1.sub)] # Last 5 activity counts in dataset
    ACTdata.1.sub.last5act.active <- sum(ACTdata.1.sub.last5act, na.rm = T) >= (5 * length(ACTdata.1.sub.last5act)) # Is there on average more than 5 counts per obs?
    print("Task: Checking for Activity in Last 5 observations")
    if (ACTdata.1.sub.last5act.active == FALSE) {
      print("Warning: No Activity in Last 5 observations!")
      print("Last 5 Activity Counts, before Correction:")
      print(ACTdata.1.sub.last5act)
      ACTdata.1.sub <- ACTdata.1.sub[1:max(which(ACTdata.1.sub$Activity >= (5 * length(ACTdata.1.sub.last5act)))), ] # Shortens data untill reached last activity

      # exception for when there remains less than 10 obs
      if (nrow(ACTdata.1.sub) < 10){

        warning("Less than 10 obs. remain after removing trailing, zero-value obs.")
        next()

      }

      ACTdata.1.sub.last5act <- ACTdata.1.sub$Activity[(nrow(ACTdata.1.sub) - 4):nrow(ACTdata.1.sub)] # Last 5 activity counts in dataset
      ACTdata.overview$last5act.active[i] <- FALSE
      print("Last 5 Activity Counts, after Correction:")
      print(ACTdata.1.sub.last5act)
      print("Task DONE: Dataset Skipped to last Activity.")
      print("")
    } else {
      print("Task OK: Dataset contained Activity in Last 5 observations.")
      print("")
    }


    ## Update overview file:
    ## Write number of observations, total recording time, and end date/time, as these values
    ## might have been altered after previous data-management steps.
    ACTdata.overview[i, "numberofobs2"] <- nrow(ACTdata.1.sub)
    ACTdata.overview[i, "recordingtime2"] <- round(as.POSIXct(ACTdata.1.sub$Date[1]) - as.POSIXct(ACTdata.1.sub$Date[nrow(ACTdata.1.sub)]), 2)
    ACTdata.overview[i, "end2"] <- as.character(ACTdata.1.sub$Date[nrow(ACTdata.1.sub)]) # write end date to overview


    ## Step 2.3: Write managed data to file for analyses-------------------------------------------
    ## Create a new directory in working directory for writing managed data files.
    dir.create(file.path(workdir, "Managed Datasets"), showWarnings = FALSE)
    setwd(file.path(workdir, "Managed Datasets"))
    ## Set parameters and go to new directory
    wd <- getwd()
    name <- paste(gsub(pattern = ".csv", replacement = "", x = ACTdata.files[i]))
    newdir <- paste(wd, name, sep = "/")
    dir.create(newdir, showWarnings = FALSE)
    setwd(newdir)
    ## Write managed data:
    write.table(ACTdata.1.sub, row.names = FALSE, col.names = FALSE,
                file = paste(gsub(pattern = ".csv", replacement = "", x = ACTdata.files[i]), "MANAGED.txt"))


    ## Step 2.4: Initialising analyses and funtionalities--------------------------------------
    ## Description: .....

    ## Read managed dataset for analyses and functionalities:
    CRV.data <- read.table(file = file.path(newdir, paste(gsub(pattern = ".csv", replacement = "", x = ACTdata.files[i]), "MANAGED.txt")),
                           stringsAsFactors = FALSE)

    # colnames(CRV.data) <- c("Date", "Time", "Activity") # Actical only 2 columns here
    if (ncol(CRV.data) > 2) {
      colnames(CRV.data) <- c("Date", "Time", "Activity")
    } else if (ncol(CRV.data) == 2) {
      colnames(CRV.data) <- c("Date", "Activity")
    }


    sleepdata.overview <- NULL
    rollingwindow.results <- NA
    ## Moving/Rolling Window
    ## Check first if Moving Window is required, as this requires it's own analysis calls.
    #! Add Sleep-analysis for Rolling Window!
    #! Add possibility to change 'jump-length' of rolling window (now 1 day) to multiple days!
    if (movingwindow) {
      rollingwindow <- function(x, window, jump) {
        ## Initialise parameters:
        out <- data.frame()
        n <- nrow(x)
        rollingwindow.results <- as.data.frame(matrix(nrow = (floor(((n - window) / jump))), ncol = 21))
        ## Set number of iterations at number of rows of (data - windowsize) / (minutes per day (1440) * jump)
        for (i in 1:((floor(((n - window) / jump))) + 1)) {
          ## Take data as 1 till windowsize for first iteration, for further iterations take
          ## data as starting at ((iteration - 1) * (minutes per day * jump)), and ending at
          ## ((iteration - 1) * (minutes per day * jump)) plus windowsize.
          if (i == 1) {
            out <- x[i:window, ]
          } else {
            out <- x[((i - 1) * jump):(((i - 1) * jump) + window), ]
          }
          ## Write selected period to dataset (CRV.data) and add relevant column names:
          CRV.data <- out
          if (ncol(CRV.data) > 2) {
            colnames(CRV.data) <- c("Date", "Time", "Activity")
          } else if (ncol(CRV.data) == 2) {
            colnames(CRV.data) <- c("Date", "Activity")
          }

          ## Use the nparcalc{ACTman} function to calculate circadian rhythm variables over
          ## the selected period within the window. Write results to rollingwindow.results.
          r2 <- nparcalc(myACTdevice = myACTdevice, movingwindow = movingwindow, CRV.data = CRV.data, ACTdata.1.sub = ACTdata.1.sub, out = out)
          rollingwindow.results[i, 1] <- as.character(strftime(CRV.data[1, "Date"], format = "%Y-%m-%d %H:%M:%S"))
          rollingwindow.results[i, 2] <- as.character(strftime(CRV.data[nrow(CRV.data), "Date"], format = "%Y-%m-%d %H:%M:%S"))
          rollingwindow.results[i, 3] <- r2$IS
          rollingwindow.results[i, 4] <- r2$IV
          rollingwindow.results[i, 5] <- round(r2$RA, 2)
          rollingwindow.results[i, 6] <- round(r2$L5, 2)
          rollingwindow.results[i, 7] <- r2$L5_starttime
          rollingwindow.results[i, 8] <- round(r2$M10, 2)
          rollingwindow.results[i, 9] <- r2$M10_starttime
          rollingwindow.results[i, 10] <- r2$Mean
          rollingwindow.results[i, 11] <- r2$Variance
          rollingwindow.results[i, 12] <- r2$SD
          rollingwindow.results[i, 13] <- r2$CoV
          rollingwindow.results[i, 14] <- r2$Skewness
          rollingwindow.results[i, 15] <- r2$Kurtosis
          rollingwindow.results[i, 16] <- r2$Autocorr
          rollingwindow.results[i, 17] <- r2$Autocorr_lag2
          rollingwindow.results[i, 18] <- r2$Autocorr_lag3
          rollingwindow.results[i, 19] <- r2$Autocorr_lag60
          rollingwindow.results[i, 20] <- r2$Autocorr_lag120
          rollingwindow.results[i, 21] <- r2$Time_to_Recovery
          colnames(rollingwindow.results) <- c("starttime", "endtime", "IS", "IV", "RA", "L5", "L5_starttime",
                                               "M10", "M10_starttime", "Mean", "Variance", "SD",
                                               "Coeff_of_Var", "Skewness", "Kurtosis", "Autocorr_lag1",
                                               "Autocorr_lag2", "Autocorr_lag3", "Autocorr_lag60",
                                               "Autocorr_lag120", "Time_to_Recovery")

          ## Report the calculated circadian results in console:
          print("---------------------------------------------------------------------------------")
          print(paste("Rolling window CRV analysis output - Window step:", (i - 1)))
          print(paste("Begin time:", CRV.data[1, "Date"]))
          print(paste("End time:", CRV.data[nrow(CRV.data), "Date"]))
          print(paste("nOBS:", nrow(CRV.data)))
          print("")
          print("Circadian Rhythm Variables")
          print(paste("IS: ", r2$IS))
          print(paste("IV: ", r2$IV))
          print(paste("RA: ", round(r2$RA, 2)))
          print(paste("L5: ", round(r2$L5, 2)))
          print(paste("L5_starttime: ", r2$L5_starttime))
          print(paste("M10: ", round(r2$M10, 2)))
          print(paste("M10_starttime: ", r2$M10_starttime))
          print("")
          print("Early-Warning Signals")
          print(paste("Mean: ", r2$Mean))
          print(paste("Variance: ", r2$Variance))
          print(paste("SD: ", r2$SD))
          print(paste("Coefficient of Variation: ", r2$CoV))
          print(paste("Skewness: ", r2$Skewness))
          print(paste("Kurtosis: ", r2$Kurtosis))
          print(paste("Autocorr at-lag-1: ", r2$Autocorr))
          print(paste("Autocorr at-lag-2: ", r2$Autocorr_lag2))
          print(paste("Autocorr at-lag-3: ", r2$Autocorr_lag3))
          print(paste("Autocorr at-lag-60: ", r2$Autocorr_lag60))
          print(paste("Autocorr at-lag-120: ", r2$Autocorr_lag120))
          print(paste("Time_to_Recovery: ", r2$Time_to_Recovery))
          print("---------------------------------------------------------------------------------")

        }
        rollingwindow.results # Needed for output in .CSV

        rollingwindow.results <- rollingwindow.results
      }
      ## Assign results from rolling window:
      rollingwindow.results <- rollingwindow(x = CRV.data, window = (1440 * (movingwindow.size)), jump = (1440 * (movingwindow.jump)))

      ## Initialise normal circadian rhythm analysis without moving window:
    } else {
      if (circadian_analysis) {
        ## Use the nparcalc{ACTman} function to calculate circadian rhythm variables over
        ## the whole period:

        r2 <- nparcalc(myACTdevice = myACTdevice, movingwindow = movingwindow, CRV.data = CRV.data, ACTdata.1.sub = ACTdata.1.sub)

        ## Attach r2 output to overview
        ACTdata.overview[i, "r2.IS"] <- r2$IS
        ACTdata.overview[i, "r2.IV"] <- r2$IV
        ACTdata.overview[i, "r2.RA"] <- round(r2$RA, 2)
        ACTdata.overview[i, "r2.L5"] <- r2$L5
        ACTdata.overview[i, "r2.L5_starttime"] <- r2$L5_starttime
        ACTdata.overview[i, "r2.M10"] <- round(r2$M10, 2)
        ACTdata.overview[i, "r2.M10_starttime"] <- r2$M10_starttime
      }
    }
    ## If a comparison with another actigraphy R package is required, run nparACT_base_loop{nparACT}:
    if (nparACT_compare) {

      ## Use nparACT Package to calculate circadian rhythm variables:
      r <- nparACT::nparACT_base_loop(path = newdir, SR = 1/60, fulldays = T, plot = T)

      ## Attach nparACT output to overview
      ACTdata.overview[i, "IS"] <- r$IS
      ACTdata.overview[i, "IV"] <- r$IV
      ACTdata.overview[i, "RA"] <- r$RA
      ACTdata.overview[i, "L5"] <- r$L5
      ACTdata.overview[i, "L5_starttime"] <- r$L5_starttime
      ACTdata.overview[i, "M10"] <- r$M10
      ACTdata.overview[i, "M10_starttime"] <- r$M10_starttime
    }

    ## Set wd back to main workdir
    setwd(workdir)


    ## Create "Results" directory for rolling window results:
    workdir_temp <- getwd()
    dir.create(file.path(workdir_temp, "Results"), showWarnings = FALSE)
    setwd(file.path(workdir_temp, "Results"))

    ## Write rollingwindow.results to .CSV
    # if (movingwindow) {
    #   write.table(rollingwindow.results,
    #               file = paste("rollingwindow-results", i, ".csv", sep = ""),
    #               row.names = F, sep = ",")
    # }

    if (movingwindow) {
      write.table(rollingwindow.results,
                  file = paste(substr(ACTdata.files[i], 1, (nchar(ACTdata.files[i]) - 4)),
                               "-rollingwindow-results.csv", sep = ""),
                  row.names = F, sep = ",")
    }


    paste(substr(ACTdata.files[i], 1, (nchar(ACTdata.files[i]) - 4)),
          "-sleepresults.csv", sep = "")

    ## Set working directory back to main working directory:
    setwd(workdir_temp)
    rm(workdir_temp)


    ## Sleep Analysis:
    ## Use the sleepdata_overview{ACTman} function to calculate sleep variables over
    ## the whole period.
    if (iwantsleepanalysis) {
      # sleepdata.overview <- sleepdata_overview(workdir = sleepdatadir, actdata = ACTdata.1.sub, i = i, lengthcheck = lengthcheck)
      sleepdata.overview <- sleepdata_overview(workdir = workdir, actdata = ACTdata.1.sub, i = i, lengthcheck = lengthcheck, ACTdata.files = ACTdata.files)
    }

    ## Actogram:
    ## Use the plot_actogram{ACTman} function to plot an Actogram of the whole period.
    if (plotactogram != FALSE) {
      plot_actogram(workdir = workdir, ACTdata.1.sub = ACTdata.1.sub, i = i, plotactogram = plotactogram,
                    myACTdevice = myACTdevice, rollingwindow.results = rollingwindow.results, i_want_EWS = i_want_EWS,
                    ACTdata.files = ACTdata.files)
    }


    ## Report progress in console:
    print(paste("--------------------------------------", "END OF DATASET", i, "---", "@",
                round(i * (100 / length(ACTdata.files))), "% DONE",  "--------------------------------------"))
  }


  ## Step 3: After loop processing-----------------------------------------------------------------

  ## Transform negative recordingtimes to positive:
  ACTdata.overview$recordingtime <- ((ACTdata.overview$recordingtime) ^ 2) ^ (1 / 2)
  ACTdata.overview$recordingtime2 <- ((ACTdata.overview$recordingtime2) ^ 2) ^ (1 / 2)

  ## Assign zero to missings column in overview when there are no missings:
  ACTdata.overview[is.na(ACTdata.overview[, "missings"]), "missings"] <- 0

  ## Update overview if comparison to nparACt is not required:
  if (!nparACT_compare) {
    ACTdata.overview["IS"] <- NULL
    ACTdata.overview["IV"] <- NULL
    ACTdata.overview["RA"] <- NULL
    ACTdata.overview["L5"] <- NULL
    ACTdata.overview["L5_starttime"] <- NULL
    ACTdata.overview["M10"] <- NULL
    ACTdata.overview["M10_starttime"] <- NULL
    colnames(ACTdata.overview) <- gsub(pattern = "r2.", x = colnames(ACTdata.overview), replacement = "")
  }

  ## Subset experimental variables
  ACTdata.1.sub.expvars <- ACTdata.overview[c("IS", "IV", "RA", "L5", "L5_starttime", "M10", "M10_starttime")]
  # colnames(ACTdata.1.sub.expvars) <- c("IS", "IV", "RA", "L5", "L5 Start time", "M10", "M10 Start time")


  ## Create "Results" directory:
  workdir_temp <- getwd()
  dir.create(file.path(workdir_temp, "Results"), showWarnings = FALSE)
  setwd(file.path(workdir_temp, "Results"))

  ## Write results of circadian analysis to .CSV
  if (circadian_analysis) {
    write.table(ACTdata.1.sub.expvars, file = "ACTdata_circadian_res.csv", sep = ",", row.names = F)
  }

  ## Write ACTdata.overview to .CSV
  write.table(ACTdata.overview, file = "ACTdata_overview.csv", sep = ",", row.names = F)

  ## Set working directory back to main working directory:
  setwd(workdir_temp)
  rm(workdir_temp)


  ## Returned result.
  if (iwantsleepanalysis) {
    sleepdata.overview
  } else if (movingwindow) {
    rollingwindow.results
  } else {
    ACTdata.overview
  }
}

#' Data included in the package
#'
#' @name ACTdata.1
#' @docType data
#' @author Yoram K. Kunkels \email{y.k.kunkels@umcg.nl}
#' @references \url{exampledata.com}
#' @keywords data
NULL
