
######################################
#### Sleeplog from Marker buttons ####
#### YKK                          ####
####~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*####


#' sleeplog_from_markers
#'
#' Calculate sleeplog from markers
#'
#' @param workdir The directory where the sleep files are located.
#' @param i The index of the current file in ACTdata.files
#' @param ACTdata.files The current file in ACTdata.files
#'
#' @return Returns a sleeplog
#'
#' @importFrom utils read.csv
#' @importFrom utils write.csv
#'
sleeplog_from_markers <- function(workdir, i, ACTdata.files) {

  ## Set wd
  setwd(workdir)

  ## List marker button files and read data
  mb_files <- list.files(pattern = "markers.csv")


  mb_files <- mb_files[pmatch(substr(ACTdata.files[i], 1, 4), mb_files)] # only take marker file from this ppn

  #!
  # mb_data <- read.csv(mb_files)
  # mb_data <- read.delim(mb_files)

  mb_data <- read.csv(mb_files)

  # ## Name column names
  # print("!!!DEBUG!!!")
  # print(ncol(mb_data))
  # print(head(mb_data))
  # View(mb_data)



  # if (ncol(mb_data) == 3) {
  #   mb_data <- read.csv(mb_files)
  # }
  if (ncol(mb_data) == 1) {
    # mb_data <- read.delim(mb_files)
    mb_data <- read.csv(mb_files, sep = ';')
  }
  # if (ncol(mb_data) != 1 & ncol(mb_data) != 3) {
  #   message("Marker Button file does not contain expected number of rows (1 or 3)!")
  #   message("Is your file in comma- or tab-seperated format?")
  # }




  ##
  #! Probable Bug: tab-seperated (sep = '\t') file instead if comma?

  colnames(mb_data) <- c("Name/Type", "Date", "Time")

  ## Remove header
  mb_data <- mb_data[((grep("^Name/Type", mb_data[, 1]) + 1):nrow(mb_data)), ]


  ## Remove footer
  mb_data <- mb_data[(1:(which(grepl("Sleep Analysis", mb_data[, 1])) - 2)), (1:3)]


  ## Frequencies of marker presses per day
  mb_data_datefreq <- as.data.frame(table(unlist(mb_data$Date)))
  mb_data_datefreq <- mb_data_datefreq[!(mb_data_datefreq$Freq == 0), ]


  # ## Copy original data for modifications
  # mb_data_new <- mb_data

  ## Make new temp column for removal variable
  mb_data$Remove <- 0

  ## Make new temp column for Bedtime/Gotup
  mb_data$Morning_evening <- 0

  ## Make new temp column for Day_measure_No
  mb_data$Freq <- 0

  ## Make new temp column for sleep_after_midnight
  mb_data$sleep_after_midnight <- 0


  ## Fill in initial Bedtimes and Gotups
  for (a in 1:nrow(mb_data)) {

      if (as.POSIXct(mb_data[a, "Time"], format = "%H:%M:%S") >  as.POSIXct("04:00:00", format = "%H:%M:%S") &&
         as.POSIXct(mb_data[a, "Time"], format = "%H:%M:%S") <  as.POSIXct("14:00:00", format = "%H:%M:%S")) {

        mb_data[a, "Morning_evening"] <- "Gotup"

      } else if (as.POSIXct(mb_data[a, "Time"], format = "%H:%M:%S") >  as.POSIXct("14:00:00", format = "%H:%M:%S") &&
                as.POSIXct(mb_data[a, "Time"], format = "%H:%M:%S") <  (as.POSIXct("04:00:00", format = "%H:%M:%S") + (1440 * 60))) {

        mb_data[a, "Morning_evening"] <- "Bedtime"

      }


    mb_data[a, "Freq"] <- mb_data_datefreq[which(mb_data[a, "Date"] == mb_data_datefreq[, "Var1"]), "Freq"]


  }



  for (a in 1:nrow(mb_data)) {

    if ((mb_data[a, "Morning_evening"] == 0) && (mb_data[(a - 1), "Morning_evening"] == "Gotup") &&
       (mb_data[(a + 1), "Morning_evening"] == "Gotup") && (as.POSIXct(mb_data[a, "Time"], format = "%H:%M:%S") <
                                                            as.POSIXct("05:00:00", format = "%H:%M:%S"))) {

      mb_data[a, "Morning_evening"] <- "Bedtime"
      mb_data[a, "sleep_after_midnight"] <- 1

    }


  }



  for (a in 1:(nrow(mb_data) - 1)) {

    if ((mb_data[a, "Morning_evening"] == "Gotup") &&
       identical(mb_data[a, "Morning_evening"], mb_data[(a + 1), "Morning_evening"]) &&
       identical(mb_data[a, "Date"], mb_data[(a + 1), "Date"])) {

      mb_data[a, "Remove"] <- 1

    }


    if ((mb_data[a, "Morning_evening"] == "Bedtime") &&
       identical(mb_data[a, "Morning_evening"], mb_data[(a + 1), "Morning_evening"]) &&
       identical(mb_data[a, "Date"], mb_data[(a + 1), "Date"])) {

      mb_data[a, "Remove"] <- 1

    }

  }



  ## Edit Dates of "sleep_after_midnight" occurences to previous day
  for (a in 1:(nrow(mb_data))) {

    if (mb_data[a, "sleep_after_midnight"] == 1) {

      mb_data[a, "Date"] <- mb_data[(a - 1), "Date"]

    }

  }




  ## Remove incorrect markers & temp columns
 #! mb_data <- mb_data[-(which(mb_data$Remove == 1)), ] #! source of empty sleeplogfiles?
  mb_data <- mb_data[(which(mb_data$Remove == 0)), ]

  mb_data <- mb_data[, c("Date", "Time", "Morning_evening", "sleep_after_midnight")]



   ## Initialise sleeplog
  sleeplog_nrow <- nrow(mb_data_datefreq)
  # sleeplog <- matrix(nrow = (sleeplog_nrow * 2), ncol = 3) #! Gve error when using abel file
  sleeplog <- matrix(nrow = sleeplog_nrow, ncol = 3)
  # colnames(sleeplog) <- c("Date", "Time", "Gotup")
  colnames(sleeplog) <- c("Date", "Gotup", "Bedtime")
  # sleeplog[, "Date"] <- as.character(sort(rep(unique(mb_data[, "Date"]), 1)))


  #! Exception for unequal length
  if (length(sleeplog[, "Date"]) != length(as.character(sort(rep(unique(mb_data[, "Date"]), 1))))) {

    if (length(sleeplog[, "Date"]) > length(as.character(sort(rep(unique(mb_data[, "Date"]), 1))))) {

      # Find lenght diff between sleeplog and subset from mb_data
      sleeplog_diff <- abs(length(sleeplog[, "Date"]) - length(as.character(sort(rep(unique(mb_data[, "Date"]), 1)))))

      sleeplog[((1 + sleeplog_diff):sleeplog_nrow), "Date"] <- as.character(sort(rep(unique(mb_data[, "Date"]), 1)))


    }

  } else {
    sleeplog[, "Date"] <- as.character(sort(rep(unique(mb_data[, "Date"]), 1)))
  }





#! changed index_value 'i' to 'b'
  for (b in 1:nrow(sleeplog)) {

    if (sleeplog[b, "Date"] %in% mb_data[, "Date"]) {

      mb_TEMP <- mb_data[which(mb_data[, "Date"] == sleeplog[b, "Date"]), ]


      if ("Gotup" %in% mb_TEMP[, "Morning_evening"]) {

          #! Exception for more than one Gotup times
          if (length(as.character(mb_TEMP[which(mb_TEMP[, "Morning_evening"] == "Gotup"), "Time"])) > 1) {

            sleeplog[b, "Gotup"] <- min(as.character(mb_TEMP[which(mb_TEMP[, "Morning_evening"] == "Gotup"), "Time"]))

          } else {
            sleeplog[b, "Gotup"] <- as.character(mb_TEMP[which(mb_TEMP[, "Morning_evening"] == "Gotup"), "Time"])}

      } else {
        message(paste("Gotup time is missing at day", sleeplog[b, "Date"], "!!"))}



      if ("Bedtime" %in% mb_TEMP[, "Morning_evening"]) {

        #! Exception for more than one Gotup times
        if (length(as.character(mb_TEMP[which(mb_TEMP[, "Morning_evening"] == "Bedtime"), "Time"])) > 1) {

          sleeplog[b, "Bedtime"] <- min(as.character(mb_TEMP[which(mb_TEMP[, "Morning_evening"] == "Bedtime"), "Time"]))

        } else {
          sleeplog[b, "Bedtime"] <- as.character(mb_TEMP[which(mb_TEMP[, "Morning_evening"] == "Bedtime"), "Time"])}



      } else {
        message(paste("Bedtime time is missing at day", sleeplog[b, "Date"], "!!"))}

    }

    rm(mb_TEMP)
  }




  ## Check for missings and give user control
  #! Mean option was changed to median!!
  if (sum(is.na(sleeplog)) != 0) {

    message(paste("Warning:", sum(is.na(sleeplog)), "markers are missing!"))

    message("How do you want to continue?")
    missings_prompt_answer <- readline(prompt = "Enter 'm' to impute missing markers with median values,
                                       'n' to impute missing markers with mean of nearest neighbours,
                                       'f' to fill in the missings by hand, or 'q' to Abort:")

    if (missings_prompt_answer == "m") {
      message("Imputing missing markers using median!")



      format(median(strptime(sleeplog[, "Bedtime"], "%H:%M:%S")), "%H:%M:%S")


      ## Using mean on normal Bedtime times causes large differences (due to some times being after midnight(!))
      ## Add 12 hours to get to midday and the calculate mean.
      #! Changing mean to median due to aforementioned issues..
      #! Removed the added 12 hours..
      # sleeplog_TEMP <- (as.POSIXct(sleeplog[, "Bedtime"], format = "%H:%M:%S") + (60 * 60 * 12))
      sleeplog_TEMP <- (as.POSIXct(sleeplog[, "Bedtime"], format = "%H:%M:%S"))

      if (TRUE %in% is.na(sleeplog[, "Gotup"])) {
        sleeplog[is.na(sleeplog[, "Gotup"]), "Gotup"] <- substr(median(as.POSIXct(sleeplog[, "Gotup"], format = "%H:%M:%S"), na.rm = TRUE), start = 12, stop = 19)
      }

      if (TRUE %in% is.na(sleeplog[, "Bedtime"])) {
        sleeplog[is.na(sleeplog[, "Bedtime"]), "Bedtime"] <- substr(median(as.POSIXct(sleeplog_TEMP, format = "%H:%M:%S"), na.rm = TRUE), start = 12, stop = 19)
      }

    }

    # if (missings_prompt_answer == "n") {
    #
    #   for (missings_count in 1:sum(is.na(sleeplog[, "Gotup"]))) {
    #
    #     Gotup_missing_index <- which(is.na(sleeplog[, "Gotup"]) == TRUE)
    #     Gotup_nonmissings_index <- which(is.na(sleeplog[, "Gotup"]) == FALSE)
    #
    #     Gotup_nearest_nonmissings_loc <- which(abs(Gotup_nonmissings_index - Gotup_missing_index) ==
    #                                            sort(abs(Gotup_nonmissings_index - Gotup_missing_index))[1:2])
    #
    #     sleeplog[Gotup_missing_index, "Gotup"] <- mean(as.POSIXct(sleeplog[Gotup_nonmissings_index[Gotup_nearest_nonmissings_loc], "Gotup"], format = "%H:%M:%S"))
    #
    #   }
    #
    #   for (missings_count2 in 1:sum(is.na(sleeplog[, "Bedtime"]))) {
    #
    #     Bedtime_missing_index <- which(is.na(sleeplog[, "Bedtime"]) == TRUE)
    #     Bedtime_nonmissings_index <- which(is.na(sleeplog[, "Bedtime"]) == FALSE)
    #
    #     Bedtime_nearest_nonmissings_loc <- which(abs(Bedtime_nonmissings_index - Bedtime_missing_index) ==
    #                                              sort(abs(Bedtime_nonmissings_index - Bedtime_missing_index))[1:2])
    #
    #     sleeplog[Bedtime_missing_index, "Bedtime"] <- mean(as.POSIXct(sleeplog[Bedtime_nonmissings_index[Bedtime_nearest_nonmissings_loc], "Bedtime"], format = "%H:%M:%S"))
    #
    #   }
    #
    # }

    if (missings_prompt_answer == "f") {
      message("Please fill in the missing markers")
      message("Thereafter, click 'File > Close' to continu")
      fix(sleeplog)
    }
    if (missings_prompt_answer == "q") {
      stop("User hit q to stop program.")
    }

    ## Check if sleeplog times end in ":00" insted of ":30" (otherwise "rownr.Gotup is NA!" Error)
    if (length(grep(pattern = ":00", x = sleeplog)) < nrow(sleeplog)) {
      message("Non-full minutes detected in sleeplog times!")
      message("Rounding sleeplog times to full minutes (':00')")
      message("")

      substr(sleeplog[, 2:3], start = 6, stop = 8) <- ":00"

    }

  }


  ## Write sleeplog to .csv
  write.csv(x = sleeplog,
            file = paste(substr(ACTdata.files[i], 1, (nchar(ACTdata.files[i]) - 4)),
                                       "-sleeplog.csv", sep = ""),
            row.names = FALSE)
}
