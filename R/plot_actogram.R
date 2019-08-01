##############################################################################
### ACTman{plot_actogram}                                                  ###
### Script authors: Yoram Kunkels, Stefan Knapen, & Ando Emerencia         ###
###========================================================================###


#' plot_actogram
#'
#' Function to plot 48 hour Actograms.
#'
#' @param workdir the working directory as supplied to ACTman.
#' @param ACTdata.1.sub The managed data set
#' @param i The index of the current file in ACTdata.files
#' @param plotactogram Value indicating if and what kind of actogram has to be plotted. Can be either '48h', '24h', or FALSE
#'
#'
#' @importFrom grDevices dev.off
#' @importFrom grDevices pdf
#' @importFrom graphics axis
#' @importFrom graphics barplot
#' @importFrom graphics par
#' @importFrom stats na.omit
plot_actogram <- function(workdir, ACTdata.1.sub, i, plotactogram, rollingwindow.results, i_want_EWS) {

### Part 1: Basic Operations ----------------------------------------------------------------------------

  # Set and save working directory
  workdir.save <- getwd()
  setwd(workdir)



  act_data <- ACTdata.1.sub # Copy data for editing required for plotting
  act_data <- within(act_data, Date <- as.character(act_data$Date)) # Date as character for plotting
  ndays.plot <- round(abs(as.numeric(round(as.POSIXct(act_data$Date[1]) - as.POSIXct(act_data$Date[nrow(act_data)]), 2))))


### Part 2: 1st day Selection & Midnight Detection 2nd day  ---------------------------------------------------

  ## Midnight Detection 2nd day
  day2start <- ((which(substr(act_data$Date, start = 12, stop = 19) == "00:00:00")[1]) - 1) # Start of day2 at midnight.

  ## 1st Day selection
  day1 <- act_data[1:day2start, ]
  day1.rest <- 1440 - day2start

  ## Filling in period before 1st day selection to get full 24h in 1st day
  day1.rest.mat <-  matrix(data = 0, nrow = day1.rest, ncol = 2) # Create empty matrix
  colnames(day1.rest.mat) <- colnames(day1) # Equalise colnames
  # day1 <- within(day1, Date <- as.POSIXct(as.character(Date), format = "%Y-%m-%d %H:%M:%S")) # Do POSIXct magic (http://grokbase.com/t/r/r-help/112jk8eyqp/r-problem-with-rbind-when-data-frame-contains-an-date-time-variable-posixt-posixlt)
  day1 <- rbind(day1.rest.mat, day1) # Combine day1 data with empty pre-day1 matrix


### Part 3: Loop for assigning subsequent days (14 days max for plot) & setting ylim's ----------------------

  ## Assign other days
  for (i.plot in 2:(ndays.plot)) {
    if (i.plot == 2) {
      assign(paste("day", i.plot, sep = ""), act_data[((day2start + (1440 ^ (i.plot - 2))):(day2start + (1440 * (i.plot - 1)))), ])
    } else {
      assign(paste("day", i.plot, sep = ""), act_data[((day2start + ((1440 * (i.plot - 2)) + 1)):(day2start + (1440 * (i.plot - 1)))), ])
    }
  }

  ## Define the range so all plots will be equal height (ylim)
  ylimit_TEMP <- ls()[setdiff(grep("day", ls()), c(grep("rest", ls()), grep("start", ls()), grep("plot", ls())))]
  ylimit <- range(na.omit(eval(parse(text = ylimit_TEMP))[, "Activity"]))

    # ylimit <- range(na.omit(c(day1$Activity, day2$Activity, day3$Activity, day4$Activity, day5$Activity,
  #                           day6$Activity, day7$Activity, day8$Activity, day9$Activity, day10$Activity,
  #                           day11$Activity, day12$Activity, day13$Activity, day14$Activity))) #! Obsolete


  ### Part 4: Combining Days & 48 hour Doubleplot -----------------------------------------------------------

  ## Combine days
  for (j.plot in 1:(ndays.plot - 1)) {
    assign(paste("day.", j.plot, ".", (j.plot + 1), sep = ""),
           rbind(eval(parse(text = paste("day", j.plot, sep = ""))), eval(parse(text =  paste("day", (j.plot + 1), sep = "")))))
  }


  ### Work in Progress ------------------------------------------------------------------------------
  #! TEST for moving window plot
  ## combine all days in 1 plot

  ## Runner
  # ACTman::ACTman(workdir = "C:/Bibliotheek/Studie/PhD/Publishing/ACTman/R-part/TEST-RESULTS",
  #                myACTdevice = "MW8", lengthcheck = F, nparACT_compare = F, iwantsleepanalysis = F,
  #                circadian_analysis = T, plotactogram = "24h", movingwindow = TRUE, movingwindow.size = 7)


if (i_want_EWS == TRUE && is.na(rollingwindow.results)) {

  message("Cannot create EWS plot without rolling window results")
  message("Please make sure that both 'i_want-EWS' AND 'movingwindow' arguments are TRUE")
  message("Quitting...")
  stop()

}


if (i_want_EWS == TRUE) {  # ## Initialise empty matrix for timestamps and activity counts

  LOLkat <- matrix(NA, nrow = (1440 * ndays.plot), ncol = 2)

  ## Assign timestamps and activity counts to matrix
  for (k.plot in 1:ndays.plot) {

     if (k.plot == 1) {
      LOLkat[k.plot:1440, 1] <- eval(parse(text = paste("day", k.plot, "[ , 1]", sep = "")))
      LOLkat[k.plot:1440, 2] <- eval(parse(text = paste("day", k.plot, "[ , 2]", sep = "")))
    } else {
      LOLkat[((((k.plot - 1) * 1440) + 1):(k.plot * 1440)), 1] <- eval(parse(text = paste("day", k.plot, "[ , 1]", sep = "")))
      LOLkat[((((k.plot - 1) * 1440) + 1):(k.plot * 1440)), 2] <- eval(parse(text = paste("day", k.plot, "[ , 2]", sep = "")))
    }

  }


  # plotme <- "Time_to_Recovery"
  plotme <- colnames(rollingwindow.results)[10:26]

  for (EWS_count in 1:length(plotme)) {


  ## Initialise .PDF plot in A4 size (11.7 x 8.3 inches)
  png(paste("Actigraphy EWS Plot - ", plotme[EWS_count], ".png"), width = 842, height = 595, units = "px")

  ## Create barplot
  bp2 <- barplot(as.numeric(LOLkat[, 2]), plot = FALSE)

  ## Obtain barplot range
  # bp2_ylim <- range(as.numeric(LOLkat[, 2]))
  bp2_ylim <- range(na.omit(as.numeric(LOLkat[, 2])))
  roundup_power_10 <- function(x) 10 ^ ceiling(log10(x))
  bp2_ylim_upper <- roundup_power_10(max(bp2_ylim))

  ## Plot barplot
  ## Also set x- and ylim here
  barplot(as.numeric(LOLkat[, 2]), ylim = c((-bp2_ylim_upper * 2.0), bp2_ylim_upper))


  ## Create labels
  x_labels2 <- substr(LOLkat[, 1], 1, 10)
  l.plot_n <- length(unique(substr(LOLkat[, 1], 1, 10)[!substr(LOLkat[, 1], 1, 10) == "0"]))
  x_labels_pos2_start <- matrix(NA, nrow = l.plot_n, ncol = 1)


  l.plot <- 0
  ## Obtain label positions
  for (l.plot in 1:l.plot_n) {

    ## Assure l.plot is not NA, otherwise assign last non-NA value
    if (is.na(unique(substr(LOLkat[, 1], 1, 10)[!substr(LOLkat[, 1], 1, 10) == "0"])[l.plot])) {
      l.plot <- max(which(is.na(unique(substr(LOLkat[, 1], 1, 10)[!substr(LOLkat[, 1], 1, 10) == "0"])) == FALSE))
    }

    x_labels_pos2_start[l.plot, ] <- which.max(x_labels2 == unique(substr(LOLkat[, 1], 1, 10)[!substr(LOLkat[, 1], 1, 10) == "0"])[l.plot])

  }


  ## Assign start and end dates
  LOLkat_startdate <- x_labels2[x_labels_pos2_start][1] # Activity data start date
  LOLkat_enddate <- x_labels2[x_labels_pos2_start][length(x_labels2[x_labels_pos2_start])] # Activity data end date

  ## Create axis and vertical day lines
  axis(side = 1, at = bp2[1 + x_labels_pos2_start], labels = x_labels2[x_labels_pos2_start], las = 2, cex.axis = 0.8)
  abline(v = bp2[1 + x_labels_pos2_start], col = "blue")


  ## Assign moving window results (remove first obs to account for non-24h day)
  if (EWS_count == 1){
  rollingwindow.results <- rollingwindow.results[2:nrow(rollingwindow.results), ]
  }

  ## Asssign occurences when days from plotted actigraphy data co-occur with days from the moving window results
  matched_dates <- paste(x_labels2[x_labels_pos2_start], "00:00:00") %in% rollingwindow.results$endtime

  ## Assign x-coordinates for moving window results
  plot_points_x <- bp2[1 + x_labels_pos2_start][matched_dates]


  # # plotme <- "Time_to_Recovery"
  # plotme <- colnames(rollingwindow.results)[10:21]
  #
  # for(EWS_count in 1:length(plotme)) {

  ## Plot results for moving window on existing barplot at designated x-coordinates
  #! N.b. Scaling should be made dynamic!
  scaling_var <- (bp2_ylim_upper / max(rollingwindow.results[(1:(nrow(rollingwindow.results) - 1)), plotme[EWS_count]]))

  ## Plotting "plotme" EWS over actogram
    ## Create right axis for EWS measure

  ## Exception for when length(at) != length(labels)
  #! Added na.omit() functions for when "to is not finite" error.
  if (length(seq(0, round(max(na.omit(rollingwindow.results[(1:(nrow(rollingwindow.results) - 1)), plotme[EWS_count]]))),
                by = (round(max(na.omit(rollingwindow.results[(1:(nrow(rollingwindow.results) - 1)), plotme[EWS_count]]))) / 2)))
     == length(c(0, (bp2_ylim_upper / 2), bp2_ylim_upper))) {

    axis(side = 4,
         labels = seq(0, round(max(na.omit(rollingwindow.results[(1:(nrow(rollingwindow.results) - 1)), plotme[EWS_count]]))),
                      by = (round(max(na.omit(rollingwindow.results[(1:(nrow(rollingwindow.results) - 1)), plotme[EWS_count]]))) / 2)),
         at = c(0, (bp2_ylim_upper / 2), bp2_ylim_upper))

  } else {

    axis(side = 4,
         labels = c((-bp2_ylim_upper), (-(bp2_ylim_upper / 2)), 0, (bp2_ylim_upper / 2), bp2_ylim_upper),
         at = c((-bp2_ylim_upper), (-(bp2_ylim_upper / 2)), 0, (bp2_ylim_upper / 2), bp2_ylim_upper))

  }

    ## Plot EWS over actogram (exceptions for when length of x- and y-values differ)
    if (length(plot_points_x) < length((rollingwindow.results[(1:(nrow(rollingwindow.results))), plotme[EWS_count]]
                                       * scaling_var))) {

        points(x = plot_points_x, type = "l", col = "red",
               y = (rollingwindow.results[(1:(nrow(rollingwindow.results))), plotme[EWS_count]]
                    * scaling_var)[1:length(plot_points_x)])

    } else if (length(plot_points_x) > length((rollingwindow.results[(1:(nrow(rollingwindow.results))), plotme] * scaling_var))) {

      points(x = plot_points_x[1:length(rollingwindow.results[(1:(nrow(rollingwindow.results))), plotme[EWS_count]])],
             type = "l", col = "red", y = (rollingwindow.results[(1:(nrow(rollingwindow.results))), plotme[EWS_count]]
                                           * scaling_var))

    } else if (length(plot_points_x) == length((rollingwindow.results[(1:(nrow(rollingwindow.results))),
                                                                     plotme[EWS_count]] * scaling_var))) {

      points(x = plot_points_x, type = "l", col = "red",
             y = (rollingwindow.results[(1:(nrow(rollingwindow.results))), plotme[EWS_count]] * scaling_var))
     }

  ## Create Title
  mtext(paste("Total Activity data with", plotme[EWS_count], "moving window"), line = 1, cex = 1.0)
  mtext(paste("start date:", LOLkat_startdate, "end date:", LOLkat_enddate), line = 0, cex = 0.8)

  dev.off()
  }

}


  ### /Work in Progress ------------------------------------------------------------------------------


  if (plotactogram ==  "48h") {

    ## Initialise .PDF plot in A4 size (11.7 x 8.3 inches)
    pdf(paste("Actigraphy Data - 48h Plot", i, ".pdf"), width = 11.7, height = 8.3)


     ## Plot initialisation & parameters
      par(mfrow = c(14, 1)) # Set plot parameters
      par(mar = c(0.5, 4, 0.5, 4)) # Set margins
      # par(mai = c(1, 1, 1, 1))
      bp <- barplot(day.1.2$Activity, ylim = ylimit, ylab = "Day 1", plot = FALSE)
      barplot(day.1.2$Activity, ylim = ylimit, ylab = "Day 1")
      # abline(v = day2start + 75, lty = 2) # Set start moment
      x_labels <- substr(day.1.2$Date, nchar(day.1.2$Date) - 8 + 1, nchar(day.1.2$Date))
      x_labels_pos <- grep("00:00", x_labels)
      x_labels <- x_labels[x_labels_pos]
      x_labels <- substr(x_labels, start = 1, stop = 5)
      axis(side = 1, at = bp[1 + x_labels_pos[ c(TRUE, FALSE)]], labels = x_labels[ c(TRUE, FALSE)])
      axis(side = 1, at = bp[1 + x_labels_pos[ c(FALSE, TRUE)]], labels = FALSE, col.ticks = "red")


      ## Filling in plot with loop for other days
      for (k.plot in 2:(ndays.plot - 1)) {

        barplot(eval(parse(text = paste("day.", k.plot, ".", (k.plot + 1), "$Activity", sep = ""))),
                ylim = ylimit, ylab = paste("Day", k.plot))

        x_labels <- substr(day.2.3$Date, nchar(day.2.3$Date) - 8 + 1, nchar(day.2.3$Date))
        x_labels_pos <- grep("00:00", x_labels)
        x_labels <- x_labels[x_labels_pos]
        x_labels <- substr(x_labels, start = 1, stop = 5)

        axis(side = 1, at = bp[1 + x_labels_pos[ c(TRUE, FALSE)]], labels = x_labels[ c(TRUE, FALSE)])
        axis(side = 1, at = bp[1 + x_labels_pos[ c(FALSE, TRUE)]], labels = FALSE, col.ticks = "red")
      }

      dev.off()

  }


  ### Part 5: 24 hour plot ------------------------------------------------------------------------

  ## 24 hour plot
  if (plotactogram == "24h") {

    ## Initialise .PDF plot in A4 size (11.7 x 8.3 inches)
    pdf(paste("Actigraphy Data - 24h Plot", i, ".pdf"), width = 11.7, height = 8.3)

      par(mfrow = c(14, 1)) # Set parameters for plots
      par(mar = c(0.5, 4, 0.5, 4)) # Set margins
      bp <- barplot(day1$Activity, ylim = ylimit,
                    ylab = substr(day1$Date[(which(day1$Date == "0")[length(which(day1$Date == "0"))] + 1)], 6, 10),
                    plot = F)
      barplot(day1$Activity, ylim = ylimit,
              ylab = substr(day1$Date[(which(day1$Date == "0")[length(which(day1$Date == "0"))] + 1)], 6, 10),
              plot = T)
      # abline(v = day2start+75, lty = 2) # Add start line (+75 = marge)

      x_labels <- substr(day2$Date, nchar(day2$Date) - 8 + 1, nchar(day2$Date))
      x_labels_pos <- grep("00:00", x_labels)
      x_labels <- x_labels[x_labels_pos]

      axis(side = 1, at = bp[1 + x_labels_pos], labels = x_labels)

      ## Filling in plot with loop for other days
      for (k.plot in 2:(ndays.plot - 1)) {

        barplot(eval(parse(text = paste("day", k.plot, "$Activity", sep = ""))),
                ylim = ylimit,
                ylab = substr((eval(parse(text = paste("day", k.plot, "$Date", sep = ""))))[1], 6, 10)
                )

        # axis(side = 1, at = (x_labels_pos), labels = x_labels)
        axis(side = 1, at = bp[1 + x_labels_pos], labels = x_labels)

      }

      dev.off()

}
  ### Part 6: Finishing Operations -------------------------------------------------------------------

  setwd(workdir.save)

}
