##############################################################################
### ACTman{plot_actogram}                                                  ###
### Script authors: Yoram Kunkels, Stefan Knapen, & Ando Emerencia         ###
### Most recent Update: 21-12-2017                                         ###
### Goal: plot activity counts over time, with 24 hour lines (or 48 hours) ###
###                                                                        ###
### To add: Grey part before start, Hour variables underneath, Title,      ###
### Export to PDF function, Days more clearly written                      ###
###~@~@~@~@~@~@~@~@~@~@~@~@~@~@~@~@~@~@~@~@~@~@~@~@~@~@~@~@~@~@~@~@~@~@~@~@###

#' plot_actogram
#'
#' Function to plot 48 hour Actograms.
#'
#' @param workdir the working directory as supplied to ACTman.
#'


plot_actogram <- function(workdir) {

### Part 1: Basic Operations ----------------------------------------------------------------------------

  # Set and save working directory
  workdir.save <- getwd()
  setwd(workdir)

  pdf("Actigraphy Data - Plot.pdf") # Initialise .PDF plot

  act_data <<- ACTdata.1.sub # Copy data for editing required for plotting
  act_data <- within(act_data, Date <- as.character(act_data$Date)) # Date as character for plotting
  ndays.plot <- round(abs(as.numeric(round(as.POSIXct(ACTdata.1.sub$Date[1]) - as.POSIXct(ACTdata.1.sub$Date[nrow(ACTdata.1.sub)]), 2))))


### Part 2: 1st day Selection & Midnight Detection 2nd day  ---------------------------------------------------

  ## Midnight Detection 2nd day
  day2start <- ((which(substr(act_data$Date, start = 12, stop = 19) == "00:00:00")[1]) - 1) # Start of day2 at midnight.

  ## 1st Day selection
  day1 <- act_data[1:day2start,]
  day1.rest <- 1440 - day2start

  ## Filling in period before 1st day selection to get full 24h in 1st day
  day1.rest.mat <-  matrix(data = 0, nrow = day1.rest, ncol = 2) # Create empty matrix
  colnames(day1.rest.mat) <- colnames(day1) # Equalise colnames
  # day1 <- within(day1, Date <- as.POSIXct(as.character(Date), format = "%Y-%m-%d %H:%M:%S")) # Do POSIXct magic (http://grokbase.com/t/r/r-help/112jk8eyqp/r-problem-with-rbind-when-data-frame-contains-an-date-time-variable-posixt-posixlt)
  day1 <- rbind(day1.rest.mat, day1) # Combine day1 data with empty pre-day1 matrix


### Part 3: Loop for assigning subsequent days (14 days max for plot) & setting ylim's ----------------------

  ## Assign other days
  for(i.plot in 2:(ndays.plot)){

    if(i.plot == 2){
      assign(paste("day", i.plot, sep = ""), act_data[((day2start + (1440 ^ (i.plot - 2))):(day2start + (1440 * (i.plot - 1)))), ])
    } else {
      assign(paste("day", i.plot, sep = ""), act_data[((day2start + ((1440 * (i.plot - 2)) + 1)):(day2start + (1440 * (i.plot - 1)))), ])
    }

  }

  ## Define the range so all plots will be equal height (ylim)
  ylimit <- range(na.omit(c(day1$Activity, day2$Activity, day3$Activity, day4$Activity, day5$Activity,
                            day6$Activity, day7$Activity, day8$Activity, day9$Activity, day10$Activity,
                            day11$Activity, day12$Activity, day13$Activity, day14$Activity)))


  ### Part 4: Combining Days & 48 hour Doubleplot -----------------------------------------------------------

  ## Combine days
  for(j.plot in 1:(ndays.plot - 1)){

    assign(paste("day.", j.plot, ".", (j.plot + 1), sep = ""),
           rbind(eval(parse(text = paste("day", j.plot, sep = ""))), eval(parse(text =  paste("day", (j.plot + 1), sep = "")))))

  }


  ## Plot initialisation & parameters
  par(mfrow = c(14, 1)) # Set plot parameters
  par(mar = c(0.5, 4, 0.5, 4)) # Set margins
  barplot(day.1.2$Activity, ylim = ylimit, ylab = "Day 1")
  abline(v = day2start + 75, lty = 2) # Set start moment

  ## Filling in plot with loop for other days
  for(k.plot in 2:(ndays.plot - 1)){

    barplot(eval(parse(text = paste("day.", k.plot, ".", (k.plot + 1), "$Activity", sep = ""))),
            ylim = ylimit, ylab = paste("Day", k.plot))

  }


  ### Part 5: 24 hour plot (to be added)---------------------------------------------------------------

  # # 24 hour plot
  # par(mfrow = c(14, 1)) # Set parameters for plots
  # par(mar = c(1, 4, 1, 4)) # Set margins so plots are closer together
  # barplot(day1$Activity, ylim = ylimit)
  # abline(v = day2start+75, lty = 2) # Add start line (+75 = marge)
  # barplot(day2$Activity, ylim = ylimit)
  # barplot(day3$Activity, ylim = ylimit)
  # barplot(day4$Activity, ylim = ylimit)
  # barplot(day5$Activity, ylim = ylimit)
  # barplot(day6$Activity, ylim = ylimit)
  # barplot(day7$Activity, ylim = ylimit)
  # barplot(day8$Activity, ylim = ylimit)
  # barplot(day9$Activity, ylim = ylimit)
  # barplot(day10$Activity, ylim = ylimit)
  # barplot(day11$Activity, ylim = ylimit)
  # barplot(day12$Activity, ylim = ylimit)
  # barplot(day13$Activity, ylim = ylimit)
  # barplot(day14$Activity, ylim = ylimit)

  ### Part 6: Finishing Operations -------------------------------------------------------------------
  dev.off()
  setwd(workdir.save)

}
