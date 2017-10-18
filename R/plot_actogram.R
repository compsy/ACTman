#### Actogram creation ####
#### Original Script author: Stefan Knapen ####
#### Edited by: YKK ####
#
# Goal: plot activity counts over time, with 24 hour lines (or 48 hours).
# number of minutes per day: 1440
#
#### TO ADD
# Grey part before start
# Hour variables underneath
# Export to PDF function
# Title
# Days more clearly written
#' plot_actogram
#'
#' Plots the actograms.
#'
#' @param CRV_data is the CRV.data object.
#'
plot_actogram <- function(CRV_data) {
  pdf("Actigraphy Data - Plot.pdf")
  # Load data
  act_data <- CRV_data
  #! Deze variabele wordt nergens gebruikt? (Heb m gecomment)
  #Days_count <- length(unique(substr(act_data$Date, start = 1, stop = 10))) # Return number of days

  # Midnight Detection 2nd day
  day2_start <- which(substr(act_data$Date, start = 12, stop = 19) == "00:00:00") # Start of day2 at midnight.
  day2.start <- day2_start[1] # To remove all other midnight startingpoints.
  day2start <- day2.start - 1

  day1 <- dplyr::slice(act_data, 1:(day2start))
  day1.rest <- 1440 - day2start

  # Use this starting point as a way to calculate days, make every day a seperate variable, and then plot everything after each other.
  temprow <- matrix(c(rep.int(0, length(day2start))), nrow = day1.rest, ncol = 2) # create vector to add rest of day one to.
  newrow <- data.frame(temprow)
  colnames(newrow) <- colnames(day1)
  day1 <- rbind(newrow, day1)

  # Other days
  #! Hier gewoon een for loop van maken met een array voor de day waarden ipv dit allemaal los op te schrijven.
  day2 <- dplyr::slice(act_data, (day2start + 1):(day2start + 1440))
  day3 <- dplyr::slice(act_data, (day2start + 1441):(day2start + (2 * 1440)))
  day4 <- dplyr::slice(act_data, (day2start + (2 * 1440) + 1):(day2start + (3 * 1440)))
  day5 <- dplyr::slice(act_data, (day2start + (3 * 1440) + 1):(day2start + (4 * 1440)))
  day6 <- dplyr::slice(act_data, (day2start + (4 * 1440) + 1):(day2start + (5 * 1440)))
  day7 <- dplyr::slice(act_data, (day2start + (5 * 1440) + 1):(day2start + (6 * 1440)))
  day8 <- dplyr::slice(act_data, (day2start + (6 * 1440) + 1):(day2start + (7 * 1440)))
  day9 <- dplyr::slice(act_data, (day2start + (7 * 1440) + 1):(day2start + (8 * 1440)))
  day10 <- dplyr::slice(act_data, (day2start + (8 * 1440) + 1):(day2start + (9 * 1440)))
  day11 <- dplyr::slice(act_data, (day2start + (9 * 1440) + 1):(day2start + (10 * 1440)))
  day12 <- dplyr::slice(act_data, (day2start + (10 * 1440) + 1):(day2start + (11 * 1440)))
  day13 <- dplyr::slice(act_data, (day2start + (11 * 1440) + 1):(day2start + (12 * 1440)))
  day14 <- dplyr::slice(act_data, (day2start + (12 * 1440) + 1):(day2start + (13 * 1440)))

  ylimit <- range(na.omit(c(day1$Activity,
                            day2$Activity,
                            day3$Activity,
                            day4$Activity,
                            day5$Activity,
                            day6$Activity,
                            day7$Activity,
                            day8$Activity,
                            day9$Activity,
                            day10$Activity,
                            day11$Activity,
                            day12$Activity,
                            day13$Activity,
                            day14$Activity))) # Define the range so all plots will be equal height.


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

  ###########±±±±±±±±±±±±###########
  # DOUBLEPLOT #
  ###########±±±±±±±±±±±±###########

  # 48 hour plot

  # Combine days
  #! Dit in een loop zetten
  day.1.2 <- rbind(day1, day2)
  day.2.3 <- rbind(day2, day3)
  day.3.4 <- rbind(day3, day4)
  day.4.5 <- rbind(day4, day5)
  day.5.6 <- rbind(day5, day6)
  day.6.7 <- rbind(day6, day7)
  day.7.8 <- rbind(day7, day8)
  day.8.9 <- rbind(day8, day9)
  day.9.10 <- rbind(day9, day10)
  day.10.11 <- rbind(day10, day11)
  day.11.12 <- rbind(day11, day12)
  day.12.13 <- rbind(day12, day13)
  day.13.14 <- rbind(day13, day14)

  # Plots
  par(mfrow = c(14, 1)) # Set plot parameters
  par(mar = c(0.5, 4, 0.5, 4)) # Set margins
  barplot(day.1.2$Activity, ylim = ylimit, ylab = "Day 1")
  abline(v = day2start + 75, lty = 2) # Set start moment
  #! Dit in een loop zetten
  barplot(day.2.3$Activity, ylim = ylimit, ylab = "Day 2")
  barplot(day.3.4$Activity, ylim = ylimit, ylab = "Day 3")
  barplot(day.4.5$Activity, ylim = ylimit, ylab = "Day 4")
  barplot(day.5.6$Activity, ylim = ylimit, ylab = "Day 5")
  barplot(day.6.7$Activity, ylim = ylimit, ylab = "Day 6")
  barplot(day.7.8$Activity, ylim = ylimit, ylab = "Day 7")
  barplot(day.8.9$Activity, ylim = ylimit, ylab = "Day 8")
  barplot(day.9.10$Activity, ylim = ylimit, ylab = "Day 9")
  barplot(day.10.11$Activity, ylim = ylimit, ylab = "Day 10")
  barplot(day.11.12$Activity, ylim = ylimit, ylab = "Day 11")
  barplot(day.12.13$Activity, ylim = ylimit, ylab = "Day 12")
  barplot(day.13.14$Activity, ylim = ylimit, ylab = "Day 13")

  ###########±±±±±±±±±±±±###########
  # DOUBLEPLOT #
  ###########±±±±±±±±±±±±###########

  # 48 hour plot

  # Combine days
  #! Dit in een loop zetten
  day.1.2 <- rbind(day1, day2)
  day.2.3 <- rbind(day2, day3)
  day.3.4 <- rbind(day3, day4)
  day.4.5 <- rbind(day4, day5)
  day.5.6 <- rbind(day5, day6)
  day.6.7 <- rbind(day6, day7)
  day.7.8 <- rbind(day7, day8)
  day.8.9 <- rbind(day8, day9)
  day.9.10 <- rbind(day9, day10)
  day.10.11 <- rbind(day10, day11)
  day.11.12 <- rbind(day11, day12)
  day.12.13 <- rbind(day12, day13)
  day.13.14 <- rbind(day13, day14)

  # Plots
  par(mfrow = c(14, 1)) # Set plot parameters
  par(mar = c(0.5, 4, 0.5, 4)) # Set margins
  barplot(day.1.2$Activity, ylim = ylimit, ylab = "Day 1")
  abline(v = day2start + 75, lty = 2) # Set start moment
  #! Dit in een loop zetten
  barplot(day.2.3$Activity, ylim = ylimit, ylab = "Day 2")
  barplot(day.3.4$Activity, ylim = ylimit, ylab = "Day 3")
  barplot(day.4.5$Activity, ylim = ylimit, ylab = "Day 4")
  barplot(day.5.6$Activity, ylim = ylimit, ylab = "Day 5")
  barplot(day.6.7$Activity, ylim = ylimit, ylab = "Day 6")
  barplot(day.7.8$Activity, ylim = ylimit, ylab = "Day 7")
  barplot(day.8.9$Activity, ylim = ylimit, ylab = "Day 8")
  barplot(day.9.10$Activity, ylim = ylimit, ylab = "Day 9")
  barplot(day.10.11$Activity, ylim = ylimit, ylab = "Day 10")
  barplot(day.11.12$Activity, ylim = ylimit, ylab = "Day 11")
  barplot(day.12.13$Activity, ylim = ylimit, ylab = "Day 12")
  barplot(day.13.14$Activity, ylim = ylimit, ylab = "Day 13")

  dev.off()
}
