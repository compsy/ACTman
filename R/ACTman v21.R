###########################################################
### ACTman package                                      ###
### Script authors: Yoram Kunkels & Stefan Knapen       ###
### Edited on: 23/06/2017                               ###
### Supported devices: Actiwatch 2 Respironics          ###
###                    & MW8                            ###
### > Made ACTman function & added arguments            ###
### > Runs sleep or CRV analyses                        ###
###~@~@~@~@~@~@~@~@~@~@~@~@~@~@~@~@~@~@~@~@~@~@~@~@~@~@~###

# Set ACTman function arguments
workdir <- "C:/Bibliotheek/Studie/PhD/Publishing/ACTman/R-part/mydata2" # mydata2 = Actiwatch2, mydata3 = MW8 sleep
myACTdevice <- "Actiwatch2" # Either Actiwatch2 or MW8
# iwantsleepanalysis <- TRUE # TRUE for mydata3, FALSE for others

ACTman <- function(workdir, myACTdevice, iwantsleepanalysis, plotactogram, selectperiod, startperiod, daysperiod, tune){

## Set myACTdevice, iwantsleepanalysis, and "data2" accordingly !!!

## Step 1: Basic Operations:
# Load and/or install required libraries
if(!require('nparACT')){install.packages('nparACT', dep = TRUE)};library('nparACT')
if(!require('gridExtra')){install.packages('gridExtra', dep = TRUE)};library('gridExtra')
if(!require('beepr')){install.packages('beepr', dep = TRUE)};library('beepr')

# Set working directory
# workdir <- "C:/Bibliotheek/Studie/PhD/Publishing/ACTman/R-part/mydata3" # mydata2 = Actiwatch2, mydata3 = MW8 sleep
setwd(workdir)

# Set ACTman function arguments
# myACTdevice <- "MW8" # Either Actiwatch2 or MW8
# iwantsleepanalysis <- TRUE # TRUE for mydata3, FALSE for others

# List files and initiate overview file
if(iwantsleepanalysis == TRUE){ # iwantsleepanalysis determines input .csv's because of added sleeplog .csv
  ACTdata.files <- list.files(setwd(workdir), pattern = "NK_data.csv")
}else{
  ACTdata.files <- list.files(setwd(workdir), pattern = ".csv")
}


ACTdata.overview <- data.frame("filename" = ACTdata.files, "start" = NA, "end" = NA, "end2" = NA , "end3" = NA,
                               "numberofobs" = NA, "numberofobs2" = NA, "numberofobs3" = NA, "recordingtime" = NA,
                               "recordingtime2" = NA, "recordingtime3" = NA, "summertime.start" = NA,
                               "summertime.end" = NA, "missings" = NA, "IS" = NA, "IV" = NA, "RA" = NA, "L5" = NA,
                               "L5_starttime" = NA, "M10" = NA, "M10_starttime" = NA, "r2.IS" = NA, "r2.IV" = NA,
                               "r2.RA" = NA, "r2.L5" = NA, "r2.L5_starttime" = NA, "r2.M10" = NA, "r2.M10_starttime" = NA,
                               "lengthcheck" = NA,"last5act.active" = NA)
# Initiate loop parameters
i <- 1 # set i
secshour <- 60*60 # Seconds per hour
secsday <- 24*secshour # Seconds per day
secs14day <- secsday*14 # Seconds in 14 days
minsaday <- (secsday/60) # Minutes per day


## END OF Step 1: Basic Operations.----------------------------------------------------------------------------

## Step 2: Loop:
## Loop Description: for each of the files listed in working directory, ...
pdf("Actigraphy Data - Plot.pdf") # Initialise .pdf plotting
for(i in 1:length(ACTdata.files)){

  print(paste("*** Start of Dataset", i, "***"))
  print("")
  print(paste("Dataset Name:", ACTdata.overview[i,"filename"]))
  print("")

  # Read and manage data:
  if(myACTdevice == "Actiwatch2"){ # Device-specific Data Management
    ACTdata.1 <- read.csv(paste(ACTdata.files[i]), header = FALSE)
    ACTdata.1.sub <- ACTdata.1[, c(4, 5, 6)]
    colnames(ACTdata.1.sub) <- c("Date", "Time", "Activity")
  }


  if(myACTdevice == "MW8"){ # Device-specific Data Management
    ACTdata.1 <- read.csv(paste(ACTdata.files[i]), header = FALSE, fill = TRUE, stringsAsFactors = FALSE, col.names = c("A", "B","C"))
    # ACTdata.1 <- read.csv2(paste(ACTdata.files[i]), header = TRUE, stringsAsFactors = FALSE)

    ACTdata.1 <- as.data.frame(ACTdata.1[((which(ACTdata.1[,1] == "Raw data:"))+2):nrow(ACTdata.1),])

    ACTdata.1.sub <- ACTdata.1
    colnames(ACTdata.1.sub) <- c("Date", "Time", "Activity")
    ACTdata.1.sub$Activity <- as.numeric(ACTdata.1.sub$Activity)
  }


  ## Step 2.1: Managing the Data
  # Reformat dates and times into required format for further processing:
  ACTdata.1.sub$Date <- gsub(pattern = "/20", replacement = "/", x = ACTdata.1.sub$Date) # Take only last two year digits
  ACTdata.1.sub$Date <- paste(ACTdata.1.sub$Date,ACTdata.1.sub$Time) # Merge Date Time, format: "2016-01-04 20:00:00"

  if(grepl("-", ACTdata.1.sub$Date[1])){
    # ACTdata.1.sub$Date <- strptime(ACTdata.1.sub$Date[1], "%d%m/%y %H:%M:%S") # Reformat Date
    format(as.POSIXct(ACTdata.1.sub$Date), "%d-%m-%y %H:%M:%S")
  }else{
    ACTdata.1.sub$Date <- strptime(ACTdata.1.sub$Date, "%d/%m/%y %H:%M:%S") # Reformat Date
  }

  # ACTdata.1.sub$Date <- as.POSIXct(ACTdata.1.sub$Date)

  ACTdata.1.sub$Time <- NULL # Remove empty Time variable

  # Check for empty first row and if so, remove it:
  if(all(is.na(ACTdata.1.sub[1,]))){ACTdata.1.sub <- ACTdata.1.sub[-1,]} # If first row is empty, remove it.

  # Add Period Selection
  # Custom enddates should be added
  if(selectperiod == TRUE){
    # startperiod <- ACTdata.1.sub$Date[1]+secsday # Test startperiod
    startperiod.loc <- which(ACTdata.1.sub$Date == startperiod[i])

    if(daysperiod != FALSE){
    ACTdata.1.sub <- ACTdata.1.sub[(startperiod.loc:(startperiod.loc+(daysperiod*minsaday))),]
    }else{ACTdata.1.sub <- ACTdata.1.sub[(startperiod.loc:(nrow(ACTdata.1.sub))),]}
  }

  # Write start- and end dates and times of actigraph data to overview file:
  ACTdata.overview[i,"start"] <- as.character(ACTdata.1.sub$Date[1]) # Write start date to overview
  ACTdata.overview[i,"end"] <- as.character(ACTdata.1.sub$Date[nrow(ACTdata.1.sub)]) # Write end date to overview

  # Assign recordingtime as time difference between start and end date to overview file:
  ACTdata.overview[i,"recordingtime"] <- round((as.POSIXct(ACTdata.1.sub$Date[1]) - as.POSIXct(ACTdata.1.sub$Date[nrow(ACTdata.1.sub)])), 2) # write recordingtime to overview

  # Write starting number of obs. to overview file:
  ACTdata.overview[i,"numberofobs"] <- nrow(ACTdata.1.sub)

  # Check for Activity in Last 5 observations:
  ACTdata.1.sub.last <- ACTdata.1.sub$Activity[(nrow(ACTdata.1.sub)-4):nrow(ACTdata.1.sub)]

  # Check if Activity in Last 5 observations contain substantial activity (on average, 5 activity counts per obs.)
  ACTdata.1.sub.done <- sum(ACTdata.1.sub.last) > 5*length(ACTdata.1.sub.last)

  # Identify Last Whole 24 Hour component and its Position: (EDITED for NK_data)
  ACTdata.1.sub.lastwhole24h <- ACTdata.1.sub[tail(grep("00:00:00", ACTdata.1.sub$Date),2), "Date"]
  ACTdata.1.sub.lastwhole24h <- ACTdata.1.sub.lastwhole24h[1]
  # ACTdata.1.sub.lastwhole24h <- as.POSIXct(ACTdata.overview[i,"start"]) + secs14day - secsday # Last whole 24 hour component

  # ACTdata.1.sub.lastwhole24h.pos <<- which(ACTdata.1.sub$Date == as.character(ACTdata.1.sub.lastwhole24h))
  ACTdata.1.sub.lastwhole24h.pos <<- tail(grep("00:00:00", ACTdata.1.sub$Date),2)[1]
  # ACTdata.1.sub.lastwhole24h.pos <- tail(which((grepl(substr(ACTdata.1.sub.lastwhole24h, 12, 19), ACTdata.1.sub$Date)) == TRUE), n = 1) # Position of Last whole 24 hour component

  # ACTdata.1.sub.lastwhole24h.pos <- which(ACTdata.1.sub$Date == ACTdata.1.sub.lastwhole24h) # Position of Last whole 24 hour component

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
  if(summertime.start.indata == TRUE){
    print("Warning: Start of Summertime Detected!")
    ACTdata.1.sub.14day <- as.POSIXct(ACTdata.1.sub$Date[1]) + secs14day # Start Date plus 14 days
    ACTdata.1.sub.14day <- ACTdata.1.sub.14day - secshour
    ACTdata.overview$summertime.start[i] <- TRUE
    print("Action: Subtracted the one hour excess from 14day length.")
    print("Task 1 DONE: Dataset corrected for Summertime.")
    print("")
  }else{
    ACTdata.1.sub.14day <- as.POSIXct(ACTdata.1.sub$Date[1]) + secs14day # Start Date plus 14 days
    print("Task 1 OK: No Start of Summertime in Dataset.")
    print("")
  }

  ## If Dataset is longer than Start Date plus 14 days, Remove data recorded thereafter:
  print("Task 2: Detecting if Dataset is longer than Start Date plus 14 full days")
  if(ACTdata.1.sub[nrow(ACTdata.1.sub),"Date"] > ACTdata.1.sub.14day){
    print("Warning: Dataset is longer than Start Date plus 14 full days!")

    # ACTdata.1.sub <- ACTdata.1.sub[1:which(ACTdata.1.sub$Date == ACTdata.1.sub.14day),]
    ACTdata.1.sub <- ACTdata.1.sub[1:((secs14day/60)+1),]

    print("Action: Observations recorded after Start Date plus 14 full days were removed.")
    print("Task 2 DONE: Dataset shortened to Start Date plus 14 days. Tasks 3, 4, and 5 also OK")
    print("")
    ACTdata.overview$lengthcheck[i] <- TRUE
  }else{
    print("Task 2 OK: Dataset not longer than Start Date plus 14 days.")
    print("")
  }


  # Update overview after removal
  ACTdata.overview[i,"numberofobs2"] <- nrow(ACTdata.1.sub)
  ACTdata.overview[i,"recordingtime2"] <- round(as.POSIXct(ACTdata.1.sub$Date[1]) - as.POSIXct(ACTdata.1.sub$Date[nrow(ACTdata.1.sub)]), 2)
  ACTdata.overview[i,"end2"] <- as.character(ACTdata.1.sub$Date[nrow(ACTdata.1.sub)]) # write end date to overview

  ## Remove NA's
  print("Task 6: Reporting NA's")
  ACTdata.overview[i,"missings"] <- table(is.na(ACTdata.1.sub))["TRUE"]  # write missings to overview
  ACTdata.1.sub <- na.omit(ACTdata.1.sub)
  print(paste("Number of NA's in this Dataset:", ACTdata.overview[i,"missings"]))
  print("")

  ## If Activity in Last 5 observations is on average zero, Skip to Last Activity:
  ACTdata.1.sub.last5act <- ACTdata.1.sub$Activity[(nrow(ACTdata.1.sub)-4):nrow(ACTdata.1.sub)] # Last 5 activity counts in dataset
  ACTdata.1.sub.last5act.active <- sum(ACTdata.1.sub.last5act) >= (5*length(ACTdata.1.sub.last5act)) # Is there on average more than 5 counts per obs?
  print("Task 7: Checking for Activity in Last 5 observations")
  if(ACTdata.1.sub.last5act.active == FALSE){
    print("Warning: No Activity in Last 5 observations!")
    print("Last 5 Activity Counts, before Correction:")
    print(ACTdata.1.sub.last5act)
    ACTdata.1.sub <- ACTdata.1.sub[1:max(which(ACTdata.1.sub$Activity >= (5*length(ACTdata.1.sub.last5act)))),] # Shortens data untill reached last activity
    ACTdata.1.sub.last5act <- ACTdata.1.sub$Activity[(nrow(ACTdata.1.sub)-4):nrow(ACTdata.1.sub)] # Last 5 activity counts in dataset
    ACTdata.overview$last5act.active[i] <- FALSE
    print("Last 5 Activity Counts, after Correction:")
    print(ACTdata.1.sub.last5act)
    print("Task 7 DONE: Dataset Skipped to last Activity.")
    print("")
  }else{
    print("Task 7 OK: Dataset contained Activity in Last 5 observations.")
    print("")
  }

  ## END OF Step 2.1: Managing the Data
  ## Assign Original & Managed data as global
  ACTdata.1 <<- ACTdata.1
  ACTdata.1.sub <<- ACTdata.1.sub

  # Update overview file after NA- and non-activity removal
  ACTdata.overview[i,"numberofobs3"] <- nrow(ACTdata.1.sub)
  ACTdata.overview[i,"recordingtime3"] <- round(as.POSIXct(ACTdata.1.sub$Date[1]) - as.POSIXct(ACTdata.1.sub$Date[nrow(ACTdata.1.sub)]), 2)
  ACTdata.overview[i,"end3"] <- as.character(ACTdata.1.sub$Date[nrow(ACTdata.1.sub)]) # write end date to overview

  ## Use nparACT Package to calculate Experimental Variables
  ## Pre-process!!

  dir.create(file.path(workdir, "Managed Datasets"))
  setwd(file.path(workdir, "Managed Datasets"))

  wd <- getwd()
  name <- paste(gsub(pattern = ".csv", replacement = "", x = ACTdata.files[i]))
  newdir <- paste(wd,name, sep="/")
  dir.create(newdir)
  setwd(newdir)

  write.table(ACTdata.1.sub, row.names=FALSE, col.names = FALSE,
              file = paste(gsub(pattern = ".csv", replacement = "", x = ACTdata.files[i]),"MANAGED.txt"))

  # Calculate IS, etc. with IS Calculation Module (Check version for correct operation!)
  ACTman.nparcalc <- function(){
        ## Read Data
        CRV.data <<- read.table(file = file.path(newdir, paste(gsub(pattern = ".csv", replacement = "", x = ACTdata.files[i]),"MANAGED.txt")))

        if(ncol(CRV.data) > 2){
          CRV.data$Date <- paste(CRV.data$V1, " ", CRV.data$V2)
          CRV.data$Date <- as.POSIXct(CRV.data$Date)
          CRV.data$Activity <- CRV.data$V3
          CRV.data <- CRV.data[,c("Date","Activity")]
        }else{colnames(CRV.data) <- c("Date","Activity")}


        ## Prune data untill only full 24h days are obtained
        CRV.data.wholehours <- CRV.data[grep("00:00", CRV.data[, "Date"]), ]
        CRV.data.start <- which(CRV.data$Date == CRV.data.wholehours[1, "Date"])

        if(myACTdevice == "MW8"){
          CRV.data.end <- ACTdata.1.sub.lastwhole24h.pos

          # # Identify Last Whole 24 Hour component and its Position: (EDITED for NK_data)
          # ACTdata.1.sub.lastwhole24h <- as.POSIXct(ACTdata.overview[i,"start"]) + secs14day - secsday # Last whole 24 hour component
          # ACTdata.1.sub.lastwhole24h.pos <- tail(which((grepl(substr(ACTdata.1.sub.lastwhole24h, 12, 19), ACTdata.1.sub$Date)) == TRUE), n = 1) # Position of Last whole 24 hour component
          # CRV.data.end <- CRV.data.wholehours[(nrow(CRV.data.wholehours)),]

        }else{CRV.data.end <- which(CRV.data[, "Date"] == CRV.data.wholehours[1, "Date"] + (secsday*13))}

        # CRV.data.end <<- ACTdata.1.sub.lastwhole24h.pos
        CRV.data <- CRV.data[CRV.data.start:CRV.data.end,]
        test.data <<- CRV.data # Checking if nparACT and IS Calculation Module use the same data (Seems OK)

        ##---------------------------------------------------------------------------------------------------------

        ## IS: Interdaily stability
        ## Source: Van Someren, E., Swaab, D., Colenda, C., Cohen, W., McCall, W. and Rosenquist, P. (1999).
        ## Bright Light Therapy: Improved Sensitivity to Its Effects on Rest-Activity Rhythms in Alzheimer Patients
        ## by Application of Nonparametric Methods. Chronobiology International, 16(4), pp.505-518.

        ## Xi
        xi <- aggregate(CRV.data[, "Activity"],
                        list(hour=cut(as.POSIXct(CRV.data[, "Date"]), breaks = "hour")),
                        mean)

        xi <- xi[1:(nrow(xi)-1),]
        xi <- xi$x

        # X
        X <- mean(xi, na.rm=T)

        xi_X <- xi-X # difference consecutive hourly means and overall mean
        sq.xi_X <- xi_X^2 # square of differences
        sum.sq.xi_X <- sum(sq.xi_X, na.rm=T) # sum of squares
        n <- sum(!is.na(xi)) # get number og hours (should be 168 for 7 day intervals (7*24))
        sum.sq.xi_X.perhour <- sum.sq.xi_X/n # sum of squares per hour


        Xh <- rowMeans(matrix(xi, nrow=24), na.rm=T)
        Xh_X <- Xh-X # difference 24 hour means and overall mean
        sq.Xh_X <- Xh_X^2 # square of difference
        sum.sq.Xh_X <- sum(Xh_X^2, na.rm=T) # sum of squares
        sum.sq.Xh_X.perhour <- sum.sq.Xh_X/24 # sum of squares per hour

        IS <- sum.sq.Xh_X.perhour/sum.sq.xi_X.perhour # hourly variance divided by overall variance
        IS <<- round(x = IS, digits = 2);IS

        ##---------------------------------------------------------------------------------------------------------

        ## IV: Interdaily Variability
        Xi_diffXi <- diff(xi) # difference Xi and previous Xi (the difference between all successive hours) (!!! Warning because no previous hour for 1st obs.)
        sum.sq.Xi_diffXi <- sum(Xi_diffXi^2, na.rm=T) # sum of squares
        sum.sq.Xi_diffXi.perhour <- sum.sq.Xi_diffXi/n # sum of squares per hour

        Xi_X <- xi-X # difference Xi and overall mean
        sq.Xi_X <- Xi_X^2 # square of difference
        sum.sq.Xi_X <- sum(Xi_X^2, na.rm=T) # sum of squares
        sum.sq.Xi_X.perhour <- sum.sq.Xi_X/(n-1) # sum of squares per hour minus 1

        IV <- sum.sq.Xi_diffXi.perhour / sum.sq.Xi_X.perhour
        IV <<- round(x = IV, digits = 2);IV

        ##---------------------------------------------------------------------------------------------------------

        ## L5: Average of the 5 Lowest Hourly Means (within each day(!))
        ## daily minute means

        # CRV.data.locmidnight <- grep(pattern = "12:00:00", x = CRV.data[, "Date"]) # Locations of midnight (!!!Is "12:00:00" midnight or "00:00:00"???)
        CRV.data.locmidnight <- grep(pattern = "00:00:00", x = CRV.data[, "Date"]) # Locations of midnight (!!!Is "12:00:00" midnight or "00:00:00"???)
        # CRV.data.locmidnight <- grep(pattern = strftime(ACTdata.1.sub$Date[1], format = "%H:%M:%S"), x = CRV.data[, "Date"])

        TEST.df.L5 <- data.frame("L5" <- NA, "L5_starttime" <- NA)

        for(f in 1:(length(CRV.data.locmidnight)-1)){

          CRV.data.full24minavg <- CRV.data[(CRV.data.locmidnight[f]:CRV.data.locmidnight[f+1]),]
          colnames(CRV.data.full24minavg) <- c("hour", "x")

          CRV.data.full24minavg <- CRV.data.full24minavg[1:1440,]

          CRV.data.loc.L5 <- NA
          CRV.data.full24minavg.act <- CRV.data.full24minavg$x
          for(g in 1:length(CRV.data.full24minavg.act)){
            CRV.data.loc.L5[g] <- mean(c(CRV.data.full24minavg.act,CRV.data.full24minavg.act[c(1:300)])[c(g:(299+g))], na.rm=T)
          } # calculating mean activity in 300 minute intervals ~ 5 hours

          L5 <- min(CRV.data.loc.L5) #round(min(CRV.data.loc.L5), 2)
          # M10 <- max(CRV.data.loc.M10) #round(max(CRV.data.loc.M10), 2)
          TEST.df.L5[f,"L5"] <- L5
          # TEST.df[f,"M10"] <- M10

          # print(M10)

          L5M10frame <- data.frame(CRV.data.loc.L5=CRV.data.loc.L5) #, CRV.data.loc.M10=CRV.data.loc.M10)

          L5onset <- which(L5M10frame$CRV.data.loc.L5==L5)[1]/60 # locating the first value that equals L5 and get the number of hours from start



          # M10onset <- which(L5M10frame$CRV.data.loc.M10==M10)[1]/60 # locating the first value that equals M10 and get the number of hours from start

          L5_starttime <- as.POSIXct(CRV.data[CRV.data.locmidnight[f],"Date"]) + (L5onset*secshour)
          # print(CRV.data[CRV.data.locmidnight[f],"Date"] + (L5onset*secshour))
          # TEST.df[f,1] <- as.POSIXct(L5_starttime, origin = "1970-01-01")
          TEST.df.L5[f,"L5_starttime"] <- L5_starttime

        }

        L5 <<- mean(TEST.df.L5[,"L5"])
        L5.temp <- as.POSIXct(paste("1970-01-01", format(as.POSIXct(TEST.df.L5[,"L5_starttime"], origin = "1970-01-01"), "%H:%M:%S")))

        for(a in 1:length(L5.temp)){

          # print(as.POSIXct("1970-01-01 00:00:00 CET") - L5.temp[a])

          if(L5.temp[a] > "1970-01-01 21:00:00 CET"){
            L5.temp[a] <- (L5.temp[a] + (secshour*6 ))
          }
          # print(L5.temp[a])
        }


        L5.temp <- as.POSIXct(paste("1970-01-01", format(L5.temp, "%H:%M:%S")))

        L5_starttime <<- mean(L5.temp)

        ##---------------------------------------------------------------------------------------------------------

        ## M10: Average of the 10 Highest Hourly Means (within each day(!))
        ## daily minute means

        TEST.df <- data.frame("M10" <- NA, "M10_starttime" <- NA)

        for(f in 1:(length(CRV.data.locmidnight)-1)){

          # CRV.data.locmidnight <- grep(pattern = "00:00:00", x = CRV.data[, "Date"]) # Locations of midnight (!!!Is "12:00:00" midnight or "00:00:00"???)

          CRV.data.full24minavg <-  aggregate(CRV.data[(CRV.data.locmidnight[f]:CRV.data.locmidnight[f+1]), "Activity"],
                                              list(hour=cut(as.POSIXct(CRV.data[(CRV.data.locmidnight[f]:CRV.data.locmidnight[f+1]), "Date"]), breaks = "min")),
                                              mean)

          CRV.data.full24minavg <<- CRV.data.full24minavg


          CRV.data.full24minavg.act <- CRV.data.full24minavg$x
          CRV.data.loc.M10 <- NA
          for(h in 1:length(CRV.data.full24minavg.act)){
            CRV.data.loc.M10[h] <- mean(c(CRV.data.full24minavg.act,CRV.data.full24minavg.act[c(1:600)])[c(h:(599+h))], na.rm=T)
          } # calculating mean activity in 600 minute intervals ~ 10 hours
          # L5 <- min(CRV.data.loc.L5) #round(min(CRV.data.loc.L5), 2)
          M10 <- max(CRV.data.loc.M10) #round(max(CRV.data.loc.M10), 2)
          # TEST.df[f,"L5"] <- L5
          TEST.df[f,"M10"] <- M10

          # print(M10)

          L5M10frame <- data.frame(CRV.data.loc.M10=CRV.data.loc.M10)

          # L5onset <- which(L5M10frame$CRV.data.loc.L5==L5)[1]/60 # locating the first value that equals L5 and get the number of hours from start
          M10onset <- which(L5M10frame$CRV.data.loc.M10==M10)[1]/60 # locating the first value that equals M10 and get the number of hours from start

          # L5_starttime <- CRV.data[CRV.data.locmidnight[f],"Date"] + (L5onset*secshour)
          # print(CRV.data[CRV.data.locmidnight[f],"Date"] + (L5onset*secshour))
          # # TEST.df[f,1] <- as.POSIXct(L5_starttime, origin = "1970-01-01")
          # TEST.df[f,"L5_starttime"] <- L5_starttime

          M10_starttime <- as.POSIXct(CRV.data[CRV.data.locmidnight[f],"Date"]) + (M10onset*secshour)
          # print(CRV.data[CRV.data.locmidnight[f],"Date"] + (M10onset*secshour))
          # TEST.df[f,2] <- as.POSIXct(M10_starttime, origin = "1970-01-01")
          TEST.df[f,"M10_starttime"] <- M10_starttime

        }

          M10 <<- mean(TEST.df[,"M10"])

        ### !!!! HERE is the whole problem (!!!) a mean is calculated over differing days and times. However, with differing days
        ### strong discrepancies appear as WHOLE days are averaged. The SOLUTION is simple, attach the same day to all times.
        ### There is, however, a smaller problem with multiple M10 starttimes being found within one day (!!)

        M10_starttime <<- mean(as.POSIXct(paste("1970-01-01", format(as.POSIXct(TEST.df[,"M10_starttime"], origin = "1970-01-01"), "%H:%M:%S"))))

        Amp <<- M10-L5 # self explanatory
        RA <<- Amp/(L5+M10)
  }
  ACTman.nparcalc()
  r2 <- IS

  # Attach r2 output to overview
  ACTdata.overview[i,"r2.IS"] <- IS
  ACTdata.overview[i,"r2.IV"] <- IV
  ACTdata.overview[i,"r2.RA"] <- round(RA,2)
  ACTdata.overview[i,"r2.L5"] <- round(L5,2)
  ACTdata.overview[i,"r2.L5_starttime"] <- as.character(strftime(L5_starttime, format = "%H:%M:%S"))
  ACTdata.overview[i,"r2.M10"] <- round(M10,2)
  ACTdata.overview[i,"r2.M10_starttime"] <- as.character(strftime(M10_starttime, format = "%H:%M:%S"))


  # write.table(test.data, row.names=FALSE, col.names = FALSE,
  #             file = paste(gsub(pattern = ".csv", replacement = "", x = ACTdata.files[i]),"MANAGED.txt"))

  ## Use nparACT Package to calculate Experimental Variables

  # Calculate IS, etc. with nparACT
  r <- nparACT_base_loop(path = newdir, SR = 1/60, fulldays = T, plot = F)

  # Attach nparACT output to overview
  ACTdata.overview[i,"IS"] <- r$IS
  ACTdata.overview[i,"IV"] <- r$IV
  ACTdata.overview[i,"RA"] <- r$RA
  ACTdata.overview[i,"L5"] <- r$ L5
  ACTdata.overview[i,"L5_starttime"] <- r$L5_starttime
  ACTdata.overview[i,"M10"] <- r$M10
  ACTdata.overview[i,"M10_starttime"] <- r$M10_starttime


  # Reset working directory to main directory
  setwd(workdir)

  # Plot data (Actogrammen)
  if(plotactogram == TRUE){
  source(file = "Actogram ACTman READYv3.R",
         local = TRUE) # This script has to be put in the data folder(e.g. "data2")(!!)
  }

  ## Sleep Analysis Source
  ## Loop for sleep_calc
  ## (!!) "Sleep_calculation_functional v1" still has to be made dynamic, now only woks for NK_data (!!)
  if(iwantsleepanalysis == TRUE){
    tryCatch({
      source(file = "Sleep_calculation_functional v4WORKS.R",
             local = TRUE) # This script has to be put in the data folder(e.g. "data2")(!!)

      # Reset working directory to main directory
      setwd(workdir)

      sleepdata.overview <<- sleepdata.overview
      # # Attach Sleep Analysis output to overview
      # ACTdata.overview[i,"sleep.start"] <- sleep.start
      # ACTdata.overview[i,"sleep.end"] <- sleep.end
      # ACTdata.overview[i,"sleep.efficiency"] <- SleepEfficiency
      # ACTdata.overview[i,"sleep.latency"] <- SleepLatency
    })
  }



  print(paste("--------------------------------------", "END OF DATASET", i, "---", "@", round(i*(100/length(ACTdata.files))), "% DONE",  "--------------------------------------"))
}
dev.off()

## END OF Step 2: Loop.----------------------------------------------------------------------------------------

## Step 3: After loop processing:
# Transform negative recordingtime to positive
ACTdata.overview$recordingtime <- ((ACTdata.overview$recordingtime)^2)^(1/2)
ACTdata.overview$recordingtime2 <- ((ACTdata.overview$recordingtime2)^2)^(1/2)
# Assign zero to missings without missings
ACTdata.overview[is.na(ACTdata.overview[,"missings"]),"missings"] <- 0
# Subset experimental variables
ACTdata.1.sub.expvars <- ACTdata.overview[c("IS", "IV", "RA", "L5", "L5_starttime", "M10", "M10_starttime", "recordingtime2")]
colnames(ACTdata.1.sub.expvars) <- c("IS", "IV", "RA", "L5", "L5 Start time", "M10", "M10 Start time", "No. of Days")

# Export Experimental variables to .pdf
pdf("Experimental Variables - Table.pdf")
grid.table(ACTdata.1.sub.expvars)
dev.off()

if(iwantsleepanalysis == TRUE){
  View(sleepdata.overview)
}else{
  View(ACTdata.overview)
}

if(tune == TRUE){
  beep(3)
}

}

## END OF Step 3: After loop processing.------------------------------------------------------------------------


## Step 4: Start up ACTman

# startperiod.dates <- c("2014-04-18 18:30:00", "2014-05-03 17:00:00", "2015-02-05 17:00:00", "2014-08-15 15:00:00", "2015-04-03 15:00:00")

# ACTman(workdir = workdir, myACTdevice = myACTdevice, iwantsleepanalysis = FALSE, plotactogram = TRUE,
#        selectperiod = TRUE, startperiod = "2014-04-18 18:30:00",
#        tune = FALSE)

startperiod.dates <- c("2016-10-03 00:00:00")

ACTman(workdir = workdir, myACTdevice = myACTdevice, iwantsleepanalysis = FALSE, plotactogram = FALSE,
       selectperiod = FALSE, startperiod = startperiod.dates, daysperiod = 14,
       tune = FALSE)




