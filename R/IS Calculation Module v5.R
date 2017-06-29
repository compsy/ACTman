##################################################
### Calculate Circadian Rhythm Variables (CRV) ###
### YKK                                        ###
###~8~*~8~*~8~*~8~*~8~*~8~*~8~*~8~*~8~*~8~*~8~*###

## Read Data

CRV.data <- read.table(file = file.path(newdir, paste(gsub(pattern = ".csv", replacement = "", x = ACTdata.files[i]),"MANAGED.txt")))

# paste(newdir, "/", paste(gsub(pattern = ".csv", replacement = "", x = ACTdata.files[i]),"MANAGED.txt"))
#
# file.path(newdir, paste(gsub(pattern = ".csv", replacement = "", x = ACTdata.files[i]),"MANAGED.txt"))



# CRV.data <- read.table("C:/Bibliotheek/Studie/PhD/Publishing/ACTman/R-part/mydata/Managed Datasets/431-BG00023_17_04_2014_18_30_00/431-BG00023_17_04_2014_18_30_00 MANAGED.txt", stringsAsFactors = T)
CRV.data$Date <- paste(CRV.data$V1, " ", CRV.data$V2)
CRV.data$Date <- as.POSIXct(CRV.data$Date)
CRV.data$Activity <- CRV.data$V3
CRV.data <- CRV.data[,c("Date","Activity")]

## Prune data untill only full 24h days are obtained
CRV.data.wholehours <- CRV.data[grep("00:00", CRV.data[, "Date"]), ]
CRV.data.start <- which(CRV.data$Date == CRV.data.wholehours[1, "Date"])

if(myACTdevice == "MW8"){
  CRV.data.end <- ACTdata.1.sub.lastwhole24h.pos
}else{CRV.data.end <- which(CRV.data[, "Date"] == CRV.data.wholehours[1, "Date"] + (secsday*13))}

CRV.data <- CRV.data[CRV.data.start:CRV.data.end,]


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
IS <- round(x = IS, digits = 2);IS

## Check with nparACT output
# View(ACTdata.1.sub.expvars)


## IV: Interdaily Variability
Xi_diffXi <- diff(xi) # difference Xi and previous Xi (the difference between all successive hours) (!!! Warning because no previous hour for 1st obs.)
sum.sq.Xi_diffXi <- sum(Xi_diffXi^2, na.rm=T) # sum of squares
sum.sq.Xi_diffXi.perhour <- sum.sq.Xi_diffXi/n # sum of squares per hour 

Xi_X <- xi-X # difference Xi and overall mean
sq.Xi_X <- Xi_X^2 # square of difference
sum.sq.Xi_X <- sum(Xi_X^2, na.rm=T) # sum of squares
sum.sq.Xi_X.perhour <- sum.sq.Xi_X/(n-1) # sum of squares per hour minus 1

IV <- sum.sq.Xi_diffXi.perhour / sum.sq.Xi_X.perhour
IV <- round(x = IV, digits = 2);IV



## M10: Average of the 10 Highest Hourly Means (within each day(!))
## daily minute means
CRV.data.locmidnight <- grep(pattern = "12:00:00", x = CRV.data[, "Date"]) # Locations of midnight

for(f in 1:(length(CRV.data.locmidnight)-1)){
  
  CRV.data.full24minavg <-  aggregate(CRV.data[(CRV.data.locmidnight[f]:CRV.data.locmidnight[f+1]), "Activity"],
                                      list(hour=cut(as.POSIXct(CRV.data[(CRV.data.locmidnight[f]:CRV.data.locmidnight[f+1]), "Date"]), breaks = "min")),
                                      mean)
  
  CRV.data.loc.L5 <- NA
  CRV.data.full24minavg.act <- CRV.data.full24minavg$x
  for(g in 1:length(CRV.data.full24minavg.act)){
    CRV.data.loc.L5[g] <- mean(c(CRV.data.full24minavg.act,CRV.data.full24minavg.act[c(1:300)])[c(g:(299+g))], na.rm=T)
  } # calculating mean activity in 300 minute intervals ~ 5 hours
  
  CRV.data.loc.M10 <- NA
  for(h in 1:length(CRV.data.full24minavg.act)){
    CRV.data.loc.M10[h] <- mean(c(CRV.data.full24minavg.act,CRV.data.full24minavg.act[c(1:600)])[c(h:(599+h))], na.rm=T)
  } # calculating mean activity in 600 minute intervals ~ 10 hours
  L5 <- min(CRV.data.loc.L5) #round(min(CRV.data.loc.L5), 2)
  M10 <- max(CRV.data.loc.M10) #round(max(CRV.data.loc.M10), 2)
  
  print(M10)
  
  L5M10frame <- data.frame(CRV.data.loc.L5=CRV.data.loc.L5, CRV.data.loc.M10=CRV.data.loc.M10)
  
  L5onset <- which(L5M10frame$CRV.data.loc.L5==L5)[1]/60 # locating the first value that equals L5 and get the number of hours from start
  M10onset <- which(L5M10frame$CRV.data.loc.M10==M10)[1]/60 # locating the first value that equals M10 and get the number of hours from start
  
  L5_starttime <- CRV.data[CRV.data.locmidnight[f],"Date"] + (L5onset*secshour)
  print(CRV.data[CRV.data.locmidnight[f],"Date"] + (L5onset*secshour))
  
  M10_starttime <- CRV.data[CRV.data.locmidnight[f],"Date"] + (M10onset*secshour)
  print(CRV.data[CRV.data.locmidnight[f],"Date"] + (M10onset*secshour))
  
  Amp <- M10-L5 # self explanatory
  
  RA <- Amp/(L5+M10)
  
  
}



# ## Requires Mean of Sliding Window
# ## Source: https://stats.stackexchange.com/questions/3051/mean-of-a-sliding-window-in-r/93022#93022?newreg=a8a0b3b4a43844008d62efc4ea7b34e5
# aaa <- aggregate(CRV.data[, "Activity"],
#           list(hour=cut(as.POSIXct(CRV.data[, "Date"]), breaks = "hour")),
#           mean)
# 
# CRV.data.x <- CRV.data$Activity
# 
# ## Divide data in 24h days
# CRV.data.x.full24h <- split(CRV.data.x, rep(1:ceiling(length(CRV.data.x)/minsaday), each=minsaday, length.out=length(CRV.data.x))) # Data divided in minsadayh chunks
# floor(length(CRV.data.x)/minsaday) # Number of full 24h days
# 
# # # For each day
# # for(k in 1:floor(length(CRV.data.x)/minsaday)){
# #   aaa.temp <- aaa.full24h[[k]]
# #   total <- length(aaa.temp)
# #   window <- 10
# #   step <- 1
# #   spots <- seq(from=1, to=(total-window), by=step)
# #   result <- vector(length = length(spots))
# #   result.total <- list()
# #   for(l in 1:length(spots)){
# #     result[l] <- mean(aaa.temp[spots[l]:(spots[l]+window)])
# #   }
# #   assign("result.total",result)
# # }
# # 
# # M10 <- mean(result.total)
# # M10 <- mean(tail(sort(result.total),10))
# 
# 
# 
# 
# aaa.x <- aaa$x
# 
# ## Divide data in 24h days
# aaa.full24h <- split(aaa.x, rep(1:ceiling(length(aaa.x)/24), each=24, length.out=length(aaa.x))) # Data divided in 24h chunks
# floor(length(aaa.x)/24) # Number of full 24 days
# 
# ## Computing highest 10-hourly means #per 24h
# 
# # aaa.temp <- aaa.x
# # total <- length(aaa.temp)
# # window <- 10
# # step <- 1
# # spots <- seq(from=1, to=(total-window), by=step)
# # result <- vector(length = length(spots))
# # result.total <- list()
# # for(l in 1:length(spots)){
# #   result[l] <- mean(aaa.temp[spots[l]:(spots[l]+window)])
# #   assign("result.total",result)
# # }
# 
# # For each day
# for(k in 1:floor(length(aaa.x)/24)){
#   aaa.temp <- aaa.full24h[[k]]
#   total <- length(aaa.temp)
#   window <- 10
#   step <- 1
#   spots <- seq(from=1, to=(total-window), by=step)
#   result <- vector(length = length(spots))
#   result.total <- list()
#   for(l in 1:length(spots)){
#     result[l] <- mean(aaa.temp[spots[l]:(spots[l]+window)])
#   }
#   assign("result.total",result)
# }
# 
# # M10 <- mean(result.total)
# M10 <- mean(tail(sort(result.total),10))
# 
# 
# 
