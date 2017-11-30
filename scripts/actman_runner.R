# actman runner
rm(list = ls(pos = '.GlobalEnv', all.names = TRUE), pos = '.GlobalEnv')
unloadNamespace('ACTman')
library('ACTman', character.only = TRUE)
setwd("/Users/ando/Documents/actman/mydata3")


# Test with ActiWatch data
#result <- ACTman(workdir = "/Users/ando/Documents/actman/mydata2",
#                 sleepdatadir = "/Users/ando/Documents/actman/mydata3",
#                 myACTdevice = "Actiwatch2",
#                 daysperiod = 14,
#                 endperiod = "2014-04-17 00:00:00",
#                 startperiod = "2014-04-17 00:00:00")


# Test with MW8 data
result2 <- ACTman(workdir = "/Users/ando/Documents/actman/mydatamw8", myACTdevice = "MW8")
