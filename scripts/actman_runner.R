# actman runner
rm(list = ls(pos = '.GlobalEnv', all.names = TRUE), pos = '.GlobalEnv')
unloadNamespace('ACTman')
library('ACTman', character.only = TRUE)
setwd("/Users/ando/Documents/actman")


# Test with ActiWatch data
result <- ACTman(workdir = "/Users/ando/Documents/actman/mydataactiwatch", myACTdevice = "Actiwatch2")


# Test with MW8 data
# result2 <- ACTman(workdir = "/Users/ando/Documents/actman/mydatamw8", myACTdevice = "MW8")
