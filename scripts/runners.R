
#
# ## Runner for Actical device
# ACTman::ACTman(workdir = "C:/Bibliotheek/Studie/PhD/Publishing/Actical Project",
#                myACTdevice = "Actical", lengthcheck = F, nparACT_compare = F, iwantsleepanalysis = F,
#                circadian_analysis = T, plotactogram = "24h", i_want_EWS = F, movingwindow = F,
#                movingwindow.size = 1, na_omit = T)






## Runner for Abel Sleep Data
## Run ACTman to obtain LOLkat data
ACTman::ACTman(workdir = "C:/mydata/",
               myACTdevice = "MW8", lengthcheck = F, nparACT_compare = F, iwantsleepanalysis = T,
               circadian_analysis = T, plotactogram = "24h", i_want_EWS = F, movingwindow = F,
               movingwindow.size = 1, na_omit = T)


## Runner for adding more EWS
## Run ACTman to obtain LOLkat data
ACTman::ACTman(workdir = "C:/mydata",
               myACTdevice = "MW8", lengthcheck = F, nparACT_compare = F, iwantsleepanalysis = T,
               circadian_analysis = T, plotactogram = "24h", i_want_EWS = F, movingwindow = TRUE,
               movingwindow.size = 1)

## See if relation between autocorrelation and lag decreases exponentially
#! global assign "Autocorr" variable needs to be fixed!
plot(x = Autocorr$lag, y = Autocorr$acf, xlab = "Lag", ylab = "Autocorrelation",
     main = "Autocorrelation values with differing lag sizes")
abline(h = 0.2)


EWS_corrtest <- rollingwindow.results[, 11:15]
corstarsl(EWS_corrtest) ## Load function from Signifistars script first!

cortab <- xtable(corstarsl(EWS_corrtest))

write.table(cortab, file = "cortable1.csv", sep = ";", col.names = TRUE, row.names = TRUE)



## Sleeplog test
ACTman::ACTman(workdir = "C:/mydata",
               myACTdevice = "MW8", iwantsleepanalysis = TRUE, lengthcheck = FALSE,
               circadian_analysis = FALSE, plotactogram = FALSE)


## Check for Circadian calc
ACTman::ACTman(workdir = "C:/mydata",
               myACTdevice = "MW8", lengthcheck = TRUE, nparACT_compare = FALSE, iwantsleepanalysis = FALSE,
               circadian_analysis = TRUE, plotactogram = "48h", movingwindow = F)


## Select multiple days
ACTman::ACTman(workdir = "C:/mydata", myACTdevice = "MW8",
               movingwindow = T, movingwindow.size = 5, movingwindow.jump = 7)


ACTman::ACTman(workdir = "C:/mydata",
               myACTdevice = "MW8", lengthcheck = TRUE, nparACT_compare = FALSE, iwantsleepanalysis = TRUE,
               circadian_analysis = FALSE, plotactogram = FALSE)






# Moving window
ACTman::ACTman(workdir = "C:/mydata", myACTdevice = "MW8", movingwindow = TRUE, movingwindow.size = 7,
               movingwindow.jump = 1)



ACTman::ACTman(workdir = "C:/mydata",
               myACTdevice = "MW8", lengthcheck = FALSE, nparACT_compare = FALSE, iwantsleepanalysis = FALSE,
               circadian_analysis = TRUE, plotactogram = "24h", movingwindow = TRUERUE, movingwindow.size = 7)
