[![CircleCI](https://circleci.com/gh/compsy/ACTman.svg?style=svg&circle-token=d4a53fd8f5a7813e4cacd9265bb5de8fe8b44336)](https://circleci.com/gh/compsy/ACTman)

ACTman
======

ACTman manages Actigraphy data.

To install, type the following:

    install.packages('devtools')
    devtools::install_github('compsy/ACTman')


### Example use

To get the actdata overview:

    View(ACTman::ACTman(workdir = "C:/Bibliotheek/Studie/PhD/Publishing/ACTman/R-part/mydata2",
                        myACTdevice = "Actiwatch2",
                        iwantsleepanalysis = FALSE,
                        plotactogram = FALSE,
                        selectperiod = FALSE,
                        startperiod = "2016-10-03 00:00:00",
                        daysperiod = 14,
                        tune = FALSE))

Type ?ACTman::ACTman for help.
