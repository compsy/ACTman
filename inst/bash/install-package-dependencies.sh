#! /usr/bin/env Rscript
options(repos = structure(c(CRAN = "http://cran.uni-muenster.de/")))
packages <- c("testthat", "roxygen2", "nparACT", "gridExtra", "dplyr", "mice", "moments")
new.packages <- packages[!(packages %in% installed.packages()[, "Package"])]
if (length(new.packages))
  install.packages(new.packages, dependencies = TRUE)
update.packages(lib.loc = Sys.getenv("R_LIBS_USER"), ask = FALSE)
