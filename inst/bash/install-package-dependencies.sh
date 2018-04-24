#! /usr/bin/env Rscript
options(repos = structure(c(CRAN = "http://cran-mirror.cs.uu.nl/")))
packages <- c("testthat", "roxygen2", "nparACT", "gridExtra", "dplyr", "mice")
new.packages <- packages[!(packages %in% installed.packages()[, "Package"])]
if (length(new.packages))
  install.packages(new.packages, dependencies = TRUE)
update.packages(lib.loc = Sys.getenv("R_LIBS_USER"), ask = FALSE)
