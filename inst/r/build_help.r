rm(list = ls(pos = '.GlobalEnv', all.names = TRUE), pos = '.GlobalEnv')
unloadNamespace('ACTman')
library('ACTman', character.only = TRUE)
unlink("/Users/ando/repos/ACTman/inst/docs", recursive = TRUE, force = TRUE)
#devtools::install_github("hadley/pkgdown")
currently_generating_help_files <<- TRUE
pkgdown::build_site(pkg = '/Users/ando/repos/ACTman')
currently_generating_help_files <<- FALSE
file.rename("/Users/ando/repos/ACTman/docs","/Users/ando/repos/ACTman/inst/docs")
cat("Done.\n")
