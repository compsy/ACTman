rm(list = ls(pos = '.GlobalEnv', all.names = TRUE), pos = '.GlobalEnv')
unloadNamespace('ACTman')
library('ACTman', character.only = TRUE)
unlink("/Users/ando/repos/ACTman/inst/help_files", recursive = TRUE, force = TRUE)
dir.create("/Users/ando/repos/ACTman/inst/help_files", recursive = TRUE)
#devtools::install_github("hadley/pkgdown")
currently_generating_help_files <<- TRUE
pkgdown::build_site(pkg = '/Users/ando/repos/ACTman', path = "/Users/ando/repos/ACTman/inst/help_files")
currently_generating_help_files <<- FALSE
cat("Done.\n")
