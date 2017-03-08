rm(list = ls(pos = '.GlobalEnv', all = TRUE), pos = '.GlobalEnv')
unloadNamespace('ACTman')
library('ACTman', character.only = TRUE)
unlink("/Users/ando/repos/ACTman/inst/help_files", recursive = TRUE, force = TRUE)
dir.create("/Users/ando/repos/ACTman/inst/help_files", recursive = TRUE)
library('staticdocs')
currently_generating_help_files <<- TRUE
build_site('/Users/ando/repos/ACTman', site_path = "/Users/ando/repos/ACTman/inst/help_files")
currently_generating_help_files <<- FALSE
cat("Done.\n")
