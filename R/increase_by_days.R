increase_by_days <- function(timeobj, nr_days) {
  time_in_seconds <- nr_days * 86400
  cttimeobj <- as.POSIXct(timeobj)
  new_time <- cttimeobj + time_in_seconds
  new_time <- new_time + as.POSIXlt(cttimeobj)$gmtoff - as.POSIXlt(new_time)$gmtoff
  new_time
}
