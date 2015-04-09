.onLoad <- function(libname, pkgname){
  
  options("logR.log" = TRUE)
  options("logR.nano" = TRUE) # require microbenchmark
  options("logR.db" = FALSE)
  options("logR.table" = "LOGR")
  options("logR.seq_view" = "LOGR_ID")
  options("logR.conn" = NULL)
  options("logR.mail" = FALSE) # require mailR
  options("logR.mail_args" = NULL)
  options("logR.wd" = NULL)
  
}
