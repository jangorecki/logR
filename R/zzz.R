.onLoad <- function(libname, pkgname){
  
  options("logR.log" = TRUE)
  options("logR.nano" = TRUE) # require microbenchmark
  options("logR.table" = "logr")
  options("logR.seq_view" = "logr_id")
  options("logR.conn" = NULL)
  options("logR.schema" = NULL)
  options("logR.meta" = list())
  options("logR.mail" = FALSE) # require mailgun.com account
  options("logR.mail_args" = NULL)
  options("logR.silent" = TRUE)
  
}
