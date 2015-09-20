.onLoad <- function(libname, pkgname){
    
    if(is.null(getOption("logR.log"))) options("logR.log" = TRUE)
    if(is.null(getOption("logR.nano"))) options("logR.nano" = TRUE) # force off-microbenchmarkCore
    if(is.null(getOption("logR.table"))) options("logR.table" = "logr")
    # options("logR.conn" = NULL)
    # options("logR.schema" = NULL)
    if(is.null(getOption("logR.meta"))) options("logR.meta" = list())
    if(is.null(getOption("logR.mail"))) options("logR.mail" = FALSE) # require mailgun.com account
    # options("logR.mail_args" = NULL)
    if(is.null(getOption("logR.silent"))) options("logR.silent" = TRUE)
    
}
