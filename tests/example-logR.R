suppressPackageStartupMessages({
  library(data.table)
  library(logR)
})

dt <- data.table(a = rnorm(10), b = sample(1:5,10,TRUE))

f_success <- function(x){
  x[,.(a=mean(a)),.(b)]
}
f_warning <- function(x){
  x[,.(a=log(a*(-1))),.(b)]
}
f_error <- function(x){
  x[,.(a=a+as.character(a)),.(b)]
}
f_double_warning <- function(){
  warning("warning A")
  r <- 5
  warning("warning B")
  r^2
}
f_nested_double_warning <- function(x){
  z <- f_double_warning()
  r <- x[,.(a=mean(a+z)),.(b)]
}
f_warning_nested_double_warning <- function(x){
  z <- f_double_warning()
  r <- x[,.(a=mean(a+z)),.(b)]
  warning("warning C")
  r
}
f_wait <- function(x){
  Sys.sleep(1.021421)
  x
}

if(requireNamespace("RH2",quietly=TRUE)){
  library(RH2)
  h2 <- list(drvName = "JDBC", conn = dbConnect(H2(), "jdbc:h2:mem:"))
  opts <- options("dwtools.db.conns"=list(h2=h2),
                  "logR.db" = TRUE,
                  "logR.conn" = "h2")
  logR_schema(vendor = "h2")
}

logR(f_success(dt))

logR(f_warning(dt),
     tag = "business_process2") # optional tag to log

logR(f_error(dt),
     tag = "business_process3",
     in_rows = nrow(dt)) # optional in_rows to log

logR(f_nested_double_warning(dt),
     tag = "business_process4",
     in_rows = nrow(dt))

logR(f_warning_nested_double_warning(dt),
     tag = "business_process5",
     in_rows = nrow(dt))

op <- options("logR.nano" = FALSE) # force proc.time instead nanotime
logR(f_wait(dt), tag="nanotime force off")
options(op)

## some expected non catched warnings

suppressWarnings({
  # silent=FALSE
  logR(f_warning_nested_double_warning(dt),
       silent = FALSE,
       tag = "business_process5")
  # invalid in_rows
  logR(f_success(dt),
       tag = "business_process1",
       in_rows = NULL)
})

## preview logs

if(requireNamespace("RH2",quietly=TRUE)){
  dwtools::db("LOGR","h2")
  options(opts)
} else {
  fread("LOGR.csv")
  file.remove("LOGR.csv")
}
