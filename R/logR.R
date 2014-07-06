#' @title logR-package
#' @docType package
#' @import data.table DBI
#' @name logR-package
NULL

#' @title Evalute and log details
#' @param expr expression to be evaluted with logging.
#' @param silent logical, if \code{TRUE} it will not raise \code{alert} (warning/error), no email alert also.
#' @param mail logical flag, if \code{TRUE} then on \code{alert} (warning/error) will send email. Hard default \code{FALSE} due to possible cascade \code{alert}, should be used only in top logR call.
#' @param mail.args list of args to be exactly passed to \code{send.mail}. Can be provided in options \code{options("logR.mail.args")}.
#' @param tag character, custom \code{logR} call metadata to be logged in db.
#' @param in_rows integer input DT nrow, \code{logR} will only \emph{guest} \code{out_rows}.
#' @param conn database connection. Can be provided in options \code{options("logR.conn")}.
#' @param log_table character vector, location in database to store logs, default \code{"logR"}, can be vector of length 2 to use schema: \code{c("public","logR")}. Can be provided in options \code{options("logR.log_table")}.
#' @param verbose integer, default \code{1}, if \code{verbose > 0} it prints \code{alert} messages to console during the processing. Can be provided in options \code{options("logR.verbose")}.
#' @description Complete logging solution. Evalutes, catch warning/error, log processing details to database. Log timing, in/out rows, custom metadata, waring/error messages. In case of \code{alert} also send email.
#' @return Result of \code{expr}, unless it's error and \code{silent=FALSE}.
#' @details As the \code{POSIXct} type is not well handled by the \code{DBI} compliant packages you should test the values stored in \code{log_table} \code{timestamp} field, also the same values after polling it from database to R. At the time of writing \code{RSQLite} is likely store it as \code{numeric}, \code{RPostgreSQL} is unable to map timezone on writing to database, this can be solved by setting global timezone as UTC: \code{Sys.setenv(TZ="UTC")}. Otherwise you can always postprocess \code{timestamp} field to correct format/timezone after polling from database.
#' @section Side effects:
#' \itemize{
#' \item entry in database \code{conn} in table \code{log_table}.
#' \item in case of warning/error if \code{mail==TRUE & silent==FALSE} email will be send according to \code{mail.args} argument.
#' }
#' @section Mail alerts:
#' It is possible to include processing log details in the email body. To achieve it just use the \code{mail.args} with \code{html=TRUE} and \code{body="logR"}, body will be overwritten. In case of any issues related to \code{mailR} debug with \code{do.call(send.mail, args = mail.args)}.
#' @export
#' @references  logR: \url{https://github.com/jangorecki/logR}\cr mailR: \url{https://github.com/rpremraj/mailR}
#' @examples
#' \dontrun{
#'   # connect and prepare data
#'   conn <- dbConnect(...)
#'   options("logR.conn" = conn)
#'   f1 <- function(x) if(x==1) stop('lowest level error') else data.table(a=1:10,b=letters[1:10])
#'   f2 <- function(x) if(x==2) stop('low level error') else logR(f1(x))
#'   f3 <- function(x) if(x==3) stop('mid level error') else logR(f2(x))
#'   f4 <- function(x) if(x==4) stop('high level error') else logR(f3(x))
#'   f5 <- function(x) if(x==5) stop('highest level error') else logR(f4(x))
#'   # run
#'   x <- 2 # 1-5 will raise errors
#'   logR(f5(x))
#'   # check
#'   (DT <- as.data.table(dbReadTable(conn, "logR")))
#'   # close
#'   dbDisconnect(conn)
#' }
logR <- function(expr, 
                 silent = FALSE,
                 tag = NA_character_, 
                 in_rows = NA_integer_,
                 conn = getOption("logR.conn",NULL),
                 log_table = getOption("logR.log_table","logR"),
                 mail = FALSE,
                 mail.args = getOption("logR.mail.args",list()),
                 verbose = getOption("logR.verbose",1)){
  if(is.null(conn)) stop("Missing database connection in logR function. Provide `conn` argument to logR function or set options('logR.conn') to database connection.",call.=TRUE)
  e <- parent.frame()
  parent_fun <- if(is.call(sys.call(-1)[1])) as.character(sys.call(-1)[1]) else NA_character_
  log_status <- "success"
  log_call <- NULL
  log_message <- NULL
  ptm <- proc.time()[[3]]
  r <- tryCatch(expr = eval(substitute(expr), envir = e),
                warning = function(w){
                  log_status <<- "warning"
                  log_call <<- w$call
                  log_message <<- w$message
                  if(silent) suppressWarnings(expr) else expr
                },
                error = function(e){
                  log_status <<- "error"
                  log_call <<- e$call
                  log_message <<- e$message
                  e
                })
  if(log_status %in% "success"){
    DT <- data.table(timestamp = Sys.time(), status = log_status, alert = FALSE,
                     parent_fun = parent_fun, tag = tag, in_rows = in_rows, 
                     out_rows = nrowDT(r), elapsed = round(proc.time()[[3]] - ptm,3), 
                     call = NA_character_, message = NA_character_)
  }
  else if(log_status %in% c("warning","error")){
    DT <- data.table(timestamp = Sys.time(), status = log_status, alert = !silent,
                     parent_fun = parent_fun, tag = tag, in_rows = in_rows, 
                     out_rows = nrowDT(r), elapsed = round(proc.time()[[3]] - ptm,3), 
                     call = paste(as.character(log_call),collapse=" "), message = log_message)
  }
  stopifnot(dbWriteTable(conn = conn, name = log_table, value = DT, row.names = FALSE, append = TRUE))
  
  if(log_status %in% c("warning","error") & !silent){
    msg <- sprintf("ALERT: %s: %s: %s: %s: %s: %s", DT$timestamp, DT$parent_fun, DT$status, DT$tag, DT$call, DT$message)
    if(verbose > 0) message(msg)
    if(mail){
      if(length(mail.args) == 0) stop(paste("In the logR function when using `mail` TRUE then also non zero length `mail.args` must be provided."))
      if(!require("mailR")) stop("mailR package required to send alert emails", call. = TRUE)
      if(isTRUE(mail.args[["html"]]) & identical(mail.args[["body"]],"logR")){
        if(!require("xtable")) stop("xtable package required to include log into alert emails", call. = TRUE)
        dbInfo <- dbGetInfo(conn)
        dbInfo <- dbInfo[names(dbInfo) %in% c("host","port","dbname")]
        mail.args$body <- paste("<html>",
                                "This is the standard email notification about <b>ALERT</b> event during R processing catched by <a href='https://github.com/jangorecki/logR'>logR</a>.<br/>",
                                gsub("\n","",print(xtable(DT), type="html", include.rownames=FALSE, print.results=FALSE)),
                                paste("For details check logs in database<i>",paste(names(dbInfo),dbInfo,sep="=",collapse=", "),"</i>and/or debug directly in R."),
                                "</html>",
                                sep="<br/>")
      }
      do.call(send.mail, args = mail.args)
    }
  }
  if(log_status %in% "error" & !silent) stop(r)
  else if(log_status %in% "error" & silent) return(NULL)
  else return(r)
}

#' @title nrow if DT else NA
#' @keywords internal
nrowDT <- function(x){
  if(any(c("data.frame","data.table") %in% class(x))) nrow(x)
  else NA_integer_
}

#' @title fix xtable POSIXct rendering
#' @references \url{http://stackoverflow.com/a/8658044/2490497}
#' @keywords internal
xtable <- function(x, ...) {
  for (i in which(sapply(x, function(y) !all(is.na(match(c("POSIXt","Date"),class(y))))))) x[[i]] <- as.character(x[[i]])
  xtable::xtable(x, ...)
}
