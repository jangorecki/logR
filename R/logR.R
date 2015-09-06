#' @title logR-package
#' @docType package
#' @import data.table DBI
#' @author Jan Gorecki
#' @name logR-package
NULL

deparse_to_char <- function(expr){
  paste(deparse(expr, width.cutoff = 500L),collapse="\n")
}

trunc_char <- function(x, n = 33L){
  if(is.na(x)) return(NA_character_)
  if(nchar(x) > n+3L) return(paste0(substr(x,1,n),"..."))
  return(x)
}

#' @title tryCatch both warnings and errors
#' @description We want to catch *and* save both errors and warnings, and in the case of a warning, also keep the computed result.
#' @param expr expression to evaluate with warnings and error handling.
#' @return a list with 'value' and 'warning', where 'value' may be an error caught.
#' @note Modified version to catch multiple warnings.
#' @references \url{https://stat.ethz.ch/pipermail/r-help/2010-December/262626.html}
#' @export
#' @author Martin Maechler
tryCatch_we <- function(expr){
  W <- NULL
  w.handler <- function(w){ # warning handler
    W <<- c(W, list(w)) # append warning to list of warnings
    invokeRestart("muffleWarning")
  }
  list(value = withCallingHandlers(tryCatch(expr, error = function(e) e),
                                   warning = w.handler),
       warning = W)
}

#' @title Prepare SQL values
#' @description Wraps character types into single quotes, numbers as non scientific.
#' @param col character vector of column names.
#' @param x data.table a one row data.table.
#' @param trunc_char_n integer number of characters to truncate character after. Default 252, \emph{...} is added to the end of string so the result will have 255 characters.
#' @return character vector
#' @export
sql_val <- function(col, x, trunc_char_n = 252L){
  if(is.na(x[[col]])){
    val <- "NULL"
  } else if(any(class(x[[col]]) %in% "character")){
    val <- paste0("'",gsub("'","''",trunc_char(x[[col]], n = trunc_char_n)),"'")
  } else if(any(class(x[[col]]) %in% c("numeric","integer"))){
    val <- format(x[[col]],scientific = FALSE)
  } else{
    val <- as.character(x[[col]])
  }
  val
}

#' @title Make SET for update statement
#' @description Helper for producing update statement, wraps character types into single quotes, numbers as non scientific.
#' @param col character vector of column names to update.
#' @param x data.table a one row data.table.
#' @return character vector which can be easily \code{paste(x,collapse=",")} to paste in update statement.
update_make_set <- function(col, x){
  paste(names(x[,col,with=FALSE]),sql_val(col,x),sep=" = ")
}

#' @title Detailed logging of R call
#' @description Complete logging solution. Writes to database call metadata before its evaluation. Evalutes with timing and warning/error catching. Updates database entry for processing details: in/out rows, custom metadata, warning/error messages. Email on warning/errors.
#' @param CALL call to be evaluted with logging.
#' @param tag character, custom metadata to be attached to log entry.
#' @param in_rows integer input DF/DT nrow, \emph{logR} can only guess \emph{out_rows}.
#' @param silent logical, if default \code{getOption("logR.silent",TRUE)} it will not raise warning or error but only log/email it.
#' @param mail logical if \emph{TRUE} then on warning/error the email will be send. Requires \emph{mail_args} to be provided. Default \code{getOption("logR.mail",FALSE)}.
#' @param mail_args list of args which will overwrite the default logR args passed to \code{mailR::send.mail}, should at least contains \emph{to, from, smtp} elements. Default \code{getOption("logR.mail_args",NULL)}. See references for mail configuration.
#' @param .db logical, when \emph{FALSE} then function will write log to csv file instead of database. Default to \code{getOption("logR.db",FALSE)}.
#' @param .conn DBI connection. Default to \code{getOption("logR.conn",NULL)}.
#' @param .table character scalar, location in database to store logs, default \code{getOption("logR.table")}.
#' @param .schema character scalar, location in database to store logs, default \code{getOption("logR.schema")}.
#' @param .log logical escape parameter, set to \emph{FALSE} to suppress logR process and just execute a call, default to \code{getOption("logR.log",TRUE)}.
#' @return Result of evaluated \emph{CALL}.
#' @note You may expect some silent data types conversion when writing to database, exactly the same directly using DBI. Only first warning will be logged to database and send on email.
#' @section Side effects:
#' \itemize{
#' \item for default \emph{.db} \emph{TRUE} and \emph{.conn} db connection - the entry in table \emph{.table}.
#' \item for \emph{.db} \emph{FALSE} - the entry in \emph{.table} csv file in working directory.
#' \item in case of warnings or error and \emph{mail} set to \emph{TRUE} also the email will be send according to \emph{mail_args}.
#' }
#' @section Database setup using SEQUENCE:
#' Logging process requires 3 database objects:
#' \itemize{
#' \item \strong{sequence} - required for transactional logging
#' \item \strong{view} - query sequence, it isolates various SQL \code{.nextval} calls on the database side
#' \item \strong{table} - place to store logs
#' }
#' You can create all three objects automatically using \link{logR_schema} function, it works for \emph{h2, sql server, postgres, oracle} databases. For other databases you can adjust scripts from \link{schema_sql}.
#' View must return \emph{logr_id} column and should be named \code{getOption("logR.seq_view","LOGR_ID")}. Default name of log table is \code{getOption("logR.table","LOGR")}.
#' Due to various supported database interfaces it is recommended to set maximum value of the sequence to \code{.Machine$integer.max} which is \emph{2147483647}.
#' @section Database setup using INSERT RETURNING:
#' TO DO
#' @section Fatal errors:
#' If your R function will manage to kill whole R session you will see that \emph{status} entry in log table will not get updated and it will stay as \emph{NA}.
#' It might be worth to schedule a watcher task to detect such cases, see \emph{How to use logR} vignette.
#' @seealso \link{logR_browser}, \link{logR_schema}, \link{schema_sql}
#' @references \url{https://github.com/rpremraj/mailR}
#' @export
#' @examples
#' N <- 1e5
#' df <- data.frame(a = rnorm(N), b = sample(seq_len(as.integer(log(N))),N,TRUE))
#' dt <- as.data.table(df)
#' 
#' # log to csv
#' options("logR.db" = FALSE)
#' dfr <- logR(with(df, aggregate(a, list(b), sum)), in_rows=nrow(df))
#' dtr <- logR(dt[,.(a=sum(a)),,b], in_rows=nrow(dt))
#' err <- logR(sum(1,"a"))
#' war <- logR(cor(c(1,1),c(2,3)))
#' logR_query()
#' file.remove("LOGR.csv")
#' 
#' # log to H2 database
#' if(require("RH2", quietly=TRUE)){
#'   options("logR.db" = TRUE,
#'           "logR.conn" = dbConnect(H2(), "jdbc:h2:mem:"))
#'   logR_schema("h2")
#'   dfr <- logR(with(df, aggregate(a, list(b), sum)), in_rows=nrow(df))
#'   dtr <- logR(dt[,.(a=sum(a)),,b], in_rows=nrow(dt))
#'   err <- logR(sum(1,"a"))
#'   war <- logR(cor(c(1,1),c(2,3)))
#'   logR_query()
#'   options("logR.db" = FALSE)
#'   dbDisconnect(getOption("logR.conn"))
#' }
logR <- function(CALL,
                 tag = NA_character_, 
                 in_rows = NA_integer_,
                 silent = getOption("logR.silent"),
                 mail = getOption("logR.mail"),
                 mail_args = getOption("logR.mail_args"),
                 .db = getOption("logR.db"),
                 .conn = getOption("logR.conn"),
                 .schema = getOption("logR.schema"),
                 .table = getOption("logR.table"),
                 .log = getOption("logR.log")){
  if(!isTRUE(.log)) return(eval.parent(CALL))
  subCALL <- substitute(CALL)
  
  if(!is.call(subCALL)) stop("logR only handle calls, wrap your input into function and pass function call to CALL argument.")
  if(!is.integer(in_rows)){
    warning("logR 'in_rows' input should be integer, using NA_integer_.")
    in_rows <- NA_integer_
  }
  logR_wd <- getwd()
  
  .db <- as.logical(.db)
  if(.db){
    if(class(.conn)[1L]=="H2Connection") dbSendQuery <- RJDBC::dbSendUpdate # remove after RH2#3
    else if(class(.conn)[1L]=="PostgreSQLConnection") invisible() # no dbIsValid method for postgres
    else if(!dbIsValid(.conn)) stop("Provided connection in 'logR.conn' option or '.conn' argument is not valid. Provide valid DBI connection.") # remove `else` after: RH2#2
  }
  mail <- as.logical(mail)
  silent <- as.logical(silent)
  status <- logr_start <- logr_end <- cond_call <- cond_message <- NULL # CRAN check
  
  CALLenv <- parent.frame()
  .logr_start <- Sys.time()
  
  # decide if to use sequence or insert returning by option
  ins.ret <- getOption("logR.insert.returning")
  do.ins.ret <- is.function(ins.ret)
  # start logging table
  if(.db){
    if(!do.ins.ret) logr <- setDT(dbGetQuery(.conn, paste("SELECT logr_id FROM",paste(c(.schema, getOption("logR.seq_view")),collapse=".")))) # view to query sequence ID, using view to be db vendor independent, set view to query from sequence according to your db vendor
    else if(do.ins.ret) logr <- data.table(logr_id = NA_integer_) # will be updated later on insert
  } else if(!.db){
    logr <- data.table(logr_id = NA_integer_)
  }
  # set meta on start
  logr[,`:=`(logr_start_int = as.integer(.logr_start),
             logr_start = as.character(.logr_start),
             call = deparse_to_char(subCALL),
             status = NA_character_,
             logr_end_int = NA_integer_,
             logr_end = NA_character_,
             timing = NA_real_,
             in_rows = as.integer(in_rows),
             out_rows = NA_integer_,
             tag = as.character(tag),
             mail = as.integer(mail),
             cond_call = NA_character_,
             cond_message = NA_character_)]
  
  # insert db logr entry
  if(.db){
    if(do.ins.ret){
      ins <- do.call(ins.ret, args = list(table = .table, logr = logr))
      r <- setDT(dbGetQuery(.conn, ins)) # fire insert
      if(!(is.data.table(r) && nrow(r))) stop("INSERT RETURNING did not work, should returns logr_id field.")
      set(logr, i=1L, j=1L, value=as.integer(r[["logr_id"]]))
    } # use do.ins.ret, update returning id
    else if(!do.ins.ret){
      ins.tab <- paste("INSERT INTO", paste(c(.schema, .table), collapse = "."))
      ins.col <- paste0("(",paste(names(logr), collapse=","),")")
      ins.val <- paste("VALUES",paste0("(",paste(vapply(names(logr),sql_val,NA_character_,logr), collapse=","),")"))
      ins <- paste0(paste(c(ins.tab,ins.col,ins.val), collapse=" "), ";")
      r <- dbSendQuery(.conn, ins) # fire insert
    } # use sequence
  }
  
  # evaluate with timing and error/warning catch
  done <- FALSE
  if(isTRUE(getOption("logR.nano"))){
    if(requireNamespace("microbenchmark", quietly=TRUE)){
      ts <- microbenchmark::get_nanotime()
      r <- tryCatch_we(expr = eval(subCALL, envir = CALLenv))
      elapsed <- (microbenchmark::get_nanotime() - ts) * 1e-9
      done <- TRUE
    } else {
      ## skip that silently
      # warning("'logR.nano' option is TRUE but there is no microbenchmark package. Install microbenchmark or set 'logR.nano' option to FALSE. Proceeding with standard proc.time time measurement.")
    }
  }
  if(!done){
    ts <- proc.time()[[3L]]
    r <- tryCatch_we(expr = eval(subCALL, envir = CALLenv))
    elapsed <- proc.time()[[3L]] - ts
    done <- TRUE
  }
  
  .logr_end <- Sys.time()
  # set meta on end
  logr[,`:=`(logr_end_int = as.integer(.logr_end),
             logr_end = as.character(.logr_end),
             timing = elapsed,
             out_rows = if(any(c("data.frame","data.table") %in% class(r[["value"]]))) nrow(r[["value"]]) else NA_integer_)]
  
  if("error" %in% class(r[["value"]])){
    logr[,`:=`(status = "error",
               cond_call = deparse_to_char(r[["value"]][["call"]]),
               cond_message = r[["value"]][["message"]])]
  } else if("warning" %in% class(r[["warning"]][[1L]])){
    logr[,`:=`(status = "warning",
               cond_call = deparse_to_char(r[["warning"]][[1L]][["call"]]),
               cond_message = r[["warning"]][[1L]][["message"]])]
  } else{
    logr[,`:=`(status = "success")]
  }
  
  # update db logr entry
  if(.db){
    cols_to_upd <- c("status","logr_end_int","logr_end","timing","out_rows","cond_call","cond_message")
    upd_set <- vapply(cols_to_upd,update_make_set,NA_character_,logr)
    sql <- paste0("UPDATE ",paste(c(.schema, .table), collapse=".")," SET ",paste(upd_set,collapse=",")," WHERE logr_id = ",format(logr[["logr_id"]],scientific = FALSE),";")
    upd <- dbSendQuery(.conn, sql)
  } else {
    # write csv log
    log_file <- paste(.table,"csv",sep=".")
    log_file_path <- paste(logR_wd,log_file,sep="/")
    write.table(logr,log_file_path,append=file.exists(log_file_path),sep=",",na="",col.names=!file.exists(log_file_path),row.names=FALSE,qmethod="double")
  }
  
  # mail - test for status nested to always check mail_args
  if(mail){
    if(requireNamespace("mailR", quietly=TRUE)){
      if(length(mail_args) == 0L) stop("In the logR function when using 'mail' TRUE then also non zero length 'mail_args' must be provided, read ?logR")
      if(!("smtp" %in% names(mail_args))) stop("Lack of mandatory elements provided in 'mail_args' required to send email. Read ?mailR::send.mail")
      if(logr[,status %in% c("error","warning")]){
        default_args <- list(subject = logr[,paste0("logR detects ",status," in call",if(!is.na(tag)) paste(" tagged as:",trunc_char(tag)) else paste(":",trunc_char(call)))],
                             body = logr[,paste0("Hello logR support,\n\nProcessing details:\n- process tag:        ",tag,"\n- process call:       ",call,"\n- process start:      ",logr_start,"\n- process end:        ",logr_end,"\n- processing status:  ",status,"\n- condition call:     ",cond_call,"\n- condition message:  ",cond_message,"\n\nHave an easy debugging :)\nlogR")])
        do.call(mailR::send.mail, args = c(mail_args,default_args[!(names(default_args) %in% names(mail_args))]))
      }
    } else {
      stop("logR mail feature requires mailR package installed.")
    }
  }
  
  # raise error/warning
  if(!silent){
    if(logr[,status %in% "error"]){
      stop(r[["value"]])
    } else if(logr[,status %in% "warning"]){
      lapply(r[["warning"]], warning)
    }
  }
  
  # finish
  return(r[["value"]])
}
