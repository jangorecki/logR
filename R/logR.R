#' @title logR-package
#' @docType package
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
sql_val <- function(col, x, trunc_char_n = 252L){
    if(is.na(x[[col]])){
        val <- "NULL"
    } else if(any(class(x[[col]]) %in% "character")){
        val <- paste0("'",gsub("'","''",trunc_char(x[[col]], n = trunc_char_n)),"'")
    } else if(any(class(x[[col]]) %in% c("numeric","integer"))){
        val <- format(x[[col]], scientific = FALSE)
    } else if(any(class(x[[col]]) %in% c("POSIXct","POSIXlt"))){
        val <- paste0("'",format(x[[col]], "%Y-%m-%d %H:%M:%OS"),"'::timestamp")
    } else if(any(class(x[[col]]) %in% "Date")){
        val <- paste0("'",format(x[[col]], "%Y-%m-%d"),"'::date")
    } else {
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

#' @title Detailed logging of R expressions
#' @description Complete logging solution. Writes to database call metadata before its evaluation. Evalutes with timing and warning/error catching. Updates database entry for processing details: in/out rows, custom metadata, warning/error messages. Email on warning/errors.
#' @param expr expression to be evaluted with logging.
#' @param alert logical, should be alert flag suppressed on warning/error, including suppressing email notification.
#' @param in_rows integer input DF/DT nrow, \emph{logR} can only guess \emph{out_rows}.
#' @param meta list, list of custom fields, each of length 1, list be always the same, fill missing with NA. Default \code{getOption("logR.meta",list())} means no meta columns. If you want to change element you need to alter table before.
#' @param silent logical, if default \code{getOption("logR.silent",TRUE)} it will not raise warning or error but only log.
#' @param mail logical if \emph{TRUE} then on alert the email will be send. Requires \emph{mail_args} to be provided. Default \code{getOption("logR.mail",FALSE)}.
#' @param mail_args list
#' @param .conn DBI connection. Default to \code{getOption("logR.conn",NULL)}.
#' @param .schema character scalar, location in database to store logs, default \code{getOption("logR.schema")}.
#' @param .table character scalar, location in database to store logs, default \code{getOption("logR.table")}.
#' @param .log logical escape parameter, set to \emph{FALSE} to suppress logR process and just execute a call, default to \code{getOption("logR.log",TRUE)}.
#' @return Result of evaluated \emph{expr}.
#' @note You may expect some silent data types conversion when writing to database, exactly the same directly using DBI. Only first warning will be logged to database and send on email.
#' @section Side effects:
#' \itemize{
#' \item entry in database table.
#' \item in case of alerts and email notification configured also the email will be send.
#' }
#' @section Database setup using SEQUENCE:
#' Logging process requires 3 database objects:
#' \itemize{
#' \item \strong{sequence} - required for transactional logging
#' \item \strong{view} - query sequence, it isolates SQL \code{.nextval} calls on the database side
#' \item \strong{table} - place to store logs
#' }
#' You can create all three objects automatically using \link{logR_schema} function.
#' View must return \emph{logr_id} column and should be named \code{getOption("logR.seq_view","LOGR_ID")}. Default name of log table is \code{getOption("logR.table","LOGR")}.
#' Due to various supported database interfaces it is recommended to set maximum value of the sequence to \code{.Machine$integer.max} which is \emph{2147483647}.
#' @section Fatal errors:
#' If your R function will manage to kill whole R session you will see that \emph{status} entry in log table will not get updated and it will stay as \emph{NA}.
#' It might be worth to schedule a watcher task to detect such cases, see \emph{How to use logR} vignette.
#' @seealso \link{logR_schema}, \link{schema_sql}, \link{logR_query}
logR = function(expr,
                alert = TRUE,
                in_rows = NA_integer_,
                meta = getOption("logR.meta"),
                silent = getOption("logR.silent"),
                mail = getOption("logR.mail"),
                mail_args = getOption("logR.mail_args"),
                .conn = getOption("logR.conn"),
                .schema = getOption("logR.schema"),
                .table = getOption("logR.table"),
                .log = getOption("logR.log")){
    if(!is.integer(in_rows)){
        warning("logR 'in_rows' input should be integer, using NA_integer_.")
        in_rows = NA_integer_
    }
    stopifnot(is.logical(alert), is.integer(in_rows), is.list(meta), is.logical(silent), is.logical(mail), !is.null(.conn), is.character(.table), is.logical(.log))
    if(!isTRUE(.log)) return(eval.parent(expr))
    subexpr = substitute(expr)
    exprenv = parent.frame()
    .logr_start = Sys.time()
    .alert = alert
    
    # get logr_id
    sql = paste("SELECT logr_id FROM",paste(c(.schema, getOption("logR.seq_view")), collapse="."))
    tryCatch(
        logr <- setDT(dbGetQuery(.conn, sql)),
        error = function(e) stop(paste0("Could not obtain logR id from the sequence, make sure below query is running fine, if needed tune logR options. See below error for details.\n",sql,"\n",as.character(e)), call. = FALSE)
    )
    
    # set meta on start
    logr[,`:=`(logr_start_int = as.integer(.logr_start),
               logr_start = .logr_start,
               expr = deparse_to_char(subexpr),
               status = NA_character_,
               alert = NA,
               logr_end_int = NA_integer_,
               logr_end = structure(NA_real_, class = c("POSIXct", "POSIXt")),
               timing = NA_real_,
               in_rows = in_rows,
               out_rows = NA_integer_,
               mail = mail,
               cond_call = NA_character_,
               cond_message = NA_character_)]
    if(length(meta)){
        if(!all(sapply(meta, function(x) length(x) == 1L))) stop("All elements of 'meta' arg must have length of 1.")
        logr[, c(names(meta)) := meta]
    }
    
    # insert db logr entry
    ins.tab = paste("INSERT INTO", paste(c(.schema, .table), collapse = "."))
    ins.col = paste0("(",paste(names(logr), collapse=","),")")
    ins.val = paste("VALUES",paste0("(",paste(vapply(names(logr),sql_val,NA_character_,logr), collapse=","),")"))
    sql = paste0(paste(c(ins.tab,ins.col,ins.val), collapse=" "), ";")
    tryCatch(
        ins <- dbSendQuery(.conn, sql),
        error = function(e) stop(paste0("Make sure you use same list of 'meta' fields each time, if you want to change it you need to alter table to match names and types, debug using below query and error.\n",sql,"\n",as.character(e)), call. = FALSE)
    )
    
    # evaluate with timing and error/warning catch
    done = FALSE
    if(isTRUE(getOption("logR.nano")) && requireNamespace("microbenchmark", quietly=TRUE)){
        ts = microbenchmark::get_nanotime()
        r = tryCatch_we(expr = eval(subexpr, envir = exprenv))
        elapsed = (microbenchmark::get_nanotime() - ts) * 1e-9
        done = TRUE
    }
    if(!done){
        ts = proc.time()[[3L]]
        r = tryCatch_we(expr = eval(subexpr, envir = exprenv))
        elapsed = proc.time()[[3L]] - ts
        done = TRUE
    }
    .logr_end = Sys.time()
    
    # set meta on end
    logr[,`:=`(logr_end_int = as.integer(.logr_end),
               logr_end = .logr_end,
               timing = elapsed,
               out_rows = if(any(c("data.frame","data.table") %in% class(r[["value"]]))) nrow(r[["value"]]) else NA_integer_)]
    if("error" %in% class(r[["value"]])){
        logr[,`:=`(status = "error",
                   alert = .alert,
                   cond_call = deparse_to_char(r[["value"]][["call"]]),
                   cond_message = r[["value"]][["message"]])]
    } else if("warning" %in% class(r[["warning"]][[1L]])){
        logr[,`:=`(status = "warning",
                   alert = .alert,
                   cond_call = deparse_to_char(r[["warning"]][[1L]][["call"]]),
                   cond_message = r[["warning"]][[1L]][["message"]])]
    } else {
        logr[,`:=`(status = "success",
                   alert = FALSE)]
    }
    
    # update db logr entry
    cols_to_upd = c("status","alert","logr_end_int","logr_end","timing","out_rows","cond_call","cond_message")
    upd_set = vapply(cols_to_upd,update_make_set,NA_character_,logr)
    sql = paste0("UPDATE ",paste(c(.schema, .table), collapse=".")," SET ",paste(upd_set,collapse=",")," WHERE logr_id = ",format(logr[["logr_id"]],scientific = FALSE),";")
    tryCatch(
        upd <- dbSendQuery(.conn, sql),
        error = function(e) stop(paste0("Error while update should not happend, debug using below query and error.\n",sql,"\n",as.character(e)), call. = FALSE)
    )
    
    # mail
    if(mail && logr$alert){
        stop("Mail feature TO DO for 2.1+")
        # will require jsonlite and httr?
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
