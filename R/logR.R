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

#' @title tryCatch2 for all conditions
#' @description Catch all conditions and keep computed results.
#' @param expr expression to evaluate with exception handling.
#' @return a list with elements 'value', 'error', 'warning', 'message', 'interrupt'.
tryCatch2 <- function(expr){
    V=E=W=M=I=NULL
    # - [x] catch errors
    e.handler = function(e){
        E <<- e
        NULL
    }
    # - [x] catch multiple warnings
    w.handler = function(w){
        W <<- c(W, list(w))
        invokeRestart("muffleWarning")
    }
    # - [x] catch multiple messages
    m.handler = function(m){
        attributes(m$call) <- NULL
        M <<- c(M, list(m))
    }
    # - [x] catch interrupt
    i.handler = function(i){
        I <<- i
        NULL
    }
    V = suppressMessages(withCallingHandlers(
        tryCatch(expr, error = e.handler, interrupt = i.handler),
        warning = w.handler,
        message = m.handler
    ))
    list(value=V, error=E, warning=W, message=M, interrupt=I)
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
        val <- paste0("'",format(x[[col]], "%Y-%m-%d %H:%M:%OS"),"'::TIMESTAMPTZ")
    } else if(any(class(x[[col]]) %in% "Date")){
        val <- paste0("'",format(x[[col]], "%Y-%m-%d"),"'::DATE")
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
#' @description Complete logging solution. Writes to database expressions metadata before its evaluation. Evalutes with timing and warning/error/interrupt/messages catching. Updates database entry for processing details: in/out rows, custom metadata, messages/warning/error message. Email on alerts.
#' @param expr expression to be evaluted with logging.
#' @param lazy logical default TRUE, use FALSE when passing language object to \emph{expr} argument.
#' @param alert logical, should be alert flag suppressed on warning/error, including suppressing email notification.
#' @param in_rows integer input DF/DT nrow, \emph{logR} can only guess \emph{out_rows}.
#' @param meta list or a function that will result such list, list of custom fields, each of length 1, list be always the same, fill missing with NA. Default \code{getOption("logR.meta",list())} means no meta columns. If the input is a function then it is evaluated with default values for all argument, and should return a valid list. If you want to change element you need to alter table before or recreate with \link{logR_schema}.
#' @param silent logical, if default \code{getOption("logR.silent",TRUE)} it will not raise warning or error but only log them.
#' @param mail logical if \emph{TRUE} then on alert the email will be send. Requires \emph{mail_args} to be provided. Default \code{getOption("logR.mail",FALSE)}.
#' @param mail_args list - mail not implemented yet.
#' @param boolean logical, default FALSE, when TRUE only `status=="success"` will be returned.  
#' @param .conn DBI connection. Default to \code{getOption("logR.conn",NULL)}.
#' @param .schema character scalar, location in database to store logs, default \code{getOption("logR.schema")}.
#' @param .table character scalar, location in database to store logs, default \code{getOption("logR.table")}.
#' @param .log logical escape parameter, set to \emph{FALSE} to suppress logR process and just execute a call, default to \code{getOption("logR.log",TRUE)}.
#' @return Result of evaluated \emph{expr}, NULL in case of error or interrupt. If \code{silent = FALSE} then error/warning/interrupt will raise warning/error.
#' @note Only first warning/message will be logged to database and send on email.
#' @section Side effects:
#' \itemize{
#' \item entry in database table.
#' \item in case of alerts and email notification configured also the email will be send.
#' }
#' @section Database setup:
#' You can create db objects automatically using \link{logR_schema} function or manually using code from \link{schema_sql}.
#' @section Fatal errors:
#' If your R function will manage to kill whole R session you will see that \emph{status} entry in log table will not get updated and it will stay as \emph{NA}.
#' It might be worth to schedule a watcher task to detect such cases, see \link{logR_watcher}.
#' @seealso \link{logR_schema}, \link{logR_query}
logR = function(expr,
                lazy = TRUE,
                alert = TRUE,
                in_rows = NA_integer_,
                meta = getOption("logR.meta"),
                silent = getOption("logR.silent"),
                mail = getOption("logR.mail"),
                mail_args = getOption("logR.mail_args"),
                boolean = FALSE,
                .conn = getOption("logR.conn"),
                .schema = getOption("logR.schema"),
                .table = getOption("logR.table"),
                .log = getOption("logR.log")){
    if(!is.integer(in_rows)){
        # - [x] `in_rows` handle numeric or integers, raise warning on other types and proceed with `NA_integer_`
        in_rows = if(is.numeric(in_rows)) as.integer(in_rows) else {
            warning("logR 'in_rows' arg should be integer, using NA_integer_.")
            NA_integer_
        }
    }
    stopifnot(is.logical(lazy), is.logical(alert), is.integer(in_rows), is.list(meta) || is.function(meta), is.logical(silent), is.logical(mail), !is.null(.conn), is.character(.table), is.logical(.log))
    
    subexpr = if(lazy) substitute(expr) else expr

    # - [x] allow easy escape from logging using `.log` arg, keep error catching
    if(!isTRUE(.log)){
        r = tryCatch2(expr = eval(subexpr, envir = parent.frame()))
        if(!silent){
            if(!is.null(r$error)){
                stop(r$error)
            } else if(!is.null(r$warning)){
                lapply(r$warning, warning)
            } else if(!is.null(r$interrupt)){
                stop(paste0("Evaluation of following expression has been interrupted.\n", deparse_to_char(subexpr)))
            }
        }
        return(r$value) # potentially NULL
    }

    # - [x] allow to pass function to `meta` argument which makes it dynamically calculated
    if(is.function(meta)) meta = meta()

    # set meta on start
    logr = data.table(logr_start = Sys.time(),
                      expr = deparse_to_char(subexpr),
                      in_rows = in_rows,
                      mail = mail)
    if(length(meta)){
        if(!identical(uniqueN(names(meta)),length(meta))) stop("All elements of 'meta' must be unique named to reflects columns in database.")
        if(!all(sapply(meta, function(x) length(x) == 1L))) stop("All elements of 'meta' arg must have length of 1.")
        if(!all(sapply(meta, is.atomic))) stop("All elements of 'meta' arg must be of atomic type.")
        logr[, c(names(meta)) := meta]
    }

    # - [x] insert db logr entry returning `logr_id`
    ins.tab = paste("INSERT INTO", paste(c(.schema, .table), collapse = "."))
    ins.col = paste0("(",paste(names(logr), collapse=","),")")
    ins.val = paste("VALUES",paste0("(",paste(vapply(names(logr),sql_val,NA_character_,logr), collapse=","),")"))
    ins.ret = paste("RETURNING logr_id")
    sql = paste0(paste(c(ins.tab,ins.col,ins.val,ins.ret), collapse=" "),";")
    tryCatch(
        logr_id <- dbGetQuery(.conn, sql)$logr_id,
        error = function(e) stop(paste0("Make sure you use same list of 'meta' fields each time, if you want to change it you need to alter table to match names and types, debug using below query and error.\n",sql,"\n",as.character(e)), call. = FALSE)
    )
    if(!length(logr_id) || is.na(logr_id)){
        stop("logR failed to achieve value from sequence, ensure that logR is connected.")
    }

    # - [x] evaluate with timing and catch interrupt/messages/warnings/error
    if(isTRUE(getOption("logR.nano")) && requireNamespace("microbenchmarkCore", quietly=TRUE)){
        if(isTRUE(getOption("logR.nano.debug"))) message("Using microbenchmarkCore `get_nanotime` for timing precision.")
        # - [x] use microbenchmarkCore for nano timing when possible
        ts = microbenchmarkCore::get_nanotime()
        r = tryCatch2(expr = eval(subexpr, envir = parent.frame()))
        timing = (microbenchmarkCore::get_nanotime() - ts) * 1e-9
    } else {
        if(isTRUE(getOption("logR.nano.debug"))) message("Using base R `proc.time` for timing precision.")
        ts = proc.time()[[3L]]
        r = tryCatch2(expr = eval(subexpr, envir = parent.frame()))
        timing = proc.time()[[3L]] - ts
    }

    # set meta on end
    logr[,`:=`(logr_id = logr_id,
               logr_end = Sys.time(),
               timing = timing,
               out_rows = if(any(c("data.frame","data.table") %in% class(r$value))) nrow(r$value) else NA_integer_)]
    if(!is.null(r$error)){
        logr[,`:=`(status = "error",
                   alert = alert,
                   cond_call = deparse_to_char(r$error$call),
                   cond_message = r$error$message)]
    } else if(!is.null(r$warning)){
        # - [x] in case of both error and warning then log status='error' and error's condition call+message
        logr[,`:=`(status = "warning",
                   alert = alert,
                   cond_call = deparse_to_char(r$warning[[1L]]$call),
                   cond_message = r$warning[[1L]]$message)]
    } else {
        logr[,`:=`(status = "success",
                   alert = FALSE,
                   cond_call = NA_character_,
                   cond_message = NA_character_)]
    }

    # - [x] log first of the messages - can pass arbitrary string from the logged function to log table
    logr[,`:=`(message = if(!is.null(r$message)) r$message[[1L]]$message else NA_character_)]

    if(!is.null(r$interrupt)){
        # - [x] on interrupt override status and raise alert
        logr[,`:=`(status = "interrupt",
                   alert = TRUE)]
    }

    # - [x] handle R integer max limit (2.147 billion) warn and log alert when 1e6 IDs remains
    if(logr$logr_id >= .Machine$integer.max - 1e6L){
        msg = paste0("restart your logr_id serial column sequence in ",paste(c(.schema,.table),collapse=".")," as it will soon hit the R max integer ",.Machine$integer.max,".")
        warning(msg)
        logr[,`:=`(alert = TRUE,
                   message = paste(msg, message, sep=" Original message: "))]
    }

    # - [x] update db logr entry with timings, statuses, etc.
    cols_to_upd = c("status","alert","logr_end","timing","out_rows","message","cond_call","cond_message")
    upd_set = vapply(cols_to_upd,update_make_set,NA_character_,logr)
    sql = paste0("UPDATE ",paste(c(.schema, .table), collapse=".")," SET ",paste(upd_set,collapse=",")," WHERE logr_id = ", format(logr$logr_id, scientific = FALSE),";")
    tryCatch(
        upd <- dbSendQuery(.conn, sql),
        error = function(e) stop(paste0("Error while update should not happend, debug using below query and error.\n",sql,"\n",as.character(e)), call. = FALSE)
    )

    # - [ ] send mail on alerts
    if(logr$mail && logr$alert){
        stop("Mail feature TO DO for 2.1+")
        # will require jsonlite and httr?
    }

    # - [x] raise error/warning/interrupt for `silent=FALSE`, messages are not forwarded
    if(!silent){
        if(logr$status %in% "error"){
            stop(r$error)
        } else if(logr$status %in% "warning"){
            lapply(r$warning, warning)
        } else if(logr$status %in% "interrupt"){
            stop(paste0("Evaluation of below expression has been interrupted.\n", deparse_to_char(logr$expr)))
        }
    }
    
    # - [x] return just status when boolean==TRUE
    if(boolean) return(identical(logr$status, "success"))

    # - [x] return evaluated expression or NULL on error/interrupt
    return(r$value)
}
