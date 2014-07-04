#' @title logR
#' @docType package
#' @import mailR data.table
#' @name logR
NULL

#' @title measure and log
#' @param expr expression to measure and log
#' @param gcFirst clean cache, use when comparing timings
#' @param silent do not raise warning/error, no email alert too
#' @param mail a list of args to be passed to \code{\link{send.mail}}, body element will be overwritten by alert.
#' @param tag character
#' @param in_rows integer input DT nrow
#' @param conn database connection
#' @param log_table length 2 vector of schema and table name, default \code{c("public","logR")}
#' @param verbose integer print status messages to console
#' @param ddl only create table for logR logs, skip processing
#' @param ddl.purge logical if drop exists log table and recreate
#' @description Complete log solution, log status, timing, in-out rows to database. In case of alert also send email.
#' @return result of expr, unless it's error
logR <- function(expr, gcFirst = FALSE, silent = FALSE, mail = list(),
                 tag = NA_character_, 
                 in_rows = NA_integer_,
                 conn = NULL,
                 log_table = c("public","logR"), # schema, table
                 verbose = 0, ddl = FALSE, ddl.purge = FALSE){
  if(ddl){
    DT <- data.table(timestamp = structure(numeric(0), class = c("POSIXct","POSIXt"), tzone = "UTC"), 
                            status = character(0), alert = logical(0),
                            parent_fun = character(0), tag = character(0), in_rows = integer(0), 
                            out_rows = integer(0),
                            elapsed = numeric(0), call = character(0), message = character(0))
    if(dbExistsTable(conn = conn, name = log_table)){
      if(ddl.purge) dbRemoveTable(conn = conn, name = log_table)
      else stop(paste("table",paste(log_table,collapse="."),"already exists, use 'ddl.purge' arg to drop existing tables"))
    }
    stopifnot(dbWriteTable(conn = conn, name = log_table, 
                           value = DT, 
                           append = FALSE, row.names = FALSE))
    if(verbose > 0) message(paste0("table created: ",paste(log_table,collapse="."),", consider index on that table"))
    return(DT)
  }
  if(gcFirst) gc(FALSE)
  parent_fun <- if(is.call(sys.call(-1)[1])) as.character(sys.call(-1)[1]) else NA_character_
  ptm <- proc.time()[[3]]
  status <- "success"
  alert <- FALSE
  call <- NA_character_
  message <- NA_character_
  browser()
  r <- tryCatch(expr = eval.parent(substitute(expr)),
                warning = function(w){
                  status <<- "warning"
                  alert <<- !silent
                  call <<- paste(as.character(w$call),collapse=" ")
                  message <<- w$message
                  expr
                },
                error = function(e){
                  status <<- "error"
                  alert <<- !silent
                  call <<- paste(as.character(e$call),collapse=" ")
                  message <<- e$message
                  e
                })
  DT <- data.table(timestamp = Sys.time(), status = status, alert = alert,
                   parent_fun = parent_fun, tag = tag, in_rows = in_rows, 
                   out_rows = nrowDT(r),
                   elapsed = round(proc.time()[[3]] - ptm,3), 
                   call = call, message = message)
  if(verbose > 0) print(DT)
  if(alert){
    msgDT <- DT[,{
      msg <- sprintf("%s: %s: ALERT: %s: %s: %s: %s", timestamp, parent_fun, tag, status, call, message)
      warning(msg, call.=FALSE, immediate. = TRUE)
      list(msg = msg)
    }]
    if(length(mail) > 1){
      mail$body <- msgDT[["msg"]]
      do.call(send.mail, args = mail)
    }
  }
  stopifnot(dbWriteTable(conn = conn, name = log_table, value = DT, row.names = FALSE, append = TRUE))
  return(r)
}

#' @title nrow if DT else NA
#' @keywords internal
nrowDT <- function(x){
  if(any(c("data.frame","data.table") %in% class(x))) nrow(x)
  else NA_integer_
}

#' @title test logR
#' @keywords internal
logR_tester <- function(case){
  Sys.sleep(runif(1))
  r <- if(case==0){
    NULL
  } else if(case==1){
    numeric(0)
  } else if(case==2){
    character(1)
  } else if(case==3){
    data.table(a = 1:12, b = letters[1:12])
  } else if(case==4){
    warning("case 4 warning")
    data.table(a = 1:12, b = letters[1:12])
  } else if(case==5){
    warning("case 5a warning")
    warning("case 5b warning")
    data.table(NULL)
  } else if(case==6){
    stop("case 6 raise error")
    data.table(a = 1:12, b = letters[1:12])
  } else stop("unknown 'case' arg in logR_tester") 
  return(r)
}
