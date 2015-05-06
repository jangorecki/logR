#' @title Query logR data
#' @description Query logR logs from defined database or csv file.
#' @param .db logical.
#' @param .conn DBI connection.
#' @param .table character.
#' @param .wd character working directory path to be used when logging to csv file.
#' @details By default all function arguments will be taken from options which are used to setup logR logging, for arguments description see \link{logR}.
#' @seealso \link{logR}, \link{logR_browser}
#' @export
#' @examples
#' logR(Sys.sleep(0.01))
#' logR_query()
#' file.remove("LOGR.csv")
logR_query <- function(.db = getOption("logR.db"), .conn = getOption("logR.conn"), .table = getOption("logR.table"), .wd = getOption("logR.wd",getwd())){
  if(is.null(.db)) stop("logR options has not been set up, reload the logR package or set 'logR.db' option.")
  else if(!isTRUE(.db)){
    log_file <- paste(.wd,paste(.table,"csv",sep="."),sep="/")
    if(!file.exists(log_file)) stop("logR is set to log into csv file but there is no no csv file in working directory.")
  }
  else if(isTRUE(.db)){
    # if(!dbIsValid(.conn)) stop("You must provide valid connection for database.") # uncomment after RH2#2
  }
  logr_id <- logr_start_int <- logr_start <- status <- logr_end_int <- logr_end <- timing <- in_rows <- out_rows <- tag <- mail <- cond_call <- cond_message <- NULL
  dt <- if(!isTRUE(.db)) setDT(read.table(paste(.wd,paste(.table,"csv",sep="."),sep="/"), sep=",", header=TRUE, na.strings=""))[] else setDT(dbGetQuery(.conn, paste("SELECT * FROM",.table)))
  dt[,`:=`(logr_id = as.integer(logr_id),
           logr_start_int = as.integer(logr_start_int),
           logr_start = as.POSIXct(logr_start, origin="1970-01-01"),
           call = as.character(call),
           status = as.character(status),
           logr_end_int = as.integer(logr_end_int),
           logr_end = as.POSIXct(logr_end, origin="1970-01-01"),
           timing = as.numeric(timing),
           in_rows = as.integer(in_rows),
           out_rows = as.integer(out_rows),
           tag = as.character(tag),
           mail = as.logical(mail),
           cond_call = as.character(cond_call),
           cond_message = as.character(cond_message))
     ][]
}

#' @title Browse logR data
#' @description Shiny application which simply displays the logR logs.
#' @seealso \link{logR}, \link{logR_query}
#' @export
#' @examples
#' library(data.table)
#' N <- 1e5
#' df <- data.frame(a = rnorm(N), b = sample(seq_len(as.integer(log(N))),N,TRUE))
#' dt <- as.data.table(df)
#' dfr <- logR(with(df, aggregate(a, list(b), sum)), in_rows=nrow(df))
#' dtr <- logR(dt[,.(a=sum(a)),,b], in_rows=nrow(dt))
#' err <- logR(sum(1,"a"))
#' war <- logR(cor(c(1,1),c(2,3)))
#' if(interactive()) logR_browser()
#' file.remove("LOGR.csv")
logR_browser <- function(){
  if(!requireNamespace("shiny", quietly=TRUE)){
    stop("shiny package required to browse logR data")
  } else{
    # options
    options("logR.wd" = getwd())
    # shinyApp
    shiny::runApp(system.file("shinyApp", package = "logR"))
  }
}
