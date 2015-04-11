#' @title Query logR data
#' @description Query logR logs from defined database or csv file.
#' @param .db logical.
#' @param .conn character.
#' @param .table character.
#' @param .db.conns list of connection defined for \link[dwtools]{db}.
#' @param .wd character working directory path to be used when logging to csv file.
#' @details By default all function arguments will be taken from options which are used to setup logR logging, for arguments description see \link{logR}.
#' @seealso \link{logR}, \link{logR_browser}
#' @export
#' @examples
#' logR(Sys.sleep(0.01))
#' logR_query() 
logR_query <- function(.db = getOption("logR.db"), .conn = getOption("logR.conn"), .table = getOption("logR.table"), .db.conns = getOption("dwtools.db.conns"), .wd = getOption("logR.wd",getwd())){
  if(is.null(.db)) stop("logR options has not been set up, reload the logR package or set 'logR.db' option.")
  else if(!isTRUE(.db)){
    log_file <- paste(.wd,paste(.table,"csv",sep="."),sep="/")
    if(!file.exists(log_file)) stop("logR is set to log into csv file but there is no no csv file in working directory.")
  }
  else if(isTRUE(.db)){
    if(is.null(.conn)) stop("logR is set to log into database but no database connection name defined in 'logR.conn' option.")
    if(is.null(.db.conns)) stop("logR is set to log into database but no database connections defined in 'dwtools.db.conns' option.")
    if(!(.conn %in% .db.conns)) stop("logR database connection name is not matching to names of connections defined in 'dwtools.db.conns' option.")
  }
  
  if(!isTRUE(.db)) setDT(read.table(paste(.wd,paste(.table,"csv",sep="."),sep="/"), sep=",", header=TRUE))[]
  else db(.table,.conn)
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
logR_browser <- function(){
  if(!requireNamespace("shiny",quietly=TRUE)){
    stop("shiny package required to browse logR data")
  } else{
    # options
    options("logR.wd" = getwd())
    # shinyApp
    shiny::runApp(system.file("shinyApp", package = "logR"))
  }
}
