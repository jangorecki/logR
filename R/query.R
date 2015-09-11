#' @title Query logR data
#' @description Query logR logs from defined database or csv file.
#' @param .conn DBI connection.
#' @param .table character.
#' @param .schema character.
#' @details By default all function arguments will be taken from options which are used to setup logR logging, for arguments description see \link{logR}.
#' @seealso \link{logR}, \link{logR_browser}
logR_query = function(where, .conn = getOption("logR.conn"), .table = getOption("logR.table"), .schema = getOption("logR.schema")){
    sql = paste0("SELECT * FROM ",paste(c(.schema,.table),collapse="."))
    tryCatch(
        logr <- setDT(dbGetQuery(.conn, sql)),
        error = function(e) stop(paste0("Query to logR table fails. See below sql and error for details.\n",sql,"\n",as.character(e)), call. = FALSE)
    )
    logr[]
}
