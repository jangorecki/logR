#' @title Query logR data
#' @description Query logR logs from defined database or csv file.
#' @param alert logical, if TRUE then filter to alerts.
#' @param status logical, if TRUE then filter to non-success.
#' @param since POSIXt or Date, if provided then filter on start timestamp.
#' @param .conn DBI connection.
#' @param .table character.
#' @param .schema character.
#' @details By default all function arguments will be taken from options which are used to setup logR logging, for arguments description see \link{logR}.
#' @seealso \link{logR}
logR_query = function(alert, status, since, .conn = getOption("logR.conn"), .table = getOption("logR.table"), .schema = getOption("logR.schema")){
    sql = paste0("SELECT * FROM ",paste(c(.schema,.table),collapse="."))
    where = character()
    if(!missing(alert)){
        if(isTRUE(alert)) where = c(where, "alert = TRUE")
    }
    if(!missing(status)){
        if(isTRUE(status)) where = c(where, "status != 'success'")
    }
    if(!missing(since)){
        if(!class(since) %in% c("POSIXct","POSIXlt","Date")) stop("logR_query 'since' argument must be POSIXt or Date.")
        if(class(since) %in% "Date") since = as.POSIXct(since)
        where = c(where, paste0("logr_start >= '",format(since, "%Y-%m-%d %H:%M:%OS"),"'::timestamp"))
    }
    if(length(where)){
        sql = paste(sql, paste(where, collapse=" AND "), sep = " WHERE ")
    }
    sql = paste0(sql,";")
    tryCatch(
        logr <- setDT(dbGetQuery(.conn, sql)),
        error = function(e) stop(paste0("Query to logR table fails. See below sql and error for details.\n",sql,"\n",as.character(e)), call. = FALSE)
    )
    logr[]
}
