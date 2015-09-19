#' @title Query logR data
#' @description Query logR logs from defined database or csv file.
#' @param alert logical, if TRUE then filter out non-alerts.
#' @param status logical, if TRUE then filter out success.
#' @param since POSIXt or Date, if provided then filter on start timestamp.
#' @param limit integer, if provided then limit numbers of rows returned.
#' @param .conn DBI connection.
#' @param .table character.
#' @param .schema character.
#' @details By default all function arguments will be taken from options which are used to setup logR logging, for arguments description see \link{logR}.
#' @seealso \link{logR}
logR_query = function(alert, status, since, limit, .conn = getOption("logR.conn"), .table = getOption("logR.table"), .schema = getOption("logR.schema")){
    sql = paste0("SELECT * FROM ",paste(c(.schema,.table),collapse="."))
    where = character()
    if(!missing(alert)){
        if(isTRUE(alert)) where = c(where, "(alert = TRUE OR alert IS NULL)")
    }
    if(!missing(status)){
        if(isTRUE(status)) where = c(where, "(status != 'success' OR status IS NULL)")
    }
    if(!missing(since)){
        if(!any(class(since) %in% c("POSIXct","POSIXlt","Date"))) stop("logR_query 'since' argument must be POSIXt or Date.")
        if(class(since) %in% "Date") since = as.POSIXct(since)
        where = c(where, paste0("logr_start >= '",format(since, "%Y-%m-%d %H:%M:%OS"),"'::TIMESTAMPTZ"))
    }
    if(length(where)){
        sql = paste(sql, paste(where, collapse=" AND "), sep = " WHERE ")
    }
    sql = paste(sql,"ORDER BY logr_id DESC")
    if(!missing(limit)){
        sql = paste(sql, paste("LIMIT",as.integer(limit)))
    }
    sql = paste0(sql,";")
    tryCatch(
        logr <- setDT(dbGetQuery(.conn, sql)),
        error = function(e) stop(paste0("Query to logR table fails. See below sql and error for details.\n",sql,"\n",as.character(e)), call. = FALSE)
    )
    logr[]
}

#' @title Detects invalid logs
#' @description On unlikely fatal error R session crash while evaluting logged expression it will not update \code{status} field in database. This functions helps to detect such cases.
#' @param since POSIXt or Date, must be provided for this function.
#' @details \code{since} parameter default value is current timestamp minus one day, so it should be scheduled daily. If you don't want to provide \code{since} param then use \code{logR_query} directly.
#' @return Will raise error in case if it will find any NULL stats logs. Otherwise NULL invisibly.
#' @seealso \link{logR_query}
logR_watcher = function(since = Sys.time()-86400){
    dt = logR_query(status=TRUE, since=since)[is.na(status)]
    if(nrow(dt) > 0L) stop(paste("Unknown 'status' detected by logR_watcher, count:", nrow(dt)))
    invisible()
}
