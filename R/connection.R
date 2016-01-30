#' @title Connect logR to postgres
#' @description Wrapper on \code{DBI::dbConnect} with defaults from envir.
#' @param host character hostname/ip by default from ENV var `POSTGRES_HOST`
#' @param port character port by default from ENV var `POSTGRES_PORT`
#' @param dbname character port by default from ENV var `POSTGRES_DB`
#' @param user character port by default from ENV var `POSTGRES_USER`
#' @param password character port by default from ENV var `POSTGRES_PASSWORD`
#' @param quoted logical if TRUE it will return unevaluated expression which can be passed to remote R session. Database authentication will be included in expression.
#' @return logical, if successfully connected then TRUE, otherwise FALSE. Use \code{getOption("logR.conn")} to access actual connection, which is set as side effect.
logR_connect = function(host = Sys.getenv("POSTGRES_HOST", "127.0.0.1"),
                        port = Sys.getenv("POSTGRES_PORT", "5432"),
                        dbname = Sys.getenv("POSTGRES_DB", "postgres"),
                        user = Sys.getenv("POSTGRES_USER", "postgres"),
                        password = Sys.getenv("POSTGRES_PASSWORD", "postgres"),
                        quoted = FALSE){
    q.dbConnect = substitute({
        r = tryCatch2(dbConnect(PostgreSQL(), host = .host, port = .port, dbname = .dbname, user = .user, password = .password))
        if(is.null(r$error) && is.null(r$warning) && typeof(r$value)=="S4" && inherits(r$value, "PostgreSQLConnection")){
            options("logR.conn" = r$value)
            r = TRUE
        } else r = FALSE
        r
    }, env = list(.host = host, .port = port, .dbname = dbname, .user = user, .password = password))
    if(!quoted) eval(q.dbConnect) else q.dbConnect
}

#' @title Disconnect logR from postgres
#' @description Simple wrapper to \code{DBI::dbDisconnect}.
#' @param quoted logical if TRUE it will return unevaluated expression which can be passed to remote R session. Database authentication will be included in expression.
logR_disconnect = function(quoted = FALSE){
    if(!quoted) dbDisconnect(getOption("logR.conn")) else quote(dbDisconnect(getOption("logR.conn")))
}
