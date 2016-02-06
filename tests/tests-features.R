
suppressMessages(library(logR))

# setup connection
logR_connect(dbname = "logrdb", user = "logruser", password = "logrpass")

# public schema
logR_schema(drop = TRUE)
# default
options("logR.schema" = NULL,
        "logR.meta" = list())

# lazy ----

# expected parsed expr
r = character()

# lazy TRUE
logR(1+2)

# lazy FALSE
qexpr = quote(1+2)
logR(qexpr, lazy = FALSE)

# cheat lazy TRUE
eval(substitute(logR(.expr), list(.expr = qexpr)))

# cheat lazy FALSE
logR(quote(1 + 2), lazy = FALSE)

logr = logR_dump()
stopifnot(
    logr[1L,expr]=="1 + 2",
    uniqueN(logr$expr)==1L
)

# microbenchmarkCore debug ----

if(requireNamespace("microbenchmarkCore", quietly = TRUE)){
    op = options(logR.nano.debug = FALSE, logR.nano = FALSE)
    r1 = tryCatch2(logR(1+2))
    options(logR.nano.debug = FALSE, logR.nano = TRUE)
    r2 = tryCatch2(logR(1+2))
    options(logR.nano.debug = TRUE, logR.nano = TRUE)
    r3 = tryCatch2(logR(1+2))
    options(logR.nano.debug = TRUE, logR.nano = FALSE)
    r4 = tryCatch2(logR(1+2))
    options(op)
    stopifnot(
        r1$value==3L, is.null(r1$message),
        r2$value==3L, is.null(r2$message),
        r3$value==3L, !is.null(r3$message), r3$message[[1L]]$message=="Using microbenchmarkCore `get_nanotime` for timing precision.\n",
        r4$value==3L, !is.null(r4$message), r4$message[[1L]]$message=="Using base R `proc.time` for timing precision.\n"
    )
}

# preview logs ----

logr = logR_dump()
print(logr)

# disconnect ----
logR_disconnect()
