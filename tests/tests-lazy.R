
suppressMessages(library(logR))

# setup connection
logR_connect(dbname = "logrdb", user = "logruser", password = "logrpass")

# public schema
logR_schema(drop = TRUE)

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

print(logr)

# disconnect
logR_disconnect()
