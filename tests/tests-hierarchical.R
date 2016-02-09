
suppressMessages(library(logR))

# setup connection
logR_connect(dbname = "logrdb", user = "logruser", password = "logrpass")

# public schema
logR_schema(drop = TRUE)
# default
options("logR.schema" = NULL,
        "logR.meta" = list())

# parent `logr_id` passing ----

f = function(x) logR(sum(x))
g = function(x) logR(f(x))
h = function(x) list(logR(g(x)), logR(g(x)))
logR(h(1:4))
logR(h(1:4))

logr = logR_dump()
stopifnot(
    nrow(logr)==14L,
    identical(logr[is.na(parent_id), which = TRUE], c(1L, 8L)),
    all.equal(logr[1:4, .(logr_id, parent_id, expr)], data.table(logr_id = 1:4, parent_id = c(NA, 1L, 2L, 3L), expr = c("h(1:4)", "g(x)", "f(x)", "sum(x)")))
)

print(logr)

# disconnect ----
logR_disconnect()
