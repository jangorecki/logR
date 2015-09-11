suppressMessages(library(logR))

# create db and user is, will be done in travis

# meta columns to log

meta = local({
    batch_id = as.integer(Sys.time())
    function(ruser = "someuser", comment = NA_character_) list(batch_id=batch_id, ruser=ruser, comment=comment)
})
create_meta = list(batch_id = "INTEGER", ruser = "VARCHAR(255)", comment = "VARCHAR(255)")

# setup connection, options and schema

conn = dbConnect(PostgreSQL(), 
                 host="192.168.56.101", 
                 port="5432", 
                 dbname="rdb",
                 user="ruser",
                 password="userpassr")
options("logR.conn" = conn,
        "logR.schema" = "logr",
        "logR.meta" = meta())

logR_schema(meta = create_meta, drop=TRUE)

# log expressions

N = 1e5
set.seed(1)
df = data.frame(a = rnorm(N), b = sample(seq_len(as.integer(log(N))),N,TRUE))
dt = as.data.table(df)

dfr = logR(with(df, aggregate(a, list(b), sum)), in_rows=nrow(df))
dtr = logR(dt[,.(a=sum(a)),,b], in_rows=nrow(dt), meta=meta())
err = logR(sum(1,"a"), meta=meta(comment="sum num and char"))
war = logR(cor(c(1,1),c(2,3)), meta=meta(ruser="nobody"))

# verify logs

r = logR_query()
invisible(dbDisconnect(conn))

if(!isTRUE(all.equal(
    r[order(logr_id),.(expr, status, alert, ruser, comment)],
    data.table(expr = c("with(df, aggregate(a, list(b), sum))", "dt[, .(a = sum(a)), , b]", "sum(1, \"a\")", "cor(c(1, 1), c(2, 3))"),
               status = c("success", "success", "error", "warning"), 
               alert = c(FALSE, FALSE, TRUE, TRUE), 
               ruser = c("someuser", "someuser", "someuser", "nobody"), 
               comment = c(NA, NA, "sum num and char", NA))
))) stop("Fetched logs not matched to expected")

q("no")
