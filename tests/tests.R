
# initialize --------------------------------------------------------------

suppressMessages(library(logR))

# meta columns to log

meta = local({
    batch_id = as.integer(Sys.time())
    function(ruser = "someuser", comment = NA_character_) list(batch_id=batch_id, ruser=ruser, comment=comment)
})
create_meta = list(batch_id = "INTEGER", ruser = "VARCHAR(255)", comment = "VARCHAR(255)")

# setup connection, options and schema

logR_connect()
options("logR.schema" = "logr",
        "logR.meta" = meta())
logR_schema(meta = create_meta)

# populate scenarios ------------------------------------------------------

# simple
w1 = function(){ warning("w1"); "ok" }
e1 = function(){ stop("e1") }
m1 = function(){ message("m1"); "ok" }
# cascade
w2 = function(){ w1(); warning("w2"); "ok" }
m2 = function(){ m1(); message("m2"); "ok" }
# combined cascade
m1w1 = function(){ m1(); w1() }
w2m2 = function(){ w2(); m2() }
m2e1 = function(){ m2(); e1() }
w2e1 = function(){ w2(); e1() }
m1w1e1 = function(){ m1(); w1(); e1() }
# nested
n1 = function(){ logR(m1w1()) }
n2 = function(){ logR(m1w1()); m1w1() }
n1m1w1 = function(){ logR(m1w1(), silent=FALSE) }
n2m1w1 = function(){ logR(m1w1(), silent=FALSE); m1w1() }
# nested recursive
nX = function(i){ if(i > 1L) logR(nX(i = i-1L)) else i }

# tests -------------------------------------------------------------------

# ad-hoc
N = 1e5
set.seed(1)
dt = data.table(a = rnorm(N), b = sample(seq_len(as.integer(log(N))),N,TRUE))
df = as.data.frame(dt)
dfr = logR(with(df, aggregate(a, list(b), sum)), in_rows=nrow(df))
dtr = logR(dt[,.(a=sum(a)),,b], in_rows=nrow(dt), meta=meta())
err = logR(sum(1,"a"), meta=meta(comment="sum num and char"))
war = logR(cor(c(1,1),c(2,3)), alert=FALSE, meta=meta(ruser="nobody"))

# scenarios
logR(w1())
logR(e1())
logR(m1())
logR(w2())
logR(m2())
logR(m1w1())
logR(w2m2())
logR(m2e1())
logR(w2e1())
logR(m1w1e1())
logR(n1())
logR(n2())
logR(n1m1w1())
logR(n2m1w1())
logR(nX(3L))

# verify ------------------------------------------------------------------

r = logR_query()
invisible(logR_disconnect())

print(r)

if(!isTRUE(all.equal(
    r[, .(logr_id, expr, status, alert, in_rows, out_rows, mail, message, cond_call, cond_message, ruser, comment)],
    data.table(logr_id = c(25:1),
               expr = c("nX(i = i - 1L)", "nX(i = i - 1L)", "nX(3L)", "m1w1()", "n2m1w1()", "m1w1()", "n1m1w1()", "m1w1()", "n2()", "m1w1()", "n1()", "m1w1e1()", "w2e1()", "m2e1()", "w2m2()", "m1w1()", "m2()", "w2()", "m1()", "e1()", "w1()", "cor(c(1, 1), c(2, 3))", "sum(1, \"a\")", "dt[, .(a = sum(a)), , b]", "with(df, aggregate(a, list(b), sum))"),
               status = c("success", "success", "success", "warning", "warning", "warning", "warning", "warning", "warning", "warning", "success", "error", "error", "error", "warning", "warning", "success", "warning", "success", "error", "warning", "warning", "error", "success", "success"), 
               alert = c(FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE), 
               in_rows = c(rep(NA, 23L), 100000L, 100000L), 
               out_rows = c(rep(NA, 23L), 11L, 11L), 
               mail = rep(FALSE, 25L), message = c(NA, NA, NA, "m1\n", "m1\n", "m1\n", NA, "m1\n", "m1\n", "m1\n", NA, "m1\n", NA, "m1\n", "m1\n", "m1\n", "m1\n", NA, "m1\n", NA, NA, NA, NA, NA, NA), 
               cond_call = c(NA, NA, NA, "w1()", "w1()", "w1()", "w1()", "w1()", "w1()", "w1()", NA, "e1()", "e1()", "e1()", "w1()", "w1()", NA, "w1()", NA, "e1()", "w1()", "cor(c(1, 1), c(2, 3))", "sum(1, \"a\")", NA, NA), 
               cond_message = c(NA, NA, NA, "w1", "w1", "w1", "w1", "w1", "w1", "w1", NA, "e1", "e1", "e1", "w1", "w1", NA, "w1", NA, "e1", "w1", "the standard deviation is zero", "invalid 'type' (character) of argument", NA, NA), 
               ruser = c(rep("someuser", 21L), "nobody", rep("someuser", 3L)), 
               comment = c(rep(NA, 22L), "sum num and char", rep(NA, 2L)))
))) stop("Fetched logs not matching to expected content.")

q("no")
