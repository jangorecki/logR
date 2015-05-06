
# prepare data and tests --------------------------------------------------

library(logR)
N <- 1e5
df <- data.frame(a = rnorm(N), b = sample(seq_len(as.integer(log(N))),N,TRUE))
dt <- as.data.table(df)
do_db_test <- function(){
  dfr <- logR(with(df, aggregate(a, list(b), sum)), in_rows=nrow(df))
  dtr <- logR(dt[,.(a=sum(a)),,b], in_rows=nrow(dt))
  err <- logR(sum(1,"a"), tag="sum num and char")
  war <- logR(cor(c(1,1),c(2,3)))
  invisible()
}
dbname=""
user=""
password=""

# postgres sequence -------------------------------------------------------

options("logR.insert.returning" = NULL)
library(RPostgreSQL)
psql <- list(drvName="PostgreSQL", host="localhost", port="5432", dbname=dbname, user=user)
psql$conn <- dbConnect(PostgreSQL(), host=psql$host, port=psql$port, dbname=psql$dbname, user=psql$user, password=password)
opts <- options("logR.db" = TRUE,
                "logR.conn" = psql$conn)
logR_schema("postgres", drop=TRUE)
do_db_test()
logR_query()

# postgres insert returning -----------------------------------------------

insert.returning.postgres <- function(table, logr){
  ins.tab <- paste("INSERT INTO", table)
  ins.col <- paste0("(",paste(names(logr[,-1L,with=FALSE]), collapse=","),")")
  ins.val <- paste("VALUES",paste0("(",paste(vapply(names(logr[,-1L,with=FALSE]),sql_val,NA_character_,logr[,-1L,with=FALSE]), collapse=","),")"))
  paste0(paste(c(ins.tab,ins.col,ins.val,"RETURNING logr_id"), collapse=" "), ";")
}
options("logR.insert.returning" = insert.returning.postgres,
        "logR.insert.driver" = "DBI")
library(RPostgreSQL)
psql <- list(drvName="PostgreSQL", host="localhost", port="5432", dbname=dbname, user=user)
psql$conn <- dbConnect(PostgreSQL(), host=psql$host, port=psql$port, dbname=psql$dbname, user=psql$user, password=password)
opts <- options("logR.db" = TRUE,
                "logR.conn" = psql$conn)
table <- getOption("logR.table")
seq_view <- getOption("logR.seq_view")
try(dbSendQuery(getOption("logR.conn"), paste0("DROP TABLE ",table,";")), silent = TRUE)
try(dbSendQuery(getOption("logR.conn"), paste0("DROP VIEW ",seq_view,";")), silent = TRUE)
try(dbSendQuery(getOption("logR.conn"), "DROP SEQUENCE SEQ_LOGR_ID;"), silent = TRUE)

# create table
create_logr <- schema_sql()["postgres", create_logr]
ct <- strsplit(create_logr,"\n",fixed=TRUE)[[1L]]
ct[2L] <- sub("INTEGER PRIMARY KEY","SERIAL PRIMARY KEY",ct[2L],fixed=TRUE)
dbSendQuery(getOption("logR.conn"), paste(ct,collapse="\n"))

do_db_test()
logR_query()
