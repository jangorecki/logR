#' @title logR schema sql scripts dictionary
#' @param table character table name for log storing.
#' @param seq_view character name of view which will query sequence.
#' @note You can PR new database scripts.
#' @seealso \link{logR_schema}, \link{logR}
#' @export
#' @examples
#' # available vendors
#' schema_sql()[, vendor]
#' # scripts for each vendor
#' schema_sql()[, .(sql = names(.SD)), vendor]
#' # create view statements for each vendor
#' schema_sql()[, create_view, vendor]
#' # print scripts for one vendor
#' invisible(schema_sql()["sqlserver", cat(paste(.SD,collapse="\n")), .SDcols=-"vendor"])
schema_sql <- function(table = getOption("logR.table"), seq_view = getOption("logR.seq_view")){
  data.table(vendor = c("h2","sqlserver","postgres","oracle"),
             create_seq = c("CREATE SEQUENCE SEQ_LOGR_ID MINVALUE 1 MAXVALUE 2147483647;",
                            "CREATE SEQUENCE SEQ_LOGR_ID MINVALUE 1 MAXVALUE 2147483647;",
                            "CREATE SEQUENCE SEQ_LOGR_ID MINVALUE 1 MAXVALUE 2147483647;",
                            "CREATE SEQUENCE SEQ_LOGR_ID MINVALUE 1 MAXVALUE 2147483647;"),
             create_view = c(paste("CREATE VIEW",seq_view,"AS SELECT SEQ_LOGR_ID.nextval AS logr_id FROM DUAL;"), # h2
                             paste("CREATE VIEW",seq_view,"AS SELECT NEXT VALUE FOR SEQ_LOGR_ID AS logr_id;"), # sqlserver
                             paste("CREATE VIEW",seq_view,"AS SELECT nextval('SEQ_LOGR_ID') AS logr_id;"), # postgres
                             paste("CREATE VIEW",seq_view,"AS SELECT SEQ_LOGR_ID.nextval AS logr_id FROM DUAL;")), # oracle
             create_logr = c(paste('CREATE TABLE',table,'(
                                   "logr_id" INTEGER PRIMARY KEY,
                                   "logr_start_int" INTEGER,
                                   "logr_start" VARCHAR(255),
                                   "call" VARCHAR(255),
                                   "status" VARCHAR(255),
                                   "logr_end_int" INTEGER,
                                   "logr_end" VARCHAR(255),
                                   "timing" DOUBLE PRECISION,
                                   "in_rows" INTEGER,
                                   "out_rows" INTEGER,
                                   "tag" VARCHAR(255),
                                   "mail" INTEGER,
                                   "cond_call" VARCHAR(255),
                                   "cond_message" VARCHAR(255)
                                   );'), # h2
                             paste('CREATE TABLE',table,'(
                                   "logr_id" INTEGER PRIMARY KEY,
                                   "logr_start_int" INTEGER,
                                   "logr_start" VARCHAR(255),
                                   "call" VARCHAR(255),
                                   "status" VARCHAR(255),
                                   "logr_end_int" INTEGER,
                                   "logr_end" VARCHAR(255),
                                   "timing" DOUBLE PRECISION,
                                   "in_rows" INTEGER,
                                   "out_rows" INTEGER,
                                   "tag" VARCHAR(255),
                                   "mail" INTEGER,
                                   "cond_call" VARCHAR(255),
                                   "cond_message" VARCHAR(255)
                                   );'), # sqlserver
                             paste('CREATE TABLE',table,'(
                                   "logr_id" INTEGER PRIMARY KEY,
                                   "logr_start_int" INTEGER,
                                   "logr_start" VARCHAR(255),
                                   "call" VARCHAR(255),
                                   "status" VARCHAR(255),
                                   "logr_end_int" INTEGER,
                                   "logr_end" VARCHAR(255),
                                   "timing" DOUBLE PRECISION,
                                   "in_rows" INTEGER,
                                   "out_rows" INTEGER,
                                   "tag" VARCHAR(255),
                                   "mail" INTEGER,
                                   "cond_call" VARCHAR(255),
                                   "cond_message" VARCHAR(255)
                                   );'), # postgres
                             paste('CREATE TABLE',table,'(
                                   "logr_id" INTEGER PRIMARY KEY,
                                   "logr_start_int" INTEGER,
                                   "logr_start" VARCHAR(255),
                                   "call" VARCHAR(255),
                                   "status" VARCHAR(255),
                                   "logr_end_int" INTEGER,
                                   "logr_end" VARCHAR(255),
                                   "timing" DOUBLE PRECISION,
                                   "in_rows" INTEGER,
                                   "out_rows" INTEGER,
                                   "tag" VARCHAR(255),
                                   "mail" INTEGER,
                                   "cond_call" VARCHAR(255),
                                   "cond_message" VARCHAR(255)
                                   );')), # oracle
                           key = "vendor")
}

#' @title Populate logR schema
#' @description There are three database objects required, all are populated by this function call. To view scripts see \link{schema_sql}.
#' @param conn.name character name of defined db connection. See examples.
#' @param vendor character, currently supported \code{c("h2","sqlserver","postgres","oracle")}.
#' @param drop logical, try drop before creation.
#' @seealso \link{schema_sql}, \link{logR}
#' @export
#' @examples
#' if(requireNamespace("RH2",quietly=TRUE)){
#'   library(RH2)
#'   # define connection
#'   h2 <- list(drvName = "JDBC", conn = dbConnect(H2(), "jdbc:h2:mem:"))
#'   # setup options and connection
#'   opts <- options("dwtools.db.conns"=list(h2=h2),
#'                   "logR.db" = TRUE,
#'                   "logR.conn" = "h2")
#'   # run build schema scripts
#'   logR_schema(vendor = "h2")
#'   
#'   # check if exists
#'   library(dwtools)
#'   db("SELECT sequence_name, current_value FROM INFORMATION_SCHEMA.SEQUENCES")
#'   db("SELECT table_type, table_name
#'       FROM INFORMATION_SCHEMA.TABLES
#'       WHERE table_schema != 'INFORMATION_SCHEMA'")
#' }
logR_schema <- function(conn.name = getOption("logR.conn"), vendor = c("h2","sqlserver","postgres","oracle"), drop = FALSE){
  if(is.null(conn.name)) stop("You must provide connection name for database.")
  .conn <- conn.name
  .db.conns <- names(getOption("dwtools.db.conns"))
  if(!(.conn %in% .db.conns)) stop("Provided database connection name in 'conn.name' was not set up in getOption('dwtools.db.conns'). Read ?logR or ?dwtools::db")
  stopifnot(length(vendor) == 1L, vendor %in% c("h2","sqlserver","postgres","oracle"))
  
  if(isTRUE(drop)){
    table <- getOption("logR.table")
    seq_view <- getOption("logR.seq_view")
    try(db(paste0("DROP TABLE ",table,";"),.conn), silent = TRUE)
    try(db(paste0("DROP VIEW ",seq_view,";"),.conn), silent = TRUE)
    try(db("DROP SEQUENCE SEQ_LOGR_ID;",.conn), silent = TRUE)
  }
  
  schema_sql()[vendor, lapply(.SD,db,.conn), .SDcols=c("create_seq","create_view","create_logr")]
  
  invisible(TRUE)
}
