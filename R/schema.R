#' @title logR schema scripts dictionary
#' @param table character table name for log storing.
#' @param seq_view character name of view which will query sequence.
#' @note You can PR new database scripts.
#' @seealso \link{logR_schema}, \link{logR}
#' @export
#' @examples
#' # scripts for each vendor
#' schema_sql()[, .(sql = names(.SD)), vendor]
#' # create view statements for each vendor
#' schema_sql()[, create_view, vendor]
#' # print scripts for one vendor
#' invisible(schema_sql()["sqlserver", cat(paste(.SD,collapse="\n")), .SDcols=-"vendor"])
schema_sql <- function(table = getOption("logR.table"), seq_view = getOption("logR.seq_view")){
  data.table(vendor = c("h2","sqlserver","postgres","oracle"),
             create_seq = c("CREATE SEQUENCE SEQ_LOGR_ID MINVALUE 1 MAXVALUE 2147483647;",
                            NA_character_,
                            "CREATE SEQUENCE SEQ_LOGR_ID MINVALUE 1 MAXVALUE 2147483647;",
                            NA_character_),
             create_view = c(paste("CREATE VIEW",seq_view,"AS SELECT SEQ_LOGR_ID.nextval AS logr_id FROM DUAL;"), # h2
                             NA_character_, # sqlserver
                             paste("CREATE VIEW",seq_view,"AS SELECT nextval('SEQ_LOGR_ID') AS logr_id;"), # postgres
                             NA_character_), # oracle
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
#' @param vendor character, currently supported \code{c("h2","sqlserver","postgres","oracle")}.
#' @param .conn DBI connection.
#' @param drop logical, try drop before creation.
#' @seealso \link{schema_sql}, \link{logR}
#' @export
#' @examples
#' if(require("RH2", quietly=TRUE)){
#'   library(RH2)
#'   # setup options and connection
#'   opts <- options("logR.db" = TRUE,
#'                   "logR.conn" = dbConnect(H2(), "jdbc:h2:mem:"))
#'   # run build schema scripts
#'   logR_schema(vendor = "h2")
#'   
#'   # check if exists
#'   dbGetQuery(getOption("logR.conn"),
#'              "SELECT sequence_name, current_value FROM INFORMATION_SCHEMA.SEQUENCES")
#'   dbGetQuery(getOption("logR.conn"),
#'              "SELECT table_type, table_name 
#'               FROM INFORMATION_SCHEMA.TABLES 
#'               WHERE table_schema != 'INFORMATION_SCHEMA'")
#'  dbDisconnect(getOption("logR.conn"))
#' }
logR_schema <- function(vendor = c("h2","sqlserver","postgres","oracle"), .conn = getOption("logR.conn"), drop = FALSE){
  if(class(.conn)[1L]=="H2Connection") dbSendQuery <- RJDBC::dbSendUpdate # remove after RH2#3
  else if(!dbIsValid(.conn)) stop("You must provide valid connection for database.") # remove `else` after RH2#2
  stopifnot(length(vendor) == 1L, vendor %in% c("h2","sqlserver","postgres","oracle"))
  
  if(isTRUE(drop)){
    table <- getOption("logR.table")
    seq_view <- getOption("logR.seq_view")
    try(dbSendQuery(.conn, paste0("DROP TABLE ",table,";")), silent = TRUE)
    try(dbSendQuery(.conn, paste0("DROP VIEW ",seq_view,";")), silent = TRUE)
    try(dbSendQuery(.conn, "DROP SEQUENCE SEQ_LOGR_ID;"), silent = TRUE)
  }
  
  schema_sql()[vendor, lapply(.SD, function(sql, .conn) dbSendQuery(.conn, sql), .conn = .conn), .SDcols=c("create_seq","create_view","create_logr")]
  
  invisible(TRUE)
}
