#' @title logR schema scripts dictionary
#' @param table character table name for log storing.
#' @param seq_view character name of view which will query sequence.
#' @param schema character schema name for log storing.
#' @param meta list of metadata columns, postgres data types as character string named by column name. Avoid reserved postgres keywords.
#' @seealso \link{logR_schema}, \link{logR}
schema_sql = function(table = getOption("logR.table"), seq_view = getOption("logR.seq_view"), schema = getOption("logR.schema"), meta = list()){
    list(
        schema = if(length(schema)) paste0("CREATE SCHEMA ",schema,";"),
        sequence = paste("CREATE SEQUENCE",paste(c(schema,"SEQ_LOGR_ID"),collapse="."),"MINVALUE 1 MAXVALUE 2147483647;"),
        view = paste("CREATE VIEW",paste(c(schema,seq_view),collapse="."),"AS SELECT",paste0("nextval('",paste(c(schema,"SEQ_LOGR_ID"), collapse="."),"')"),"AS logr_id;"),
        table = paste0('CREATE TABLE ', paste(c(schema,table),collapse="."), ' (
                                         "logr_id" INTEGER PRIMARY KEY,
                                         "logr_start_int" INTEGER,
                                         "logr_start" TIMESTAMP,
                                         "expr" VARCHAR(255),
                                         "status" VARCHAR(255),
                                         "alert" BOOLEAN,
                                         "logr_end_int" INTEGER,
                                         "logr_end" TIMESTAMP,
                                         "timing" DOUBLE PRECISION,
                                         "in_rows" INTEGER,
                                         "out_rows" INTEGER,
                                         "mail" BOOLEAN,
                                         "cond_call" VARCHAR(255),
                                         "cond_message" VARCHAR(255)',
                       if(length(meta)) paste0(",",paste(paste0('"',names(meta),'"'), unlist(meta), sep = " ", collapse=", "))
                       ,');')
    )
}

#' @title Populate logR schema
#' @description There are three database objects required, plus optional schema, all are populated by this function call. To view scripts see \link{schema_sql}.
#' @param meta list of metadata columns, postgres data types as character string named by column name. Avoid reserved postgres keywords.
#' @param .conn DBI connection.
#' @param drop logical, try drop before creation.
#' @seealso \link{schema_sql}, \link{logR}
logR_schema = function(meta = list(), .conn = getOption("logR.conn"), drop = FALSE){
    if(length(meta)){
        if(!all(sapply(meta, function(x) length(x) == 1L))) stop("All elements of 'meta' arg must have length of 1.")
    }
    if(isTRUE(drop)){
        schema = getOption("logR.schema")
        table = getOption("logR.table")
        seq_view = getOption("logR.seq_view")
        try(dbSendQuery(.conn, paste0("DROP TABLE ",paste(c(schema,table),collapse="."),";")), silent = TRUE)
        try(dbSendQuery(.conn, paste0("DROP VIEW ",paste(c(schema,seq_view),collapse="."),";")), silent = TRUE)
        try(dbSendQuery(.conn, paste0("DROP SEQUENCE ",paste(c(schema,"SEQ_LOGR_ID"),collapse="."),";")), silent = TRUE)
        if(length(schema)) try(dbSendQuery(.conn, paste0("DROP SCHEMA ",schema,";")), silent = TRUE)
    }
    lapply(schema_sql(meta = meta), function(sql, .conn) if(length(sql)) dbSendQuery(.conn, sql), .conn = .conn)
    invisible(TRUE)
}
