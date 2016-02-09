#' @title logR schema scripts dictionary
#' @param schema character schema name for log storing.
#' @param table character table name for log storing.
#' @param meta list of metadata columns, postgres data types as character string named by column name like \code{list(tag='VARCHAR(255)', val_col='DOUBLE PRECISION', int_col='INTEGER', systimecol='TIMESTAMPTZ')}. Avoid reserved postgres keywords.
#' @description Use \link{logR_schema} to execute all scripts at once.
#' @seealso \link{logR_schema}, \link{logR}
#' @examples 
#' sql = schema_sql(schema="logr")
#' invisible(lapply(sql, sapply, cat, sep="\n"))
schema_sql = function(schema = getOption("logR.schema"), table = getOption("logR.table"), meta = list()){
    # - [x] drop and create sql scripts for schema (if provided) and table
    list(
        drop = list(
            table =  paste0("DROP TABLE ",paste(c(schema,table),collapse="."),";"),
            schema = if(length(schema)) paste0("DROP SCHEMA ",schema,";")
        ),
        create = list(
            schema = if(length(schema)) paste0("CREATE SCHEMA ",schema,";"),
            table = paste0('CREATE TABLE ', paste(c(schema,table),collapse="."), ' (',
                           '"logr_id" SERIAL NOT NULL PRIMARY KEY,',
                           '"parent_id" INTEGER,',
                           '"logr_start" TIMESTAMPTZ,',
                           '"expr" VARCHAR(255),',
                           '"status" VARCHAR(255),',
                           '"alert" BOOLEAN,',
                           '"logr_end" TIMESTAMPTZ,',
                           '"timing" DOUBLE PRECISION,',
                           '"in_rows" INTEGER,',
                           '"out_rows" INTEGER,',
                           '"mail" BOOLEAN,',
                           '"message" VARCHAR(255),',
                           '"cond_call" VARCHAR(255),',
                           '"cond_message" VARCHAR(255)',
                           # - [x] support for custom list of metadata columns to create table sql script
                           if(length(meta)) paste0(",",paste(paste0('"',names(meta),'"'), unlist(meta), sep = " ", collapse=", "))
                           ,');')
        )
    )
}

#' @title Populate logR schema
#' @description Function will execute scripts create schema and table to store logs. To view scripts see \link{schema_sql}.
#' @param meta list of metadata columns, postgres data types as character string named by column name like \code{list(tag='VARCHAR(255)', val_col='DOUBLE PRECISION', int_col='INTEGER', systimecol='TIMESTAMPTZ')}. Avoid reserved postgres keywords.
#' @param drop logical, drop before creation.
#' @param silent logical, silently try calls to db
#' @param .conn DBI connection.
#' @seealso \link{schema_sql}, \link{logR}
logR_schema = function(meta = list(), drop = FALSE, silent = TRUE, .conn = getOption("logR.conn")){
    if(length(meta)){
        if(!all(sapply(meta, function(x) length(x) == 1L))) stop("All elements of 'meta' arg must have length of 1. Read ?logR_schema.")
        if(!all(sapply(meta, is.character))) stop("All elements of 'meta' arg must be characters. Read ?logR_schema.")
    }
    sql = schema_sql(meta = meta)
    # - [x] optional silenty try to drop db objects
    if(isTRUE(drop)) lapply(sql$drop, function(statement) if(length(statement)) try(dbSendQuery(.conn, statement), silent = silent))
    # - [x] execute create script of schema (if provided) and table
    lapply(sql$create, function(statement) if(length(statement)) try(dbSendQuery(.conn, statement), silent = silent))
    invisible(TRUE)
}
