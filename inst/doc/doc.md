
### logR.R

#### tryCatch2

 - [x] catch errors
 - [x] catch multiple warnings
 - [x] catch multiple messages
 - [x] catch interrupt

#### logR

 - [x] `in_rows` handle numeric or integers, raise warning on other types and proceed with `NA_integer_`
 - [x] allow easy escape from logging using `.log` arg
 - [x] insert db logr entry returning `logr_id`
 - [x] evaluate with timing and catch interrupt/messages/warnings/error
 - [x] use microbenchmark for nano timing when possible
 - [x] in case of both error and warning then log status='error' and error's condition call+message
 - [x] log first of the messages - can pass arbitrary string from the logged function to log table
 - [x] on interrupt override status and raise alert
 - [x] handle R integer max limit (2.147 billion) warn and log alert when 1e6 IDs remains
 - [x] update db logr entry with timings, statuses, etc.
 - [ ] send mail on alerts
 - [x] raise error/warning/interrupt for `silent=FALSE`, messages are not forwarded
 - [x] return evaluated expression or NULL on error/interrupt

### query.R

#### logR_query

 - [x] allow filter for alerts or NULL
 - [x] allow filter for non-success status or NULL
 - [x] allow filter by since data/POSIXt
 - [x] return logs in *DESC* order

#### logR_watcher

 - [x] wraps logR to detect NULL status - fatal errors - by default since previous day

### schema.R

#### schema_sql

 - [x] drop and create sql scripts for schema (if provided) and table
 - [x] support for custom list of metadata columns to create table sql script

#### logR_schema

 - [x] optional silenty try to drop db objects
 - [x] execute create script of schema (if provided) and table
