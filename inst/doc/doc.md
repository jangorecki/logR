
### logR.R

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
