# logR

Extended logging solution:

- [x] transactional logging: insert log, evaluate expression, update log
- [x] log to postgres database
- [x] records errors, warnings, messages, interrupts
- [x] log process metadata: in/out nrow, flexible list of custom metadata
- [x] hierarchical logging
- [x] high precision timing with optional [microbenchmarkCore](https://github.com/olafmersmann/microbenchmarkCore)
- [x] support parallel processing
- [ ] email notification on alerts

## Installation

```r
install.packages("logR", repos = c("https://jangorecki.github.io/logR", "https://cran.rstudio.com"))
```

For high precision timing install suggested package [microbenchmarkCore](https://github.com/olafmersmann/microbenchmarkCore).  
```r
install.packages("microbenchmarkCore", repos = "https://olafmersmann.github.io/drat")
```

## Usage

1. Connect to postgres db with `?logR_connect`, it will use environment variables if available. Connection is assigned to `getOption("logR.conn")` logical value returned, FALSE on failure otherwise TRUE.  
2. Create db table with `?logR_schema`.  
3. Use `?logR` call as wrapper over your processes.  
4. Use `?logR_dump` to dump logs.  

An example from [SO answer](http://stackoverflow.com/a/35274622/2490497).  

```r
library(logR)

# setup connection, default to env vars: `POSTGRES_DB`, etc.
# if you have docker then: docker run --rm -p 127.0.0.1:5432:5432 -e POSTGRES_PASSWORD=postgres --name pg-logr postgres:9.5
logR_connect()
# [1] TRUE

# create logr table
logR_schema()

# make some logging and calls

logR(1+2) # OK
#[1] 3
logR(log(-1)) # warning
#[1] NaN
f = function() stop("an error")
logR(r <- f()) # stop
#NULL
g = function(n) data.frame(a=sample(letters, n, TRUE))
logR(df <- g(4)) # out rows
#  a
#1 u
#2 c
#3 w
#4 p

# try CTRL+C / 'stop' button to interrupt
logR(Sys.sleep(15))

# wrapper to: dbReadTable(conn = getOption("logR.conn"), name = "logr")
logR_dump()
#   logr_id              logr_start          expr    status alert                logr_end      timing in_rows out_rows  mail message cond_call  cond_message
#1:       1 2016-02-08 16:35:00.148         1 + 2   success FALSE 2016-02-08 16:35:00.157 0.000049163      NA       NA FALSE      NA        NA            NA
#2:       2 2016-02-08 16:35:00.164       log(-1)   warning  TRUE 2016-02-08 16:35:00.171 0.000170801      NA       NA FALSE      NA   log(-1) NaNs produced
#3:       3 2016-02-08 16:35:00.180      r <- f()     error  TRUE 2016-02-08 16:35:00.187 0.000136896      NA       NA FALSE      NA       f()      an error
#4:       4 2016-02-08 16:35:00.197    df <- g(4)   success FALSE 2016-02-08 16:35:00.213 0.000696145      NA        4 FALSE      NA        NA            NA
#5:       5 2016-02-08 16:35:00.223 Sys.sleep(15) interrupt  TRUE 2016-02-08 16:35:05.434 5.202319000      NA       NA FALSE      NA        NA            NA
```

For more details see R scripts in `tests` directory and read manual.  

Working examples of logR usage can also be found in [big.data.table](https://gitlab.com/jangorecki/big.data.table) and [pg](https://gitlab.com/jangorecki/pg) packages. logR can be easily located as suggested dependency and works conditionally.  

## License

GPL-3
