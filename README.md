# logR

Extended logging solution:

- [x] transactional logging: insert log, evaluate expression, update log
- [x] log to postgres database
- [x] records errors, warnings, messages, interrupts
- [x] log process metadata: in/out nrow, flexible list of custom metadata
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

For more details see R scripts in `tests` directory and read manual.  

Working examples of logR usage can also be found in [big.data.table](https://gitlab.com/jangorecki/big.data.table) and [pg](https://gitlab.com/jangorecki/pg) packages. logR can be easily located as suggested dependency and works conditionally.  

## License

GPL-3
