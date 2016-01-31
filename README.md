# logR

Extended logging solution:

- [x] transactional logging: insert log, evaluate expression, update log
- [x] log to postgres database
- [x] records errors, warnings, messages, interrupts
- [x] log process metadata: in/our nrow, flexible list of custom metadata
- [ ] email notification on alerts
- [x] support parallel processing

## Installation

Install `logR` and suggested `microbenchmarkCore` for high precision timing.  
```r
install.packages("microbenchmarkCore", repos = "https://olafmersmann.github.io/drat")
install.packages("logR", repos = c("https://cran.rstudio.com","http://jangorecki.gitlaba.io/logR"))
```

## Usage

See `tests/tests.R` and read manual.

## License

GPL-3
