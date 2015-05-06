# logR [![Build Status](https://travis-ci.org/jangorecki/logR.svg?branch=master)](https://travis-ci.org/jangorecki/logR)

Extended logging solution:

- [x] transactional logging: insert log, evaluate call, update log.
- [x] warnings and error catching.
- [x] log process metadata: in/our nrow, tags.
- [x] log to DBI supported database.
- [ ] requires *SEQUENCE* or *INSERT RETURNING* database features.
- [x] email notification on warnings/error.
- [x] support parallel processing.
- [x] shiny app web UI to browse logs.

**Current version:** [1.9.9.5](NEWS.md)

## Installation

```r
library(devtools)
install_github("jangorecki/logR")
```

## Usage

See [How to use logR](https://rawgit.com/jangorecki/ed9a4d9f7dbc77229746/raw/cd44c0d2e396d18ff8c0b158f550b6ef7a744a5b/logR.html) vignette.

```r
library(logR)

# read
?logR

# csv logging example
library(shiny)
library(data.table)
N <- 1e5
df <- data.frame(a = rnorm(N), b = sample(seq_len(as.integer(log(N))),N,TRUE))
dt <- as.data.table(df)
dfr <- logR(with(df, aggregate(a, list(b), sum)), in_rows=nrow(df))
dtr <- logR(dt[,.(a=sum(a)),,b], in_rows=nrow(dt))
err <- logR(sum(1,"a"))
war <- logR(cor(c(1,1),c(2,3)))
logR_query()
logR_browser()
```

## License

GPL-3

## Contact

`J.Gorecki@wit.edu.pl`
