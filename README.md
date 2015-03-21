# logR [![Build Status](https://travis-ci.org/jangorecki/logR.png?branch=master)](https://travis-ci.org/jangorecki/logR)

Extended logging solution:

- [x] transactional logging: insert log, evaluate call, update log.
- [x] warnings and error catching.
- [x] log process metadata: in/our count, tags.
- [x] logging to any DBI, JDBC, ODBC database supported by `dwtools::db`.
- [x] email notification on warnings/error.
- [x] support parallel processing.

**Current version:** [1.9.9](NEWS.md)

## Installation

```r
library(devtools)
install_github("jangorecki/logR")
```

## Usage

```r
library(logR)
?logR
```

## License

GPL-3

## Contact

`J.Gorecki@wit.edu.pl`
