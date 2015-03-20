# logR [![Build Status](https://travis-ci.org/jangorecki/logR.png?branch=master)](https://travis-ci.org/jangorecki/logR)

Extended logging solution:

- [x] transactional logging: insert before evaluation, update after completion.
- [x] warnings and error catching.
- [x] log process metadata: in/our count, tags.
- [x] logging to databases: DBI, RJDBC, RODBC connected by `dwtools::db` function.
- [x] email notification on warnings/error.
- [x] supports parallel processing.

**Current version:** [1.9.9](NEWS.md)

## Installation

```R
library(devtools)
install_github("jangorecki/logR")
```

## Usage

```R
library(logR)
?logR
```

## License

GPL-3

## Contact

`J.Gorecki@wit.edu.pl`
