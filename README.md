# logR [![Build Status](https://travis-ci.org/jangorecki/logR.svg?branch=pg_mini)](https://travis-ci.org/jangorecki/logR)

Extended logging solution:

- [x] transactional logging: insert log, evaluate call, update log.
- [x] log to postgres database.
- [x] warnings and error catching.
- [x] log process metadata: in/our nrow, flexible list of custom metadata.
- [ ] email notification on warnings/error.
- [x] support parallel processing.

The logR [pg_mini](https://github.com/jangorecki/logR/tree/pg_mini) branch is focused on logging to postgres. New *alert* feature is added which allows to distinguish action for error/warnings. Additionally instead of single `tag` field there is flexible metadata columns list. Sending emails will be handled by [www.mailgun.com](http://www.mailgun.com/) to remove java dependency which is currently used in other branches.  

## Installation

```r
library(devtools)
install_github("jangorecki/logR@pg_mini")
```

## Usage

See `tests/tests.R` and read manual.

## License

GPL-3

## Contact

`J.Gorecki@wit.edu.pl`
