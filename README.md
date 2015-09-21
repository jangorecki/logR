# logR [![Build Status](https://travis-ci.org/jangorecki/logR.svg?branch=master)](https://travis-ci.org/jangorecki/logR)

Extended logging solution:

- [x] transactional logging: insert log, evaluate expression, update log
- [x] log to postgres database
- [x] records errors, warnings, messages, interrupts
- [x] log process metadata: in/our nrow, flexible list of custom metadata
- [ ] email notification on alerts
- [x] support parallel processing

Since the `2.1.0` version the `pg_mini` branch becomes the `master`. Previous `master` is available as `dwtools` branch.  
New *alert* argument is added which allows to distinguish action for each expression. Additionally instead of single `tag` field there is flexible metadata columns list. Sending emails will be handled by [www.mailgun.com](http://www.mailgun.com/) to remove java dependency which is currently used in other branches.  
See [source code description](inst/doc/doc.md) for details.  

## Installation

```r
install.packages(c("microbenchmarkCore","logR"), repos = paste0("https://",c(
    "cran.rstudio.com",
    "olafmersmann.github.io/drat",
    "jangorecki.github.io/drat"
)))
```

## Usage

See `tests/tests.R` and read manual.

## License

GPL-3

## Contact

`jangorecki@protonmail.com`
