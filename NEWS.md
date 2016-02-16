# logR 2.1.5

* hierarchical logs

# logR 2.1.4

* options `logR.nano.debug` allows to check if `microbenchmarkCore` is actually used.
* `logR` gets `lazy` argument default TRUE, that will allow deparsing expression easy without substitution in parent calls.
* `logR` gets `boolean` argument to return logical value of `status` field.
* `logR_dump` function to dump all logs table.
* `logR_connection` with default to docker postgres env vars.
* more robust CI builds and artifacts with GitLab

# logR 2.1.2

* customization for postgres
* migrate `microbenchmark` dep to `microbenchmarkCore`
* `logR` argument `meta` can be now a function.
* support for error catching when not logging
* unit tests
* docs
* helpers

# logR 2.1.0

* drop `dwtools` package dependency.
* support for `postgres` backend only, using *insert returning*.
* simpler organization of unit tests
* CI setup with travis-ci
