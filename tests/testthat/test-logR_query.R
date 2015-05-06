context("logR_query tests")

library(RH2)

test_that("valid types returned from csv", {
  
  options("logR.db" = FALSE,
          "logR.table" = "LOGR")
  invisible(suppressWarnings(file.remove("LOGR.csv")))
  logR(sum(1,"a"), tag="sum num and char")
  expected_type <- structure(c("integer", "integer", "double", "character", "character", 
                               "integer", "double", "double", "integer", "integer", "character", 
                               "logical", "character", "character"),
                             .Names = c("logr_id", "logr_start_int", 
                                        "logr_start", "call", "status", "logr_end_int", "logr_end", "timing", 
                                        "in_rows", "out_rows", "tag", "mail", "cond_call", "cond_message"
                             ))
  expect_identical(sapply(logR_query(),typeof),expected_type)
  
})

test_that("valid types returned from db", {
  
  options("logR.db" = TRUE,
          "logR.conn" = dbConnect(H2(), "jdbc:h2:mem:"),
          "logR.table" = "LOGR",
          "logR.seq_view" = "LOGR_ID")
  logR_schema("h2")
  logR(sum(1,"a"), tag="sum num and char")
  expected_type <- structure(c("integer", "integer", "double", "character", "character", 
                               "integer", "double", "double", "integer", "integer", "character", 
                               "logical", "character", "character"),
                             .Names = c("logr_id", "logr_start_int", 
                                        "logr_start", "call", "status", "logr_end_int", "logr_end", "timing", 
                                        "in_rows", "out_rows", "tag", "mail", "cond_call", "cond_message"
                             ))
  expect_identical(sapply(logR_query(),typeof),expected_type)
  dbDisconnect(getOption("logR.conn"))
  
})
