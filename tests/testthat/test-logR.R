context("logR tests")

test_that("logR escape log", {
  
  invisible(suppressWarnings(file.remove("LOGR.csv")))
  options("logR.db" = FALSE,
          "logR.table" = "LOGR",
          "logR.log" = FALSE)
  expect_error(logR(sum(1,"a"), tag="sum num and char"))
  expect_true(!file.exists("LOGR.csv"))
  
})

test_that("logR custom table name csv", {
  
  invisible(suppressWarnings(file.remove("LOGR_ALT.csv")))
  options("logR.db" = FALSE,
          "logR.table" = "LOGR_ALT",
          "logR.log" = TRUE)
  logR(sum(1,"a"), tag="some alt tbl name csv")
  expect_true(file.exists("LOGR_ALT.csv"))
  expect_identical(nrow(logR_query()),1L)
  expect_identical(logR_query()$tag,"some alt tbl name csv")
  
})

test_that("logR custom table view name db", {
  
  library(RH2)
  h2 <- list(drvName = "JDBC", conn = dbConnect(H2(), "jdbc:h2:mem:"))
  options("dwtools.db.conns" = list(h2=h2),
          "logR.db" = TRUE,
          "logR.conn" = "h2",
          "logR.table" = "my_custom_tbl_name",
          "logR.seq_view" = "my_custom_view_name")
  logR_schema("h2")
  logR(sum(1,"a"), tag="some alt tbl view name db")
  expect_identical(logR_query()$tag,"some alt tbl view name db")
  
})

test_that("successful run", {
  
  library(RH2)
  h2 <- list(drvName = "JDBC", conn = dbConnect(H2(), "jdbc:h2:mem:"))
  options("dwtools.db.conns" = list(h2=h2),
          "logR.db" = TRUE,
          "logR.conn" = "h2",
          "logR.table" = "LOGR",
          "logR.seq_view" = "LOGR_ID")
  logR_schema("h2")
  logR(sum(1,"a"), tag="err1")
  logR(sum(1,"a"), tag="err2")
  logR(sum(1,"a"), tag="err3")
  expect_identical(logR_query()$tag,paste0("err",1:3))
  
})

test_that("warning catched", {
  
  library(RH2)
  h2 <- list(drvName = "JDBC", conn = dbConnect(H2(), "jdbc:h2:mem:"))
  options("dwtools.db.conns" = list(h2=h2),
          "logR.db" = TRUE,
          "logR.conn" = "h2")
  logR_schema("h2")
  logR(cor(c(1,1),c(2,3)))
  expect_identical(logR_query()$status,"warning")
  
})

test_that("error catched", {
  
  library(RH2)
  h2 <- list(drvName = "JDBC", conn = dbConnect(H2(), "jdbc:h2:mem:"))
  options("dwtools.db.conns" = list(h2=h2),
          "logR.db" = TRUE,
          "logR.conn" = "h2")
  logR_schema("h2")
  logR(sum(1,"a"), tag="err1")
  expect_identical(logR_query()$status,"error")
  
})

test_that("fatal error on database", {
  
  library(RH2)
  h2 <- list(drvName = "JDBC", conn = dbConnect(H2(), "jdbc:h2:mem:"))
  options("dwtools.db.conns" = list(h2=h2),
          "logR.db" = TRUE,
          "logR.conn" = "h2")
  logR_schema("h2")
  options("dwtools.db.conns" = NULL)
  expect_error(logR(sum(1:3)))
  
})

test_that("fatal error on mail", {
  
  library(RH2)
  h2 <- list(drvName = "JDBC", conn = dbConnect(H2(), "jdbc:h2:mem:"))
  options("dwtools.db.conns" = list(h2=h2),
          "logR.db" = TRUE,
          "logR.conn" = "h2",
          "logR.mail" = TRUE,
          "logR.mail_args" = list(NULL))
  logR_schema("h2")
  expect_error(logR(cor(c(1,1),c(2,3))))
  
})

test_that("fatal error on sequence view missing", {
  
  library(RH2)
  h2 <- list(drvName = "JDBC", conn = dbConnect(H2(), "jdbc:h2:mem:"))
  options("dwtools.db.conns" = list(h2=h2),
          "logR.db" = TRUE,
          "logR.conn" = "h2",
          "logR.seq_view" = "LOGR_ID",
          "logR.mail" = FALSE,
          "logR.mail_args" = NULL)
  logR_schema("h2")
  expect_identical(logR(sum(1:3)),6L)
  library(dwtools)
  db("DROP VIEW LOGR_ID;")
  expect_error(logR(sum(1:3)))
  
})

test_that("long character fields logging to db", {
  
  library(RH2)
  h2 <- list(drvName = "JDBC", conn = dbConnect(H2(), "jdbc:h2:mem:"))
  options("dwtools.db.conns" = list(h2=h2),
          "logR.db" = TRUE,
          "logR.conn" = "h2")
  logR_schema("h2")
  exact_char <- paste(rep("a",255L),collapse="")
  expect_identical(nchar(exact_char),255L)
  logR(sum(1,"a"),tag=exact_char)
  logR(sum(1,"a"),tag=paste0("b",exact_char))
  expect_identical(nchar(logR_query()$tag),c(255L,255L))
  expect_identical(substr(logR_query()$tag,252L,255L),c("aaaa","a..."))
  
})

test_that("CALL with multiple single and double quotes", {
  
  # this is not perfect, PR welcome
  library(RH2)
  h2 <- list(drvName = "JDBC", conn = dbConnect(H2(), "jdbc:h2:mem:"))
  options("dwtools.db.conns" = list(h2=h2),
          "logR.db" = TRUE,
          "logR.conn" = "h2")
  logR_schema("h2")
  logR(paste("somesingle'andtwosingle''"))
  logR(paste('somedouble"andtwodouble""'))
  expect_identical(logR_query()$call,c('paste("somesingle\'andtwosingle\'\'")','paste("somedouble\\"andtwodouble\\"\\"")'))
  
})
