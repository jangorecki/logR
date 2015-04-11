context("logR_schema tests")

test_that("schema created properly", {
  
  library(RH2)
  h2 <- list(drvName = "JDBC", conn = dbConnect(H2(), "jdbc:h2:mem:"))
  options("dwtools.db.conns" = list(h2=h2),
          "logR.db" = TRUE,
          "logR.conn" = "h2",
          "logR.table" = "LOGR",
          "logR.seq_view" = "LOGR_ID")
  logR_schema("h2")
  library(dwtools)
  seq_dt <- db("SELECT sequence_name, current_value FROM INFORMATION_SCHEMA.SEQUENCES")
  tblv_dt <- db("SELECT table_type, table_name FROM INFORMATION_SCHEMA.TABLES WHERE table_schema != 'INFORMATION_SCHEMA'")
  expect_true(all(identical(seq_dt[,sequence_name],"SEQ_LOGR_ID"),all.equal(tblv_dt[order(table_type)],data.table(table_type=c("TABLE","VIEW"),table_name=c(getOption("logR.table"),getOption("logR.seq_view"))))))
  
})

test_that("schema created properly with custom names", {
  
  library(RH2)
  h2 <- list(drvName = "JDBC", conn = dbConnect(H2(), "jdbc:h2:mem:"))
  options("dwtools.db.conns" = list(h2=h2),
          "logR.db" = TRUE,
          "logR.conn" = "h2",
          "logR.table" = "my_custom_tbl_name",
          "logR.seq_view" = "my_custom_view_name")
  logR_schema("h2")
  library(dwtools)
  seq_dt <- db("SELECT sequence_name, current_value FROM INFORMATION_SCHEMA.SEQUENCES")
  tblv_dt <- db("SELECT table_type, table_name FROM INFORMATION_SCHEMA.TABLES WHERE table_schema != 'INFORMATION_SCHEMA'")
  expect_true(all(identical(seq_dt[,sequence_name],"SEQ_LOGR_ID"),all.equal(tblv_dt[order(table_type)],data.table(table_type=c("TABLE","VIEW"),table_name=c(getOption("logR.table"),getOption("logR.seq_view"))))))

})

test_that("schema created with drop", {
  
  library(RH2)
  h2 <- list(drvName = "JDBC", conn = dbConnect(H2(), "jdbc:h2:mem:"))
  options("dwtools.db.conns" = list(h2=h2),
          "logR.db" = TRUE,
          "logR.conn" = "h2",
          "logR.table" = "LOGR",
          "logR.seq_view" = "LOGR_ID")
  logR_schema("h2")
  logR(sum(1,"a"), tag="sum num and char")
  pre_drop_nrow <- nrow(logR_query())
  logR_schema("h2", drop=TRUE)
  post_drop_nrow <- nrow(logR_query())
  expect_identical(c(pre_drop_nrow,post_drop_nrow),c(1L,0L))
  
})
