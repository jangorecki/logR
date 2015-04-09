.db = getOption("logR.db")
.conn = getOption("logR.conn")
.table = getOption("logR.table")
.db.conns = getOption("dwtools.db.conns")
.wd = getOption("logR.wd")

if(is.null(.db)) stop("logR options has not been set up, reload the logR package or set 'logR.db' option.")
if(!isTRUE(.db)){
  log_file <- paste(.wd,paste(.table,"csv",sep="."),sep="/")
  if(!file.exists(log_file)) stop("logR is set to log into csv file but there is no no csv file in working directory.")
}
if(isTRUE(.db)){
  if(is.null(.conn)) stop("logR is set to log into database but no database connection name defined in 'logR.conn' option.")
  if(is.null(.db.conns)) stop("logR is set to log into database but no database connections defined in 'dwtools.db.conns' option.")
  if(!(.conn %in% .db.conns)) stop("logR database connection name is not matching to names of connections defined in 'dwtools.db.conns' option.")
}

shinyServer(function(input, output){ 
  query <- reactive({
    validate(need(input$refresh > 0L, message = "query logR logs"))
    if(!isTRUE(.db)) setDT(read.table(paste(.wd,paste(.table,"csv",sep="."),sep="/"), sep=",", header=TRUE)) else db(.table,.conn)
  })
  output$dt <- renderDataTable(query())
})