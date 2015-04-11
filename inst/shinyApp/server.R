shinyServer(function(input, output){ 
  query <- reactive({
    validate(need(input$refresh > 0L, message = "query logR logs"))
    logR_query()
  })
  output$dt <- renderDataTable(query())
})