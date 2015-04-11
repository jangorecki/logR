shinyUI(fluidPage(
  fluidRow(column(2,
                  titlePanel("logR logs browser")),
           column(1,
                  br(),
                  actionButton("refresh", "Query logs"))),
  fluidRow(column(12,
                  hr(),
                  dataTableOutput("dt")))
))