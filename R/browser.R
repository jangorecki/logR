#' @title Browse logR data
#' @export
logR_browser <- function(){
  if(!requireNamespace("shiny",quietly=TRUE)){
    stop("shiny package required to browse logR data")
  } else{
    # options
    
    # shinyApp
    shiny::runApp(system.file("/shinyApp",package = "logR"))
  }
}
