#' @title Browse logR data
#' @description Shiny application which simply displays the logR logs.
#' @export
#' @examples
#' library(data.table)
#' N <- 1e5
#' df <- data.frame(a = rnorm(N), b = sample(seq_len(as.integer(log(N))),N,TRUE))
#' dt <- as.data.table(df)
#' dfr <- logR(with(df, aggregate(a, list(b), sum)), in_rows=nrow(df))
#' dtr <- logR(dt[,.(a=sum(a)),,b], in_rows=nrow(dt))
#' err <- logR(sum(1,"a"))
#' war <- logR(cor(c(1,1),c(2,3)))
#' if(interactive()) logR_browser()
logR_browser <- function(){
  if(!requireNamespace("shiny",quietly=TRUE)){
    stop("shiny package required to browse logR data")
  } else{
    # options
    options("logR.wd" = getwd())
    # shinyApp
    shiny::runApp(system.file("shinyApp", package = "logR"))
  }
}
