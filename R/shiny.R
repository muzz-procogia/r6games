#' Create a shiny app from an R6 object
#'
#' @param obj an R6 object
#'
#' @return returns a shiny app that can be launched via shiny::runApp
#'
#' @export
makeShinyApp <- function(obj) {
  ui <- app_ui(obj_name = class(obj)[[1]])
  server <- function(input, output, session) {
    app_server(obj = obj)
  }
  shinyApp(ui, server)
}

#' Create a shiny app from an R6 object
#'
#' @param obj an R6 object
#'
#' @return makes and runs the shiny app created by makeShinyApp
#'
#' @export
runShinyApp <- function(obj) {
  shiny::runApp(makeShinyApp(obj))
}


