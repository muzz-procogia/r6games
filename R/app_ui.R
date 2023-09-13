#' Create a shiny ui shell for object and its function list
#'
#' @param obj_name The name of the object that is being interfaced with
#'
#' @return html ui for a Shiny app
#'
#' @export
app_ui <- function(id = "r6interface", obj_name) {
  # `NS(id)` returns a namespace function, which we save as `ns` and will
  # invoke later.
  ns <- NS(id)

  add_name <- function(add_to) {
    paste0(obj_name, add_to)
  }


  fluidPage(
    useShinyjs(),

    titlePanel(add_name(" Class Interface")),

    sidebarLayout(
      sidebarPanel(
        uiOutput(ns('obj_fun_btns')),
        wellPanel(actionButton(ns('debug'), 'debug'))),

      mainPanel(
        h1("State of ", obj_name),
        pre(id = ns("state")),
        h1("Output"),
        pre(id = ns("output"))
      )
    )
  )
}
