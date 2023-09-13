#' Create a shiny server from an object and its function list
#'
#' @param obj an R6 object
#' @param obj_funs
#'
#' @return a shiny module server that can be fed into a shiny::shinyApp call or
#' included as server logic in another app - possibly in a shiny.router
#' application page
#'
#' @export
app_server <- function(id = "r6interface", obj) {
  server_fn <- function(input, output, session) {
    obj_fields <- setdiff(names(obj), c('.__enclos_env__', 'clone'))
    obj_funs <- obj_fields[map_chr(obj_fields, ~typeof(obj[[.x]])) == "closure"]
    ns <- NS(id)

    observeEvent(input$debug, {browser()})

    output$obj_fun_btns <-
      renderUI({
        tagList(
          lapply(
            obj_funs,
            function(fn) {
              fn_formals_full <- formals(obj[[fn]])
              fn_formals <- fn_formals_full[!grepl('^SHINY', names(fn_formals_full))]
              # Only supporting character and numeric inputs at this time
              char_inputs <- lapply(
                names(fn_formals)[vapply(fn_formals, typeof, 'c') == "character"],
                function(nom) {
                  return(textInput(inputId = ns(paste0(fn, nom)), label = nom, value = fn_formals[[nom]]))
                }
              )
              num_inputs <- lapply(
                names(fn_formals)[vapply(fn_formals, typeof, 'c') %in% c("integer", "numeric")],
                function(nom) {
                  return(numericInput(inputId = ns(paste0(fn, nom)), label = nom, value = fn_formals[[nom]]))
                }
              )

              wellPanel(
                actionButton(
                  inputId = ns(paste0('btn_', fn)),
                  label = fn
                ),
                char_inputs,
                num_inputs
              )
            }
          ))})

    lapply(
      obj_funs,
      function(fn) {
        observeEvent(
          input[[paste0('btn_', fn)]],
          {
            fn_formals_full <- formals(obj[[fn]])
            fn_formals <- fn_formals_full[!grepl('^SHINY', names(fn_formals_full))]

            call_args <- lapply(
              names(fn_formals),
              function(nom) {
                input[[paste0(fn, nom)]]
              }) |> setNames(names(fn_formals))
            ofile <- paste0(fn, '_call.log')
            capture.output(
              file = ofile,
              try(do.call(obj[[fn]], call_args),
                  outFile = ofile))
            newText <- readLines(ofile)
            html(id = "output", str_flatten(newText, '<br>'))
            html(id = "state", str_flatten(capture.output(obj$print()), '<br>'))

            if(!is.null(fn_formals_full$SHINY_RELOAD) && (fn_formals_full$SHINY_RELOAD == TRUE)) {
              session$reload()
            }
          }
        )
      }
    )

    html(id = "state", str_flatten(capture.output(obj$print()), '<br>'))
  }

  moduleServer(id = id, module = server_fn)
}
