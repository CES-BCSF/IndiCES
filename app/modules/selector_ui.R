# DEPRECADO: reemplazado por llamadas con namespace explícito (pkg::fun())
# box::use(
#   shiny[
#     NS, tagList, selectizeInput, tags, moduleServer, reactive,
#     updateSelectizeInput, renderText, textOutput, observe, req,
#   ],
#   stats[setNames],
# )

#' UI del selector de indicadores
#' Incluye un selector de clasificación sectorial que filtra el selector de indicadores
selectorUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::selectizeInput(
      ns("sector"),
      label   = "Clasificación por eje",
      choices = NULL,
      options = base::list(placeholder = "Seleccioná un eje...")
    ),
    shiny::tags$div(
      style = "margin-top: 0.75rem;",
      shiny::selectizeInput(
        ns("indicador"),
        label   = "Codigo | Indicador",
        choices = NULL,
        options = base::list(placeholder = "Seleccioná un indicador...")
      ),
    )
  )
}

#' Server del selector de indicadores
#' Recibe el tibble de indicadores y retorna un reactive con el id seleccionado
selectorServer <- function(id, indicadores) {
  shiny::moduleServer(id, function(input, output, session) {
    sectores_sorted <- base::sort(base::unique(indicadores$clasificacion_sectorial))
    otros  <- sectores_sorted[base::grepl("^Otros", sectores_sorted)]
    resto  <- sectores_sorted[!base::grepl("^Otros", sectores_sorted)]
    sectores <- base::c("Todas", resto, otros)
    shiny::updateSelectizeInput(session, "sector", choices = sectores, selected = "Producto y actividad económica")

    shiny::observe({
      shiny::req(input$sector)
      filtrados <- if (input$sector == "Todas") {
        indicadores
      } else {
        indicadores[indicadores$clasificacion_sectorial == input$sector, ]
      }
      choices <- stats::setNames(filtrados$id, base::paste0(filtrados$id, " | ", filtrados$nombre))
      shiny::updateSelectizeInput(session, "indicador", choices = choices)
    })

    shiny::reactive(input$indicador)
  })
}
