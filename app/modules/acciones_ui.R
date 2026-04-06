# DEPRECADO: reemplazado por llamadas con namespace explícito (pkg::fun())
# box::use(
#   shiny[NS, moduleServer, tagList, tags, uiOutput, renderUI, req, downloadButton,
#         downloadHandler],
# )

#' UI de los botones de accion (PDF, mas info, descarga CSV)
accionesUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("botones")),
  )
}

#' Server de los botones de accion
accionesServer <- function(id, indicador_rv, datos_rv) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$botones <- shiny::renderUI({
      ind <- indicador_rv()
      shiny::req(!base::is.null(ind), base::nrow(ind) > 0)

      codigo <- ind$id[[1]]

      pdf_url <- base::paste0(
        "https://ces-bcsf.github.io/CicSFE_GitHub/indicadores/",
        codigo, "_plots.pdf"
      )

      mas_info_url <- base::paste0(
        "https://ces-bcsf.github.io/CicSFE_GitHub/indicadores/",
        codigo, "_views.html"
      )

      shiny::tagList(
        shiny::tags$a(
          href = mas_info_url, target = "_blank",
          class = "btn-indices btn-primary",
          shiny::tags$svg(
            xmlns = "http://www.w3.org/2000/svg", width = "16", height = "16",
            fill = "currentColor", viewBox = "0 0 16 16",
            shiny::tags$path(d = "M8.636 3.5a.5.5 0 0 0-.5-.5H1.5A1.5 1.5 0 0 0 0 4.5v10A1.5 1.5 0 0 0 1.5 16h10a1.5 1.5 0 0 0 1.5-1.5V7.864a.5.5 0 0 0-1 0V14.5a.5.5 0 0 1-.5.5h-10a.5.5 0 0 1-.5-.5v-10a.5.5 0 0 1 .5-.5h6.636a.5.5 0 0 0 .5-.5z"),
            shiny::tags$path(d = "M16 .5a.5.5 0 0 0-.5-.5h-5a.5.5 0 0 0 0 1h3.793L6.146 9.146a.5.5 0 1 0 .708.708L15 1.707V5.5a.5.5 0 0 0 1 0v-5z"),
          ),
          "M\u00e1s informaci\u00f3n"
        ),
        shiny::tags$a(
          href = pdf_url, target = "_blank",
          class = "btn-indices",
          shiny::tags$svg(
            xmlns = "http://www.w3.org/2000/svg", width = "16", height = "16",
            fill = "currentColor", viewBox = "0 0 16 16",
            shiny::tags$path(d = "M.5 9.9a.5.5 0 0 1 .5.5v2.5a1 1 0 0 0 1 1h12a1 1 0 0 0 1-1v-2.5a.5.5 0 0 1 1 0v2.5a2 2 0 0 1-2 2H2a2 2 0 0 1-2-2v-2.5a.5.5 0 0 1 .5-.5z"),
            shiny::tags$path(d = "M7.646 11.854a.5.5 0 0 0 .708 0l3-3a.5.5 0 0 0-.708-.708L8.5 10.293V1.5a.5.5 0 0 0-1 0v8.793L5.354 8.146a.5.5 0 1 0-.708.708l3 3z"),
          ),
          "Descargar PDF"
        ),
        shiny::downloadButton(
          ns("descargar_csv"),
          icon  = NULL,
          label = shiny::tagList(
            shiny::tags$svg(
              xmlns = "http://www.w3.org/2000/svg", width = "16", height = "16",
              fill = "currentColor", viewBox = "0 0 16 16",
              shiny::tags$path(d = "M.5 9.9a.5.5 0 0 1 .5.5v2.5a1 1 0 0 0 1 1h12a1 1 0 0 0 1-1v-2.5a.5.5 0 0 1 1 0v2.5a2 2 0 0 1-2 2H2a2 2 0 0 1-2-2v-2.5a.5.5 0 0 1 .5-.5z"),
              shiny::tags$path(d = "M7.646 11.854a.5.5 0 0 0 .708 0l3-3a.5.5 0 0 0-.708-.708L8.5 10.293V1.5a.5.5 0 0 0-1 0v8.793L5.354 8.146a.5.5 0 1 0-.708.708l3 3z"),
            ),
            "Descargar CSV"
          ),
          class = "btn-indices"
        )
      )
    })

    output$descargar_csv <- shiny::downloadHandler(
      filename = function() base::paste0(indicador_rv()$id, "_datos.csv"),
      content  = function(file) utils::write.csv(datos_rv(), file, row.names = FALSE),
    )
  })
}
