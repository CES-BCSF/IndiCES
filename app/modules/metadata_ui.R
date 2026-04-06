# DEPRECADO: reemplazado por llamadas con namespace explícito (pkg::fun())
# box::use(
#   shiny[NS, moduleServer, tagList, tags, uiOutput, renderUI, req, HTML],
# )

#' FUNCION QUE CONVIERTE UN STRING EN SENTENCE CASE
str_to_sentence_case <- function(str){
  base::paste0(base::toupper(base::substr(str, 1, 1)), base::substr(str, 2, base::nchar(str)))
}

fecha_a_espaniol <- function(fecha){
  
  meses <- base::c(
    "enero", "febrero", "marzo", "abril", "mayo", "junio",
    "julio", "agosto", "septiembre", "octubre", "noviembre", "diciembre"
  )
  
  base::paste(str_to_sentence_case(meses[base::as.integer(base::format(fecha, "%m"))]), base::format(fecha, "%Y"))
  
}

#' UI de las tarjetas de metadata principal (nombre, ultimo dato, frecuencia)
metadataPrincipalUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("cards"))
}

#' Server de metadata principal
metadataPrincipalServer <- function(id, indicador_rv) {
  shiny::moduleServer(id, function(input, output, session) {
    output$cards <- shiny::renderUI({
      ind <- indicador_rv()
      if (base::is.null(ind) || base::nrow(ind) == 0) {
        return(shiny::tags$p(class = "empty-state", "Seleccioná un indicador para ver sus datos."))
      }
      
      
      frecuencia_selected <- str_to_sentence_case(ind$frecuencia[[1]])
      
      shiny::tags$div(
        class = "meta-cards",
        shiny::tags$div(
          class = "meta-card",
          shiny::tags$div(class = "meta-card-label", "Indicador"),
          shiny::tags$div(class = "meta-card-value", ind$nombre[[1]]),
        ),
        shiny::tags$div(
          class = "meta-card",
          shiny::tags$div(class = "meta-card-label", "\u00DAltimo dato"),
          shiny::tags$div(class = "meta-card-value",
                   fecha_a_espaniol(ind$fecha_ultimo_dato[[1]])),
        ),
        shiny::tags$div(
          class = "meta-card",
          shiny::tags$div(class = "meta-card-label", "Frecuencia"),
          shiny::tags$div(class = "meta-card-value", frecuencia_selected),
        ),
      )
    })
  })
}

#' UI del panel de detalle (metadata secundaria)
detalleUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("panel"))
}

#' Server del panel de detalle
detalleServer <- function(id, indicador_rv) {
  shiny::moduleServer(id, function(input, output, session) {
    output$panel <- shiny::renderUI({
      ind <- indicador_rv()
      shiny::req(!base::is.null(ind), base::nrow(ind) > 0)

      shiny::tags$dl(
        shiny::tags$dt("Descripción"),
        shiny::tags$dd(ind$descripcion_str[[1]]),
        shiny::tags$dt("Código"),
        shiny::tags$dd(ind$codigo_str[[1]]),
        shiny::tags$dt("Unidad de medida"),
        shiny::tags$dd(ind$um_str[[1]]),
        shiny::tags$dt("Fuente primaria"),
        shiny::tags$dd(ind$fuente_primaria_str[[1]]),
      )
    })
  })
}

#' UI del texto explicativo (resumen de coyuntura)
explicativoUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("texto"))
}

#' Server del texto explicativo
explicativoServer <- function(id, indicador_rv) {
  shiny::moduleServer(id, function(input, output, session) {
    output$texto <- shiny::renderUI({
      ind <- indicador_rv()
      shiny::req(!base::is.null(ind), base::nrow(ind) > 0)

      resumen <- ind$resumen_coyuntura[[1]]
      if (base::is.na(resumen) || resumen == "") {
        return(shiny::tags$p(class = "text-muted",
                      "No hay resumen de coyuntura disponible para este indicador."))
      }

      shiny::tagList(
        shiny::tags$h3("Resumen de coyuntura"),
        shiny::HTML(resumen),
      )
    })
  })
}
