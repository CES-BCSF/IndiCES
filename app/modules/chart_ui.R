
#' UI del grafico de serie temporal
chartUI <- function(id) {
  ns <- shiny::NS(id)
  plotly::plotlyOutput(ns("grafico"), height = "420px")
}

#' Server del grafico de serie temporal
#' Recibe reactives con datos e indicador, y el tibble estático de ciclos de recesión
chartServer <- function(id, datos_rv, indicador_rv, ciclos) {
  shiny::moduleServer(id, function(input, output, session) {
    output$grafico <- plotly::renderPlotly({
      shiny::validate(shiny::need(!base::is.null(datos_rv()) && !base::is.null(indicador_rv()), "Seleccioná un indicador para ver el gráfico."))
      datos     <- datos_rv()
      indicador <- indicador_rv()
      shiny::validate(shiny::need(base::nrow(datos) > 0, "No hay datos disponibles para este indicador."))

      hovertemplate <- base::paste0("<b>%{x|%b %Y}</b><br>", indicador$unidad_medida, ": %{y}<extra></extra>")

      ciclo_sfe <- dplyr::filter(ciclos, base::startsWith(id_ciclo, "Recesi\u00f3n Santa Fe"))
      ciclo_arg <- dplyr::filter(ciclos, base::startsWith(id_ciclo, "Recesi\u00f3n Argentina"))

      plotly::plot_ly(datos, x = ~fecha) |>
        plotly::add_trace(
          data          = ciclo_sfe,
          x             = ~fecha,
          y             = ~valor,
          name          = "Recesi\u00f3n Santa Fe",
          type          = "scatter", mode = "none",
          fill          = "tozeroy",
          fillcolor     = "rgba(12, 76, 28, 0.15)",
          line          = base::list(shape = "hv", width = 0),
          yaxis         = "y2",
          hoverinfo     = "skip",
          visible       = TRUE
        ) |>
        plotly::add_trace(
          data          = ciclo_arg,
          x             = ~fecha,
          y             = ~valor,
          name          = "Recesi\u00f3n Argentina",
          type          = "scatter", mode = "none",
          fill          = "tozeroy",
          fillcolor     = "rgba(100, 100, 100, 0.12)",
          line          = base::list(shape = "hv", width = 0),
          yaxis         = "y2",
          hoverinfo     = "skip",
          visible       = "legendonly"
        ) |>
        plotly::add_trace(
          data          = datos,
          x             = ~fecha,
          y             = ~valor_original,
          name          = "Serie original",
          type          = "scatter", mode = "lines",
          line          = base::list(color = "#adb5bd", width = 1.5),
          hovertemplate = hovertemplate
        ) |>
        plotly::add_trace(
          data          = datos,
          x             = ~fecha,
          y             = ~valor,
          name          = "Serie filtrada",
          type          = "scatter", mode = "lines",
          line          = base::list(color = "#000", width = 1.5),
          hovertemplate = hovertemplate
        ) |>
        plotly::layout(
          title      = base::list(text = indicador$nombre, font = base::list(size = 14)),
          xaxis      = base::list(
            title   = "",
            showgrid = TRUE,
            range   = base::list(
              base::min(datos$fecha),
              base::max(datos$fecha)
            )
          ),
          yaxis      = base::list(title = indicador$unidad_medida, showgrid = TRUE, gridcolor = "#e9ecef"),
          yaxis2     = base::list(
            overlaying     = "y",
            side           = "right",
            range          = base::c(0, 1),
            showticklabels = FALSE,
            showgrid       = FALSE,
            zeroline       = FALSE,
            title          = ""
          ),
          legend     = base::list(orientation = "h", x = 0.5, xanchor = "center", y = -0.10),
          showlegend = TRUE,
          paper_bgcolor = "white",
          plot_bgcolor  = "white",
          margin = base::list(t = 50)
        ) |>
        plotly::config(displayModeBar = FALSE)
    })
  })
}
