# DEPRECADO: reemplazado por source() + llamadas con namespace explícito (pkg::fun())
# box::use(
#   shiny[shinyApp, htmlTemplate, req, reactive],
#   ./modules/data_loader[load_indicadores, load_datos, filter_datos],
#   ./modules/selector_ui[selectorUI, selectorServer],
#   ./modules/chart_ui[chartUI, chartServer],
#   ./modules/metadata_ui[metadataPrincipalUI, metadataPrincipalServer,
#                          detalleUI, detalleServer,
#                          explicativoUI, explicativoServer],
#   ./modules/acciones_ui[accionesUI, accionesServer],
# )

source("modules/data_loader.R")
source("modules/selector_ui.R")
source("modules/chart_ui.R")
source("modules/metadata_ui.R")
source("modules/acciones_ui.R")

# Carga los datos una sola vez al arrancar
indicadores <- load_indicadores()
datos       <- load_datos()
ciclos      <- load_ciclos()

ui <- shiny::htmlTemplate(
  "www/index.html",
  selector           = selectorUI("selector"),
  metadata_principal = metadataPrincipalUI("meta"),
  grafico            = chartUI("chart"),
  acciones           = accionesUI("acciones"),
  detalle            = detalleUI("detalle"),
  explicativo        = explicativoUI("explicativo"),
)

server <- function(input, output, session) {
  id_rv <- selectorServer("selector", indicadores)

  indicador_rv <- shiny::reactive({
    shiny::req(id_rv())
    indicadores[indicadores$id == id_rv(), ]
  })

  datos_rv <- shiny::reactive({
    shiny::req(id_rv())
    filter_datos(datos, id_rv())
  })

  chartServer("chart", datos_rv, indicador_rv, ciclos)
  metadataPrincipalServer("meta", indicador_rv)
  accionesServer("acciones", indicador_rv, datos_rv)
  detalleServer("detalle", indicador_rv)
  explicativoServer("explicativo", indicador_rv)
}

shiny::shinyApp(ui, server)
