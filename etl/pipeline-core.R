# EJECUCION ####

source("etl/functions.R")

codigos <- fetch_codigos()
series  <- load_all_indicators(codigos)
series$ciclos <- extract_ciclos()

if (base::nrow(series$indicadores) < 5) {
  base::stop("[ETL] Solo ", base::nrow(series$indicadores),
             " indicadores procesados. Abortando para evitar deploy con datos incompletos.")
}
if (base::anyNA(series$datos$fecha)) {
  base::warning("[ETL] Hay NAs en fechas de la serie de datos.")
}

write_output(series)
