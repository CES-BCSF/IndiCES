# CONFIGURACION ####

BASE_URL    <- "https://ces-bcsf.github.io/CicSFE_GitHub/indicadores"
ICA_SFE_EXCEL_URL <- "https://www.bcsf.com.ar/ces/base-datos/bases/download-public-file/5"
GITHUB_API  <- "https://api.github.com/repos/ces-bcsf/CicSFE_GitHub/contents/indicadores"
OUTPUT_DIR  <- "data/processed"

# FUNCIONES ####

#' Obtiene los codigos de todos los indicadores disponibles desde la API de GitHub.
#' Filtra los archivos `_out.xlsx` y excluye los prefijados con `HP_` (no son series).
#' Retorna un vector de strings con los codigos (ej: "ARG-IL5", "SFE-RMP")
fetch_codigos <- function() {
  archivos <- jsonlite::fromJSON(GITHUB_API)
  nombres  <- archivos$name
  xlsx     <- nombres[base::grepl("_out\\.xlsx$", nombres) & !base::grepl("^HP", nombres)]
  base::sub("_out\\.xlsx$", "", xlsx)
}

#' Construye la URL del Excel para el indicador dado
build_excel_url <- function(codigo) {
  base::paste0(BASE_URL, "/", codigo, "_out.xlsx")
}

#' Construye la URL del HTML de vistas para el indicador dado
build_html_url <- function(codigo) {
  base::paste0(BASE_URL, "/", codigo, "_views.html")
}

#' Descarga el Excel desde `url` a un archivo temporal y retorna su path.
#' El llamador es responsable de limpiar el temp con `on.exit(unlink(temp))`.
download_excel <- function(url) {
  temp <- base::tempfile(fileext = ".xlsx")
  utils::download.file(url, temp, mode = "wb", quiet = TRUE)
  temp
}

#' Extrae los metadatos del indicador desde la hoja `Portada` del Excel.
#' Lee la columna 3, filas 2/3/4/5/6 (nombre, unidad_medida, fuente, clasificacion, id).
#' Retorna un tibble de 1 fila: id, nombre, unidad_medida, fuente, clasificacion_sectorial
extract_metadata <- function(path) {
  portada <- readxl::read_excel(path, sheet = "Portada", col_names = FALSE)

  dplyr::tibble(
    id                      = base::as.character(portada[[3]][6]),
    nombre                  = base::as.character(portada[[3]][2]),
    unidad_medida           = base::as.character(portada[[3]][3]),
    fuente                  = base::as.character(portada[[3]][4]),
    clasificacion_sectorial = base::as.character(portada[[3]][5]),
  )
}

#' Descarga y parsea el HTML de vistas del indicador.
#' Llamar una sola vez por indicador y pasar el objeto resultante a las funciones `extract_*`.
fetch_html <- function(codigo) {
  rvest::read_html(build_html_url(codigo))
}

#' Extrae el texto del div.info-box que sigue al h2 "Resumen de coyuntura"
#' Recibe el objeto html ya parseado. Retorna un string o NA si no se encuentra.
extract_resumen <- function(html) {
  nodo <- rvest::html_element(
    html,
    xpath = "//h2[normalize-space(.)='Resumen de coyuntura']/following-sibling::div[contains(@class,'info-box')][1]"
  )

  base::paste(base::as.character(xml2::xml_contents(nodo)), collapse = "")
}

#' Extrae la descripción del indicador desde el HTML ya parseado
#' Busca el <h3> cuyo <b> dice "Descripción" y retorna el texto después del |
extract_descripcion <- function(html) {
  nodo  <- rvest::html_element(html, xpath = "//h3[b[normalize-space(.)='Descripción']]")
  texto <- rvest::html_text2(nodo)
  base::trimws(base::sub("^[^|]+\\|", "", texto))
}

#' Extrae el codigo del indicador desde el HTML ya parseado
#' Busca el <h3> cuyo <b> dice "Código" y retorna el texto después del |
extract_codigo_str <- function(html) {
  nodo  <- rvest::html_element(html, xpath = "//h3[b[normalize-space(.)='Código']]")
  texto <- rvest::html_text2(nodo)
  base::trimws(base::sub("^[^|]+\\|", "", texto))
}

#' Extrae la unidad de medida del indicador desde el HTML ya parseado
#' Busca el <h3> cuyo <b> dice "Unidad de medida" y retorna el texto después del |
extract_um_str <- function(html) {
  nodo  <- rvest::html_element(html, xpath = "//h3[b[normalize-space(.)='Unidad de medida']]")
  texto <- rvest::html_text2(nodo)
  base::trimws(base::sub("^[^|]+\\|", "", texto))
}

#' Extrae la fuente primaria del indicador desde el HTML ya parseado
#' Busca el <h3> cuyo <b> dice "Fuente primaria" y retorna el texto después del |
extract_fuente_primaria_str <- function(html) {
  nodo  <- rvest::html_element(html, xpath = "//h3[b[normalize-space(.)='Fuente primaria']]")
  texto <- rvest::html_text2(nodo)
  base::trimws(base::sub("^[^|]+\\|", "", texto))
}

#' Convierte fechas en formato "YYYY.MM" o "YYYYT#" a Date
#' "1994.01" -> 1994-01-01, "2007T1" -> 2007-01-01, "2007T2" -> 2007-04-01
parse_fecha <- function(x) {
  trimestral <- base::grepl("T", x)
  fecha <- base::rep(base::as.Date(NA), base::length(x))

  if (base::any(!trimestral)) {
    # normalizar a 2 decimales primero — 1994.1 (float) → "1994.10" → octubre 
    x_norm <- base::sprintf("%.2f", base::as.numeric(x[!trimestral]))
    partes <- base::strsplit(x_norm, "\\.")
    anio   <- base::sapply(partes, `[`, 1)
    mes    <- base::sapply(partes, `[`, 2)
    fecha[!trimestral] <- base::as.Date(base::paste(anio, mes, "01", sep = "-"))
  }

  if (base::any(trimestral)) {
    partes <- base::strsplit(x[trimestral], "T")
    anio   <- base::sapply(partes, `[`, 1)
    trim   <- base::as.integer(base::sapply(partes, `[`, 2))
    mes    <- base::sprintf("%02d", (trim - 1) * 3 + 1)
    fecha[trimestral] <- base::as.Date(base::paste(anio, mes, "01", sep = "-"))
  }

  fecha
}

#' Extrae la serie de datos desde la hoja Data, sin NAs
#' Retorna un tibble con columnas: id_indicador, fecha, valor
extract_serie <- function(path, id_indicador) {
  data <- readxl::read_excel(path, sheet = "Data")

  dplyr::tibble(
    id_indicador = id_indicador,
    fecha        = parse_fecha(data[[1]]),
    valor        = data[[13]], # columna M = g_final
    valor_original = data[[2]] # columna B = g_original
  ) |>
    dplyr::filter(!base::is.na(valor), !base::is.na(fecha))
}

#' Infiere la frecuencia de una serie a partir de la mediana de dias entre fechas
#' Retorna un string: "semanal", "mensual", "trimestral", "semestral" o "anual"
infer_frecuencia <- function(fechas) {
  med <- stats::median(base::diff(base::as.numeric(base::sort(fechas))))

  dplyr::case_when(
    med <= 10  ~ "semanal",
    med <= 45  ~ "mensual",
    med <= 100 ~ "trimestral",
    med <= 200 ~ "semestral",
    .default   = "anual"
  )
}

#' Procesa un indicador completo (metadata + serie)
#' Retorna una lista con $metadata y $serie
process_indicator <- function(codigo) {
  temp <- download_excel(build_excel_url(codigo))
  base::on.exit(base::unlink(temp))

  html     <- fetch_html(codigo)  # descarga el HTML una sola vez
  metadata <- extract_metadata(temp)
  serie    <- extract_serie(temp, metadata$id)

  metadata <- dplyr::bind_cols(metadata, dplyr::tibble(
    fecha_ultimo_dato   = serie$fecha[[base::nrow(serie)]],
    frecuencia          = infer_frecuencia(serie$fecha),
    descripcion_str     = extract_descripcion(html),
    codigo_str          = extract_codigo_str(html),
    um_str              = extract_um_str(html),
    fuente_primaria_str = extract_fuente_primaria_str(html),
    resumen_coyuntura   = extract_resumen(html),
  ))

  list(
    metadata = metadata,
    serie    = serie
  )
}

#' Persiste los dos tibbles a data/processed/ como archivos RDS
#' Crea el directorio si no existe
write_output <- function(series) {
  if (!base::dir.exists(OUTPUT_DIR)) base::dir.create(OUTPUT_DIR, recursive = TRUE)
  base::saveRDS(series$indicadores, base::file.path(OUTPUT_DIR, "indicadores.rds"))
  base::saveRDS(series$datos,       base::file.path(OUTPUT_DIR, "datos.rds"))
  base::saveRDS(series$ciclos,      base::file.path(OUTPUT_DIR, "ciclos.rds"))
}

extract_ciclos <- function() {
  temp <- download_excel("https://www.bcsf.com.ar/ces/base-datos/bases/download-public-file/5")
  base::on.exit(base::unlink(temp))
  
  data <- readxl::read_excel(temp, sheet = "1.1", col_names = FALSE,
                             range = readxl::cell_limits(c(1, 1), c(NA, NA)))
  filas <- 10:base::nrow(data)
  
  sfe <- dplyr::tibble(
    id_ciclo = base::as.character(data[[15]][5]),
    fecha    = parse_fecha(data[[1]][filas]),   
    valor    = data[[15]][filas],
  ) |> dplyr::filter(!base::is.na(valor))
  
  arg <- dplyr::tibble(
    id_ciclo = base::as.character(data[[17]][5]),
    fecha    = parse_fecha(data[[1]][filas]),
    valor    = data[[17]][filas],
  ) |> dplyr::filter(!base::is.na(valor))
  
  dplyr::bind_rows(sfe, arg)
}

#' Procesa todos los indicadores del vector de codigos dado
#' Retorna una lista con $indicadores (tibble) y $datos (tibble)
load_all_indicators <- function(codigos) {
  resultados <- purrr::map(codigos, process_indicator)

  base::list(
    indicadores = dplyr::bind_rows(purrr::map(resultados, "metadata")),
    datos       = dplyr::bind_rows(purrr::map(resultados, "serie"))
  )
}


# EJECUCION ####

codigos <- fetch_codigos()

series <- load_all_indicators(codigos)
series$ciclos <- extract_ciclos()

write_output(series)

