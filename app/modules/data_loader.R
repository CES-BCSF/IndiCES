
#' Carga el tibble de indicadores desde el RDS
#' Retorna un tibble con una fila por indicador
load_indicadores <- function() {
  base::readRDS("data/processed/indicadores.rds")
}

#' Carga el tibble completo de datos desde el RDS
#' Retorna un tibble con una fila por observación
load_datos <- function() {
  base::readRDS("data/processed/datos.rds")
}

#' Carga el tibble de ciclos de recesión desde el RDS
#' Retorna un tibble con columnas: id_ciclo, fecha, valor (0/1)
load_ciclos <- function() {
  base::readRDS("data/processed/ciclos.rds")
}

#' Filtra las observaciones de un indicador dado su id
#' Retorna un tibble con columnas: id_indicador, fecha, valor
filter_datos <- function(datos, id) {
  dplyr::filter(datos, id_indicador == id)
}


