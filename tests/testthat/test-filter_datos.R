source(here::here("app/modules/data_loader.R"))

datos_mock <- dplyr::tibble(
  id_indicador   = c("ARG-IL5", "ARG-IL5", "SFE-RMP"),
  fecha          = as.Date(c("2024-01-01", "2024-02-01", "2024-01-01")),
  valor          = c(1.0, 2.0, 3.0),
  valor_original = c(1.1, 2.1, 3.1)
)

test_that("retorna solo las filas del indicador pedido", {
  resultado <- filter_datos(datos_mock, "ARG-IL5")
  expect_equal(nrow(resultado), 2)
  expect_true(all(resultado$id_indicador == "ARG-IL5"))
})

test_that("retorna cero filas para un id inexistente", {
  resultado <- filter_datos(datos_mock, "ZZZ-XXX")
  expect_equal(nrow(resultado), 0)
})

test_that("no modifica las columnas del tibble", {
  resultado <- filter_datos(datos_mock, "SFE-RMP")
  expect_equal(names(resultado), c("id_indicador", "fecha", "valor", "valor_original"))
})
