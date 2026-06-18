source(here::here("etl/functions.R"))

test_that("clasifica series mensuales", {
  fechas <- seq(as.Date("2020-01-01"), by = "month", length.out = 24)
  expect_equal(infer_frecuencia(fechas), "mensual")
})

test_that("clasifica series trimestrales", {
  fechas <- seq(as.Date("2010-01-01"), by = "quarter", length.out = 20)
  expect_equal(infer_frecuencia(fechas), "trimestral")
})

test_that("clasifica series anuales", {
  fechas <- seq(as.Date("2000-01-01"), by = "year", length.out = 15)
  expect_equal(infer_frecuencia(fechas), "anual")
})

test_that("clasifica series semanales", {
  fechas <- seq(as.Date("2024-01-01"), by = "week", length.out = 52)
  expect_equal(infer_frecuencia(fechas), "semanal")
})
