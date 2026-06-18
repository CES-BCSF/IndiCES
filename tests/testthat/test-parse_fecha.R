source(here::here("etl/functions.R"))

test_that("convierte meses con formato YYYY.MM", {
  expect_equal(parse_fecha("1994.01"), as.Date("1994-01-01"))
  expect_equal(parse_fecha("2024.12"), as.Date("2024-12-01"))
  expect_equal(parse_fecha("2000.06"), as.Date("2000-06-01"))
})

test_that("normaliza floats: 1994.1 (octubre, no enero)", {
  # Excel puede leer "1994.10" como el float 1994.1
  expect_equal(parse_fecha("1994.1"), as.Date("1994-10-01"))
  expect_equal(parse_fecha("2020.3"),  as.Date("2020-03-01"))
})

test_that("convierte trimestres YYYYT# al primer mes del trimestre", {
  expect_equal(parse_fecha("2007T1"), as.Date("2007-01-01"))
  expect_equal(parse_fecha("2007T2"), as.Date("2007-04-01"))
  expect_equal(parse_fecha("2007T3"), as.Date("2007-07-01"))
  expect_equal(parse_fecha("2007T4"), as.Date("2007-10-01"))
})

test_that("maneja vectores mixtos de meses y trimestres", {
  input     <- c("2020.01", "2020T2", "2020.12")
  resultado <- parse_fecha(input)
  expect_equal(resultado[1], as.Date("2020-01-01"))
  expect_equal(resultado[2], as.Date("2020-04-01"))
  expect_equal(resultado[3], as.Date("2020-12-01"))
})
