library(dplyr)
library(tidyr)

input_path <- "data/raw/TB_F100_SICOVID.csv"
col_names <- c(
  "fecha_prueba", "id_tipo_prueba", "id_resultado_prueba", "id_ubigeo_prueba"
)
tests_raw <- data.table::fread(
  input_path, select = list(character = col_names), na.strings = "",
  nThread = 4
)
tests <- tests_raw |>
  na.omit() |>
  mutate(test_date = as.Date(fecha_prueba, format = "%d/%m/%Y")) |>
  select(-fecha_prueba)
rm(tests_raw)

col_names <- c(
  "fecha_prueba", "id_tipo_prueba", "id_resultado_prueba", "id_ubigeo_prueba"
)
tests_raw <- data.table::fread(
  "data/raw/TB_F100_RESULTADOPRUEBA.csv", sep = ";",
  # select = list(character = col_names),
  na.strings = "",
  nThread = 4
)
tests <- tests_raw |>
  na.omit() |>
  mutate(test_date = as.Date(fecha_prueba, format = "%d/%m/%Y")) |>
  select(-fecha_prueba)
rm(tests_raw)

# Fecha mínima: 2020-03-15, Fecha máxima: 2023-05-19

daily_tests = tests |>
  group_by(test_date) |>
  summarise(nm_tests = n()) |>
  complete(test_date = seq(min(test_date), max(test_date), by = "day")) |>
  replace_na(list(nm_tests = 0)) |>
  ungroup()

weekly_tests = daily_tests |>
  mutate(
    week_start = lubridate::floor_date(test_date, unit = "week", week_start = 7)
  ) |>
  group_by(week_start) |>
  summarise(nm_tests = sum(nm_tests), .groups = "drop")

# TODO: Verificar edades

weekly_path = "data/interim/weekly/nm-tests.csv"
readr::write_csv(weekly_tests, weekly_path)

daily_tests_type = tests |>
  mutate(
    type = case_when(
      id_tipo_prueba == 1 ~ "Serológica",
      id_tipo_prueba == 2 ~ "Antígena",
      id_tipo_prueba == 3 ~ "Quimioluminiscencia"
    )
  ) |>
  group_by(type, test_date) |>
  summarise(tests = n(), .groups = "drop") |>
  complete(type, test_date = seq(min(test_date), max(test_date), by = "day")) |>
  replace_na(list(tests = 0))

weekly_tests_type = daily_tests_type |>
  mutate(
    week_start = lubridate::floor_date(test_date, unit = "week", week_start = 7)
  ) |>
  group_by(type, week_start) |>
  summarise(tests = sum(tests), .groups = "drop")

weekly_type_path = "data/interim/weekly-type/nm-tests-type.csv"
readr::write_csv(weekly_tests_type, weekly_type_path)
