library(dplyr)
library(tidyr)

input_path <- "data/raw/pm24Abr2023.csv"
char_cols <- c(
  "FECHA_MUESTRA", "UBIGEO_PACIENTE", "DEPARTAMENTO_PACIENTE",
  "PROVINCIA_PACIENTE", "DISTRITO_PACIENTE", "DEPARTAMENTO_MUESTRA",
  "PROVINCIA_MUESTRA","DISTRITO_MUESTRA", "TIPO_MUESTRA", "RESULTADO"
)

tests_raw <- data.table::fread(
  input_path, sep = "|", nrows = 10539887,
  select = list(character = char_cols, integer = c("Edad")), na.strings = "",
  fill = TRUE, nThread = 6
)
names(tests_raw) <- tolower(names(tests_raw))
tests <- tests_raw |>
  na.omit() |>
  filter(
    between(edad, 0, 123),
    !(
      departamento_paciente %in% c(
        "[NO DEFINIDO]", "CREADO PARA RRCC (AC", "SIN DATOS"
      )
    ),
    !(
      provincia_paciente %in% c(
        "[NO DEFINIDO]", "CREADO PARA RRCC (ACTAS MANUALES)", "SIN DATOS"
      )
    ),
    !(
      distrito_paciente %in% c(
        "[NO DEFINIDO]", "0001301123392", "CREADO PARA RRCC (ACTAS MANUALES)",
        "SIN DATOS"
      )
    )
  ) |>
  mutate(
    test_date = lubridate::ymd(fecha_muestra)
  ) |>
  filter(lubridate::year(test_date) > 2019) |>
  mutate(
    dx = case_when(
      resultado == "NEGTIVO" ~ "NEGATIVO",
      resultado == "POSTIVO" ~ "POSITIVO",
      resultado == "NEGATI" ~ "NEGATIVO",
      resultado == "- POSITIVO" ~ "POSITIVO",
      grepl("-  POSITIVO -", resultado) ~ "POSITIVO",
      .default = as.character(resultado)
    )
  ) |>
  select(-c(fecha_muestra, resultado))
rm(tests_raw)

tests_20240201_raw <- data.table::fread(
  "data/raw/pm27Sep2023.csv", sep = ";",
  # select = list(character = char_cols, integer = c("Edad")),
  na.strings = "",
  # fill = TRUE,
  nThread = 6
)

names(tests_20240201_raw) <- tolower(names(tests_20240201_raw))

# TODO:Fecha mínima: 2020-01-01, Fecha máxima: 2023-04-24

daily_tests = tests |>
  group_by(test_date) |>
  summarise(m_tests = n()) |>
  complete(test_date = seq(min(test_date), max(test_date), by = "day")) |>
  replace_na(list(m_tests = 0)) |>
  ungroup()

weekly_tests = daily_tests |>
  mutate(
    week_start = lubridate::floor_date(test_date, unit = "week", week_start = 7)
  ) |>
  group_by(week_start) |>
  summarise(m_tests = sum(m_tests), .groups = "drop")

weekly_path = "data/interim/weekly/m-tests.csv"
readr::write_csv(weekly_tests, weekly_path)

daily_tests_type = tests |>
  mutate(type = stringr::str_to_sentence(tipo_muestra)) |>
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

weekly_type_path = "data/interim/weekly-type/m-tests-type.csv"
readr::write_csv(weekly_tests_type, weekly_type_path)
