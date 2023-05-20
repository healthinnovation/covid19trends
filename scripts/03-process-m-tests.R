library(dplyr)
library(tidyr)

input_path = "data/raw/pm24Abr2023.csv"

char_cols = c(
  "FECHA_MUESTRA", "UBIGEO_PACIENTE", "DEPARTAMENTO_PACIENTE",
  "PROVINCIA_PACIENTE", "DISTRITO_PACIENTE", "DEPARTAMENTO_MUESTRA",
  "PROVINCIA_MUESTRA","DISTRITO_MUESTRA", "TIPO_MUESTRA", "RESULTADO"
)

tests_raw = data.table::fread(
  input_path, sep = "|", nrows = 10539887,
  select = list(character = char_cols, integer = c("Edad")), na.strings = "",
  fill = TRUE, nThread = 6
)
names(tests_raw) = tolower(names(tests_raw))

tests = tests_raw |>
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

output_path = "data/interim/m-tests.csv"
readr::write_csv(weekly_tests, output_path)
