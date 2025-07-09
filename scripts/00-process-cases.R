library(dplyr)
library(tidyr)

input_path <- "data/raw/positivos_covid-20240208.csv"
cases_raw <- readr::read_delim(input_path, delim = ";", col_types = "cccccicccc")
names(cases_raw) <- tolower(names(cases_raw))
cases <- cases_raw |>
  drop_na(ubigeo, fecha_resultado) |>
  filter(
    between(edad, 0, 123), provincia != "EN INVESTIGACIÓN",
    distrito != "EN INVESTIGACIÓN"
  ) |>
  mutate(dx_date = lubridate::ymd(fecha_resultado)) |>
  filter(dx_date >= as.Date("2020-03-01"))

# Fecha mínima: 2020-03-06, Fecha máxima: 2024-01-20
# Hay más de un tipo de prueba de diagnóstico
# Según El Peruano, la persona más longeva del Perú cumplió 123 años en el 2023
# Algunas personas presentan prueba positiva en menos de 6 meses, según el id_persona

ubigeo <- readr::read_csv(
  "data/raw/population.csv", col_select = ubigeo, col_types = "c") |>
  pull() |>
  unique()

ubigeo_dates_grid <- tidyr::expand_grid(
    ubigeo = ubigeo,
    dx_date = seq(min(cases$dx_date), max(cases$dx_date), by = "day")
  ) |>
  arrange(ubigeo, dx_date)

daily_cases_raw <- cases |>
  group_by(ubigeo, dx_date) |>
  summarise(cases = n(), .groups = "drop")

daily_cases <- ubigeo_dates_grid |>
  left_join(daily_cases_raw, by = c("ubigeo", "dx_date")) |>
  group_by(ubigeo) |>
  complete(dx_date = seq.Date(min(dx_date), max(dx_date), by = "day")) |>
  replace_na(list(cases = 0)) |>
  ungroup()

weekly_cases <- daily_cases |>
  mutate(
    week_start = lubridate::floor_date(dx_date, unit = "week", week_start = 7)
  ) |>
  group_by(ubigeo, week_start) |>
  summarise(cases = sum(cases), .groups = "drop")

output_path <- "data/interim/weekly/cases.csv"
readr::write_csv(weekly_cases, output_path)

