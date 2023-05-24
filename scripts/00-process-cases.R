library(dplyr)
library(tidyr)

input_path = "data/raw/positivos_covid.csv"
cases_raw = readr::read_delim(input_path, delim = ";", col_types = "cccccicccc")
names(cases_raw) = tolower(names(cases_raw))

cases = cases_raw |>
  drop_na() |>
  filter(
    between(edad, 0, 123), provincia != "EN INVESTIGACIÓN",
    distrito != "EN INVESTIGACIÓN"
  ) |>
  mutate(dx_date = lubridate::ymd(fecha_resultado))

# Fecha mínima: 2020-03-06, Fecha máxima: 2023-05-12
# Hay más de un tipo de prueba de diagnóstico
# Según El Peruano, la persona más longeva del Perú cumplió 123 años en el 2023
# Algunas personas presentan prueba positiva en menos de 6 meses, según el id_persona

daily_cases = cases |>
  group_by(dx_date) |>
  summarise(cases = n()) |>
  complete(dx_date = seq(min(dx_date), max(dx_date), by = "day")) |>
  replace_na(list(cases = 0)) |>
  ungroup()

weekly_cases = daily_cases |>
  mutate(
    week_start = lubridate::floor_date(dx_date, unit = "week", week_start = 7)
  ) |>
  group_by(week_start) |>
  summarise(cases = sum(cases), .groups = "drop")

output_path = "data/interim/weekly/cases.csv"
readr::write_csv(weekly_cases, output_path)
