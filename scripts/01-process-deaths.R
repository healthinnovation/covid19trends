library(dplyr)
library(tidyr)

input_path = "data/raw/fallecidos_covid.csv"
deaths_raw = readr::read_delim(input_path, delim = ";", col_types = "cciccccccc")
names(deaths_raw) = tolower(names(deaths_raw))

deaths = deaths_raw |>
  drop_na() |>
  filter(between(edad_declarada, 0, 123)) |>
  mutate(death_date = lubridate::ymd(fecha_fallecimiento)) |>
  distinct(
    fecha_fallecimiento, edad_declarada, sexo, ubigeo, uuid, .keep_all = TRUE
  )

# Fecha mínima: 2020-03-03, Fecha máxima: 2023-05-12
# Hay más de un tipo de criterio de muerte
# Según El Peruano, la persona más longeva del Perú cumplió 123 años en el 2023
# Hay UUID duplicados

daily_deaths = deaths |>
  group_by(death_date) |>
  summarise(deaths = n()) |>
  complete(death_date = seq(min(death_date), max(death_date), by = "day")) |>
  replace_na(list(deaths = 0)) |>
  ungroup()

weekly_deaths = daily_deaths |>
  mutate(
    week_start = lubridate::floor_date(death_date, unit = "week", week_start = 7)
  ) |>
  group_by(week_start) |>
  summarise(deaths = sum(deaths), .groups = "drop")

output_path = "data/interim/weekly/deaths.csv"
readr::write_csv(weekly_deaths, output_path)
