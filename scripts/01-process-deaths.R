library(dplyr)
library(tidyr)

input_path <- "data/raw/fallecidos_covid-20240208.csv"

# deaths_raw <- readr::read_delim(input_path, delim = ";", col_types = "cciccccccc")
# names(deaths_raw) <- tolower(names(deaths_raw))
# deaths <- deaths_raw |>
#   drop_na(ubigeo, fecha_fallecimiento, uuid) |>
#   filter(between(edad_declarada, 0, 123)) |>
#   mutate(death_date = lubridate::ymd(fecha_fallecimiento), .keep = "unused") |>
#   distinct(
#     death_date, edad_declarada, sexo, ubigeo, uuid, .keep_all = TRUE
#   )

deaths_raw <- readr::read_delim(input_path, delim = ";", col_types = "cciccccccc")
names(deaths_raw) <- tolower(names(deaths_raw))
deaths <- deaths_raw |>
  drop_na(ubigeo, fecha_fallecimiento) |>
  filter(between(edad_declarada, 0, 123)) |>
  mutate(death_date = lubridate::ymd(fecha_fallecimiento), .keep = "unused") |>
  filter(death_date >= as.Date("2020-03-01"))

# Fecha mínima: 2020-03-03, Fecha máxima: 2024-01-21
# Hay más de un tipo de criterio de muerte
# Según El Peruano, la persona más longeva del Perú cumplió 123 años en el 2023
# Hay UUID duplicados

ubigeo <- readr::read_csv(
  "data/raw/population.csv", col_select = ubigeo, col_types = "c") |>
  pull() |>
  unique()

ubigeo_dates_grid <- tidyr::expand_grid(
    ubigeo = ubigeo,
    death_date = seq(min(deaths$death_date), max(deaths$death_date), by = "day")
  ) |>
  arrange(ubigeo, death_date)

daily_deaths_raw <- deaths |>
  group_by(ubigeo, death_date) |>
  summarise(deaths = n(), .groups = "drop")

daily_deaths <- ubigeo_dates_grid |>
  left_join(daily_deaths_raw, by = c("ubigeo", "death_date")) |>
  group_by(ubigeo) |>
  complete(death_date = seq(min(death_date), max(death_date), by = "day")) |>
  replace_na(list(deaths = 0)) |>
  ungroup()

weekly_deaths <- daily_deaths |>
  mutate(
    week_start = lubridate::floor_date(death_date, unit = "week", week_start = 7)
  ) |>
  group_by(ubigeo, week_start) |>
  summarise(deaths = sum(deaths), .groups = "drop")

output_path <- "data/interim/weekly/deaths.csv"
readr::write_csv(weekly_deaths, output_path)
