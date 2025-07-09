vacc_raw <- data.table::fread(
  "data/raw/vacunas_covid.csv",
  select = c(
    "UUID" = "character", "EDAD" = "integer", "FECHA_VACUNACION" = "character",
    "DOSIS" = "integer", "DEPARTAMENTO" = "character"
  )
)
names(vacc_raw) <- tolower(names(vacc_raw))

vacc_20240201_raw <- data.table::fread(
  "data/raw/vacunas_covid_20240201.csv", sep = ";",
  # select = c(
  #   "UUID" = "character", "EDAD" = "integer", "FECHA_VACUNACION" = "character",
  #   "DOSIS" = "integer", "DEPARTAMENTO" = "character"
  # )
)
names(vacc_20240201_raw) <- tolower(names(vacc_20240201_raw))

library(dplyr)

vacc = vacc_raw %>%
  tidyr::drop_na(everything()) |>
  filter(between(edad, 0, 123)) |>
  mutate(
    vacc_date = lubridate::ymd(fecha_vacunacion),
    dosis = case_when(
      dosis > 3 ~ "More than 3",
      .default = as.character(dosis)
    ),
    .keep = "unused"
  ) |>
  select(-edad)

# cluster = new_cluster(4)

daily_vacc = vacc |>
  group_by(department = departamento, vacc_date, dosis) |>
  summarise(vacc = n(), .groups = "drop") |>
  tidyr::complete(
    department,
    vacc_date = seq(min(vacc_date), max(vacc_date), by = "day"),
    dosis,
    fill = list(vacc = 0)
  )

weekly_vacc = daily_vacc |>
  mutate(
    week_start = lubridate::floor_date(vacc_date, unit = "week", week_start = 7),
    .keep = "unused"
  ) |>
  group_by(department, week_start, dosis) |>
  summarise(vacc = sum(vacc), .groups = "drop")

weekly_vacc_path = "data/interim/weekly/vaccinations.csv"
readr::write_csv(weekly_vacc, weekly_vacc_path)
