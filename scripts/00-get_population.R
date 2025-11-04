population_url <- "https://cdn.www.gob.pe/uploads/document/file/5501252/3464927-anexo-1.xlsx?v=1701294767"
population_filepath <- "data/raw/population_projections.xlsx"
download.file(population_url, population_filepath, mode = "wb")

population_raw <- readxl::read_excel(
  "data/raw/population_projections.xlsx", col_names = FALSE, skip = 5
)

population_2022 <- population_raw |>
  tidyr::drop_na(`...1`, `...2`) |>
  dplyr::filter(!grepl("UBIGEO", `...1`)) |>
  dplyr::filter(stringr::str_sub(`...1`, 5, 6) != "00")

names(population_2022) <- c(
  "ubigeo", "district", "population_2018", "population_2019", "population_2020",
  "population_2021", "population_2022"
)

parent_ubigeo <- readr::read_csv(
  "data/raw/parent_ubigeo_2022.csv", col_types = "ccc",
  col_select = dplyr::starts_with("ubigeo")
)

pop_projections_raw <- population_2022 |>
  dplyr::left_join(parent_ubigeo, by = c("ubigeo" = "ubigeo_2022")) |>
  dplyr::mutate(ubigeo_2017 = ifelse(is.na(ubigeo_2017), ubigeo, ubigeo_2017))

pop_projections <- pop_projections_raw |>
  dplyr::mutate(dplyr::across(dplyr::starts_with("population"), \(x) ifelse(grepl("[a-zA-Z]", x), "0", x))) |>
  dplyr::mutate(dplyr::across(dplyr::starts_with("population"), \(x) as.numeric(x))) |>
  dplyr::group_by(ubigeo_2017) |>
  dplyr::summarise(
    dplyr::across(dplyr::starts_with("population"), \(x) sum(x, na.rm = TRUE))
  ) |>
  dplyr::rename(ubigeo = ubigeo_2017)

pop_projections_long <- pop_projections |>
  tidyr::pivot_longer(
    starts_with("population"),
    names_prefix = "population_",
    names_to = "year",
    values_to = "population"
  ) |>
  dplyr::group_by(ubigeo) |>
  tidyr::complete(year = as.character(2018:2022)) |>
  dplyr::mutate(year = as.integer(year)) |>
  dplyr::ungroup()

readr::write_csv(pop_projections_long, "data/interim/population_2018-2022.csv")


