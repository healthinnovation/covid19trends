library(dplyr)
library(ggplot2)

file_paths = fs::dir_ls("data/interim/weekly")
datasets = purrr::map(file_paths, \(x) readr::read_csv(x, col_types = "Di"))
dataset = purrr::reduce(datasets, \(x, y) inner_join(x, y, by = "week_start"))

dataset_long = tidyr::pivot_longer(dataset, -week_start)

facet_names = c(
  "cases" = "Casos positivos", "deaths" = "Fallecimientos",
  "m_tests" = "Pruebas moleculares", "nm_tests" = "Pruebas no moleculares"
)

time_plot_base = ggplot(dataset_long, aes(week_start, value)) +
  facet_wrap(
    ~name, ncol = 2, scales = "free", labeller = labeller(name = facet_names)
  ) +
  geom_line()

time_plot = time_plot_base +
  scale_x_date(date_breaks = "5 months", date_labels = "%b, %Y") +
  scale_y_continuous(labels = scales::label_comma(), expand = c(0, 0)) +
  labs(y = NULL, x = NULL) +
  theme_minimal() +
  theme(
    axis.line.x = element_line(color = "lightgray"),
    axis.ticks.x = element_line(color = "lightgray"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )

ggsave("figures/time-plot.png", width = 12, height = 6)
