library(here)

source(here("R/functions.R"))

import_data() |>
  convert_to_improvement_db() |>
  summary_table() |>
  produce_dia_prop_db() |>
  gg_diagnoses() |>
  save_plot_if_interactive()
