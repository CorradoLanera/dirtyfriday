list.files(here::here("R"), full.names = TRUE, pattern = "\\.R$") |>
  purrr::walk(source)

baseline <- read_patient_data("baseline.csv")
track <- read_patient_data("track.csv")
log <- read_patient_data("log.csv")
daily <- read_patient_data("daily.csv")

patient_history_plot("TS015", baseline, track, log, daily)
patient_history_plot("NO004", baseline, track, log, daily)
patient_history_plot("TS012", baseline, track, log, daily)
